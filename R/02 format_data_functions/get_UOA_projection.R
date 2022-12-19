setwd("N:/_Everyone/Mohammed_Emran/UDA_Projection_Monthly/UOA_projection")
library(tidyverse)
library(readxl)
library(DBI)
library(odbc)
library(scales)

pull_UOA_scheduled_data <- function(){
  
  con <- dbConnect(odbc::odbc(), "NCDR")
  
  sql <- "SELECT *
  FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[UOA_scheduled]"
  result <- dbSendQuery(con, sql)
  UOA_scheduled_data <- dbFetch(result)
  dbClearResult(result)
  
  UOA_scheduled_data
}
data_all <- pull_UOA_scheduled_data() %>%
  mutate(month = format(data_month, "%B-%Y")) ## pulling all the UOA scheduled data

## pulling all the UOA target and value data shared by BSA 
data_contracted<- read_excel("BSA_wr_Data_Contract_Value_Delivery_England_202223_202210.xlsx") %>% 
  filter(UOA_Performance_Target>0) %>%
  rename(contract_number  = Contract_Number) %>%
  select (contract_number, UOA_Financial_Value, UOA_Performance_Target)

##sum(data_contracted$UOA_Performance_Target, na.rm=TRUE)

data_wide <- data_all %>%
  select(data_month, contract_number, #name_or_company_name, commissioner_name, region_name, contract_start_date, contract_end_date, annual_contracted_UDA, 
         UOA_delivered) %>%
  filter(data_month >= as.Date("2022-04-01")) %>%
  left_join(data_contracted, by = "contract_number") %>%
  arrange(data_month) %>%
  filter(UOA_Performance_Target>0)%>%
  ##mutate(month = format(data_month, "%B-%Y")) %>%
  pivot_wider(names_from = data_month,
              values_from = UOA_delivered,
              names_prefix = "Delivered UOA in ",
              values_fill = 0) %>%
  mutate_at(vars(starts_with("Delivered UOA")), ~replace_na(.,0))


#get averages
data_means <- data_all %>%
  #filter(month >= lubridate::floor_date(max(data$month) - months(6), "month")) %>%
  filter(data_month > "2022-03-01") %>% 
  mutate(UOA_delivered = replace_na(UOA_delivered, 0)) %>%
  group_by(contract_number) %>%
  summarise(avrg_previous_months_delivered = mean(UOA_delivered, na.rm = TRUE))

#get most recent month's contract details
data_contract_details <- data_all %>%
  filter(data_month == as.Date(max(data_all$data_month))) %>%
  select(contract_number, name_or_company_name, commissioner_name, region_name, contract_start_date, contract_end_date)

#get number of months left in financial year
num_months_left_in_financial_year <- 12 - (as.numeric(substr(max(data_all$data_month), 6, 7)) - 3)

str(data)
#join in UDA means to rest of data
data <- data_wide %>%
  left_join(data_contract_details) %>%
  left_join(data_means, by = "contract_number") %>%
  mutate(projected_of_rest_year_delivery = avrg_previous_months_delivered * num_months_left_in_financial_year) %>%
  mutate(YTD_delivery = rowSums(across(starts_with("Delivered UOA")), na.rm = TRUE)) %>%
  mutate(projected_total_year_delivery = YTD_delivery + projected_of_rest_year_delivery) %>%
  mutate(projected_percentage_delivery_of_contracted_UOAs = 
           (projected_total_year_delivery /UOA_Performance_Target)) %>%
  mutate (projected_financial_value = case_when (is.na(avrg_previous_months_delivered) ~ 0,
                                                 avrg_previous_months_delivered == 0 ~ 0,
                                                 projected_percentage_delivery_of_contracted_UOAs < 100 ~ (UOA_Financial_Value *projected_percentage_delivery_of_contracted_UOAs),
                                                 projected_percentage_delivery_of_contracted_UOAs >= 110 ~ (UOA_Financial_Value *1.1),
                                                 projected_percentage_delivery_of_contracted_UOAs <= 110 ~ ((1-projected_percentage_delivery_of_contracted_UOAs) 
                                                                                                            * UOA_Financial_Value))) %>%  
  mutate(performance_category = case_when(is.na(avrg_previous_months_delivered) ~ 'ignore',
                                          avrg_previous_months_delivered == 0 ~ 'ignore',
                                          projected_percentage_delivery_of_contracted_UOAs < 96 ~ 'Projected to deliver less than 96%',
                                          projected_percentage_delivery_of_contracted_UOAs >= 96 ~ 'Projected to deliver 96% or more')) %>% # changed to >=96 from >95
  mutate_if(is.numeric, ~round(., 2)) %>%
  select(contract_number, name_or_company_name, commissioner_name, region_name, contract_start_date, contract_end_date, everything())

#write.csv (data, "N:/_Everyone/Mohammed_Emran/UDA_Projection_Monthly/UOA_projection/final.csv")
