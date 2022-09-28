library(tidyverse)
library(readxl)

#load in data
#get list of all the files in the Unplanned closures folder and filter for ones starting with "unpl"
filenames <- list.files(path="data/Historical UDAs 2016 onwards")

#initialise blank data frame to be filled with data from each region
all_UDA_scheduled_historical_data <- data.frame()

for(f in filenames){
  
  data <- read_excel(paste0("data/Historical UDAs 2016 onwards/", f), skip = 3)
  # 
  # data <- read_excel(paste0("N:/_Everyone/Primary Care Group/Unplanned Closures PHARM-2022_23-003/", f), 
  #                    sheet = "data entry", 
  #                    col_types = "text"
  # )
  
  #remove last 4 rows with eDen source
  data <- data[1:(nrow(data) - 4),]
  
  data_year <- substr(f, 22, 25)
  data_month <- substr(f, 26, 27)
  
  data <- data %>%
    mutate(month = as.Date(paste0(data_year, "-", data_month, "-01")))
  
  #for debugging to see which file fails if it does
  print(f)
  
  all_UDA_scheduled_historical_data <- bind_rows(all_UDA_scheduled_historical_data, data) 
  
}


all_UDA_scheduled_historical_data <- all_UDA_scheduled_historical_data %>%
  select(month,
         contract_number = "Contract Number", 
         name_or_company_name = "Name or Company Name", 
         commissioner_name = "Commissioner Name", 
         contract_type = "Contract Type", 
         paid_by_BSA = "Paid by BSA", 
         contract_start_date = "Contract Start Date", 
         contract_end_date = "Contract End Date", 
         annual_contracted_UDA = "Annual contracted UDA", 
         annual_contracted_UOA = "Annual contracted UOA", 
         UDA_delivered = "Total UDA Delivered", 
         UDA_band_1 = "UDA - Band 1", 
         UDA_band_2 = "UDA - Band 2", 
         UDA_band_3 = "UDA - Band 3", 
         UDA_urgent = "UDA - Urgent", 
         UDA_other = "UDA - Other (General)", 
         general_FP17s = "General FP17s", 
         FP17s_band_1 = "FP17s - Band 1", 
         FP17s_band_2 = "FP17s - Band 2", 
         FP17s_band_3 = "FP17s - Band 3", 
         FP17s_band_urgent = "FP17s - Urgent", 
         FP17s_band_other = "FP17s - Other") %>%
  filter(month < as.Date("2021-04-01")) %>%
  mutate(year = case_when(month < as.Date("2017-04-01") ~ "2016/17",
                          month < as.Date("2018-04-01") ~ "2017/18",
                          month < as.Date("2019-04-01") ~ "2018/19",
                          month < as.Date("2020-04-01") ~ "2019/20",
                          month < as.Date("2021-04-01") ~ "2020/21"))


contracted_UDAs <- payments_to_dentists_data %>%
  select(Contract, Total.Contracted.UDA, year) %>%
  mutate(contract_number = str_replace(Contract, "/", "")) %>%
  mutate(contract_number = as.numeric(contract_number),
         Total.Contracted.UDA = as.numeric(Total.Contracted.UDA)) %>%
  select(-Contract)


all_UDA_scheduled_historical_data <- all_UDA_scheduled_historical_data %>%
  left_join(contracted_UDAs, by = c("year", "contract_number"))


all_UDA_scheduled_historical_data <- all_UDA_scheduled_historical_data %>%
  mutate(annual_contracted_UDA = if_else(is.na(annual_contracted_UDA),
                                               Total.Contracted.UDA,
                                               annual_contracted_UDA))

all_UDA_scheduled_historical_data <- all_UDA_scheduled_historical_data %>%
  select(-Total.Contracted.UDA)

ICB_region_lookup <- UDA_calendar_data %>%
  filter(month == as.Date("2022-07-01")) %>%
  select(commissioner_name, region_name) %>%
  unique()

all_UDA_scheduled_historical_data <- all_UDA_scheduled_historical_data %>%
  left_join(ICB_region_lookup, by = "commissioner_name") %>%
  select("month", "contract_number", "name_or_company_name", "commissioner_name", region_name, everything())

