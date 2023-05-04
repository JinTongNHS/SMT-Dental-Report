library(lubridate)

# 
# files <- list.files(path="data/unique_patients/202004_to_202203", pattern="*.xlsx", full.names=TRUE, recursive=FALSE)
# month <- as.Date("2020-04-01", format = "%Y-%m-%d")
# 
# for (f in files){
#   
#   print(f)
#   
#   #skip first rows
#   f_clean <- read_excel(f, skip = 18)
#   
#   #remove last 7 rows
#   f_clean <- f_clean %>%
#     rename("contract_number" = "Contract Number", 
#            "unique_patients_rolling_12M" = "Unique Patient Count Rolling 12M", 
#            "band1_unique_patients_rolling_12M" = "Band 1 Unique Patient Count Rolling 12M", 
#            "band2_or_3_unique_patients_rolling_12M" = "Band 2 or Band 3 Unique Patient Count Rolling 12M", 
#            "band1_urgent_unique_patients_rolling_12M" = "Band 1 Urgent Unique Patient Count Rolling 12M", 
#            "band_other_unique_patients_rolling_12M" = "Other Unique Patient Count Rolling 12M") %>%
#     filter(!is.na(contract_number)) %>%
#     mutate(month_ending = month)
#   
#   unique_patients_rolling_202004_202203 <- bind_rows(unique_patients_rolling_202004_202203, f_clean)
#   
#   month <- month %m+% period("1 month")
# 
#   
#   }



files <- list.files(path="data/historic_UOA_data_to_load", pattern="*.xlsx", full.names=TRUE, recursive=FALSE)
historical_UOA_data <- data.frame()

for (f in files){
  
  print(f)
  
  #skip first rows
  f_clean <- read_excel(f, skip = 4)
  
  #remove last 7 rows
  f_clean <- f_clean %>%
    select(data_month_condensed = "Year Month",
           contract_number = "Contract Number", 
           contract_type = "Contract Type", 
           name_or_company_name = "Name or Company Name", 
           commissioner_name = "Commissioner Name", 
           commissioner_ods_code_icb = "Commissioner Code", 
           region_code = "Region Code", 
           region_description ="Region Description", 
           paid_by_BSA = "Paid by BSA", 
           contract_start_date = "Contract Start Date", 
           contract_end_date = "Contract End Date", 
           annual_contracted_UOA = "Annual contracted UOA", 
           annual_contracted_UDA = "Annual contracted UDA",
           UOA_delivered = "Monthly UOA Delivered",
           orthodontic_FP17s = "Monthly Orthodontic FP17s",
           orthodontic_starts = "Monthly Orthodontic Starts",
           orthodontic_completions = "Monthly Orthodontic Completions", 
           UOA_financial_value = "UOA Financial Value", 
           monthly_net_payment_to_dental_contract = "Monthly Net Payment To Dental Contract", 
           YTD_UOA_delivered = "YTD UOA Delivered",  
           YTD_percentage_of_UOA_delivered = "UOA YTD % Delivered"
           ) %>%
    filter(!is.na(contract_number)) 
  
  historical_UOA_data <- bind_rows(historical_UOA_data, f_clean)
  
}



historical_UOA_data <- historical_UOA_data %>%
  rename(region_name = region_description,
         data_month = data_month_condensed) %>%
  mutate(data_month = as.Date(paste0(substr(data_month, 1, 4), "-", substr(data_month, 5, 6), "-01"))) 




# ################################################################################
# upload_UOA_historic <- function(){
#   
#   con <- dbConnect(odbc::odbc(), "NCDR")
#   
#   #SELECT *  FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[UDA_scheduled_historical]
#   
#   dbWriteTable(con, Id(catalog="NHSE_Sandbox_PrimaryCareNHSContracts",schema="Dental",table="UOA_scheduled_historical"),
#                value = historical_UOA_data, row.names = FALSE, append = FALSE, overwrite = TRUE)
#   
# }
