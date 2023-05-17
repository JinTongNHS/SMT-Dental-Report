library(tidyverse)
library(readxl)
library(DBI)
library(odbc)

source("R/01 import_functions/01 import_dental_data_functions.R")
source("R/01 import_functions/02 upload_data_functions.R")

#load in lookup
STP_ICB_lookup_codes <- read_excel("N:/_Everyone/Primary Care Group/SMT_Dental DENT 2022_23-008/data_for_monthly_report/STP_ICB_lookup_codes.xlsx")

#gets latest month 
data_month <- lubridate::floor_date(Sys.Date()  - lubridate::weeks(4), unit = "month")
data_month_name <- format(Sys.Date()  - lubridate::weeks(4), format = "%b%y")

#gets paths pf data to be loaded in
raw_UDA_scheduled_data_folder_path <- "N:/_Everyone/Primary Care Group/SMT_Dental DENT 2022_23-008/raw_eDEN_dental_data/UDA_scheduled_raw_data/"
raw_UOA_scheduled_data_folder_path <- "N:/_Everyone/Primary Care Group/SMT_Dental DENT 2022_23-008/raw_eDEN_dental_data/UOA_scheduled_raw_data/"
raw_UDA_calendar_data_folder_path <- "N:/_Everyone/Primary Care Group/SMT_Dental DENT 2022_23-008/raw_eDEN_dental_data/UDA_calendar_raw_data/"
raw_UOA_calendar_data_folder_path <- "N:/_Everyone/Primary Care Group/SMT_Dental DENT 2022_23-008/raw_eDEN_dental_data/UOA_calendar_raw_data/"

#gets latest cleaned data ready to be uploaded
UDA_scheduled_data_latest <- import_and_clean_scheduled_UDA_data(data_path = paste0(raw_UDA_scheduled_data_folder_path, "UDA_scheduled_", data_month_name,".xlsx"),
                                                                 data_date = data_month)
UOA_scheduled_data_latest <- import_and_clean_scheduled_UOA_data(data_path = paste0(raw_UOA_scheduled_data_folder_path, "UOA_scheduled_", data_month_name,".xlsx"),
                                                                 data_date = data_month)
UDA_calendar_data_latest <- import_and_clean_calendar_UDA_data(data_path = paste0(raw_UDA_calendar_data_folder_path, "UDA_calendar_Apr_", data_month_name,".xlsx"))

UOA_calendar_data_latest <- import_and_clean_calendar_UOA_data(data_path = paste0(raw_UOA_calendar_data_folder_path, "UOA_calendar_Apr_", data_month_name,".xlsx"))

unique_patients_latest <- import_and_clean_unique_patients_data(data_path = "N:/_Everyone/Primary Care Group/SMT_Dental DENT 2022_23-008/unique_patients/",
                                                                commissioner_lookup = STP_ICB_lookup_codes)

#upload activity data
upload_scheduled_data(UDA_latest = UDA_scheduled_data_latest,
                      UOA_latest = UOA_scheduled_data_latest)

upload_calendar_data(UDA_latest = UDA_calendar_data_latest,
                     UOA_latest = UOA_calendar_data_latest)

#upload unique patients data
upload_unique_patients_data(unique_patients = unique_patients_latest)

###### To be run after the rmarkdown is run ####################################
# #Upload metrics
upload_delivery_metrics()
update_SOF_table_S109()
upload_unique_patients_metric()

#save spreadsheet of SOF metric in shared location
source("R/02 format_data_functions/pull_PCDID_data.R")
SOF_data <- get_SOF_spreadsheet_data()
SOF_109_numerator <- SOF_data[["numerator"]]
SOF_109_denominator <- SOF_data[["denominator"]]
SOF_109_rate <- SOF_data[["rate"]]

writexl::write_xlsx(SOF_109_numerator, paste0("N:/_Everyone/Primary Care Group/SOF_outputs/SOF_109_numerator_", Sys.Date(), ".xlsx"))
writexl::write_xlsx(SOF_109_denominator, paste0("N:/_Everyone/Primary Care Group/SOF_outputs/SOF_109_denominatorr_", Sys.Date(), ".xlsx"))
writexl::write_xlsx(SOF_109_rate, paste0("N:/_Everyone/Primary Care Group/SOF_outputs/SOF_109_rate_", Sys.Date(), ".xlsx"))