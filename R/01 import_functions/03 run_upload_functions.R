library(tidyverse)
library(readxl)
library(DBI)
library(odbc)

##source("R/01 import_functions/01 import_dental_data_functions.R")
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
# 
# band2_split_latest <- import_and_clean_band2_split_data(data_path = "N:/_Everyone/Primary Care Group/SMT_Dental DENT 2022_23-008/data_for_monthly_report/band_2_split_data/")

#upload activity data
upload_UDA_scheduled_data(UDA_latest = UDA_scheduled_data_latest)

upload_UOA_scheduled_data(UOA_latest = UOA_scheduled_data_latest)

upload_UDA_calendar_data(UDA_latest = UDA_calendar_data_latest)

upload_UOA_calendar_data(UOA_latest = UOA_calendar_data_latest)

#upload unique patients data
upload_unique_patients_data(unique_patients = unique_patients_latest)

#upload band 2 split data
##upload_band2_split_data(band2_split = band2_split_latest)

################################################################################
# Pulls in new data before metric tables can be uploaded

            library(tidyverse)
            library(readxl)
            library(DBI)
            library(odbc)
            library(reactable) #make sure you have the latest version by doing install.packages("reactable")
            library(downloadthis)
            library(lubridate)
            
            #source in all file from plot folder
            for (f in list.files(path = "../R/03 plot_functions/", pattern="*.R$")) {
              source(paste0("../R/03 plot_functions/", f))
            }
            
            for (f in list.files(path = "../R/02 format_data_functions/", pattern="*.R$")) {
              source(paste0("../R/02 format_data_functions/", f))
            }
            
            
            #pull data from NCDR
            UDA_calendar_data <- pull_UDA_calendar_data()
            UOA_calendar_data <- pull_UOA_calendar_data()
            UDA_scheduled_data <- pull_UDA_scheduled_data()
            UOA_scheduled_data <- pull_UOA_scheduled_data()
            payments_to_dentists <- pull_payments_to_dentist()
            
            historical_UDA_scheduled_data <- pull_UDA_scheduled_historical_data()
            historical_UDA_scheduled_data <- rename(historical_UDA_scheduled_data, month = data_month)
            
            historical_UOA_scheduled_data <- pull_UOA_scheduled_historical_data()
            historical_UOA_scheduled_data <- rename(historical_UOA_scheduled_data, month = data_month)
            
            #change column names and add regions
            UDA_calendar_data <- rename(UDA_calendar_data, month = data_month)
            UOA_calendar_data <- UOA_calendar_data %>%
              rename(month = data_month) %>%
              mutate(contract_number = as.numeric(contract_number),
                     UOA_total = as.numeric(UOA_total))
            
            
            # UOA_calendar_data <- left_join( region_STP_lookup, by = c("contract_number", "name_or_company_name", "commissioner_name"))
            UDA_scheduled_data <- rename(UDA_scheduled_data, month = data_month)
            UOA_scheduled_data <- rename(UOA_scheduled_data, month = data_month)
            
            UDA_scheduled_data <- mutate(UDA_scheduled_data, month = as.Date(month))
            UOA_scheduled_data <- mutate(UOA_scheduled_data, month = as.Date(month))
            
            #sort out STP ICB changes
            STP_ICB_lookup_codes <- read_excel("N:/_Everyone/Primary Care Group/SMT_Dental DENT 2022_23-008/data_for_monthly_report/STP_ICB_lookup_codes.xlsx")
            
            #Read local 111 file
            dental_data_111 <- read_excel("N:/_Everyone/Primary Care Group/SMT_Dental DENT 2022_23-008/111_pathways_data/dental_data_111.xlsx")
            
            #############still to fix
            prototype_contracts <- read_excel("N:/_Everyone/Primary Care Group/SMT_Dental DENT 2022_23-008/data_for_monthly_report/prototype_contracts.xlsx")
            contractor_categories <- read_excel("N:/_Everyone/Primary Care Group/SMT_Dental DENT 2022_23-008/data_for_monthly_report/contractor_categories_Jan23.xlsx")
            contractor_categories <- contractor_categories %>%
              mutate(contract_number = as.numeric(contract_number))
            contract_demographics <- readRDS("N:/_Everyone/Primary Care Group/SMT_Dental DENT 2022_23-008/data_for_monthly_report/contract_demographics.rds")
            
            #read in unique patients
            #unique_patients_rolling <- readRDS("../data/unique_patients/unique_patients_rolling.rds")
            unique_patients_rolling <- pull_unique_patients_data()




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