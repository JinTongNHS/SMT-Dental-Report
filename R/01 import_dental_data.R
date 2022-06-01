library(tidyverse)
library(readxl)
library(DBI)
library(odbc)

################################################################################
import_clean_combine_upload_all_data <- function(raw_UDA_scheduled_data_folder_path = "N:/_Everyone/Primary Care Group/SMT_Dental/raw_eDEN_dental_data/UDA_scheduled_raw_data/",
                                             raw_UOA_scheduled_data_folder_path = "N:/_Everyone/Primary Care Group/SMT_Dental/raw_eDEN_dental_data/UOA_scheduled_raw_data/",
                                             raw_UDA_calendar_data_folder_path = "N:/_Everyone/Primary Care Group/SMT_Dental/raw_eDEN_dental_data/UDA_calendar_raw_data/",
                                             raw_UOA_calendar_data_folder_path = "N:/_Everyone/Primary Care Group/SMT_Dental/raw_eDEN_dental_data/UOA_calendar_raw_data/"){

  # Gets last month 
  data_month <- lubridate::floor_date(Sys.Date()  - lubridate::weeks(4), unit = "month")
  data_month_name <- format(Sys.Date()  - lubridate::weeks(4), format = "%b%y")
  
  #UDA scheduled data
  UDA_scheduled_data_latest <- import_and_clean_scheduled_UDA_data(data_path = paste0(raw_UDA_scheduled_data_folder_path, "UDA_scheduled_", data_month_name,".xlsx"),
                                                                 data_date = data_month)

  #UOA scheduled data
  UOA_scheduled_data_latest <- import_and_clean_scheduled_UOA_data(data_path = paste0(raw_UOA_scheduled_data_folder_path, "UOA_scheduled_", data_month_name,".xlsx"),
                                                                   data_date = data_month)

  #UDA calendar data
  UDA_calendar_data_latest <- import_and_clean_calendar_UDA_data(data_path = paste0(raw_UDA_calendar_data_folder_path, "UDA_calendar_Apr_", data_month_name,".xlsx"))
  
  #UOA calendar data
  UOA_calendar_data_latest <- import_and_clean_calendar_UOA_data(data_path = paste0(raw_UOA_calendar_data_folder_path, "UOA_calendar_Apr_", data_month_name,".xlsx"))
  
  #Append schedule data and overwrite calendar data in NCDR
  con <- dbConnect(odbc::odbc(), "NCDR")
  
  #Append UDA scheduled data
  dbWriteTable(con, Id(catalog="NHSE_Sandbox_PrimaryCareNHSContracts",schema="Dental",table="UDA_scheduled"),
               value = UDA_scheduled_data_latest, row.names = FALSE, append=TRUE)

  #Append UOA scheduled data
  dbWriteTable(con, Id(catalog="NHSE_Sandbox_PrimaryCareNHSContracts",schema="Dental",table="UOA_scheduled"),
               value = UOA_scheduled_data_latest, row.names = FALSE, append=TRUE)

  #Get UDA calendar table and overwrite this financial year
  sql <- "SELECT [data_month]
      ,[contract_number]
      ,[latest_contract_type]
      ,[name_or_company_name]
      ,[commissioner_name]
      ,[region_name]
      ,[paid_by_BSA]
      ,[contract_start_date]
      ,[contract_end_date]
      ,[UDA_total]
      ,[UDA_band_1_total]
      ,[UDA_band_2_total]
      ,[UDA_band_3_total]
      ,[UDA_urgent_total]
      ,[UDA_other_total]
      ,[total_FP17s]
      ,[total_band_1_FP17s]
      ,[total_band_2_FP17s]
      ,[total_band_3_FP17s]
      ,[total_urgent_FP17s]
      ,[total_other_FP17s]
  FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[UDA_calendar]"
  result <- dbSendQuery(con, sql)
  UDA_calendar_data <- dbFetch(result)
  dbClearResult(result)

  UDA_calendar_data_latest <- UDA_calendar_data %>%
    filter(data_month < as.Date("2022-04-01")) %>%
    bind_rows(UDA_calendar_data_latest)

  dbWriteTable(con, Id(catalog="NHSE_Sandbox_PrimaryCareNHSContracts",schema="Dental",table="UDA_calendar"),
               value = UDA_calendar_data_latest, row.names = FALSE, append = FALSE, overwrite = TRUE)


  #Get UOA table and overwrite this financial year
  sql <- "SELECT [data_month]
      ,[contract_number]
      ,[contract_type]
      ,[name_or_company_name]
      ,[commissioner_name]
      ,[paid_by_BSA]
      ,[contract_start_date]
      ,[contract_end_date]
      ,[UOA_total]
  FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[UOA_calendar]"

  result <- dbSendQuery(con, sql)
  UOA_calendar_data <- dbFetch(result)
  dbClearResult(result)

  UOA_calendar_data_latest <- UOA_calendar_data %>%
    filter(data_month < as.Date("2022-04-01")) %>%
    bind_rows(UOA_calendar_data_latest)

  dbWriteTable(con, Id(catalog="NHSE_Sandbox_PrimaryCareNHSContracts",schema="Dental",table="UOA_calendar"),
               value = UOA_calendar_data_latest, row.names = FALSE, append = FALSE, overwrite = TRUE)
  

}



################################################################################
#function to import and clean data
#N.B. this assumes that the columns are in the same order each time!
import_and_clean_scheduled_UDA_data <- function(data_path = "data/raw_data/dashboard_raw_data/UDA_scheduled_raw_data/UDA_scheduled_Apr21.xlsx",
                                            data_date = as.Date("2021-04-01")){
  
  #read in data with correct types and removing top 3 rows and renaming columns 
  data <- read_excel(data_path,
                     col_types = c("numeric", "text", "text",
                                   "text", "text", "date", "date", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric"), 
                     skip = 3,
                     col_names = TRUE,
                     .name_repair = ~ paste0("X__", seq_along(.x)))
  
  #remove last 4 rows with eDen source
  data <- data[1:(nrow(data) - 4),]
  
  #check format of data - manual check should be done to see if columns are in the same order as expected
  if(ncol(data) != 46){
    print("WARNING: The data you have loaded is not in the usual format. Please check.")
  }
  
  #add column for date and rename columns
  data <- data %>%
    mutate(data_month = data_date) %>%
    select(data_month, everything()) %>%
    rename(contract_number = X__1,
           name_or_company_name = X__2,
           commissioner_name = X__3,
           contract_type = X__4,
           paid_by_BSA = X__5,
           contract_start_date = X__6,
           contract_end_date = X__7,
           annual_contracted_UDA = X__8,
           annual_contracted_UOA = X__9,
           UDA_financial_half_target = X__10, #may need to remove this
           UDA_delivered = X__11,
           general_FP17s = X__12,
           UDA_delivered_prev_year = X__13, #remove
           general_FP17s_prev_year = X__14, #remove
           UDA_delivered_prev_2_year = X__15, #remove        
           general_FP17s_prev_2_year = X__16, #remove
           UDA_band_1 = X__17,
           UDA_band_2 = X__18,         
           UDA_band_3 = X__19,
           UDA_urgent = X__20,
           UDA_other = X__21,
           FP17s_band_1 = X__22,
           FP17s_band_2 = X__23,
           FP17s_band_3 = X__24,
           FP17s_band_urgent = X__25,
           FP17s_band_other = X__26,
           UDA_band_1_prev_year = X__27, #remove
           UDA_band_2_prev_year = X__28, #remove
           UDA_band_3_prev_year = X__29, # remove
           UDA_urgent_prev_year = X__30, # remove
           UDA_other_prev_year = X__31, #remove
           FP17s_band_1_prev_year = X__32, #remove
           FP17s_band_2_prev_year = X__33, #remove
           FP17s_band3_prev_year = X__34, #remove
           FP17s_urgent_prev_year = X__35, #remove
           FP17s_other_prev_year = X__36, #remove
           UDA_band_1_prev_2_year = X__37, #remove
           UDA_band_2_prev_2_year = X__38, #remove
           UDA_band_3_prev_2_year = X__39, #remove
           UDA_urgent_prev_2_year = X__40, #remove
           UDA_other_prev_2_year = X__41, #remove
           FP17s_band_1_prev_2_year = X__42, #remove
           FP17s_band_2_prev_2_year = X__43, #remove
           FP17s_band_3_prev_2_year = X__44, #remove
           FP17s_urgent_prev_2_year = X__45, #remove
           FP17s_other_prev_2_year = X__46  #remove
    ) %>%
    select(-UDA_financial_half_target) 
  
}


################################################################################
#function to import and clean data
#N.B. this assumes that the columns are in the same order each time!
import_and_clean_calendar_UDA_data <- function(data_path = "data/raw_data/dashboard_raw_data/UDA_calendar_raw_data/UDA_calendar_Apr_Aug21.xlsx"){
  
  #read in data with correct types and removing top 6 rows and renaming columns 
  data <- read_excel(data_path,
                     col_names = FALSE, 
                     skip = 6,
                     .name_repair = ~ paste0("X__", seq_along(.x)))
  
  #convert date column into dates
  data <- data %>%
    mutate(X__7 = as.Date(X__7),
           X__8 = as.Date(X__8))
  
  #add column for date and rename columns, split data for just april
  data_apr <- data %>%
    mutate(data_month = as.Date("2022-04-01")) %>%
    select(data_month, X__1, X__2, X__3, X__4, X__5, X__6, X__7, X__8, X__9, X__10, 
           X__11, X__12, X__13, X__14, X__15, X__16, X__17, X__18, X__19, X__20) %>%
    rename(contract_number = X__1,
           latest_contract_type = X__2,
           name_or_company_name = X__3,
           commissioner_name = X__4,
           region_name = X__5,
           paid_by_BSA = X__6,
           contract_start_date = X__7, 
           contract_end_date = X__8, 
           
           UDA_total = X__9, 
           UDA_band_1_total = X__10,
           UDA_band_2_total = X__11,
           UDA_band_3_total = X__12, 
           UDA_urgent_total = X__13, 
           UDA_other_total = X__14, 
           total_FP17s = X__15, 
           total_band_1_FP17s  = X__16,
           total_band_2_FP17s = X__17,
           total_band_3_FP17s = X__18,
           total_urgent_FP17s = X__19,
           total_other_FP17s = X__20
    ) 
  
  #add column for date and rename columns, split data for just may
  data_may <- data %>%
    mutate(data_month = as.Date("2022-05-01")) %>%
    select(data_month, X__1, X__2, X__3, X__4, X__5, X__6, X__7, X__8, 
           X__21, X__22, X__23, X__24, X__25, X__26, X__27, X__28, X__29, X__30, X__31, X__32) %>%
    rename(contract_number = X__1,
           latest_contract_type = X__2,
           name_or_company_name = X__3,
           commissioner_name = X__4,
           region_name = X__5,
           paid_by_BSA = X__6,
           contract_start_date = X__7, 
           contract_end_date = X__8, 
           
           UDA_total = X__21, 
           UDA_band_1_total = X__22,
           UDA_band_2_total = X__23,
           UDA_band_3_total = X__24, 
           UDA_urgent_total = X__25, 
           UDA_other_total = X__26, 
           total_FP17s = X__27, 
           total_band_1_FP17s  = X__28,
           total_band_2_FP17s = X__29,
           total_band_3_FP17s = X__30,
           total_urgent_FP17s = X__31,
           total_other_FP17s = X__32
    ) 
  
  
  UDA_calendar_data <- bind_rows(data_apr, data_may)
  
}




################################################################################
#function to import and clean data
#N.B. this assumes that the columns are in the same order each time!
import_and_clean_calendar_UOA_data <- function(data_path = "data/raw_data/dashboard_raw_data/UOA_calendar_raw_data/UOA_calendar_Apr_Dec21.xlsx"){
  
  #read in data with correct types and removing top 6 rows and renaming columns 
  data <- read_excel(data_path,
                     col_names = FALSE, 
                     skip = 3,
                     .name_repair = ~ paste0("X__", seq_along(.x)))
  
  data <- data %>%
    mutate(X__6 = as.Date(X__6),
           X__7 = as.Date(X__7))
  
  #remove last 4 rows with eDen source
  #data <- data[1:(nrow(data) - 4),]
  
  
  #add column for date and rename columns, split data for just april
  data_apr <- data %>%
    mutate(data_month = as.Date("2022-04-01")) %>%
    select(data_month, X__1, X__2, X__3, X__4, X__5, X__6, X__7, 
           X__8) %>%
    rename(contract_number = X__1,
           contract_type = X__2,
           name_or_company_name = X__3,
           commissioner_name = X__4,
           paid_by_BSA = X__5,
           contract_start_date = X__6, 
           contract_end_date = X__7, 
           
           UOA_total = X__8
    )
  
  #add column for date and rename columns, split data for just may
  data_may <- data %>%
    mutate(data_month = as.Date("2022-05-01")) %>%
    select(data_month, X__1, X__2, X__3, X__4, X__5, X__6, X__7, 
           X__9) %>%
    rename(contract_number = X__1,
           contract_type = X__2,
           name_or_company_name = X__3,
           commissioner_name = X__4,
           paid_by_BSA = X__5,
           contract_start_date = X__6, 
           contract_end_date = X__7, 
           
           UOA_total = X__9
    )
  
  UOA_calendar_data <- bind_rows(data_apr, data_may)
  
}



################################################################################
#function to import and clean data
#N.B. this assumes that the columns are in the same order each time!
import_and_clean_scheduled_UOA_data <- function(data_path = "data/raw_data/dashboard_raw_data/UOA_scheduled_raw_data/UOA_scheduled_Apr21.xlsx",
                                                data_date = as.Date("2021-04-01")){
  
  #read in data with correct types and removing top 3 rows and renaming columns 
  data <- read_excel(data_path, 
                     col_types = c("numeric", "text", "text",
                                   "text", "text", "date", "date", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric"), 
                     skip = 3,
                     col_names = TRUE,
                     .name_repair = ~ paste0("X__", seq_along(.x)))
  
  #remove last 4 rows with eDen source
  data <- data[1:(nrow(data) - 4),]
  
  #check format of data - manual check should be done to see if columns are in the same order as expected
  if(ncol(data) != 18){
    print("WARNING: The data you have loaded is not in the usual format. Please check.")
  }
  
  #add column for date and rename columns
  data <- data %>%
    mutate(data_month = data_date) %>%
    select(data_month, everything()) %>%
    rename(contract_number = X__1,
           contract_type = X__2,
           name_or_company_name = X__3,
           commissioner_name = X__4,
           paid_by_BSA = X__5,
           contract_start_date = X__6,
           contract_end_date = X__7,
           annual_contracted_UOA = X__8,
           annual_contracted_UDA = X__9,
           UOA_financial_half_target = X__10, #may need to remove this
           UOA_delivered = X__11,
           UOA_delivered_prev_year = X__12, #may need to remove
           UOA_delivered_prev_2_year = X__13, #may need to remove
           orthodontic_FP17s = X__14, 
           orthodontic_FP17s_prev_year = X__15, #remove        
           orthodontic_FP17s_prev_2_year = X__16, #remove
           orthodontic_starts = X__17,
           orthodontic_completions = X__18 
    )
  
}


################################################################################
#function to import and clean data
#N.B. this assumes that the columns are in the same order each time!
import_and_clean_historical_scheduled_data <- function(data_path = "data/raw_data/dashboard_raw_data/UDA_calendar_raw_data/UDA_calendar_Apr_Aug21.xlsx"){
  
  #read in data with correct types and removing top 6 rows and renaming columns 
  data <- read_excel("data/historical_UDA_scheduled_data.xlsx", 
                    sheet = "Sheet6", 
                    skip = 2,
                    .name_repair = ~ paste0("X__", seq_along(.x)))

  
  #add column for date and rename columns, split data for just april
  data_apr <- data %>%
    mutate(data_month = as.Date("2020-04-01")) %>%
    select(data_month, X__1, 
           X__2, X__3, X__4, X__5, X__6) %>%
    rename(contract_number = X__1,
           band1_FP17 = X__2,
           band2_FP17 = X__3,
           band3_FP17 = X__4,
           other_FP17 = X__5,
           urgent_FP17 = X__6
    ) 
  
  #add column for date and rename columns, split data for just may
  data_may <- data %>%
    mutate(data_month = as.Date("2020-05-01")) %>%
    select(data_month, X__1, 
           X__7, X__8, X__9, X__10, X__11) %>%
    rename(contract_number = X__1,
           band1_FP17 = X__7,
           band2_FP17 = X__8,
           band3_FP17 = X__9,
           other_FP17 = X__10,
           urgent_FP17 = X__11
    )
  
  #add column for date and rename columns, split data for just june
  data_jun <- data %>%
    mutate(data_month = as.Date("2020-06-01")) %>%
    select(data_month, X__1, 
           X__12, X__13, X__14, X__15, X__16) %>%
    rename(contract_number = X__1,
           band1_FP17 = X__12,
           band2_FP17 = X__13,
           band3_FP17 = X__14,
           other_FP17 = X__15,
           urgent_FP17 = X__16
    )
  
  #add column for date and rename columns, split data for just july
  data_jul <- data %>%
    mutate(data_month = as.Date("2020-07-01")) %>%
    select(data_month, X__1, 
           X__17, X__18, X__19, X__20, X__21) %>%
    rename(contract_number = X__1,
           band1_FP17 = X__17,
           band2_FP17 = X__18,
           band3_FP17 = X__19,
           other_FP17 = X__20,
           urgent_FP17 = X__21
    )
  
  #add column for date and rename columns, split data for just august
  data_aug <- data %>%
    mutate(data_month = as.Date("2020-08-01")) %>%
    select(data_month, X__1, 
           X__22, X__23, X__24, X__25, X__26) %>%
    rename(contract_number = X__1,
           band1_FP17 = X__22,
           band2_FP17 = X__23,
           band3_FP17 = X__24,
           other_FP17 = X__25,
           urgent_FP17 = X__26
    )
  
  #add column for date and rename columns, split data for just september
  data_sep <- data %>%
    mutate(data_month = as.Date("2020-09-01")) %>%
    select(data_month, X__1, 
           X__27, X__28, X__29, X__30, X__31) %>%
    rename(contract_number = X__1,
           band1_FP17 = X__27,
           band2_FP17 = X__28,
           band3_FP17 = X__29,
           other_FP17 = X__30,
           urgent_FP17 = X__31
    )
  
  #add column for date and rename columns, split data for just october
  data_oct <- data %>%
    mutate(data_month = as.Date("2020-10-01")) %>%
    select(data_month, X__1, 
           X__32, X__33, X__34, X__35, X__36) %>%
    rename(contract_number = X__1,
           band1_FP17 = X__32,
           band2_FP17 = X__33,
           band3_FP17 = X__34,
           other_FP17 = X__35,
           urgent_FP17 = X__36
    )
  
  #add column for date and rename columns, split data for just november
  data_nov <- data %>%
    mutate(data_month = as.Date("2020-11-01")) %>%
    select(data_month, X__1, 
           X__37, X__38, X__39, X__40, X__41) %>%
    rename(contract_number = X__1,
           band1_FP17 = X__37,
           band2_FP17 = X__38,
           band3_FP17 = X__39,
           other_FP17 = X__40,
           urgent_FP17 = X__41
    )
  historical_UDA_scheduled_data <- bind_rows(data_apr, data_may, data_jun, data_jul, data_aug, 
                                              data_sep, data_oct, data_nov)
  
}



#