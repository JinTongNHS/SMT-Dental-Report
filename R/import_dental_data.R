library(tidyverse)
library(readxl)

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
    mutate(month = data_date) %>%
    select(month, everything()) %>%
    rename(contract_number = X__1,
           name_or_company_name = X__2,
           commissioner_name = X__3,
           contract_type = X__4,
           paid_by_BSA = X__5,
           contract_start_date = X__6,
           contract_end_date = X__7,
           annual_contracted_UDA = X__8,
           annual_contracted_UOA = X__9,
           UDA_target_60_for_Apr_to_Sept = X__10, #may need to remove this
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
    )
  
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
    mutate(month = data_date) %>%
    select(month, everything()) %>%
    rename(contract_number = X__1,
           contract_type = X__2,
           name_or_company_name = X__3,
           commissioner_name = X__4,
           paid_by_BSA = X__5,
           contract_start_date = X__6,
           contract_end_date = X__7,
           annual_contracted_UOA = X__8,
           annual_contracted_UDA = X__9,
           UOA_target_80_for_Apr_to_Sept = X__10, #may need to remove this
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
    mutate(month = as.Date("2021-04-01")) %>%
    select(month, X__1, X__2, X__3, X__4, X__5, X__6, X__7, X__8, X__9, X__10, 
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
    mutate(month = as.Date("2021-05-01")) %>%
    select(month, X__1, X__2, X__3, X__4, X__5, X__6, X__7, X__8, 
           X__21, X__22, X__23, X__24, X__25, X__26, X__27, X__28, X__29, X__30,
           X__31, X__32) %>%
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
  
  #add column for date and rename columns, split data for just june
  data_jun <- data %>%
    mutate(month = as.Date("2021-06-01")) %>%
    select(month, X__1, X__2, X__3, X__4, X__5, X__6, X__7, X__8, 
           X__33, X__34, X__35, X__36, X__37, X__38, X__39, X__40, X__41, X__42,
           X__43, X__44) %>%
    rename(contract_number = X__1,
           latest_contract_type = X__2,
           name_or_company_name = X__3,
           commissioner_name = X__4,
           region_name = X__5,
           paid_by_BSA = X__6,
           contract_start_date = X__7, 
           contract_end_date = X__8, 
           
           UDA_total = X__33, 
           UDA_band_1_total = X__34,
           UDA_band_2_total = X__35,
           UDA_band_3_total = X__36, 
           UDA_urgent_total = X__37, 
           UDA_other_total = X__38, 
           total_FP17s = X__39, 
           total_band_1_FP17s  = X__40,
           total_band_2_FP17s = X__41,
           total_band_3_FP17s = X__42,
           total_urgent_FP17s = X__43,
           total_other_FP17s = X__44
    )
  
  #add column for date and rename columns, split data for just july
  data_jul <- data %>%
    mutate(month = as.Date("2021-07-01")) %>%
    select(month, X__1, X__2, X__3, X__4, X__5, X__6, X__7, X__8, 
           X__45, X__46, X__47, X__48, X__49, X__50, X__51, X__52, X__53, X__54,
           X__55, X__56) %>%
    rename(contract_number = X__1,
           latest_contract_type = X__2,
           name_or_company_name = X__3,
           commissioner_name = X__4,
           region_name = X__5,
           paid_by_BSA = X__6,
           contract_start_date = X__7, 
           contract_end_date = X__8, 
           
           UDA_total = X__45, 
           UDA_band_1_total = X__46,
           UDA_band_2_total = X__47,
           UDA_band_3_total = X__48, 
           UDA_urgent_total = X__49, 
           UDA_other_total = X__50, 
           total_FP17s = X__51, 
           total_band_1_FP17s  = X__52,
           total_band_2_FP17s = X__53,
           total_band_3_FP17s = X__54,
           total_urgent_FP17s = X__55,
           total_other_FP17s = X__56
    )
  
  #add column for date and rename columns, split data for just august
  data_aug <- data %>%
    mutate(month = as.Date("2021-08-01")) %>%
    select(month, X__1, X__2, X__3, X__4, X__5, X__6, X__7, X__8, 
           X__57, X__58, X__59, X__60, X__61, X__62, X__63, X__64, X__65, X__66,
           X__67, X__68) %>%
    rename(contract_number = X__1,
           latest_contract_type = X__2,
           name_or_company_name = X__3,
           commissioner_name = X__4,
           region_name = X__5,
           paid_by_BSA = X__6,
           contract_start_date = X__7, 
           contract_end_date = X__8, 
           
           UDA_total = X__57, 
           UDA_band_1_total = X__58,
           UDA_band_2_total = X__59,
           UDA_band_3_total = X__60, 
           UDA_urgent_total = X__61, 
           UDA_other_total = X__62, 
           total_FP17s = X__63, 
           total_band_1_FP17s  = X__64,
           total_band_2_FP17s = X__65,
           total_band_3_FP17s = X__66,
           total_urgent_FP17s = X__67,
           total_other_FP17s = X__68
    )
  
  #add column for date and rename columns, split data for just september
  data_sep <- data %>%
    mutate(month = as.Date("2021-09-01")) %>%
    select(month, X__1, X__2, X__3, X__4, X__5, X__6, X__7, X__8, 
           X__69, X__70, X__71, X__72, X__73, X__74, X__75, X__76, X__77, X__78,
           X__79, X__80) %>%
    rename(contract_number = X__1,
           latest_contract_type = X__2,
           name_or_company_name = X__3,
           commissioner_name = X__4,
           region_name = X__5,
           paid_by_BSA = X__6,
           contract_start_date = X__7, 
           contract_end_date = X__8, 
           
           UDA_total = X__69, 
           UDA_band_1_total = X__70,
           UDA_band_2_total = X__71,
           UDA_band_3_total = X__72, 
           UDA_urgent_total = X__73, 
           UDA_other_total = X__74, 
           total_FP17s = X__75, 
           total_band_1_FP17s  = X__76,
           total_band_2_FP17s = X__77,
           total_band_3_FP17s = X__78,
           total_urgent_FP17s = X__79,
           total_other_FP17s = X__80
    )
  
  UDA_calendar_data <- bind_rows(data_apr, data_may, data_jun, data_jul, data_aug, data_sep)
  
}



################################################################################
#function to import and clean data
#N.B. this assumes that the columns are in the same order each time!
import_and_clean_calendar_UOA_data <- function(data_path = "data/raw_data/dashboard_raw_data/UOA_calendar_raw_data/UOA_calendar_Apr_Aug21.xlsx"){
  
  #read in data with correct types and removing top 6 rows and renaming columns 
  data <- read_excel(data_path,
                     col_names = FALSE, 
                     skip = 6,
                     .name_repair = ~ paste0("X__", seq_along(.x)))
  
  #remove last 4 rows with eDen source
  data <- data[1:(nrow(data) - 4),]
  
  #convert date column into dates
  data <- data %>%
    mutate(X__7 = as.Date(X__7),
           X__8 = as.Date(X__8))
  
  #add column for date and rename columns, split data for just april
  data_apr <- data %>%
    mutate(month = as.Date("2021-04-01")) %>%
    select(month, X__1, X__2, X__3, X__4, X__5, X__6, X__7, X__8, X__9, X__10) %>%
    rename(contract_number = X__1,
           contract_type = X__2,
           name_or_company_name = X__3,
           commissioner_name = X__4,
           region_name = X__5,
           paid_by_BSA = X__6,
           contract_start_date = X__7, 
           contract_end_date = X__8, 
           
           UOA_total = X__9, 
           total_orthodontic_starts = X__10
    ) 
  
  #add column for date and rename columns, split data for just may
  data_may <- data %>%
    mutate(month = as.Date("2021-05-01")) %>%
    select(month, X__1, X__2, X__3, X__4, X__5, X__6, X__7, X__8, 
           X__11, X__12) %>%
    rename(contract_number = X__1,
           contract_type = X__2,
           name_or_company_name = X__3,
           commissioner_name = X__4,
           region_name = X__5,
           paid_by_BSA = X__6,
           contract_start_date = X__7, 
           contract_end_date = X__8, 
           
           UOA_total = X__11, 
           total_orthodontic_starts = X__12
    ) 
  
  #add column for date and rename columns, split data for just jun
  data_jun <- data %>%
    mutate(month = as.Date("2021-06-01")) %>%
    select(month, X__1, X__2, X__3, X__4, X__5, X__6, X__7, X__8, 
           X__13, X__14) %>%
    rename(contract_number = X__1,
           contract_type = X__2,
           name_or_company_name = X__3,
           commissioner_name = X__4,
           region_name = X__5,
           paid_by_BSA = X__6,
           contract_start_date = X__7, 
           contract_end_date = X__8, 
           
           UOA_total = X__13, 
           total_orthodontic_starts = X__14
    ) 
  
  #add column for date and rename columns, split data for just jul
  data_jul <- data %>%
    mutate(month = as.Date("2021-07-01")) %>%
    select(month, X__1, X__2, X__3, X__4, X__5, X__6, X__7, X__8, 
           X__15, X__16) %>%
    rename(contract_number = X__1,
           contract_type = X__2,
           name_or_company_name = X__3,
           commissioner_name = X__4,
           region_name = X__5,
           paid_by_BSA = X__6,
           contract_start_date = X__7, 
           contract_end_date = X__8, 
           
           UOA_total = X__15, 
           total_orthodontic_starts = X__16
    ) 
  
  #add column for date and rename columns, split data for just aug
  data_aug <- data %>%
    mutate(month = as.Date("2021-08-01")) %>%
    select(month, X__1, X__2, X__3, X__4, X__5, X__6, X__7, X__8, 
           X__17, X__18) %>%
    rename(contract_number = X__1,
           contract_type = X__2,
           name_or_company_name = X__3,
           commissioner_name = X__4,
           region_name = X__5,
           paid_by_BSA = X__6,
           contract_start_date = X__7, 
           contract_end_date = X__8, 
           
           UOA_total = X__17, 
           total_orthodontic_starts = X__18
    ) 
  
  #add column for date and rename columns, split data for just sep
  data_sep <- data %>%
    mutate(month = as.Date("2021-09-01")) %>%
    select(month, X__1, X__2, X__3, X__4, X__5, X__6, X__7, X__8, 
           X__19, X__20) %>%
    rename(contract_number = X__1,
           contract_type = X__2,
           name_or_company_name = X__3,
           commissioner_name = X__4,
           region_name = X__5,
           paid_by_BSA = X__6,
           contract_start_date = X__7, 
           contract_end_date = X__8, 
           
           UOA_total = X__19, 
           total_orthodontic_starts = X__20
    )
  
  
  UOA_calendar_data <- bind_rows(data_apr, data_may, data_jun, data_jul, data_aug, data_sep)
  
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
  
  #check format of data - manual check should be done to see if columns are in the same order as expected
  if(ncol(data) != 18){
    print("WARNING: The data you have loaded is not in the usual format. Please check.")
  }
  
  #add column for date and rename columns
  data <- data %>%
    mutate(month = data_date) %>%
    select(month, everything()) %>%
    rename(contract_number = X__1,
           contract_type = X__2,
           name_or_company_name = X__3,
           commissioner_name = X__4,
           paid_by_BSA = X__5,
           contract_start_date = X__6,
           contract_end_date = X__7,
           annual_contracted_UOA = X__8,
           annual_contracted_UDA = X__9,
           UOA_target_80_for_Apr_to_Sept = X__10, #may need to remove this
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
    mutate(month = as.Date("2020-04-01")) %>%
    select(month, X__1, 
           X__2, X__3, X__4, X__5, X__6) %>%
    rename(contract_number = X__1,
           band1 = X__2,
           band2 = X__3,
           band3 = X__4,
           other = X__5,
           urgent = X__6
    ) 
  
  #add column for date and rename columns, split data for just may
  data_may <- data %>%
    mutate(month = as.Date("2020-05-01")) %>%
    select(month, X__1, 
           X__7, X__8, X__9, X__10, X__11) %>%
    rename(contract_number = X__1,
           band1 = X__7,
           band2 = X__8,
           band3 = X__9,
           other = X__10,
           urgent = X__11
    )
  
  #add column for date and rename columns, split data for just june
  data_jun <- data %>%
    mutate(month = as.Date("2020-06-01")) %>%
    select(month, X__1, 
           X__12, X__13, X__14, X__15, X__16) %>%
    rename(contract_number = X__1,
           band1 = X__12,
           band2 = X__13,
           band3 = X__14,
           other = X__15,
           urgent = X__16
    )
  
  #add column for date and rename columns, split data for just july
  data_jul <- data %>%
    mutate(month = as.Date("2020-07-01")) %>%
    select(month, X__1, 
           X__17, X__18, X__19, X__20, X__21) %>%
    rename(contract_number = X__1,
           band1 = X__17,
           band2 = X__18,
           band3 = X__19,
           other = X__20,
           urgent = X__21
    )
  
  #add column for date and rename columns, split data for just august
  data_aug <- data %>%
    mutate(month = as.Date("2020-08-01")) %>%
    select(month, X__1, 
           X__22, X__23, X__24, X__25, X__26) %>%
    rename(contract_number = X__1,
           band1 = X__22,
           band2 = X__23,
           band3 = X__24,
           other = X__25,
           urgent = X__26
    )
  
  #add column for date and rename columns, split data for just september
  data_sep <- data %>%
    mutate(month = as.Date("2020-09-01")) %>%
    select(month, X__1, 
           X__27, X__28, X__29, X__30, X__31) %>%
    rename(contract_number = X__1,
           band1 = X__27,
           band2 = X__28,
           band3 = X__29,
           other = X__30,
           urgent = X__31
    )
  
  #add column for date and rename columns, split data for just october
  data_oct <- data %>%
    mutate(month = as.Date("2020-10-01")) %>%
    select(month, X__1, 
           X__32, X__33, X__34, X__35, X__36) %>%
    rename(contract_number = X__1,
           band1 = X__32,
           band2 = X__33,
           band3 = X__34,
           other = X__35,
           urgent = X__36
    )
  
  #add column for date and rename columns, split data for just november
  data_nov <- data %>%
    mutate(month = as.Date("2020-11-01")) %>%
    select(month, X__1, 
           X__37, X__38, X__39, X__40, X__41) %>%
    rename(contract_number = X__1,
           band1 = X__37,
           band2 = X__38,
           band3 = X__39,
           other = X__40,
           urgent = X__41
    )
  hisptorical_UDA_scheduled_data <- bind_rows(data_apr, data_may, data_jun, data_jul, data_aug, 
                                              data_sep, data_oct, data_nov)
  
}


