library(tidyverse)
library(readxl)
library(DBI)
library(odbc)
STP_ICB_lookup_codes <- read_excel("N:/_Everyone/Primary Care Group/SMT_Dental DENT 2022_23-008/data_for_monthly_report/STP_ICB_lookup_codes.xlsx")


################################################################################
import_and_clean_unique_patients_data <- function(data_path = "N:/_Everyone/Primary Care Group/SMT_Dental DENT 2022_23-008/unique_patients/",
                                                  commissioner_lookup = STP_ICB_lookup_codes){
  
  # Gets last month 
  data_month <- lubridate::floor_date(Sys.Date()  - lubridate::duration("1 week"), unit = "month") ###might need to change depending on which week the code is run
  data_month_name <- format(Sys.Date()  - lubridate::duration("1 week"), format = "%b%y")
  
  unique_patients <- readxl::read_excel(paste0(data_path, "unique_patients_rolling_", data_month_name,".xlsx"),
                                        skip = 19)
  
  #remove last 7 rows with eDen source
  unique_patients <-  unique_patients[1:(nrow(unique_patients) - 7),]
  
  #fixes column names
  unique_patients <- unique_patients %>%
    mutate(month_ending = data_month) %>%
    rename(contract_number = "Contract Number",
           commissioner_ods_code_icb = "Latest Commissioner Code",
           unique_patients_rolling_12M = "Unique Patient Count Rolling 12M",
           band1_unique_patients_rolling_12M = "Band 1 Unique Patient Count Rolling 12M",
           band2_or_3_unique_patients_rolling_12M = "Band 2 or Band 3 Unique Patient Count Rolling 12M",
           band1_urgent_unique_patients_rolling_12M = "Band 1 Urgent Unique Patient Count Rolling 12M",
           band_other_unique_patients_rolling_12M = "Other Unique Patient Count Rolling 12M") %>%
    filter(!is.na(contract_number))
  
  #join in commissioner name
  commissioner_lookup <- commissioner_lookup %>%
    select(commissioner_ods_code_icb = commissioner_ONS_boundary_code_ICB, 
           commissioner_name = commissioner_name_ICB, 
           region_name) %>%
    distinct()
  
  unique_patients <- unique_patients %>%
    left_join(commissioner_lookup, by = "commissioner_ods_code_icb")
  
}

################################################################################
#function to import and clean data
#N.B. this assumes that the columns are in the same order each time!
import_and_clean_scheduled_UDA_data <- function(data_path,
                                                data_date,
                                                commissioner_lookup = STP_ICB_lookup_codes){
  
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
                                   "numeric", "numeric",
                                   
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric",
                                   "numeric", "numeric", "numeric"), 
                     skip = 3,
                     col_names = TRUE,
                     .name_repair = ~ paste0("X__", seq_along(.x)))
  
  # #remove last 4 rows with eDen source
  # data <- data[1:(nrow(data) - 4),]
  
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
           UDA_delivered = X__11,
           general_FP17s = X__12,
           UDA_band_1 = X__17,
           UDA_band_2 = X__18,         
           UDA_band_3 = X__19,
           UDA_urgent = X__20,
           UDA_other = X__21,
           FP17s_band_1 = X__22,
           FP17s_band_2 = X__23,
           FP17s_band_3 = X__24,
           FP17s_band_urgent = X__25,
           FP17s_band_other = X__26
    ) %>%
    select(-starts_with("X__")) %>%
    filter(!is.na(contract_number))
  
  #join in commissioner name
  commissioner_lookup <- commissioner_lookup %>%
    select(commissioner_ods_code_icb = commissioner_ONS_boundary_code_ICB, 
           commissioner_name = commissioner_name_ICB, 
           region_name) %>%
    distinct()
  
  data <- data %>%
    left_join(commissioner_lookup, by = "commissioner_name")
  
}

################################################################################
#function to import and clean data
#N.B. this assumes that the columns are in the same order each time!
import_and_clean_scheduled_UOA_data <- function(data_path,
                                                data_date,
                                                commissioner_lookup = STP_ICB_lookup_codes){
  
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
           UOA_delivered = X__11,
           orthodontic_FP17s = X__14,
           orthodontic_starts = X__17,
           orthodontic_completions = X__18
    ) %>%
    select(-starts_with("X__")) %>%
    filter(!is.na(contract_number))
  
  #join in commissioner code
  commissioner_lookup <- commissioner_lookup %>%
    select(commissioner_ods_code_icb = commissioner_ONS_boundary_code_ICB,
           commissioner_name = commissioner_name_ICB,
           region_name) %>%
    distinct()
  
  data <- data %>%
    left_join(commissioner_lookup, by = "commissioner_name")
  
}



################################################################################
#function to import and clean data
#N.B. this assumes that the columns are in the same order each time!
import_and_clean_calendar_UDA_data <- function(data_path,
                                               commissioner_lookup = STP_ICB_lookup_codes){
  
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
  
  #initialise list of data for each month
  data_list <- list(data_apr)

  if(ncol(data) > 20){
    
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
    
    data_list <- append(data_list, list(data_may))
    
  }
  
  if(ncol(data) > 32){
    #add column for date and rename columns, split data for just june
    data_jun <- data %>%
      mutate(data_month = as.Date("2022-06-01")) %>%
      select(data_month, X__1, X__2, X__3, X__4, X__5, X__6, X__7, X__8,
             X__33, X__34, X__35, X__36, X__37, X__38, X__39, X__40, X__41, X__42, X__43, X__44) %>%
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
    
    data_list <- append(data_list, list(data_jun))
  }
  
  if(ncol(data) > 44){
    #add column for date and rename columns, split data for just july
    data_jul <- data %>%
      mutate(data_month = as.Date("2022-07-01")) %>%
      select(data_month, X__1, X__2, X__3, X__4, X__5, X__6, X__7, X__8,
             X__45, X__46, X__47, X__48, X__49, X__50, X__51, X__52, X__53, X__54, X__55, X__56) %>%
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
    
    data_list <- append(data_list, list(data_jul))
  }
  
  if(ncol(data) > 56){
    #add column for date and rename columns, split data for just august
    data_aug <- data %>%
      mutate(data_month = as.Date("2022-08-01")) %>%
      select(data_month, X__1, X__2, X__3, X__4, X__5, X__6, X__7, X__8,
             X__57, X__58, X__59, X__60, X__61, X__62, X__63, X__64, X__65, X__66, X__67, X__68) %>%
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
    
    data_list <- append(data_list, list(data_aug))
  }
  
  if(ncol(data) > 68){
    #add column for date and rename columns, split data for just september
    data_sep <- data %>%
      mutate(data_month = as.Date("2022-09-01")) %>%
      select(data_month, X__1, X__2, X__3, X__4, X__5, X__6, X__7, X__8,
             X__69, X__70, X__71, X__72, X__73, X__74, X__75, X__76, X__77, X__78, X__79, X__80) %>%
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
    
    data_list <- append(data_list, list(data_sep))
  }
  
  if(ncol(data) > 80){
    #add column for date and rename columns, split data for just october
    data_oct <- data %>%
      mutate(data_month = as.Date("2022-10-01")) %>%
      select(data_month, X__1, X__2, X__3, X__4, X__5, X__6, X__7, X__8,
             X__81, X__82, X__83, X__84, X__85, X__86, X__87, X__88, X__89, X__90, X__91, X__92) %>%
      rename(contract_number = X__1,
             latest_contract_type = X__2,
             name_or_company_name = X__3,
             commissioner_name = X__4,
             region_name = X__5,
             paid_by_BSA = X__6,
             contract_start_date = X__7,
             contract_end_date = X__8,
             
             UDA_total = X__81,
             UDA_band_1_total = X__82,
             UDA_band_2_total = X__83,
             UDA_band_3_total = X__84,
             UDA_urgent_total = X__85,
             UDA_other_total = X__86,
             total_FP17s = X__87,
             total_band_1_FP17s  = X__88,
             total_band_2_FP17s = X__89,
             total_band_3_FP17s = X__90,
             total_urgent_FP17s = X__91,
             total_other_FP17s = X__92
      )
    
    data_list <- append(data_list, list(data_oct))
  }
  
  if(ncol(data) > 92){
    #add column for date and rename columns, split data for just november
    data_nov <- data %>%
      mutate(data_month = as.Date("2022-11-01")) %>%
      select(data_month, X__1, X__2, X__3, X__4, X__5, X__6, X__7, X__8,
             X__93, X__94, X__95, X__96, X__97, X__98, X__99, X__100, X__101, X__102, X__103, X__104) %>%
      rename(contract_number = X__1,
             latest_contract_type = X__2,
             name_or_company_name = X__3,
             commissioner_name = X__4,
             region_name = X__5,
             paid_by_BSA = X__6,
             contract_start_date = X__7,
             contract_end_date = X__8,
             
             UDA_total = X__93,
             UDA_band_1_total = X__94,
             UDA_band_2_total = X__95,
             UDA_band_3_total = X__96,
             UDA_urgent_total = X__97,
             UDA_other_total = X__98,
             total_FP17s = X__99,
             total_band_1_FP17s  = X__100,
             total_band_2_FP17s = X__101,
             total_band_3_FP17s = X__102,
             total_urgent_FP17s = X__103,
             total_other_FP17s = X__104
      )
    
    data_list <- append(data_list, list(data_nov))
  }
  
  if(ncol(data) > 104){
    #add column for date and rename columns, split data for just december
    data_dec <- data %>%
      mutate(data_month = as.Date("2022-12-01")) %>%
      select(data_month, X__1, X__2, X__3, X__4, X__5, X__6, X__7, X__8,
             X__105, X__106, X__107, X__108, X__109, X__110, X__111, X__112, X__113, X__114, X__115, X__116) %>%
      rename(contract_number = X__1,
             latest_contract_type = X__2,
             name_or_company_name = X__3,
             commissioner_name = X__4,
             region_name = X__5,
             paid_by_BSA = X__6,
             contract_start_date = X__7,
             contract_end_date = X__8,
             
             UDA_total = X__105,
             UDA_band_1_total = X__106,
             UDA_band_2_total = X__107,
             UDA_band_3_total = X__108,
             UDA_urgent_total = X__109,
             UDA_other_total = X__110,
             total_FP17s = X__111,
             total_band_1_FP17s  = X__112,
             total_band_2_FP17s = X__113,
             total_band_3_FP17s = X__114,
             total_urgent_FP17s = X__115,
             total_other_FP17s = X__116
      )
    
    data_list <- append(data_list, list(data_dec))
  }
  
  if(ncol(data) > 116){
    #add column for date and rename columns, split data for just january
    data_jan <- data %>%
      mutate(data_month = as.Date("2023-01-01")) %>%
      select(data_month, X__1, X__2, X__3, X__4, X__5, X__6, X__7, X__8,
             X__117, X__118, X__119, X__120, X__121, X__122, X__123, X__124, X__125, X__126, X__127, X__128) %>%
      rename(contract_number = X__1,
             latest_contract_type = X__2,
             name_or_company_name = X__3,
             commissioner_name = X__4,
             region_name = X__5,
             paid_by_BSA = X__6,
             contract_start_date = X__7,
             contract_end_date = X__8,
             
             UDA_total = X__117,
             UDA_band_1_total = X__118,
             UDA_band_2_total = X__119,
             UDA_band_3_total = X__120,
             UDA_urgent_total = X__121,
             UDA_other_total = X__122,
             total_FP17s = X__123,
             total_band_1_FP17s  = X__124,
             total_band_2_FP17s = X__125,
             total_band_3_FP17s = X__126,
             total_urgent_FP17s = X__127,
             total_other_FP17s = X__128
      )
    
    data_list <- append(data_list, list(data_jan))
  }
  
  if(ncol(data) > 128){
    #add column for date and rename columns, split data for just february
    data_feb <- data %>%
      mutate(data_month = as.Date("2023-02-01")) %>%
      select(data_month, X__1, X__2, X__3, X__4, X__5, X__6, X__7, X__8,
             X__129, X__130, X__131, X__132, X__133, X__134, X__135, X__136, X__137, X__138, X__139, X__140) %>%
      rename(contract_number = X__1,
             latest_contract_type = X__2,
             name_or_company_name = X__3,
             commissioner_name = X__4,
             region_name = X__5,
             paid_by_BSA = X__6,
             contract_start_date = X__7,
             contract_end_date = X__8,
             
             UDA_total = X__129,
             UDA_band_1_total = X__130,
             UDA_band_2_total = X__131,
             UDA_band_3_total = X__132,
             UDA_urgent_total = X__133,
             UDA_other_total = X__134,
             total_FP17s = X__135,
             total_band_1_FP17s  = X__136,
             total_band_2_FP17s = X__137,
             total_band_3_FP17s = X__138,
             total_urgent_FP17s = X__139,
             total_other_FP17s = X__140
      )
    
    data_list <- append(data_list, list(data_feb))
  }
  
  if(ncol(data) > 140){
    #add column for date and rename columns, split data for just march
    data_mar <- data %>%
      mutate(data_month = as.Date("2023-03-01")) %>%
      select(data_month, X__1, X__2, X__3, X__4, X__5, X__6, X__7, X__8,
             X__141, X__142, X__143, X__144, X__145, X__146, X__147, X__148, X__149, X__150, X__151, X__152) %>%
      rename(contract_number = X__1,
             latest_contract_type = X__2,
             name_or_company_name = X__3,
             commissioner_name = X__4,
             region_name = X__5,
             paid_by_BSA = X__6,
             contract_start_date = X__7,
             contract_end_date = X__8,
             
             UDA_total = X__141,
             UDA_band_1_total = X__142,
             UDA_band_2_total = X__143,
             UDA_band_3_total = X__144,
             UDA_urgent_total = X__145,
             UDA_other_total = X__146,
             total_FP17s = X__147,
             total_band_1_FP17s = X__148,
             total_band_2_FP17s = X__149,
             total_band_3_FP17s = X__150,
             total_urgent_FP17s = X__151,
             total_other_FP17s = X__152
      )
    
    data_list <- append(data_list, list(data_mar))
  }
  

  UDA_calendar_data <- bind_rows(data_list)

  #join in commissioner code
  commissioner_lookup <- commissioner_lookup %>%
    select(commissioner_ods_code_icb = commissioner_ONS_boundary_code_ICB,
           commissioner_name = commissioner_name_ICB) %>%
    distinct()

  UDA_calendar_data <- UDA_calendar_data %>%
    left_join(commissioner_lookup, by = "commissioner_name")

}




################################################################################
#function to import and clean data
#N.B. this assumes that the columns are in the same order each time!
import_and_clean_calendar_UOA_data <- function(data_path,
                                               commissioner_lookup = STP_ICB_lookup_codes){
  
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
  
  #initialise data list
  data_list <- list(data_apr)
  
  if(ncol(data) > 8){
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
    
    data_list <- append(data_list, list(data_may))
    
  }
  
  if(ncol(data) > 9){
    #add column for date and rename columns, split data for just jun
    data_jun <- data %>%
      mutate(data_month = as.Date("2022-06-01")) %>%
      select(data_month, X__1, X__2, X__3, X__4, X__5, X__6, X__7, 
             X__10) %>%
      rename(contract_number = X__1,
             contract_type = X__2,
             name_or_company_name = X__3,
             commissioner_name = X__4,
             paid_by_BSA = X__5,
             contract_start_date = X__6, 
             contract_end_date = X__7, 
             
             UOA_total = X__10
      )
    
    data_list <- append(data_list, list(data_jun))
    
  }
  
  if(ncol(data) > 10){
    #add column for date and rename columns, split data for just jul
    data_jul <- data %>%
      mutate(data_month = as.Date("2022-07-01")) %>%
      select(data_month, X__1, X__2, X__3, X__4, X__5, X__6, X__7, 
             X__11) %>%
      rename(contract_number = X__1,
             contract_type = X__2,
             name_or_company_name = X__3,
             commissioner_name = X__4,
             paid_by_BSA = X__5,
             contract_start_date = X__6, 
             contract_end_date = X__7, 
             
             UOA_total = X__11
      )

    data_list <- append(data_list, list(data_jul))
    
  }
  
  if(ncol(data) > 11){
    #add column for date and rename columns, split data for just aug
    data_aug <- data %>%
      mutate(data_month = as.Date("2022-08-01")) %>%
      select(data_month, X__1, X__2, X__3, X__4, X__5, X__6, X__7, 
             X__12) %>%
      rename(contract_number = X__1,
             contract_type = X__2,
             name_or_company_name = X__3,
             commissioner_name = X__4,
             paid_by_BSA = X__5,
             contract_start_date = X__6, 
             contract_end_date = X__7, 
             
             UOA_total = X__12
      )
    
    data_list <- append(data_list, list(data_aug))
    
  }
  
  if(ncol(data) > 12){
    #add column for date and rename columns, split data for just sep
    data_sep <- data %>%
      mutate(data_month = as.Date("2022-09-01")) %>%
      select(data_month, X__1, X__2, X__3, X__4, X__5, X__6, X__7, 
             X__13) %>%
      rename(contract_number = X__1,
             contract_type = X__2,
             name_or_company_name = X__3,
             commissioner_name = X__4,
             paid_by_BSA = X__5,
             contract_start_date = X__6, 
             contract_end_date = X__7, 
             
             UOA_total = X__13
      )
    
    data_list <- append(data_list, list(data_sep))
    
  }
  
  if(ncol(data) > 13){
    #add column for date and rename columns, split data for just oct
    data_oct <- data %>%
      mutate(data_month = as.Date("2022-10-01")) %>%
      select(data_month, X__1, X__2, X__3, X__4, X__5, X__6, X__7, 
             X__14) %>%
      rename(contract_number = X__1,
             contract_type = X__2,
             name_or_company_name = X__3,
             commissioner_name = X__4,
             paid_by_BSA = X__5,
             contract_start_date = X__6, 
             contract_end_date = X__7, 
             
             UOA_total = X__14
      )
    
    data_list <- append(data_list, list(data_oct))
    
  }
  
  if(ncol(data) > 14){
    #add column for date and rename columns, split data for just nov
    data_nov <- data %>%
      mutate(data_month = as.Date("2022-11-01")) %>%
      select(data_month, X__1, X__2, X__3, X__4, X__5, X__6, X__7, 
             X__15) %>%
      rename(contract_number = X__1,
             contract_type = X__2,
             name_or_company_name = X__3,
             commissioner_name = X__4,
             paid_by_BSA = X__5,
             contract_start_date = X__6, 
             contract_end_date = X__7, 
             
             UOA_total = X__15
      )
    
    data_list <- append(data_list, list(data_nov))
    
  }
  
  if(ncol(data) > 15){
    #add column for date and rename columns, split data for just dec
    data_dec <- data %>%
      mutate(data_month = as.Date("2022-12-01")) %>%
      select(data_month, X__1, X__2, X__3, X__4, X__5, X__6, X__7, 
             X__16) %>%
      rename(contract_number = X__1,
             contract_type = X__2,
             name_or_company_name = X__3,
             commissioner_name = X__4,
             paid_by_BSA = X__5,
             contract_start_date = X__6, 
             contract_end_date = X__7, 
             
             UOA_total = X__16
      )
    
    data_list <- append(data_list, list(data_dec))
    
  }
  
  if(ncol(data) > 16){
    #add column for date and rename columns, split data for just jan
    data_jan <- data %>%
      mutate(data_month = as.Date("2023-01-01")) %>%
      select(data_month, X__1, X__2, X__3, X__4, X__5, X__6, X__7, 
             X__17) %>%
      rename(contract_number = X__1,
             contract_type = X__2,
             name_or_company_name = X__3,
             commissioner_name = X__4,
             paid_by_BSA = X__5,
             contract_start_date = X__6, 
             contract_end_date = X__7, 
             
             UOA_total = X__17
      )
    
    data_list <- append(data_list, list(data_jan))
    
  }
  
  if(ncol(data) > 17){
    #add column for date and rename columns, split data for just feb
    data_feb <- data %>%
      mutate(data_month = as.Date("2023-02-01")) %>%
      select(data_month, X__1, X__2, X__3, X__4, X__5, X__6, X__7, 
             X__18) %>%
      rename(contract_number = X__1,
             contract_type = X__2,
             name_or_company_name = X__3,
             commissioner_name = X__4,
             paid_by_BSA = X__5,
             contract_start_date = X__6, 
             contract_end_date = X__7, 
             
             UOA_total = X__18
      )
    
    data_list <- append(data_list, list(data_feb))
    
  }
  
  if(ncol(data) > 18){
    #add column for date and rename columns, split data for just mar
    data_mar <- data %>%
      mutate(data_month = as.Date("2023-03-01")) %>%
      select(data_month, X__1, X__2, X__3, X__4, X__5, X__6, X__7, 
             X__19) %>%
      rename(contract_number = X__1,
             contract_type = X__2,
             name_or_company_name = X__3,
             commissioner_name = X__4,
             paid_by_BSA = X__5,
             contract_start_date = X__6, 
             contract_end_date = X__7, 
             
             UOA_total = X__19
      )
    
    data_list <- append(data_list, list(data_mar))
    
  }
  
  UOA_calendar_data <- bind_rows(data_list)
  
  #join in commissioner code
  commissioner_lookup <- commissioner_lookup %>%
    select(commissioner_ods_code_icb = commissioner_ONS_boundary_code_ICB, 
           commissioner_name = commissioner_name_ICB,
           region_name) %>%
    distinct()
  
  UOA_calendar_data <- UOA_calendar_data %>%
    left_join(commissioner_lookup, by = "commissioner_name")
  
}


# ################################################################################
# #function to import and clean data
# #N.B. this assumes that the columns are in the same order each time!
# import_and_clean_scheduled_UOA_data <- function(data_path = "data/raw_data/dashboard_raw_data/UOA_scheduled_raw_data/UOA_scheduled_Apr21.xlsx",
#                                                 data_date = as.Date("2021-04-01"),
#                                                 commissioner_lookup = contract_ICB_lookup){
#   
#   #read in data with correct types and removing top 3 rows and renaming columns 
#   data <- read_excel(data_path, 
#                      col_types = c("numeric", "text", "text",
#                                    "text", "date", "date", "numeric", 
#                                    "numeric", "numeric", "numeric", "numeric"), 
#                      skip = 3,
#                      col_names = TRUE,
#                      .name_repair = ~ paste0("X__", seq_along(.x)))
#   
#   #remove last 4 rows with eDen source
#   data <- data[1:(nrow(data) - 4),]
#   
#   # #check format of data - manual check should be done to see if columns are in the same order as expected
#   # if(ncol(data) != 18){
#   #   print("WARNING: The data you have loaded is not in the usual format. Please check.")
#   # }
#   
#   #add column for date and rename columns
#   data <- data %>%
#     mutate(data_month = data_date) %>%
#     select(data_month, everything()) %>%
#     rename(contract_number = X__1,
#            contract_type = X__2,
#            name_or_company_name = X__3,
#            #commissioner_name = X__4,
#            paid_by_BSA = X__4,
#            contract_start_date = X__5,
#            contract_end_date = X__6,
#            annual_contracted_UOA = X__7,
#            annual_contracted_UDA = X__8,
#            UOA_delivered = X__9,
#            orthodontic_FP17s = X__10#, 
#            #orthodontic_starts = X__17,
#            #orthodontic_completions = X__18 
#     ) %>%
#     select(-starts_with("X__")) %>%
#     filter(!is.na(contract_number))
#   
#   #join in commissioner code
#   data <- data %>%
#     left_join(contract_ICB_lookup, by = "contract_number")
#   
# }