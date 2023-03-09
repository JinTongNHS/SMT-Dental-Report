load_in_band2_split_data <- function(){
  
  files <- list.files(path="N:/_Everyone/Primary Care Group/SMT_Dental DENT 2022_23-008/data_for_monthly_report/band_2_split_data/", pattern="*.xlsx", full.names=TRUE, recursive=FALSE)
  month <- as.Date("2022-12-01", format = "%Y-%m-%d")
  
  band2_split_data <- data.frame()
  
  for (f in files){
    
    print(f)
    
    #skip first rows
    f_clean <- read_excel(f,
                       col_names = FALSE, 
                       skip = 4,
                       .name_repair = ~ paste0("X__", seq_along(.x)))
    
    #remove last 7 rows
    f_clean <- f_clean %>%
      rename(contract_number = X__1,
             contract_type = X__2,
             name_or_company_name = X__3,
             commissioner_name = X__4,
             paid_by_BSA = X__5,
             contract_start_date = X__6,
             contract_end_date = X__7,
             total_band2_UDAs_delivered = X__8,
             YTD_band2_UDAs_delivered_before_Nov22 = X__9,
             band_2a_UDAs = X__10,
             band_2b_UDAs = X__11,
             band_2c_UDAs = X__12,
             total_band2_FP17s_delivered = X__13,
             YTD_band2_FP17ss_delivered_before_Nov22 = X__14,
             band_2a_FP17s = X__15,
             band_2b_FP17s = X__16,
             band_2c_FP17s = X__17
             ) %>%
      filter(!is.na(contract_number)) %>%
      mutate(month = month)
    
    band2_split_data <- bind_rows(band2_split_data, f_clean)
    
    month <- month %m+% period("1 month")
    
    
  }
  
  band2_split_data <- band2_split_data %>%
    select(month, everything())
  
}