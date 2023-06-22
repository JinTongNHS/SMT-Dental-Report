clean_treatment_month_columns <- function(data = UDA_treatment_month_non_FD_Apr23,
                                          commissioner_lookup = STP_ICB_lookup_codes
                                          ){
  
  
  data <- data %>%
    rename(month = TREATMENT_MONTH, 
           contract_number = CONTRACT_NUMBER, 
           name_or_company_name = PROVIDER_NAME, 
           commissioner_name = COMMISSIONER_NAME, 
           contract_type = CONTRACT_TYPE, 
           paid_by_BSA = PAID_BY_BSA, 
           contract_start_date = CONT_START_DATE, 
           contract_end_date = CONT_END_DATE, 
           annual_contracted_UDA = CONTRACTED_UDA, 
           annual_contracted_UOA = CONTRACTED_UOA, 
           UDA_delivered = TOTAL_UDA, 
           UDA_band_1 = BAND1_UDA, 
           UDA_band_2a = BAND2A_UDA, 
           UDA_band_2b = BAND2B_UDA, 
           UDA_band_2c = BAND2C_UDA, 
           UDA_band_2 = BAND2_UDA, 
           UDA_band_3 = BAND3_UDA, 
           UDA_urgent = URGENT_UDA, 
           UDA_other = OTHER_UDA, 
           general_FP17s = TOTAL_FORM_COUNT, 
           FP17s_band_1 = BAND1_FORM_COUNT, 
           FP17s_band_2a = BAND2A_FORM_COUNT, 
           FP17s_band_2b = BAND2B_FORM_COUNT, 
           FP17s_band_2c = BAND2C_FORM_COUNT, 
           FP17s_band_2 = BAND2_FORM_COUNT, 
           FP17s_band_3 = BAND3_FORM_COUNT, 
           FP17s_band_urgent = URGENT_FORM_COUNT, 
           FP17s_band_other = OTHER_FORM_COUNT) %>% 
           select(-c(contract_start_date, contract_end_date))
  
  #join in commissioner code
  commissioner_lookup <- commissioner_lookup %>%
    select(commissioner_ods_code_icb = commissioner_ONS_boundary_code_ICB,
           commissioner_name = commissioner_name_ICB,
           region_name) %>%
    distinct()
  
  data <- data %>%
    left_join(commissioner_lookup, by = "commissioner_name") %>%
    mutate(year = substr(month, 1, 4),
           month = substr(month, 5, 6)) %>%
    mutate(month = as.Date(paste0(year,"-", month, "-01"))) 
  
}