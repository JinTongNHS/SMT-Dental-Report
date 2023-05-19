################################################################################
upload_scheduled_data <- function(UDA_latest,
                                  UOA_latest){
  
  #Append schedule data and overwrite calendar data in NCDR
  con <- dbConnect(odbc::odbc(), "NCDR")
  
  #Append UDA scheduled data
  dbWriteTable(con, Id(catalog="NHSE_Sandbox_PrimaryCareNHSContracts",schema="Dental",table="UDA_scheduled"),
               value = UDA_latest, row.names = FALSE, append=TRUE)
  
  #Append UOA scheduled data
  dbWriteTable(con, Id(catalog="NHSE_Sandbox_PrimaryCareNHSContracts",schema="Dental",table="UOA_scheduled"),
               value = UOA_latest, row.names = FALSE, append=TRUE)
  
}

################################################################################
upload_calendar_data <- function(UDA_latest,
                                 UOA_latest){
  
  con <- dbConnect(odbc::odbc(), "NCDR")
  
  #Get UDA calendar table and overwrite this financial year
  sql <- "SELECT *
  FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[UDA_calendar]"
  result <- dbSendQuery(con, sql)
  UDA_calendar_data_full <- dbFetch(result)
  dbClearResult(result)
  
  UDA_calendar_data_latest <- UDA_calendar_data_full %>%
    filter(data_month < as.Date("2023-04-01")) %>% ###needs to be updates every financial year
    bind_rows(UDA_latest)
  
  dbWriteTable(con, Id(catalog="NHSE_Sandbox_PrimaryCareNHSContracts",schema="Dental",table="UDA_calendar"),
               value = UDA_calendar_data_latest, row.names = FALSE, append = FALSE, overwrite = TRUE)


  #Get UOA table and overwrite this financial year
  sql <- "SELECT *
  FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[UOA_calendar]"

  result <- dbSendQuery(con, sql)
  UOA_calendar_data_full <- dbFetch(result)
  dbClearResult(result)

  UOA_calendar_data_latest <- UOA_calendar_data_full %>%
    filter(data_month < as.Date("2023-04-01")) %>% ###need to be updated every financial yeat
    bind_rows(UOA_latest)

  dbWriteTable(con, Id(catalog="NHSE_Sandbox_PrimaryCareNHSContracts",schema="Dental",table="UOA_calendar"),
               value = UOA_calendar_data_latest, row.names = FALSE, append = FALSE, overwrite = TRUE)

}

################################################################################
upload_unique_patients_data <- function(unique_patients){
  
  #Append latest data to table on NCDR
  con <- dbConnect(odbc::odbc(), "NCDR")
  
  dbWriteTable(con, Id(catalog="NHSE_Sandbox_PrimaryCareNHSContracts",schema="Dental",table="unique_patients_rolling_12_month"),
               value = unique_patients, row.names = FALSE, append = TRUE)
  
}

#This is the table used for PCDID
################################################################################
upload_delivery_metrics <- function(calendar_data = UDA_calendar_data,  
                                 scheduled_data = UDA_scheduled_data,
                                 historic_data = historical_UDA_scheduled_data,
                                 lookup = STP_ICB_lookup_codes){

  UDA_delivery_calendar <- get_delivery_data_calendar(calendar_data = calendar_data,
                                                      scheduled_data = scheduled_data,
                                                      remove_prototypes = T,
                                                      UDAorUOA = "UDA",
                                                      regional_lines = F,
                                                      STP_lines = T,
                                                      cat_lines = F,
                                                      renameColumns = T)

  UDA_delivery_scheduled <- get_delivery_data(data = bind_rows(scheduled_data, historic_data),
                                              remove_prototypes = T,
                                              UDAorUOA = "UDA",
                                              all_regions_and_STPs = T,
                                              renameColumns = T)
  
  UDA_delivery_scheduled_contract_level <- get_delivery_data(data = scheduled_data,
                                                             remove_prototypes = FALSE, #we want all contracts in this data
                                                             UDAorUOA = "UDA",
                                                             all_regions_and_STPs = F,
                                                             renameColumns = F,contractor_level = T)
  
  lookup <- lookup %>%
    select(commissioner_name = commissioner_name_ICB,
           commissioner_ODS_code_ICB = commissioner_ONS_boundary_code_ICB)
  
  UDA_delivery_calendar <- UDA_delivery_calendar %>%
    left_join(lookup) %>%
    select("calendar_month", "commissioner_name",
           commissioner_ODS_code_ICB,
           "region_name", "monthly_UDAs_delivered",
           "scaled_monthly_UDAs_delivered", "annual_contracted_UDAs", "scaled_perc_UDAs_delivered",
           "threshold_perc")

  UDA_delivery_scheduled <- UDA_delivery_scheduled %>%
    left_join(lookup) %>%
    select("scheduled_month", "commissioner_name",
           commissioner_ODS_code_ICB,
           "region_name",
           "monthly_UDAs_delivered",
           "scaled_monthly_UDAs_delivered", "annual_contracted_UDAs", "scaled_perc_UDAs_delivered",
           "threshold_perc")
  
  UDA_delivery_scheduled_contract_level <- UDA_delivery_scheduled_contract_level %>%
    left_join(lookup, by = "commissioner_name") %>%
    select(scheduled_month = month, 
           contract_number,
           commissioner_name, 
           commissioner_ODS_code_ICB,
           monthly_UDAs_delivered = monthly_UDA_UOAs_delivered,
           scaled_monthly_UDAs_delivered = scaled_monthly_UDA_UOAs_delivered, 
           annual_contracted_UDAs = annual_contracted_UDA_UOA, 
           scaled_perc_UDAs_delivered = perc_UDA_UOA_delivered) %>%
    mutate(scaled_perc_UDAs_delivered = if_else(is.nan(scaled_perc_UDAs_delivered) | is.infinite(scaled_perc_UDAs_delivered), as.numeric(NA), scaled_perc_UDAs_delivered))
  
  
  
  #Overwrite data in NCDR
  con <- dbConnect(odbc::odbc(), "NCDR")

  dbWriteTable(con, Id(catalog="NHSE_Sandbox_PrimaryCareNHSContracts",schema="Dental",table="UDA_delivery_calendar"),
               value = UDA_delivery_calendar, row.names = FALSE, append = FALSE, overwrite = TRUE)

  dbWriteTable(con, Id(catalog="NHSE_Sandbox_PrimaryCareNHSContracts",schema="metric",table="UDA_delivery_scheduled_ICB"), 
               value = UDA_delivery_scheduled, row.names = FALSE, append = FALSE, overwrite = TRUE)
  
  dbWriteTable(con, Id(catalog="NHSE_Sandbox_PrimaryCareNHSContracts",schema="Dental",table="UDA_delivery_scheduled"), 
               value = UDA_delivery_scheduled, row.names = FALSE, append = FALSE, overwrite = TRUE)

  dbWriteTable(con, Id(catalog="NHSE_Sandbox_PrimaryCareNHSContracts",schema="metric",table="UDA_delivery_scheduled_contract_level"), ##NB schema has changed
               value = UDA_delivery_scheduled_contract_level, row.names = FALSE, append = FALSE, overwrite = TRUE)
  
}


#This is the table used for SOF
################################################################################
update_SOF_table_S109 <- function(scheduled_data = UDA_scheduled_data,
                                  historic_data = historical_UDA_scheduled_data,
                                  lookup = STP_ICB_lookup_codes){
  
  scheduled_data <- bind_rows(scheduled_data, historic_data)
  
  #wide_data <- pull_PCDID_data()
  UDA_delivery_scheduled <- get_delivery_data(data = scheduled_data,
                                              remove_prototypes = T,
                                              UDAorUOA = "UDA",
                                              all_regions_and_STPs = T,
                                              renameColumns = T)
  
  lookup <- lookup %>%
    select(commissioner_name = commissioner_name_ICB,
           commissioner_ONS_boundary_code_ICB)
  
  UDA_delivery_scheduled <- UDA_delivery_scheduled %>%
    left_join(lookup, by = "commissioner_name") %>%
    ungroup() %>%
    filter(scheduled_month == max(UDA_delivery_scheduled$scheduled_month))
  
  SOF_data <- UDA_delivery_scheduled  %>%
    select(OrgCode = commissioner_ONS_boundary_code_ICB,
           Period = scheduled_month,
           Den = annual_contracted_UDAs,
           Num = scaled_monthly_UDAs_delivered,
           Rate = scaled_perc_UDAs_delivered) %>%
    mutate(Rate = Num / Den) %>%
    pivot_longer(cols = c("Den", "Num", "Rate"),
                 names_to = "MetricType",
                 values_to = "Value") %>%
    mutate(MetricID = "S109a",
           MetricName = "Units of Dental Activity delivered as a proportion of all Units of Dental Activity contracted",
           Domain = "Primary Care",
           OrgType = "ICB",
           LastModified = Sys.Date(),
           Period = substr(Period, 1, 7)) %>%
    select(MetricID,
           MetricName,
           Domain,
           OrgType,
           OrgCode,
           Period,
           MetricType,
           Value,
           LastModified
    )
  
  #append latest data to table in NCDR
  #[NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[SOF_S109a]
  con <- dbConnect(odbc::odbc(), "NCDR")
  
  dbWriteTable(con, Id(catalog="NHSE_Sandbox_PrimaryCareNHSContracts",schema="Dental",table="SOF_S109a"),
               value = SOF_data, row.names = FALSE, append = TRUE)
  
}


################################################################################
upload_unique_patients_metric <- function(data = unique_patients_rolling,
                                          scheduled_data = UDA_scheduled_data,
                                          commissioner_lookup = STP_ICB_lookup_codes){
  
  metric_data <- plot_unique_patients_rolling(data = data,
                                              scheduled_data = UDA_scheduled_data,
                                              level = "National",
                                              region_STP_name = NULL,
                                              plotChart = FALSE,
                                              remove_prototypes = TRUE,
                                              all_regiona_and_STPs = TRUE)
  
  metric_data <- metric_data %>%
    rename(period_end_date = "12 month period end", 
           region_name = "Region Name", 
           commissioner_name = "Commissioner Name", 
           any_band_unique_patients_rolling_12M = "Any band unique patients rolling 12 month", 
           band1_unique_patients_rolling_12M = "Band 1 unique patients rolling 12 month", 
           band2_or_3_unique_patients_rolling_12M = "Band 2 or 3 unique patients rolling 12 month", 
           band1_urgent_unique_patients_rolling_12M = "Urgent band 1 unique patients rolling 12 month", 
           band_other_unique_patients_rolling_12M = "Other unique patients rolling 12 month")
  
  #join in commissioner code
  commissioner_lookup <- commissioner_lookup %>%
    select(commissioner_ods_code_icb = commissioner_ONS_boundary_code_ICB,
           commissioner_name = commissioner_name_ICB) %>%
    distinct()

  metric_data <- metric_data %>%
    left_join(commissioner_lookup, by = "commissioner_name") %>%
    select(period_end_date, 
           commissioner_name,
           commissioner_ods_code_icb,
           region_name, 
           any_band_unique_patients_rolling_12M, 
           band1_unique_patients_rolling_12M,
           band2_or_3_unique_patients_rolling_12M, 
           band1_urgent_unique_patients_rolling_12M,
           band_other_unique_patients_rolling_12M)

  
  con <- dbConnect(odbc::odbc(), "NCDR")

  dbWriteTable(con, Id(catalog="NHSE_Sandbox_PrimaryCareNHSContracts",schema="metric",table="unique_patients_rolling_ICB"),
               value = metric_data, row.names = FALSE, append = FALSE, overwrite = TRUE)
}



