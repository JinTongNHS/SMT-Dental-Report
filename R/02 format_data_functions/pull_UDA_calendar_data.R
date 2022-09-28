################################################################################
pull_UDA_calendar_data <- function(){
  
  con <- dbConnect(odbc::odbc(), "NCDR")
  
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
  
  UDA_calendar_data
}