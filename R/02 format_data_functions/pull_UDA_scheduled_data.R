################################################################################
pull_UDA_scheduled_data <- function(){
  
  con <- dbConnect(odbc::odbc(), "NCDR")
  
  sql <- "SELECT [data_month]
      ,[contract_number]
      ,[name_or_company_name]
      ,[commissioner_name]
      ,[contract_type]
      ,[paid_by_BSA]
      ,[contract_start_date]
      ,[contract_end_date]
      ,[annual_contracted_UDA]
      ,[annual_contracted_UOA]
      ,[UDA_delivered]
      ,[general_FP17s]
      ,[UDA_delivered_prev_year]
      ,[general_FP17s_prev_year]
      ,[UDA_delivered_prev_2_year]
      ,[general_FP17s_prev_2_year]
      ,[UDA_band_1]
      ,[UDA_band_2]
      ,[UDA_band_3]
      ,[UDA_urgent]
      ,[UDA_other]
      ,[FP17s_band_1]
      ,[FP17s_band_2]
      ,[FP17s_band_3]
      ,[FP17s_band_urgent]
      ,[FP17s_band_other]
      ,[UDA_band_1_prev_year]
      ,[UDA_band_2_prev_year]
      ,[UDA_band_3_prev_year]
      ,[UDA_urgent_prev_year]
      ,[UDA_other_prev_year]
      ,[FP17s_band_1_prev_year]
      ,[FP17s_band_2_prev_year]
      ,[FP17s_band3_prev_year]
      ,[FP17s_urgent_prev_year]
      ,[FP17s_other_prev_year]
      ,[UDA_band_1_prev_2_year]
      ,[UDA_band_2_prev_2_year]
      ,[UDA_band_3_prev_2_year]
      ,[UDA_urgent_prev_2_year]
      ,[UDA_other_prev_2_year]
      ,[FP17s_band_1_prev_2_year]
      ,[FP17s_band_2_prev_2_year]
      ,[FP17s_band_3_prev_2_year]
      ,[FP17s_urgent_prev_2_year]
      ,[FP17s_other_prev_2_year]
  FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[UDA_scheduled]"
  result <- dbSendQuery(con, sql)
  UDA_scheduled_data <- dbFetch(result)
  dbClearResult(result)
  
  UDA_scheduled_data
}
