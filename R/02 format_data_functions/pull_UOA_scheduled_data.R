################################################################################
pull_UOA_scheduled_data <- function(){
  
  con <- dbConnect(odbc::odbc(), "NCDR")
  
  sql <- "SELECT [data_month]
      ,[contract_number]
      ,[contract_type]
      ,[name_or_company_name]
      ,[commissioner_name]
      ,[paid_by_BSA]
      ,[contract_start_date]
      ,[contract_end_date]
      ,[annual_contracted_UOA]
      ,[annual_contracted_UDA]
      ,[UOA_delivered]
      ,[UOA_delivered_prev_year]
      ,[UOA_delivered_prev_2_year]
      ,[orthodontic_FP17s]
      ,[orthodontic_FP17s_prev_year]
      ,[orthodontic_FP17s_prev_2_year]
      ,[orthodontic_starts]
      ,[orthodontic_completions]
      ,[UOA_financial_half_target]
  FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[UOA_scheduled]"
  result <- dbSendQuery(con, sql)
  UOA_scheduled_data <- dbFetch(result)
  dbClearResult(result)
  
  UOA_scheduled_data
}