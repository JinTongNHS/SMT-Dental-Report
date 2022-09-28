################################################################################
pull_UOA_calendar_data <- function(){
  
  con <- dbConnect(odbc::odbc(), "NCDR")
  
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
  
  UOA_calendar_data
}