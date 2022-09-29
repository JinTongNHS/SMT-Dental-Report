################################################################################
pull_UOA_scheduled_data <- function(){
  
  con <- dbConnect(odbc::odbc(), "NCDR")
  
  sql <- "SELECT *
  FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[UOA_scheduled]"
  result <- dbSendQuery(con, sql)
  UOA_scheduled_data <- dbFetch(result)
  dbClearResult(result)
  
  UOA_scheduled_data
}