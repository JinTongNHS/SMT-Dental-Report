################################################################################
pull_UDA_scheduled_historical_data <- function(){
  
  con <- dbConnect(odbc::odbc(), "NCDR")
  
  sql <- "SELECT *  FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[UDA_scheduled_historical]"
  result <- dbSendQuery(con, sql)
  UDA_scheduled_historical_data <- dbFetch(result)
  dbClearResult(result)
  
  UDA_scheduled_historical_data
}