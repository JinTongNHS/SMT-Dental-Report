################################################################################
pull_UDA_scheduled_data <- function(){
  
  con <- dbConnect(odbc::odbc(), "NCDR")
  
  sql <- "SELECT *
  FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[UDA_scheduled]"
  result <- dbSendQuery(con, sql)
  UDA_scheduled_data <- dbFetch(result)
  dbClearResult(result)
  
  UDA_scheduled_data
}
