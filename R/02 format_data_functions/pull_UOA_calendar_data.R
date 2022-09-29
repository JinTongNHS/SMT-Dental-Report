################################################################################
pull_UOA_calendar_data <- function(){
  
  con <- dbConnect(odbc::odbc(), "NCDR")
  
  sql <- "SELECT *
  FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[UOA_calendar]"
  result <- dbSendQuery(con, sql)
  UOA_calendar_data <- dbFetch(result)
  dbClearResult(result)
  
  UOA_calendar_data
}