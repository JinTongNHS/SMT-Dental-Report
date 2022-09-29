################################################################################
pull_unique_patients_data <- function(){
  
  con <- dbConnect(odbc::odbc(), "NCDR")
  
  sql <- "SELECT *
  FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[unique_patients_rolling_12_month]"
  result <- dbSendQuery(con, sql)
  unique_patients_data<- dbFetch(result)
  dbClearResult(result)
  
  unique_patients_data
}