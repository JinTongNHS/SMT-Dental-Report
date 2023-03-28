pull_new_vs_returning_patients <- function(){
  
  con <- dbConnect(odbc::odbc(), "NCDR")
  sql <- "SELECT * FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[New_vs_Returning_Patients]"
  
  result <- dbSendQuery (con, sql)
  new_vs_return <- dbFetch(result)
  dbClearResult(result)
  new_vs_return
}