################################################################################
pull_payments_to_dentist <- function(){
  
  con <- dbConnect(odbc::odbc(), "NCDR")
  
  sql <- "SELECT *  FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[BSA_payments_to_dentists]"
  result <- dbSendQuery(con, sql)
  payments_to_dentists <- dbFetch(result)
  dbClearResult(result)
  
  payments_to_dentists
  
}