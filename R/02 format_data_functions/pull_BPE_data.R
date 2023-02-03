pull_BPE_data <- function(){
  
  con <- dbConnect(odbc::odbc(), "NCDR")
  sql <- "SELECT * FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[BPE]"
  result <- dbSendQuery (con, sql)
  bpe_all <- dbFetch(result)
  dbClearResult(result)
  bpe_all
  
}