pull_September_BSA_UDA_value_data <- function(){
  
  con <- dbConnect(odbc::odbc(), "NCDR")
  sql <- "Select *
   from [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[September_UDA_BSA]"
  
  result <- dbSendQuery (con, sql)
  UDA_Data_pull <- dbFetch(result)
  dbClearResult(result)
  
  UDA_Data_pull
  
  
}