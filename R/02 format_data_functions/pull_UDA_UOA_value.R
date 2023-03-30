################################################################################
pull_UDA_UOA_value <- function(){
  
  con <- dbConnect(odbc::odbc(), "NCDR")
  
  sql <- "SELECT *
  FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[UDA_UOA_Value_Reg]"

  result <- dbSendQuery(con, sql)
  data <- dbFetch(result)
  dbClearResult(result)
  
  data
  
}