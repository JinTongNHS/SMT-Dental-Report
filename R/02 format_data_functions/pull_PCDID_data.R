################################################################################
pull_PCDID_data <- function(){
  
  con <- dbConnect(odbc::odbc(), "NCDR")
  
  sql <- "SELECT *
  FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[UDA_delivery_scheduled]"
  result <- dbSendQuery(con, sql)
  UDA_calendar_data <- dbFetch(result)
  dbClearResult(result)
  
  UDA_calendar_data
}


################################################################################
pull_SOF_data <- function(){
  
  con <- dbConnect(odbc::odbc(), "NCDR")
  
  sql <- "SELECT *
  FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[SOF_S109a]"
  result <- dbSendQuery(con, sql)
  SOF_data <- dbFetch(result)
  dbClearResult(result)
  
  SOF_data
}