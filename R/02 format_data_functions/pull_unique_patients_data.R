################################################################################
pull_unique_patients_data <- function(){
  
  con <- dbConnect(odbc::odbc(), "NCDR")
  
  sql <- "SELECT [month_ending]
      ,[contract_number]
      ,[unique_patients_rolling_12M]
      ,[band1_unique_patients_rolling_12M]
      ,[band2_or_3_unique_patients_rolling_12M]
      ,[band1_urgent_unique_patients_rolling_12M]
      ,[band_other_unique_patients_rolling_12M]
  FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[unique_patients_rolling_12_month]"
  result <- dbSendQuery(con, sql)
  unique_patients_data<- dbFetch(result)
  dbClearResult(result)
  
  unique_patients_data
}