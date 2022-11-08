pull_patient_exemption_data <- function(){
  
  con <- dbConnect(odbc::odbc(), "NCDR")
  sql <- "SELECT [Treatment Charge Band] as Treatment_Charge_Band 
      ,[Regions]
      ,[FY]
      ,[Child]
      ,[Adult-Exempt] as adult_exempt
      ,[Adult Non-Exempt] as adult_non_exempt
   FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[GEM\\MEmran].[Dental.[Exempt_UDA_2021]" 
  ##(problem with / solved by adding \\)
  
  result <- dbSendQuery (con, sql)
  exempt_data_2021 <- dbFetch(result)
  dbClearResult(result)
  
  exempt_data_2021
}

