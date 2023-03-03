################################################################################
pull_UDA_UOA_value <- function(){
  
  con <- dbConnect(odbc::odbc(), "NCDR")
  
  sql <- "SELECT [Date]
      ,[Latest.Region]
      ,[Latest.Region.Description]
      ,[Commissioner.Code]
      ,[Commissioner.Name]
      ,[Contract.Number]
      ,[Latest.Provider.Name]
      ,[Contract.Type]
      ,[Latest.Contract.Start.Date]
      ,[Latest.Contract.End.Date]
      ,REPLACE([Total.Financial.Value], '£', '') AS [Total.Financial.Value]
      ,REPLACE([UDA.Financial.Value], '£', '') AS [UDA.Financial.Value]
      ,[UDA.Performance.Target]
      ,[UDA.Carry.Forward]
      ,REPLACE([UOA.Financial.Value], '£', '') AS [UOA.Financial.Value]
      ,[UOA.Performance.Target]
      ,[UOA.Carry.Forward]
      ,REPLACE([Cost.per.UDA], '£', '') AS [Cost.per.UDA]
      ,REPLACE([Cost.Per.UOA], '£', '') AS [Cost.Per.UOA]
  FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[UDA_UOA_Value_Reg]"

  result <- dbSendQuery(con, sql)
  data <- dbFetch(result)
  dbClearResult(result)
  
  data
  
}