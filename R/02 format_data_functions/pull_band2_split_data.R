################################################################################
pull_band2_split_data <- function(){
  
  con <- DBI::dbConnect(odbc::odbc(), 
                        Driver = "ODBC Driver 18 for SQL Server", 
                        Server = "PRODNHSESQL101", 
                        Encrypt = "Optional", 
                        Trusted_connection = "yes") 
  
  
  sql <- "SELECT *
  FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[UDA_scheduled_band_2_split]"
  result <- dbSendQuery(con, sql)
  band2_split_data <- dbFetch(result)
  dbClearResult(result)
  
  band2_split_data
}
