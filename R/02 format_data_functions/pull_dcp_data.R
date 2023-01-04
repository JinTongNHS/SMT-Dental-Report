pull_dcp_data <- function(){
  con <- dbConnect(odbc::odbc(), "NCDR")
  sql <- "SELECT * FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[DCP]"
  result <- dbSendQuery (con, sql)
  DCP_all <- dbFetch(result)
  dbClearResult(result)
  
  DCP_all <- DCP_all %>%
    rename(Contract_Number = `Contract Number`,
           Contract.Type = `Contract Type`,
           Name.or.Company.Name = `Name or Company Name`, 
           commissioner_name = `Commissioner Name`, 
           Paid.by.BSA =`Paid by BSA`, 
           Contract.Start.Date = `Contract Start Date`, 
           Contract.End.Date = `Contract End Date`,  
           DCP_description = `DCP Description`,
           UDA_Delivered_Current_Year =  `@{pv_mth} UDA Delivered Current Year`,               
           FP17_Current_Year_total = `@{pv_mth} General FP17s Current Year`, 
           UDA_Delivered_Previous.Year_NN = `@{pv_mth} UDA Delivered Previous Year`, 
           General.FP17s.Previous.Year_NN = `@{pv_mth} General FP17s Previous Year`, 
           Band_1._UDA = `UDA - Band 1 @{pv_mth} (Current Year)`,          
           Band_2._UDA = `UDA - Band 2 @{pv_mth} (Current Year)` ,        
           Band_3._UDA = `UDA - Band 3 @{pv_mth} (Current Year)`,
           Urgent_UDA = `UDA - Urgent @{pv_mth} (Current Year)`,
           other_UDA = `UDA - Other (General) @{pv_mth} (Current Year)`, 
           FP17_Current_Year_B1= `FP17s - Band 1 @{pv_mth} (Current Year)`,       
           FP17_Current_Year_B2 = `FP17s - Band 2 @{pv_mth} (Current Year)`,       
           FP17_Current_Year_B3 = `FP17s - Band 3 @{pv_mth} (Current Year)`,       
           FP17_Current_Year_urgent = `FP17s - Band Urgent @{pv_mth} (Current Year)`,
           FP17_Current_Year_other= `FP17s - Band Other @{pv_mth} (Current Year)`)
  
  DCP_all
}