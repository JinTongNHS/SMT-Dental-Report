library(odbc)
con <- dbConnect(odbc::odbc(), "NCDR")

# sql=" 
# SELECT distinct 
# [Month]
#   ,[ContractorCode] = [Contractor Code]
#   ,[Measure]
#   ,[Figure]
#   FROM [NHSE_Sandbox_DispensingReporting].[dbo].[Pharmacy_Dashboard_Master]
#   where [Measure] in ('Medicine Use Review and Prescription Intervention Service (MUR) Activity',
#                       'Appliance Use Reviews (AUR) Activity',
#                       'Items Dispensed',
#                       'Covid Vaccination Service Activity',
#                       'Hepatitis C Antibody Testing Service Activity',
#                       'New Medicine Service (NMS) Activity',
#                       -- 'Community Pharmacist Consultation Service (CPCS) Activity',
#                       'Stoma Customisation (STOMA) Activity')
#    "
# 
# result<-dbSendQuery(con,sql)
# Master<-dbFetch(result)

dbWriteTable(con, name = "[NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[UDA_calendar]", value = UDA_calendar_data, row.names = FALSE, append = FALSE, overwrite = TRUE)

#CPCS data
dbClearResult(result)