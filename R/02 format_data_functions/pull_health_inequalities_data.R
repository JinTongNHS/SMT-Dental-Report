################################################################################
pull_health_inequalities <- function(){
  
  con <- dbConnect(odbc::odbc(), "NCDR")
  
  sql <- "SELECT *
  FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[health_inequalities_data]"
  result <- dbSendQuery(con, sql)
  data <- dbFetch(result)
  dbClearResult(result)
  
  data
}

# #initialise blank data frame
# data_health_inequalities_new <- data.frame()
# 
# for (f in list.files(path = "data/health_inequalities_data/new_data/", pattern="*.xlsx$")) {
#   
#   data <- read_excel(paste0("data/health_inequalities_data/new_data/",f), skip = 25)
#   
#   data<- data %>% 
#     rename(region_code = "Current Region Code", 
#            region_name = "Current Region Name", 
#            commissioner_code = "Commissioner Code", 
#            commissioner_name = "Commissioner Name", 
#            age_group = "Age Group", 
#            gender = "Gender", 
#            ethnic_group = "Ethnic Group Description Groupings", 
#            patient_charge_status = "Patient Charge Status", 
#            exemption_type = "Exemption/Remission Type", 
#            band1 = "Forms With Band 1", 
#            band2 = "Forms With Band 2", 
#            band3 = "Forms With Band 3", 
#            urgent = "Forms With Urgent Treatment", 
#            total_FP17 = "Total FP17s", 
#            year_month = "Year Month") %>%
#     mutate(data_month = as.Date(paste0(substr(year_month, 1, 4), "-",substr(year_month, 5, 6), "-01"))) %>%
#     filter(!is.na(date_month)) #filters out additional lines read in by excel
#     
#   
#   data_health_inequalities_new <- bind_rows(data_health_inequalities_new, data)
#     
#     # mutate(ethnic_group = if_else(ethnic_group == "N/A",
#     #                               "Not specified"))
# }
# 
# 
# 
# dbWriteTable(con, Id(catalog="NHSE_Sandbox_PrimaryCareNHSContracts",schema="Dental",table="health_inequalities_data"),
#              value = testo, row.names = FALSE, append = FALSE, overwrite = TRUE)