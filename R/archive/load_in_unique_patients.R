library(lubridate)


files <- list.files(path="data/unique_patients/202004_to_202203", pattern="*.xlsx", full.names=TRUE, recursive=FALSE)
month <- as.Date("2020-04-01", format = "%Y-%m-%d")

for (f in files){
  
  print(f)
  
  #skip first rows
  f_clean <- read_excel(f, skip = 18)
  
  #remove last 7 rows
  f_clean <- f_clean %>%
    rename("contract_number" = "Contract Number", 
           "unique_patients_rolling_12M" = "Unique Patient Count Rolling 12M", 
           "band1_unique_patients_rolling_12M" = "Band 1 Unique Patient Count Rolling 12M", 
           "band2_or_3_unique_patients_rolling_12M" = "Band 2 or Band 3 Unique Patient Count Rolling 12M", 
           "band1_urgent_unique_patients_rolling_12M" = "Band 1 Urgent Unique Patient Count Rolling 12M", 
           "band_other_unique_patients_rolling_12M" = "Other Unique Patient Count Rolling 12M") %>%
    filter(!is.na(contract_number)) %>%
    mutate(month_ending = month)
  
  unique_patients_rolling_202004_202203 <- bind_rows(unique_patients_rolling_202004_202203, f_clean)
  
  month <- month %m+% period("1 month")

  
  }