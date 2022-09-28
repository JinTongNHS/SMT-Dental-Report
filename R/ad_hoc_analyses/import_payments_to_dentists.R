library(tidyverse)
library(readxl)

#load in data
payments_to_dentists_2015_2016 <- read_excel("data/payments_to_dentists/payments_to_dentists_2015_2016.xlsx",sheet = "2015-16 contract payment", col_types = "text")
payments_to_dentists_2016_2017 <- read_excel("data/payments_to_dentists/payments_to_dentists_2016_2017.xlsx",sheet = "2016-17 contract payment", col_types = "text")
payments_to_dentists_2017_2018 <- read_excel("data/payments_to_dentists/payments_to_dentists_2017_2018.xlsx",sheet = "2017-18 contract payment", col_types = "text")
payments_to_dentists_2018_2019 <- read_excel("data/payments_to_dentists/payments_to_dentists_2018_19.xlsx",sheet = "2018-19 contract payment", col_types = "text")
payments_to_dentists_2019_2020 <- read_excel("data/payments_to_dentists/payments_to_dentists_2019_20.xlsx",sheet = "19-20 Payments to Dentists", col_types = "text")
payments_to_dentists_2020_2021 <- read_excel("data/payments_to_dentists/payments_to_dentists_2020_2021.xlsx",sheet = "20-21 Payments to Dentists", col_types = "text")
area_to_region_lookup <- read_excel("data/payments_to_dentists/area_to_region_lookup.xlsx")

#add a column with the financial year to each dataset
payments_to_dentists_2015_2016 <- mutate(payments_to_dentists_2015_2016, year = "2015/16")
payments_to_dentists_2016_2017 <- mutate(payments_to_dentists_2016_2017, year = "2016/17")
payments_to_dentists_2017_2018 <- mutate(payments_to_dentists_2017_2018, year = "2017/18")
payments_to_dentists_2018_2019 <- mutate(payments_to_dentists_2018_2019, year = "2018/19")
payments_to_dentists_2019_2020 <- mutate(payments_to_dentists_2019_2020, year = "2019/20")
payments_to_dentists_2020_2021 <- mutate(payments_to_dentists_2020_2021, year = "2020/21")

#bind datasets together
payments_to_dentists_data <- bind_rows(payments_to_dentists_2015_2016,
                                       payments_to_dentists_2016_2017,
                                       payments_to_dentists_2017_2018,
                                       payments_to_dentists_2018_2019,
                                       payments_to_dentists_2019_2020,
                                       payments_to_dentists_2020_2021)

#remove spaces from column names
names(payments_to_dentists_data) <- names(payments_to_dentists_data) %>% make.names()
names(area_to_region_lookup) <- names(area_to_region_lookup) %>% make.names()

#remove duplicate rows and correct miscoded area
area_to_region_lookup <- area_to_region_lookup %>%
  mutate(region = if_else(Area.Code == "Q70", "South East", region)) %>%
  select("Area.Code","region") %>%
  distinct()

#join in region based on area
payments_to_dentists_data <- payments_to_dentists_data %>%
  left_join(area_to_region_lookup, by = "Area.Code")


#fill in region column 
payments_to_dentists_data <- payments_to_dentists_data %>%
  mutate(Region_Name = case_when(is.na(Region.Name) ~ region,
                                 TRUE ~ Region.Name))
# 
# #group by and summarise data that you need
# payments_to_dentists_summary <- payments_to_dentists_data %>%
#   mutate(Baseline.Contract = as.numeric(Baseline.Contract),
#          Total.Contracted.UDA = as.numeric(Total.Contracted.UDA),
#          Total.Contracted.UOA = as.numeric(Total.Contracted.UOA)) %>%
#   group_by(year, Region_Name) %>%
#   summarise(`Baseline contract (£)` = sum(Baseline.Contract, na.rm = TRUE),
#             `UDAs commissioned` = sum(Total.Contracted.UDA, na.rm = TRUE),
#             `UOAs commissioned` = sum(Total.Contracted.UOA, na.rm = TRUE)
#             )
# 
# num_contracts <- payments_to_dentists_data %>%
#   group_by(year, Region_Name) %>%
#   count() %>%
#   rename(`Number of contracts` = n)
#   
# 
# #Join in number of contracts
# payments_to_dentists_summary <- payments_to_dentists_summary %>%
#   left_join(num_contracts, by = c("year", "Region_Name")) %>%
#   rename(`Financial year` = year,
#          `Region name` = Region_Name) %>%
#   filter(!is.na(`Region name`))
#   
# ################################################################################
# #the same but not grouped by region
# #group by and summarise data that you need
#   payments_to_dentists_national_summary <- payments_to_dentists_data %>%
#     mutate(Baseline.Contract = as.numeric(Baseline.Contract),
#            Total.Contracted.UDA = as.numeric(Total.Contracted.UDA),
#            Total.Contracted.UOA = as.numeric(Total.Contracted.UOA)) %>%
#     group_by(year) %>%
#     summarise(`Baseline contract (£)` = sum(Baseline.Contract, na.rm = TRUE),
#               `UDAs commissioned` = sum(Total.Contracted.UDA, na.rm = TRUE),
#               `UOAs commissioned` = sum(Total.Contracted.UOA, na.rm = TRUE)
#     )
# 
#   num_contracts_national <- payments_to_dentists_data %>%
#     group_by(year) %>%
#     count() %>%
#     rename(`Number of contracts` = n)
# 
# 
#   #Join in number of contracts
#   payments_to_dentists_national_summary <- payments_to_dentists_national_summary %>%
#     left_join(num_contracts_national, by = c("year")) %>%
#     rename(`Financial year` = year)
# 
# ################################################################################
# #the same but not grouped by year
# #group by and summarise data that you need
# payments_to_dentists_regional_summary <- payments_to_dentists_data %>%
#   mutate(Baseline.Contract = as.numeric(Baseline.Contract),
#          Total.Contracted.UDA = as.numeric(Total.Contracted.UDA),
#          Total.Contracted.UOA = as.numeric(Total.Contracted.UOA)) %>%
#   group_by(Region_Name) %>%
#   summarise(`Baseline contract (£)` = sum(Baseline.Contract, na.rm = TRUE),
#             `UDAs commissioned` = sum(Total.Contracted.UDA, na.rm = TRUE),
#             `UOAs commissioned` = sum(Total.Contracted.UOA, na.rm = TRUE)
#   )
# 
# num_contracts_regional <- payments_to_dentists_data %>%
#   group_by(Region_Name) %>%
#   count() %>%
#   rename(`Number of contracts` = n)
# 
# 
# #Join in number of contracts
# payments_to_dentists_regional_summary <- payments_to_dentists_regional_summary %>%
#   left_join(num_contracts_regional, by = c("Region_Name")) %>%
#   rename(`Region name` = Region_Name) %>%
#   filter(!is.na(`Region name`))
# 
#   
# 
# ################################################################################
# payments_to_dentists_summary2 <- payments_to_dentists_summary %>%
#   mutate(`Financial year` = case_when(`Financial year` == "2015/16" ~ 2015,
#                               `Financial year` == "2016/17" ~ 2016,
#                               `Financial year` == "2017/18" ~ 2017,
#                               `Financial year` == "2018/19" ~ 2018,
#                               `Financial year` == "2019/20" ~ 2019,
#                               `Financial year` == "2020/21" ~ 2020))
# 
# ggplot(payments_to_dentists_summary2) +
#   geom_line(aes(x = `Financial year`,
#                 y = `Number of contracts`,
#                 colour = `Region name`
#                 ))


