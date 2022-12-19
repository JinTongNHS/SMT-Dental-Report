library(tidyverse)
library(readxl)
library(DBI)

source("R/02 format_data_functions/pull_UDA_scheduled_historical_data.R")
historical_UDA_scheduled_data <- pull_UDA_scheduled_historical_data()
STP_ICB_lookup_codes <- read_excel("N:/_Everyone/Primary Care Group/SMT_Dental DENT 2022_23-008/data_for_monthly_report/STP_ICB_lookup_codes.xlsx")
national_acorn <- read_csv("N:/_Everyone/Primary Care Group/SMT_Dental DENT 2022_23-008/Acorn/national_acorn.csv")
X2018_icb_npop_2022_11_02 <- read_csv("N:/_Everyone/Primary Care Group/SMT_Dental DENT 2022_23-008/Acorn/2018_icb_npop_2022-11-02.csv")
X2019_icb_npop_2022_11_02 <- read_csv("N:/_Everyone/Primary Care Group/SMT_Dental DENT 2022_23-008/Acorn/2019_icb_npop_2022-11-02.csv")
X2020_icb_npop_2022_11_02 <- read_csv("N:/_Everyone/Primary Care Group/SMT_Dental DENT 2022_23-008/Acorn/2020_icb_npop_2022-11-02.csv")
X2021_icb_npop_2022_11_02 <- read_csv("N:/_Everyone/Primary Care Group/SMT_Dental DENT 2022_23-008/Acorn/2021_ward_icb_2022-11-02.csv")

national_npop_udas <- read_csv("N:/_Everyone/Primary Care Group/SMT_Dental DENT 2022_23-008/Acorn/national_acorn.csv")

#bind together ICB level acorn data across years
X2018_icb_npop_2022_11_02 <- X2018_icb_npop_2022_11_02 %>%
  mutate(year = "2018/19")

X2019_icb_npop_2022_11_02 <- X2019_icb_npop_2022_11_02 %>%
  mutate(year = "2019/20")

X2020_icb_npop_2022_11_02 <- X2020_icb_npop_2022_11_02 %>%
  mutate(year = "2020/21")

X2021_icb_npop_2022_11_02 <- X2021_icb_npop_2022_11_02 %>%
  mutate(year = "2021/22")

icb_npop <- bind_rows(X2018_icb_npop_2022_11_02, X2019_icb_npop_2022_11_02,
                      X2020_icb_npop_2022_11_02, X2021_icb_npop_2022_11_02)


#filters out "not known" and "not specified" genders
national_npop_udas <- national_npop_udas %>%
  filter(Gender %in% c(1,2)) 

#change column names
national_npop_udas <- national_npop_udas %>%
  select(acorn_category = Acorn_Category, 
         gender = Gender,
         age_band = Age.Group,
         national_UDAs_delivered_per_head_of_population_segment = "National UDAs per head of population")

#join in national UDAs per head of population segment to ICB level data 
icb_npop_comparison <- icb_npop %>%
  filter(gender %in% c(1,2)) %>%
  left_join(national_npop_udas, by = c("gender", "acorn_category", "age_band")) %>%
  mutate(expected_UDAs_delivered = national_UDAs_delivered_per_head_of_population_segment * npop)

#sum across all population segments for each ICB
icb_expected_UDAs_delivered <- icb_npop_comparison %>%
  group_by(year, icb) %>%
  summarise(npop = sum(npop),
            expected_UDAs_delivered = sum(expected_UDAs_delivered, na.rm = TRUE))

#Join in commissioner codes 
ICB_code_lookup <- STP_ICB_lookup_codes %>%
  select(icb = commissioner_ONS_boundary_code_ICB,
         commissioner_name = commissioner_name_ICB)

#Get actual UDAs commissioned and delivered
icb_actual_UDAs_commissioned_and_delivered <- historical_UDA_scheduled_data %>%
  group_by(year, commissioner_name) %>%
  summarise(annual_contracted_UDA = sum(annual_contracted_UDA, na.rm = TRUE) / 12,
            UDA_delivered = sum(UDA_delivered, na.rm = TRUE)) %>%
  left_join(ICB_code_lookup, by = "commissioner_name")

#compare actual figures with expected
icb_expected_vs_actual_UDAs_commissioned <- icb_expected_UDAs_delivered %>%
  left_join(icb_actual_UDAs_commissioned_and_delivered, by = c("year", "icb")) %>%
  #mutate(actual_UDAs_commissioned_divided_by_expected_UDAs_delivered = annual_contracted_UDA / expected_UDAs_delivered) %>%
  mutate(actual_UDAs_delivered_divided_by_expected_UDAs_delivered = UDA_delivered / expected_UDAs_delivered)


  
