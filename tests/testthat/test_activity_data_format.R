library(testthat)
library(tidyverse)
library(readxl)
library(DBI)
library(odbc)

source("../../R/02 format_data_functions/pull_UDA_calendar_data.R")
source("../../R/02 format_data_functions/pull_UOA_calendar_data.R")


test_that("UDA calendar data has the correct columns",{
  
  UDA_calendar_data <- pull_UDA_calendar_data()
  
  correct_answers <- c("data_month", "contract_number", "latest_contract_type", "name_or_company_name", 
                       "commissioner_name", "commissioner_ods_code_icb", "region_name", 
                       "paid_by_BSA", "contract_start_date", "contract_end_date", "UDA_total", 
                       "UDA_band_1_total", "UDA_band_2_total", "UDA_band_3_total", "UDA_urgent_total", 
                       "UDA_other_total", "total_FP17s", "total_band_1_FP17s", "total_band_2_FP17s", 
                       "total_band_3_FP17s", "total_urgent_FP17s", "total_other_FP17s")

  result <- colnames(UDA_calendar_data)
  
  expect_equal(result, correct_answers)
  
  
})

test_that("UOA calendar data has the correct columns",{
  
  UOA_calendar_data <- pull_UOA_calendar_data()
  
  correct_answers <- c("data_month", "contract_number", "contract_type", "name_or_company_name", 
                       "commissioner_name", "commissioner_ods_code_icb", "region_name", 
                       "paid_by_BSA", "contract_start_date", "contract_end_date", "UOA_total")
  
  result <- colnames(UOA_calendar_data)
  
  expect_equal(result, correct_answers)
  
  
})

#might want to test column types, number of rows for historic data