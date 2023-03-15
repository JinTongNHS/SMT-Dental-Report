library(testthat)
library(tidyverse)
library(readxl)
library(DBI)
library(odbc)

source("../../R/02 format_data_functions/get_delivery_profile_data.R")


test_that("UDA calendar data has the correct columns",{
  
test_data <- read_excel("testdata/test_get_delivery_profile_data_data.R.xlsx")
  
  expect_equal(result, correct_answers)
  
  
})

