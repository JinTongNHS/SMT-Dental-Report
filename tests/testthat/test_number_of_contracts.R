library(testthat)

source("../../R/SMT_functions.R")


test_that("data has the correct number of columns",{
  
  UDA_calendar_data <- readRDS("../../data/cleaned_data_up_to_october/UDA_calendar_data.rds")

  result <- ncol(UDA_calendar_data)
  
  expect_equal(result, 21)
  
  
})


test_that("number of contracts is calculated correctly for UDAs",{
  
  result <- get_num_contracts()
  
  expect_equal(result, 6906)
  
  
})


test_that("number of contracts is calculated correctly for UOAs",{
  
  result <- get_num_contracts(data = UOA_calendar_data, UDAorUOA = "UOA")
  
  expect_equal(result, 753)
  
  
})