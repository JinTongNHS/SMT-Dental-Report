################################################################################
pull_PCDID_data <- function(){
  
  con <- dbConnect(odbc::odbc(), "NCDR")
  
  sql <- "SELECT *
  FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[UDA_delivery_scheduled]"
  result <- dbSendQuery(con, sql)
  UDA_calendar_data <- dbFetch(result)
  dbClearResult(result)
  
  UDA_calendar_data
}


################################################################################
pull_SOF_data <- function(){
  
  con <- dbConnect(odbc::odbc(), "NCDR")
  
  sql <- "SELECT *
  FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[SOF_S109a]"
  result <- dbSendQuery(con, sql)
  SOF_data <- dbFetch(result)
  dbClearResult(result)
  
  SOF_data
}

################################################################################
pull_delivery_metric_data <- function(){
  
  con <- dbConnect(odbc::odbc(), "NCDR")
  
  sql <- "SELECT *
  FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[metric].[UDA_delivery_scheduled_ICB] "
  result <- dbSendQuery(con, sql)
  SOF_data <- dbFetch(result)
  dbClearResult(result)
  
  SOF_data
}


################################################################################
get_SOF_spreadsheet_data <- function(){
  
  data <- pull_delivery_metric_data()
  SOF_commissioner_code_lookup <- read_excel("N:/_Everyone/Primary Care Group/SMT_Dental DENT 2022_23-008/data_for_monthly_report/SOF_commissioner_code_lookup.xlsx")
  
  data <- data %>%
    filter(scheduled_month >= as.Date("2019-04-01")) %>%
    left_join(SOF_commissioner_code_lookup, by = "commissioner_ODS_code_ICB") %>%
    arrange(scheduled_month, commissioner_name_SOF)
  
  numerator_data <- data %>%
    select(month = scheduled_month,
           commissioner_ODS_code_ICB,
           commissioner_name_SOF,
           scaled_monthly_UDAs_delivered) %>%
    arrange(month)
  
  denominator_data <- data %>%
    select(month = scheduled_month,
           commissioner_ODS_code_ICB,
           commissioner_name_SOF,
           annual_contracted_UDAs) %>%
    arrange(month)
  
  rate_data <- data %>%
    select(month = scheduled_month,
           commissioner_ODS_code_ICB,
           commissioner_name_SOF,
           scaled_perc_UDAs_delivered) %>%
    arrange(month)
  
  numerator_data <- numerator_data %>%
    pivot_wider(names_from = month,
                values_from = scaled_monthly_UDAs_delivered) %>%
    arrange(commissioner_name_SOF)
  
  denominator_data <- denominator_data %>%
    pivot_wider(names_from = month,
                values_from = annual_contracted_UDAs) %>%
    arrange(commissioner_name_SOF)
  
  rate_data <- rate_data %>%
    pivot_wider(names_from = month,
                values_from = scaled_perc_UDAs_delivered) %>%
    arrange(commissioner_name_SOF)
  
  list(numerator = numerator_data, denominator = denominator_data, rate = rate_data)
  
}