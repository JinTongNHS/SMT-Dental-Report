library(tidyverse)

################################################################################
pull_UDA_calendar_data <- function(){
  
  con <- dbConnect(odbc::odbc(), "NCDR")
  
  sql <- "SELECT [data_month]
      ,[contract_number]
      ,[latest_contract_type]
      ,[name_or_company_name]
      ,[commissioner_name]
      ,[region_name]
      ,[paid_by_BSA]
      ,[contract_start_date]
      ,[contract_end_date]
      ,[UDA_total]
      ,[UDA_band_1_total]
      ,[UDA_band_2_total]
      ,[UDA_band_3_total]
      ,[UDA_urgent_total]
      ,[UDA_other_total]
      ,[total_FP17s]
      ,[total_band_1_FP17s]
      ,[total_band_2_FP17s]
      ,[total_band_3_FP17s]
      ,[total_urgent_FP17s]
      ,[total_other_FP17s]
  FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[UDA_calendar]"
  result <- dbSendQuery(con, sql)
  UDA_calendar_data <- dbFetch(result)
  dbClearResult(result)
  
  UDA_calendar_data
}

################################################################################
pull_UOA_calendar_data <- function(){
  
  con <- dbConnect(odbc::odbc(), "NCDR")
  
  sql <- "SELECT [data_month]
      ,[contract_number]
      ,[contract_type]
      ,[name_or_company_name]
      ,[commissioner_name]
      ,[paid_by_BSA]
      ,[contract_start_date]
      ,[contract_end_date]
      ,[UOA_total]
  FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[UOA_calendar]"
  result <- dbSendQuery(con, sql)
  UOA_calendar_data <- dbFetch(result)
  dbClearResult(result)
  
  UOA_calendar_data
}


################################################################################
pull_UDA_scheduled_data <- function(){
  
  con <- dbConnect(odbc::odbc(), "NCDR")
  
  sql <- "SELECT [data_month]
      ,[contract_number]
      ,[name_or_company_name]
      ,[commissioner_name]
      ,[contract_type]
      ,[paid_by_BSA]
      ,[contract_start_date]
      ,[contract_end_date]
      ,[annual_contracted_UDA]
      ,[annual_contracted_UOA]
      ,[UDA_delivered]
      ,[general_FP17s]
      ,[UDA_delivered_prev_year]
      ,[general_FP17s_prev_year]
      ,[UDA_delivered_prev_2_year]
      ,[general_FP17s_prev_2_year]
      ,[UDA_band_1]
      ,[UDA_band_2]
      ,[UDA_band_3]
      ,[UDA_urgent]
      ,[UDA_other]
      ,[FP17s_band_1]
      ,[FP17s_band_2]
      ,[FP17s_band_3]
      ,[FP17s_band_urgent]
      ,[FP17s_band_other]
      ,[UDA_band_1_prev_year]
      ,[UDA_band_2_prev_year]
      ,[UDA_band_3_prev_year]
      ,[UDA_urgent_prev_year]
      ,[UDA_other_prev_year]
      ,[FP17s_band_1_prev_year]
      ,[FP17s_band_2_prev_year]
      ,[FP17s_band3_prev_year]
      ,[FP17s_urgent_prev_year]
      ,[FP17s_other_prev_year]
      ,[UDA_band_1_prev_2_year]
      ,[UDA_band_2_prev_2_year]
      ,[UDA_band_3_prev_2_year]
      ,[UDA_urgent_prev_2_year]
      ,[UDA_other_prev_2_year]
      ,[FP17s_band_1_prev_2_year]
      ,[FP17s_band_2_prev_2_year]
      ,[FP17s_band_3_prev_2_year]
      ,[FP17s_urgent_prev_2_year]
      ,[FP17s_other_prev_2_year]
  FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[UDA_scheduled]"
  result <- dbSendQuery(con, sql)
  UDA_scheduled_data <- dbFetch(result)
  dbClearResult(result)
  
  UDA_scheduled_data
}

################################################################################
pull_UOA_scheduled_data <- function(){
  
  con <- dbConnect(odbc::odbc(), "NCDR")
  
  sql <- "SELECT [data_month]
      ,[contract_number]
      ,[contract_type]
      ,[name_or_company_name]
      ,[commissioner_name]
      ,[paid_by_BSA]
      ,[contract_start_date]
      ,[contract_end_date]
      ,[annual_contracted_UOA]
      ,[annual_contracted_UDA]
      ,[UOA_delivered]
      ,[UOA_delivered_prev_year]
      ,[UOA_delivered_prev_2_year]
      ,[orthodontic_FP17s]
      ,[orthodontic_FP17s_prev_year]
      ,[orthodontic_FP17s_prev_2_year]
      ,[orthodontic_starts]
      ,[orthodontic_completions]
      ,[UOA_financial_half_target]
  FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[UOA_scheduled]"
  result <- dbSendQuery(con, sql)
  UOA_scheduled_data <- dbFetch(result)
  dbClearResult(result)
  
  UOA_scheduled_data
}

################################################################################
pull_UDA_scheduled_historical_data <- function(){
  
  con <- dbConnect(odbc::odbc(), "NCDR")
  
  sql <- "SELECT [month]
      ,[contract_number]
      ,[band1_FP17]
      ,[band2_FP17]
      ,[band3_FP17]
      ,[other_FP17]
      ,[urgent_FP17]
      ,[band1_UDA]
      ,[band2_UDA]
      ,[band3_UDA]
      ,[other_UDA]
      ,[urgent_UDA]
      ,[total_FP17s]
      ,[total_UDAs]
  FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[UDA_scheduled_historical]"
  result <- dbSendQuery(con, sql)
  UOA_scheduled_historical_data <- dbFetch(result)
  dbClearResult(result)
  
  UOA_scheduled_historical_data
}


################################################################################
get_data_for_cumulative_plot <- function(data = UDA_calendar_data, 
                                            scheduled_data = UDA_scheduled_data,
                                            remove_prototypes = T,
                                         all_regions_and_STPs = F){
  
  #join in contracted UDAs from scheduled data
  contracted_UDAs <- scheduled_data %>%
    select(month, contract_number, annual_contracted_UDA)
  
  data <- data %>%
    left_join(contracted_UDAs, by = c("month", "contract_number"))
  
  #remove prototype contracts if specified
  if(remove_prototypes){
    #create not in function
    `%notin%` = Negate(`%in%`)
    data <- data %>%
      filter(contract_number %notin% prototype_contracts$prototype_contract_number) %>%
      filter(annual_contracted_UDA > 100)
  }
  
  #option to have data for all regions and STPs in one
  if(all_regions_and_STPs == TRUE){
    UDAs_delivered <- data %>%
      group_by(month, region_name, commissioner_name)
  }else{
    UDAs_delivered <- data %>%
      group_by(month)
  }

  #group by month and sum UDAs delivered
  UDAs_delivered <- UDAs_delivered %>%
    summarise(monthly_UDA_UOAs_delivered = sum(UDA_total, na.rm = T),
              total_annual_UDA_UOAs_contracted = sum(annual_contracted_UDA)) %>%
    mutate(threshold_perc = case_when(month >= as.Date("2021-04-01") & month < as.Date("2021-10-01") ~ 0.6,
                                   month >= as.Date("2021-10-01") & month < as.Date("2022-01-01") ~ 0.65,
                                   month >= as.Date("2022-01-01") & month < as.Date("2022-04-01") ~ 0.85,
                                   month >= as.Date("2022-04-01") & month < as.Date("2022-07-01") ~ 0.95)) %>%
    mutate(threshold_period = case_when(month >= as.Date("2021-04-01") & month < as.Date("2021-10-01") ~ lubridate::interval(as.Date("2021-04-01"), as.Date("2021-09-30")),
                                      month >= as.Date("2021-10-01") & month < as.Date("2022-01-01") ~ lubridate::interval(as.Date("2021-10-01"), as.Date("2021-12-31")),
                                      month >= as.Date("2022-01-01") & month < as.Date("2022-04-01") ~ lubridate::interval(as.Date("2022-01-01"), as.Date("2022-03-31")),
                                      month >= as.Date("2022-04-01") & month < as.Date("2022-07-01") ~ lubridate::interval(as.Date("2022-04-01"), as.Date("2022-06-30")))) %>%
    mutate(threshold_UDA_UOAs_contracted_in_threshold_period = total_annual_UDA_UOAs_contracted * threshold_perc * lubridate::time_length(threshold_period, "month")/ 12) 
  
}

################################################################################
get_data_for_cumulative_plot_UOA <- function(data = UOA_calendar_data, 
                                            scheduled_data = UOA_scheduled_data,
                                            remove_prototypes = TRUE,
                                            all_regions_and_STPs = FALSE){
  
  #join in contracted UDAs from scheduled data
  contracted_UOAs <- scheduled_data %>%
    select(month, contract_number, annual_contracted_UOA)
  
  data <- data %>%
    left_join(contracted_UOAs, by = c("month", "contract_number"))

  #remove prototype contracts if specified
  if(remove_prototypes){
    #create not in function
    `%notin%` = Negate(`%in%`)
    
    
    data <- data %>%
      filter(contract_number %notin% prototype_contracts$prototype_contract_number) %>%
    filter(annual_contracted_UOA > 0)
  }
  
  if(all_regions_and_STPs == FALSE){
    UOAs_delivered <- data %>%
      group_by(month)
  }else{
    UOAs_delivered <- data %>%
      group_by(month, region_name, commissioner_name)
  }
  #group by month and sum UDAs delivered
  UOAs_delivered <- UOAs_delivered %>%
    summarise(monthly_UDA_UOAs_delivered = sum(UOA_total, na.rm = T),
              total_annual_UDA_UOAs_contracted = sum(annual_contracted_UOA, na.rm = T)) %>%
    mutate(threshold_perc = case_when(month >= as.Date("2021-04-01") & month < as.Date("2021-10-01") ~ 0.8,
                                      month >= as.Date("2021-10-01") & month < as.Date("2022-01-01") ~ 0.85,
                                      month >= as.Date("2022-01-01") & month < as.Date("2022-04-01") ~ 0.90,
                                      month >= as.Date("2022-04-01") & month < as.Date("2022-07-01") ~ 1)) %>%
    mutate(threshold_period = case_when(month >= as.Date("2021-04-01") & month < as.Date("2021-10-01") ~ lubridate::interval(as.Date("2021-04-01"), as.Date("2021-09-30")),
                                        month >= as.Date("2021-10-01") & month < as.Date("2022-01-01") ~ lubridate::interval(as.Date("2021-10-01"), as.Date("2021-12-31")),
                                        month >= as.Date("2022-01-01") & month < as.Date("2022-04-01") ~ lubridate::interval(as.Date("2022-01-01"), as.Date("2022-03-31")),
                                        month >= as.Date("2022-04-01") & month < as.Date("2022-07-01") ~ lubridate::interval(as.Date("2022-04-01"), as.Date("2022-06-30")))) %>%
    mutate(threshold_UDA_UOAs_contracted_in_threshold_period = total_annual_UDA_UOAs_contracted * threshold_perc * lubridate::time_length(threshold_period, "month")/ 12) 
}



################################################################################
#function to get dental data into the right format for slide 4
get_delivery_data <- function(data = UDA_scheduled_data,
                                     calendar_data = UDA_calendar_data,
                                   #existing_data = slide5_UDA_delivery_historic, 
                                   remove_prototypes = T,
                                   UDAorUOA = "UDA",
                                   all_regions_and_STPs = F,
                                   renameColumns = F){
  

  #create not in function
  `%notin%` = Negate(`%in%`)
  
  #remove prototype contracts if specified
  if(remove_prototypes & UDAorUOA == "UDA"){
    data <- data %>%
      filter(contract_number %notin% prototype_contracts$prototype_contract_number)%>%
      filter(annual_contracted_UDA > 100)
  }else if(remove_prototypes & UDAorUOA == "UOA"){
    data <- data %>%
      filter(contract_number %notin% prototype_contracts$prototype_contract_number) %>%
      filter(annual_contracted_UOA > 0)
  }
  
  if(all_regions_and_STPs){
    # #add a region column to the data
    # region_STP_lookup <- calendar_data %>%
    #   select(commissioner_name, region_name) %>%
    #   distinct()
    # 
    # data <- left_join(data, region_STP_lookup, by = c("commissioner_name"))
    data <- data %>%
      group_by(month, region_name, commissioner_name)
  }else{
    data <- data %>%
      group_by(month)
  }
  
  #group by month and sum UDAs delivered
  UDA_UOAs_delivered <- data %>%
    summarise(monthly_UDA_UOAs_delivered = ifelse(UDAorUOA == "UDA", 
                                                   sum(UDA_delivered, na.rm = T),
                                                   sum(UOA_delivered, na.rm = T)),
              annual_contracted_UDA_UOA = ifelse(UDAorUOA == "UDA",
                                                 sum(annual_contracted_UDA, na.rm = T),
                                                 sum(annual_contracted_UOA, na.rm = T))
              )
  
  new_data <- UDA_UOAs_delivered %>%
    #April data needs scaling differently
    mutate(scaled_monthly_UDA_UOAs_delivered = if_else(month != as.Date("2021-04-01") & month != as.Date("2020-04-01") 
                                                       & month != as.Date("2019-04-01") & month != as.Date("2022-04-01"),
                                                       monthly_UDA_UOAs_delivered * 12,
                                                       monthly_UDA_UOAs_delivered * 18)) %>%
    mutate(perc_UDA_UOA_delivered = round(scaled_monthly_UDA_UOAs_delivered * 100 / annual_contracted_UDA_UOA)) 
  
  #for PCOR and SOF table
  if(renameColumns){
    
    #add a region column to the data
    region_STP_lookup <- calendar_data %>%
      select(commissioner_name, region_name) %>%
      distinct()
    
    new_data <- left_join(new_data, region_STP_lookup, by = c("commissioner_name"))
    
    new_data <- new_data %>%
      select(scheduled_month = month,
             commissioner_name,
             region_name,
             monthly_UDAs_delivered = monthly_UDA_UOAs_delivered,
             scaled_monthly_UDAs_delivered = scaled_monthly_UDA_UOAs_delivered,
             annual_contracted_UDAs = annual_contracted_UDA_UOA,
             scaled_perc_UDAs_delivered = perc_UDA_UOA_delivered)
  }
  
  new_data

}



################################################################################
#function to get dental data into the right format for slide 4
get_delivery_data_calendar <- function(calendar_data = UDA_calendar_data,  
                                              scheduled_data = UDA_scheduled_data,
                                     remove_prototypes = T,
                                     UDAorUOA = "UDA",
                                     regional_lines = F,
                                     STP_lines = F,
                                     cat_lines = F, 
                                     renameColumns = F){
  
  if(UDAorUOA == "UDA"){
    #join in contracted UDAs from scheduled data
    contracted_UDAs <- scheduled_data %>%
      select(month, contract_number, annual_contracted_UDA)
    
    data <- calendar_data %>%
      left_join(contracted_UDAs, by = c("month", "contract_number"))
  }else{
    contracted_UOAs <- scheduled_data %>%
      select(month, contract_number, annual_contracted_UOA, annual_contracted_UDA)
    
    data <- calendar_data %>%
      left_join(contracted_UOAs, by = c("month", "contract_number"))
  }
  
  
  #create not in function
  `%notin%` = Negate(`%in%`)
  
  #remove prototype contracts if specified
  if(remove_prototypes & UDAorUOA == "UDA"){
    data <- data %>%
      filter(contract_number %notin% prototype_contracts$prototype_contract_number) %>%
      filter(annual_contracted_UDA > 100)
  }else if(remove_prototypes & UDAorUOA == "UOA"){
    data <- data %>%
      filter(contract_number %notin% prototype_contracts$prototype_contract_number) %>%
      filter(annual_contracted_UOA > 0)
  }
  
  if(regional_lines){
    data <- data %>%
      group_by(month, region_name)
  }else if(STP_lines){
    data <- data %>%
      group_by(month, region_name, commissioner_name)
  }else if(cat_lines){
    data <- data %>%
      group_by(month, category_sub_type)
  }else{
    data <- data %>%
      group_by(month)
  }

  data <- data %>%
    summarise(monthly_UDA_UOAs_delivered = ifelse(UDAorUOA == "UDA",
                                                  sum(UDA_total, na.rm = T),
                                                  sum(UOA_total, na.rm = T)),
              annual_contracted_UDA_UOA = ifelse(UDAorUOA == "UDA",
                                                 sum(annual_contracted_UDA, na.rm = T),
                                                 sum(annual_contracted_UOA, na.rm = T))) %>%
    mutate(scaled_monthly_UDA_UOAs_delivered = monthly_UDA_UOAs_delivered * 12) %>%
    mutate(scaled_perc_UDA_UOA_delivered = monthly_UDA_UOAs_delivered * 12 * 100 / annual_contracted_UDA_UOA) %>%
    mutate(month = as.Date(month))


  #for PCOR and SOF table
  if(renameColumns){

    #add a region column to the data
    region_STP_lookup <- calendar_data %>%
      select(commissioner_name, region_name) %>%
      distinct()

    data <- left_join(data, region_STP_lookup, by = c("commissioner_name"))

    data <- data %>%
      select(calendar_month = month,
             commissioner_name,
             region_name,
             monthly_UDAs_delivered = monthly_UDA_UOAs_delivered,
             scaled_monthly_UDAs_delivered = scaled_monthly_UDA_UOAs_delivered,
             annual_contracted_UDAs = annual_contracted_UDA_UOA,
             scaled_perc_UDAs_delivered = scaled_perc_UDA_UOA_delivered)
  }

  data
}


################################################################################
get_banded_COTs_data <- function(data = UDA_scheduled_data, 
                                   historic_data = historical_UDA_scheduled_data,
                                   remove_prototypes = F){
  
  #bind old data to new data 
  data <- data %>%
    rename(total_FP17s = general_FP17s, band1_UDA = UDA_band_1, band2_UDA = UDA_band_2, 
           band3_UDA = UDA_band_3, urgent_UDA = UDA_urgent, other_UDA = UDA_other, band1_FP17 = FP17s_band_1, 
           band2_FP17 = FP17s_band_2, band3_FP17 = FP17s_band_3, urgent_FP17 = FP17s_band_urgent, 
           other_FP17 = FP17s_band_other) %>%
    select(month, contract_number, total_FP17s, band1_UDA, band2_UDA, band3_UDA, 
           urgent_UDA, other_UDA, band1_FP17, band2_FP17, band3_FP17, urgent_FP17, other_FP17) %>%
    mutate(total_UDAs = band1_UDA + band2_UDA + band3_UDA + other_UDA + urgent_UDA)
  
  data <- bind_rows(data, historic_data)
  
  #remove prototype contracts if specified
  if(remove_prototypes){
    #create not in function
    `%notin%` = Negate(`%in%`)
    data <- data %>%
      filter(contract_number %notin% prototype_contracts_orth$prototype_contract_number)%>%
      filter(annual_contracted_UDA > 100)
  }
  
  #group by month and sum UDAs delivered
  new_data <- data %>%
    group_by(month) %>%
    summarise(band1 = sum(band1_FP17, na.rm = T),
              band2 = sum(band2_FP17, na.rm = T),
              band3 = sum(band3_FP17, na.rm = T),
              other = sum(other_FP17, na.rm = T),
              urgent = sum(urgent_FP17, na.rm = T)
              ) #%>%
    #filter(month == max(month))

}


################################################################################
get_delivery_profile_data <- function(data = UDA_scheduled_data,
                             UDAorUOA = "UDA",
                             remove_prototypes = T,
                             all_regions_and_STPs = FALSE){
  
  #filter out ended contracts
  data <- data %>%
    filter(is.na(contract_end_date) | contract_end_date > month)
  
  #create not in function
  `%notin%` = Negate(`%in%`)
  
  #remove prototype contracts if specified
  if(remove_prototypes & UDAorUOA == "UDA"){
    data <- data %>%
      filter(contract_number %notin% prototype_contracts$prototype_contract_number)%>%
      filter(annual_contracted_UDA > 100)
  }else if(remove_prototypes & UDAorUOA == "UOA"){
    data <- data %>%
      filter(contract_number %notin% prototype_contracts$prototype_contract_number) %>%
      filter(annual_contracted_UOA > 0)
  }
  
  if(UDAorUOA == "UDA"){
    #create column for 12 month scaled % of UDAs delivered. April must be scaled differently.
    performance_table <- data %>%
      mutate(monthly_perc_scaled = if_else(month != as.Date("2021-04-01") & month != as.Date("2022-04-01"),
                                           round(UDA_delivered * 12 * 100 / annual_contracted_UDA),
                                           round(UDA_delivered * 18 * 100 / annual_contracted_UDA))) 
  }else{
    #create column for 12 month scaled % of UOAs delivered 
    performance_table <- data %>%
      mutate(monthly_perc_scaled = if_else(month != as.Date("2021-04-01") & month != as.Date("2022-04-01"),
                                           round(UOA_delivered * 12 * 100 / annual_contracted_UOA),
                                           round(UOA_delivered * 18 * 100 / annual_contracted_UOA)))
  }
    
  #put into bands then sum across these bands by month
  performance_table <- performance_table %>%
    mutate(performance_band = if_else(monthly_perc_scaled >= 0 & monthly_perc_scaled <10, "0-9%",
                                      if_else(monthly_perc_scaled >= 10 & monthly_perc_scaled <20, "10-19%",
                                              if_else(monthly_perc_scaled >= 20 & monthly_perc_scaled <30, "20-29%",
                                                      if_else(monthly_perc_scaled >= 30 & monthly_perc_scaled <40, "30-39%",
                                                              if_else(monthly_perc_scaled >= 40 & monthly_perc_scaled <50, "40-49%",
                                                                      if_else(monthly_perc_scaled >= 50 & monthly_perc_scaled <60, "50-59%",
                                                                              if_else(monthly_perc_scaled >= 60 & monthly_perc_scaled <70, "60-69%",
                                                                                      if_else(monthly_perc_scaled >= 70 & monthly_perc_scaled <80, "70-79%",
                                                                                              if_else(monthly_perc_scaled >= 80 & monthly_perc_scaled <90, "80-89%",
                                                                                                      if_else( monthly_perc_scaled >= 90 & monthly_perc_scaled <100, "90-99%",
                                                                                                               "100% +")
                                                                                              )
                                                                                      )
                                                                              )
                                                                      )
                                                              )
                                                      )
                                              )
                                      )
    )) %>%
    #exclude NAs
    filter(!is.na(performance_band)) 
  
  if(all_regions_and_STPs == FALSE){
    performance_table <- performance_table %>%
      group_by(month)
  }else{
    performance_table <- performance_table %>%
      group_by(month, region_name, commissioner_name)
  }
  
  performance_table <- performance_table %>%
    count(performance_band) %>%
    mutate(no_of_contracts = sum(n)) %>%
    ungroup() %>%
    mutate(perc_of_contracts = n * 100 / no_of_contracts) %>%
    mutate(month = as.POSIXct(month))

  performance_table
}


################################################################################
clean_workforce_returns <- function(data = dental_workforce_returns){
  
  data <- data %>%
    rename(total_workforce_returns_due = workforce_returns_due) %>%
    pivot_longer(cols = starts_with("workforce_returns"),
                 names_to = "old_column_names") %>%
    mutate(year = substr(old_column_names, 19, 22)) %>%
    mutate(month = substr(old_column_names, 23, 24)) %>%
    mutate(date = as.Date(paste0(year,"-", month,"-01"))) %>%
    rename(workforce_returns = value)
}

################################################################################
clean_dental_recalls <- function(data = dental_recalls_STP_2021_22){
  
  data <- data %>%
    pivot_longer(cols = c("Band_1", "Band_2", "Band_3", "Other", "Urgent"),
                 names_to = "Band",
                 values_to = "forms")
  
}