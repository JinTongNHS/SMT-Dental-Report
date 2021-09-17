library(tidyverse)


################################################################################
#function to get dental data into the right format for slide 4
get_into_slide4_format_calendar <- function(data = UDA_calendar_data, 
                                            scheduled_data = UDA_scheduled_data,
                                            remove_prototypes = T){
  
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
      filter(contract_number %notin% prototype_contracts$proto_contracts) %>%
      filter(annual_contracted_UDA > 100)
  }

  #group by month and sum UDAs delivered
  UDAs_delivered <- data %>%
    group_by(month) %>%
    summarise(monthly_UDA_UOAs_delivered = sum(UDA_total, na.rm = T),
              total_annual_UDA_UOAs_contracted = sum(annual_contracted_UDA)) %>%
    mutate(target_UDA_UOAs_delivered_by_sept = total_annual_UDA_UOAs_contracted * 0.6 / 2) #%>%
    #mutate(target_UDA_UOAs_delivered_by_sept = 25313404)
  
}

################################################################################
#function to get dental data into the right format for slide 4
get_into_slide6_format_calendar <- function(data = UOA_calendar_data, 
                                            scheduled_data = UOA_scheduled_data,
                                            remove_prototypes = F){
  
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
      filter(contract_number %in% prototype_contracts_orth$proto_contracts)
  }

  #group by month and sum UDAs delivered
  UOAs_delivered <- data %>%
    group_by(month) %>%
    summarise(monthly_UDA_UOAs_delivered = sum(UOA_total, na.rm = T),
              total_annual_UDA_UOAs_contracted = sum(annual_contracted_UOA, na.rm = T)) %>%
    mutate(target_UDA_UOAs_delivered_by_sept = total_annual_UDA_UOAs_contracted * 0.8 / 2) #%>%
    #mutate(target_UDA_UOAs_delivered_by_sept = 1661898)
}



################################################################################
#function to get dental data into the right format for slide 4
get_into_slide5_format <- function(data = UDA_scheduled_data, remove_prototypes = T){
  
  #remove prototype contracts if specified
  if(remove_prototypes){
    #create not in function
    `%notin%` = Negate(`%in%`)
    data <- data %>%
      filter(contract_number %notin% prototype_contracts$proto_contracts)%>%
      filter(annual_contracted_UDA > 100)
  }
  
  #group by month and sum UDAs delivered
  UDAs_delivered <- data %>%
    group_by(month) %>%
    summarise(monthly_UDA_UOAs_delivered = sum(UDA_delivered, na.rm = T))
  
  #group by month and sum contracted UDAs
  UDAs_contracted <- data %>%
    group_by(month) %>%
    summarise(annual_contracted_UDA_UOA = sum(annual_contracted_UDA, na.rm = T))
  
  slide4_UDA <- full_join(UDAs_delivered, UDAs_contracted, by = "month")
  # slide4_UDA <- slide4_UDA %>%
  #   mutate(month = if_else(Year.Month == 202104, as.Date("2021-04-01"), 
  #                          if_else(Year.Month == 202105, as.Date("2021-05-01"),
  #                                  if_else(Year.Month == 202106, as.Date("2021-06-01"),
  #                                          if_else(Year.Month == 202107, as.Date("2021-07-01"),
  #                                                  if_else(Year.Month == 202108, as.Date("2021-08-01"),
  #                                                          as.Date(NA))))))
  #  )
}



################################################################################
get_into_slide7_format <- function(data = UOA_scheduled_data, remove_prototypes = F){
  
  #remove spaces from column names
  colnames(data) <- make.names(colnames(data), unique = T)
  
  #remove prototype contracts if specified
  if(remove_prototypes){
    #create not in function
    `%notin%` = Negate(`%in%`)
    data <- data %>%
      filter(Contract.Number %in% prototype_contracts_orth$proto_contracts)%>%
      filter(Contract.Number %notin% UDAs_less_than_100$Contract.Number)
  }
  
  #group by month and sum UDAs delivered
  UOAs_delivered <- data %>%
    group_by(Year.Month) %>%
    summarise(monthly_UDA_UOAs_delivered = sum(UOA, na.rm = T))
  
  #group by month and sum contracted UDAs
  UOAs_contracted <- data %>%
    group_by(Year.Month) %>%
    summarise(annual_contracted_UDA_UOA = sum(Annual.contracted.UOA, na.rm = T))
  
  slide7_UOA <- full_join(UOAs_delivered, UOAs_contracted, by = "Year.Month")
  slide7_UOA <- slide7_UOA %>%
    rename(month = "Year.Month") %>%
    mutate(perc_UDA_UOA_delivered = monthly_UDA_UOAs_delivered * 12 / annual_contracted_UDA_UOA)
}



################################################################################
get_into_slide8_format <- function(data = UDA_scheduled_data, remove_prototypes = F){
  
  #remove prototype contracts if specified
  if(remove_prototypes){
    #create not in function
    `%notin%` = Negate(`%in%`)
    data <- data %>%
      filter(contract_number %in% prototype_contracts_orth$proto_contracts)%>%
      filter(annual_contracted_UDA > 100)
  }
  
  #group by month and sum UDAs delivered
  UOAs_delivered <- data %>%
    group_by(month) %>%
    summarise(band1 = sum(FP17s_band_1, na.rm = T),
              band2 = sum(FP17s_band_2, na.rm = T),
              band3 = sum(FP17s_band_3, na.rm = T),
              other = sum(FP17s_band_other, na.rm = T),
              urgent = sum(FP17s_band_urgent, na.rm = T)
              )
}


################################################################################
get_slide5_table <- function(data = UDA_scheduled_data, remove_prototypes = F){
  
  #remove prototype contracts if specified
  if(remove_prototypes){
    #create not in function
    `%notin%` = Negate(`%in%`)
    data <- data %>%
      filter(contract_number %notin% prototype_contracts$proto_contracts) %>%
      filter(annual_contracted_UDA < 100)
  }
  
  #create column for 12 month scaled % of UDAs delivered and put into bands
  #then sum across these bands by month
  performance_table <- data %>%
    mutate(monthly_perc_scaled = UDA_delivered * 12 * 100/ annual_contracted_UDA) %>%
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
    )
                                      
    ) %>%
    #exclude NAs
    filter(!is.na(performance_band)) %>%
    group_by(month) %>%
    count(performance_band) %>%
    mutate(no_of_contracts = sum(n)) %>%
    ungroup() %>%
    mutate(perc_contracts_in_band = n * 100 / no_of_contracts)

}


################################################################################
get_slide7_table <- function(data = UOA_scheduled_data, remove_prototypes = F){
  
  #remove spaces from column names
  colnames(data) <- make.names(colnames(data), unique = T)
  
  #remove prototype contracts if specified
  if(remove_prototypes){
    #create not in function
    `%notin%` = Negate(`%in%`)
    data <- data %>%
      filter(Contract.Number %notin% prototype_contracts$proto_contracts)%>%
      filter(Contract.Number %notin% UDAs_less_than_100$Contract.Number)
  }
  
  #create column for 12 month scaled % of UDAs delivered and put into bands
  #then sum across these bands by month
  performance_table <- data %>%
    mutate(monthly_perc_scaled = UOA * 12 * 100/ Annual.contracted.UOA) %>%
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
    )
    
    ) %>%
    #exclude NAs
    filter(!is.na(performance_band)) %>%
    group_by(Year.Month) %>%
    count(performance_band) %>%
    mutate(no_of_contracts = sum(n)) %>%
    ungroup() %>%
    mutate(perc_contracts_in_band = n * 100 / no_of_contracts)
  
}