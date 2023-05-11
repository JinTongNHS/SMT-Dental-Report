################################################################################
#function to plot delivery profile month by month 
plot_UDA_UOA_delivery_profile_full_year <- function(data = UDA_scheduled_data, 
                                                    historic_data = historical_UDA_scheduled_data,
                                                    UDAorUOA = "UDA",
                                                    level = "National",
                                                    region_STP_name = NULL,
                                                    plotChart = TRUE,
                                                    all_regions_and_STPs = FALSE,
                                                    include_historic = TRUE,
                                                    financial_year = "2022/23",
                                                    remove_prototypes = TRUE){
  
  #bind in historic data if required
  if(include_historic == TRUE & UDAorUOA == "UDA"){
    data <- data %>%
      select(month, contract_number, commissioner_name, region_name, 
             annual_contracted_UDA, UDA_delivered)
    
    historic_data <- historic_data %>%
      mutate(month = as.Date(month)) %>%
      filter(!is.na(annual_contracted_UDA)) %>% #filters out contracts with no data on contracted UDAs
      select(month, contract_number, commissioner_name, region_name, 
             annual_contracted_UDA, 
             UDA_delivered)
    
    data <- bind_rows(data, historic_data)
    
  }
  
  #filter for specified financial year
  if(financial_year == "2022/23"){
    data <- data %>%
      filter(month >= as.Date("2022-04-01") & month < as.Date("2023-04-01"))
    
  }else if(financial_year == "2021/22"){
    data <- data %>%
      filter(month >= as.Date("2021-04-01") & month < as.Date("2022-04-01"))
    
  }else if(financial_year == "2020/21"){
    data <- data %>%
      filter(month >= as.Date("2020-04-01") & month < as.Date("2021-04-01"))
    
  }else if(financial_year == "2019/20"){
    data <- data %>%
      filter(month >= as.Date("2019-04-01") & month < as.Date("2020-04-01"))
    
  }else if(financial_year == "2018/19"){
    data <- data %>%
      filter(month >= as.Date("2018-04-01") & month < as.Date("2019-04-01"))
    
  }
  
  #filter for STP or region
  if(level == "Regional"){
    data <- data %>% 
      filter(region_name == region_STP_name )
    subtitle <- region_STP_name 
  }else if(level == "STP"){
    data <- data %>% 
      filter(commissioner_name == region_STP_name)
    subtitle <- region_STP_name
  }else{
    subtitle <- "England"
  }
  
  #get data into the right format
  data <- get_delivery_profile_data_full_year(data = data, UDAorUOA = UDAorUOA, remove_prototypes = remove_prototypes, all_regions_and_STPs = all_regions_and_STPs)
  
  #change title for UDA or UOA
  if(UDAorUOA == "UDA"){
    title <- paste("Proportion of contracts delivering in each performance band \nof total contracted UDA for finacnial year", financial_year)
    legTitle <- "Performance band of \nUDA delivery"
    
    if(remove_prototypes == TRUE){
      captionTitle <- "*Excluding contracts with annual contracted UDA < 100. Excluding prototype contracts up until April 2022.
                    **These are scheduled months and April data is for the reporting period 1st April - 
                    21st April therefore the April data has been scaled up by 18 instead of 12."
      
    }else{
      captionTitle <- "*INCLUDING contracts with annual contracted UDA < 100 and prototype contracts.
                    **These are scheduled months and April data is for the reporting period 1st April - 
                    21st April therefore the April data has been scaled up by 18 instead of 12."
      
    }
    
    
  }else{
    title <- paste("Proportion of contracts delivering in each performance band \nof total contracted UOA for financial year", financial_year)
    legTitle <- "Performance band of \nUOA delivery"
    
    if(remove_prototypes == TRUE){
      captionTitle <- "*Excluding contracts with no annual contracted UOAs. Excluding prototype contracts up until April 2022.
                    **These are scheduled months and April data is for the reporting period 1st April - 
                    21st April therefore the April data has been scaled up by 18 instead of 12."
      
    }else{
      captionTitle <- "*INCLUDING contracts with no annual contracted UOAs and prototype contracts.
                    **These are scheduled months and April data is for the reporting period 1st April - 
                    21st April therefore the April data has been scaled up by 18 instead of 12."
      
    }
    
  }
  
  
  #get bars in the correct order
  data$performance_band <- factor(data$performance_band, levels = c("0-9%","10-19%", 
                                                                    "20-29%", "30-39%", "40-49%", "50-59%",
                                                                    "60-69%", "70-79%", "80-89%", "90-99%",
                                                                    "100% +", "UDA delivery data \ninvalid or not given"))
  #colour blind friendly palette
  cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
                 "#52854C", "#4E84C4", "#293352", "#FFDB6D", "#999999")
  
  if(plotChart == TRUE){
    #plot code
    ggplot(data,
           aes(fill = performance_band,
               y = perc_of_contracts,
               x = performance_band)) +
      geom_bar(position = "dodge",
               stat = "identity") +
      labs(title = title,
           x = "",
           y = "Percentage of contracts delivering in this band",
           fill = legTitle,
           subtitle = subtitle,
           caption = captionTitle) +
      scale_fill_manual(values = cbPalette) +
      scale_y_continuous(breaks = scales::breaks_pretty()) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90))
  }else{
    data #%>%
      # rename(`Region Name` = "region_name",
      #        `Commissioner Name` = "commissioner_name",
      #        `Performance band` = "performance_band",
      #        `Number of contracts in performance band`= "n",
      #        `Total number of contracts` = "no_of_contracts",
      #        `Percentage of contracts in performance band` = "perc_of_contracts")
  }

}
