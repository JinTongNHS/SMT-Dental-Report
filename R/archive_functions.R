#functions from previous versions of the report

#function to plot first chart on slide 4
#old data source is in teams folder "Monthly performance data/April 21 to September 21 data/Calendar data/April to Jul UDA 2021-2022 by Treatment month.xlsx"
#pass in raw data - either UDA_calendar_data or orthodontic_data_combined_calendar
plot_UDA_UOA_to_target <- function(data = UDA_calendar_data, UDAorUOA = "UDA", 
                                   level = "National",
                                   region_STP_name = NULL,
                                   contractor_cats = contractor_categories,
                                   cat = NULL){
  
  #join in MY categories
  data <- data %>%
    left_join(contractor_cats)
  
  if(!is.null(cat)){
    data <- filter(data, category_sub_type == cat)
    
    numOfCats <- count(contractor_cats, category_sub_type)
    numOfCats <- numOfCats %>%
      filter(category_sub_type == cat)
    numOfCats <- numOfCats[1,2]
    cat_sub <- paste0(" (", cat, " contracts only", " - ", numOfCats,"/6906)")
    
  }else{
    cat_sub <- " (All categories)"
  }
  
  #filter for region or STP
  if(level == "Regional"){
    data <- data %>% 
      filter(region_name == region_STP_name)
    subtitle <- region_STP_name
  }else if(level == "STP"){
    data <- data %>% 
      filter(commissioner_name == region_STP_name)
    subtitle <- region_STP_name
  }else{
    subtitle <- "England"
  }
  
  
  #change titles and colours for UDA or UOA
  if(UDAorUOA == "UDA"){
    septemberTarget <- 60
    decemberTarget <- 65
    title <- "Monthly % of Q1, Q2 and Q3 contracted UDAs delivered"
    ylab <- "% of quarterly \ncontracted UDAs delivered"
    barCol <- "coral"
    
    #get raw data into the right format
    data <- get_into_slide4_format_calendar(data, remove_prototypes = T)
    
  }else{
    septemberTarget <- 80
    decemberTarget <- 85
    title <- "Monthly % of Q1, Q2 and Q3 contracted UOAs delivered"
    ylab <- "% of quarterly \ncontracted UDAs delivered"
    barCol <- "seagreen3"
    
    #get raw data into fight format
    data <- get_into_slide6_format_calendar(data, remove_prototypes = F)
  }
  
  #add blanks for future dates
  if(nrow(data) < 9){
    if(!(as.Date("2021-09-01") %in% data$month)){
      data <- data %>% add_row(month = as.Date("2021-09-01"))
    }
    if(!(as.Date("2021-10-01") %in% data$month)){
      data <- data %>% add_row(month = as.Date("2021-10-01"))
    }
    if(!(as.Date("2021-11-01") %in% data$month)){
      data <- data %>% add_row(month = as.Date("2021-11-01"))
    }
    if(!(as.Date("2021-12-01") %in% data$month)){
      data <- data %>% add_row(month = as.Date("2021-12-01"))
    }
  }
  
  #get data in the right format
  data <- data %>%
    mutate(month = as.Date(month)) %>%
    mutate(perc_of_UDA_UOA_target_delivered = monthly_UDA_UOAs_delivered * 100 / target_UDA_UOAs_delivered_in_target_period) %>%
    mutate(perc_of_UDA_UOA_target_delivered = round(perc_of_UDA_UOA_target_delivered, 1)) %>%
    mutate(perc_of_contracted_UDA_UOAs_delivered = monthly_UDA_UOAs_delivered * 100 * 4/ total_annual_UDA_UOAs_contracted) %>%
    mutate(financial_quarter = if_else(month < as.Date("2021-07-01"), "Apr-Jun (Q1)", 
                                       if_else(month < as.Date("2021-10-01"), "Jul-Sep (Q2)",
                                               "Oct-Dec (Q3)")))
  
  #ensures months with no data are still shown
  #done as separate dataframe so that annotations are not shown for months with no data
  data_to_plot <- data %>%
    mutate(perc_of_contracted_UDA_UOAs_delivered = if_else(is.na(perc_of_contracted_UDA_UOAs_delivered)
                                                           , 0, 
                                                           perc_of_contracted_UDA_UOAs_delivered)) %>%
    mutate(target = if_else(financial_quarter == "Apr-Jun (Q1)" | financial_quarter == "Jul-Sep (Q2)", septemberTarget/3,decemberTarget/3)) 
  
  #plot code
  ggplot(data_to_plot, 
         aes(x = month, 
             y = perc_of_contracted_UDA_UOAs_delivered)) +
    geom_bar(stat = "identity", 
             fill = barCol, 
             width = 10) +
    geom_line(aes(x = month, 
                  y = target,
                  colour = financial_quarter), 
              linetype = "dashed") +
    geom_point(aes(x = month,
                   y = target,
                   colour = financial_quarter),
               shape = 4,
               size = 3) +
    geom_vline(xintercept = as.Date("2021-09-01") + lubridate::days(15),
               linetype = "dotted") +
    geom_vline(xintercept = as.Date("2021-06-01") + lubridate::days(15),
               linetype = "dotted") +
    theme_bw() +
    annotate(geom = "label", 
             x = data$month, 
             y = data$perc_of_contracted_UDA_UOAs_delivered + 1, 
             label = format(round(data$perc_of_contracted_UDA_UOAs_delivered, 1), nsmall = 1), 
             size = 3,
             label.size = 0) +
    scale_x_date(date_breaks = "1 month", 
                 date_labels = "%b-%y") +
    scale_colour_manual(labels = c(paste0("Expected monthly delivery to reach \nQ1 threshold of ", septemberTarget,"% by end of Jun-21*"),
                                   paste0("Expected monthly delivery to reach \nQ2 threshold of ",septemberTarget,"% by end of Sep-21*"),
                                   paste0("Expected monthly delivery to reach \nQ3 threshold of ",decemberTarget,"% by end of Dec-22*")
    ),
    values = c("darkred", "blue", "darkgreen")) + 
    labs(title = title, 
         x = "Month", 
         y = ylab,
         subtitle = paste0(subtitle, cat_sub),
         caption = paste0("*expected monthly delivery to meet threshold is caluclated by assuming equal delivery across the 3 month period.
           For Apr-Jun and Jul-Sep the monthly expected delivery is ",septemberTarget,"/3 = ",round(septemberTarget/3,1),"%, for Oct-Dec the monthly expected delivery is ",decemberTarget,"/3 = ",round(decemberTarget/3,1),"%")) +
    theme(legend.position = "bottom", 
          legend.title = element_blank(),
          plot.caption = element_text(hjust = 0.5, 
                                      face= "italic"),
          plot.caption.position =  "plot"
    )
  
}


################################################################################
#ADJUSTED
#function to plot first chart on slide 4 adjusted for working day
#current data source is in teams folder "Monthly performance data/April 21 to September 21 data/Calendar data/April to Jul UDA 2021-2022 by Treatment month.xlsx"
plot_UDA_UOA_to_target_adjusted <- function(data = get_into_slide4_format_calendar(), UDAorUOA = "UDA"){
  
  data <- data %>% mutate(working_day_adjustment = c(0.158730159, 0.150793651, 0.174603175, 0.182539683, 0.158730159)) %>%
    mutate(adjusted_monthly_target = target_UDA_UOAs_delivered_by_sept * working_day_adjustment) %>%
    mutate(adjusted_september_target = adjusted_monthly_target * 6)
  
  data <- data %>%
    mutate(month = as.Date(month)) %>%
    #mutate(target_UDA_UOAs_delivered_by_sept = (annual_contracted_UDA_UOA) * septemberTarget / 100) %>%
    mutate(perc_of_UDA_UOA_target_delivered = monthly_UDA_UOAs_delivered * 100 / adjusted_september_target) %>%
    mutate(perc_of_UDA_UOA_target_delivered = round(perc_of_UDA_UOA_target_delivered, 1))
  
  if(UDAorUOA == "UDA"){
    septemberTarget <- 60
    title <- "Monthly % of Apr-Sept target (60%) UDAs delivered \nAdjusted for working days per month"
    ylab <- "% of target UDAs delivered"
    barCol <- "coral"
  }else{
    septemberTarget <- 80
    title <- "Monthly % of Apr-Sept target (80%) UOAs delivered"
    ylab <- "% of target UOAs delivered"
    barCol <- "seagreen3"
  }
  
  
  #ensures months with no data are still shown
  #done as separate dataframe so that annotations are not shown for months with no data
  data_to_plot <- data %>%
    mutate(perc_of_UDA_UOA_target_delivered = if_else(is.na(perc_of_UDA_UOA_target_delivered)
                                                      , 0, 
                                                      perc_of_UDA_UOA_target_delivered)) 
  
  #plot code
  ggplot(data_to_plot, 
         aes(x = month, 
             y = perc_of_UDA_UOA_target_delivered)) +
    geom_bar(stat = "identity", 
             fill = barCol, 
             width = 10) +
    theme_bw() +
    geom_hline(yintercept = 16.7, 
               colour = "blue", 
               linetype = "dashed") +
    annotate(geom = "text", 
             x = as.Date("2021-09-01"),
             y = 17.7, 
             label = "16.7% target", 
             size = 3) +
    annotate(geom = "text", 
             x = data$month, 
             y = data$perc_of_UDA_UOA_target_delivered + 1, 
             label = round(data$perc_of_UDA_UOA_target_delivered, 2), 
             size = 3) +
    scale_x_date(date_breaks = "1 month", 
                 date_labels = "%b-%y") +
    labs(title = title, 
         x = "Month", 
         y = ylab)
}



################################################################################
#unfinished
get_adjusted_number_reaching_target <- function(){
  
  #need to bind in annual contracted UDA from another table
  
  dental_data_combined_calendar_contracted4 <- group_by(dental_data_combined_calendar_contracted3, Month)
  dental_data_combined_calendar_contracted4 <- group_by(dental_data_combined_calendar_contracted4, Contract.Number)
  dental_data_combined_calendar_contracted5 <- summarise(dental_data_combined_calendar_contracted4)
  dental_data_combined_calendar_contracted5 <- summarise(dental_data_combined_calendar_contracted4, UDA.Total = sum(UDA.Total), Annual.contracted.UDA = sum(Annual.contracted.UDA), monthly_adjusted_contracted_UDA = sum(monthly_adjusted_contracted_UDA))
  dental_data_combined_calendar_contracted5 <- mutate(dental_data_combined_calendar_contracted5, perc_adjusted_target_deliverd = UDA.Total * 100 / monthly_adjusted_contracted_UDA)
  dental_data_combined_calendar_contracted5 <- mutate(dental_data_combined_calendar_contracted5, monthly_contracted_UDA = Annual.contracted.UDA / 12)
  dental_data_combined_calendar_contracted5 <- mutate(dental_data_combined_calendar_contracted5, perc_contracted_delivered = UDA.Total * 100 / monthly_contracted_UDA)
  count(dental_data_combined_calendar_contracted5, perc_contracted_delivered >= 50)
}



################################################################################
get_Q3_num_contracts_on_target <- function(data = UDA_calendar_data, 
                                           remove_prototypes = T,
                                           scheduled_data = UDA_scheduled_data,
                                           UDAorUOA = "UDA",
                                           level = "National",
                                           region_STP_name = NULL){
  
  #filter for STP or region
  if(level == "Regional"){
    data <- data %>% 
      filter(region_name == region_STP_name )
  }else if(level == "STP"){
    data <- data %>% 
      filter(commissioner_name == region_STP_name)
    subtitle <- region_STP_name
  }
  
  if(UDAorUOA == "UDA"){
    #get contracted UDAs
    contracted_UDA_UOAs <- scheduled_data %>%
      select(month, contract_number, annual_contracted_UDA) %>%
      mutate(UDA_financial_half_target = case_when(month < as.Date("2021-10-01") ~ 0.6 * annual_contracted_UDA/4,
                                                   month > as.Date("2021-10-01") ~ 0.65 * annual_contracted_UDA/4))
  }else{
    #get contracted UOAs
    contracted_UDA_UOAs <- scheduled_data %>%
      select(month, contract_number, annual_contracted_UOA) %>%
      mutate(UDA_financial_half_target = case_when(month < as.Date("2021-10-01") ~ 0.8 * annual_contracted_UOA/4,
                                                   month > as.Date("2021-10-01") ~ 0.85 * annual_contracted_UOA/4))
  }
  
  
  
  #join in contracted UDA/UOAs from scheduled data
  data <- data %>%
    left_join(contracted_UDA_UOAs, by = c("month", "contract_number")) %>%
    filter(month >= as.Date("2021-10-01") & month < as.Date("2022-01-01"))
  
  #create not in function
  `%notin%` = Negate(`%in%`)
  
  #remove prototype contracts if specified
  if(remove_prototypes & UDAorUOA == "UDA"){
    data <- data %>%
      filter(contract_number %notin% prototype_contracts$prototype_contract_number)%>%
      filter(annual_contracted_UDA > 100)
  }else if(remove_prototypes & UDAorUOA == "UOA"){
    data <- data %>%
      filter(contract_number %notin% prototype_contracts$prototype_contract_number)#%>%
    #filter(annual_contracted_UOA > 100)
  }
  
  #way to progress through the months
  if(max(data$month) == as.Date("2021-10-01")){
    month_factor <- 1
  }
  if(max(data$month) == as.Date("2021-11-01")){
    month_factor <- 2
  }
  if(max(data$month) == as.Date("2021-12-01")){
    month_factor <- 3
  }
  
  if(UDAorUOA == "UDA"){
    #count number of contracts meeting target
    data <- data %>%
      group_by(contract_number) %>%
      summarise(mean_annual_contracted_UDA = mean(annual_contracted_UDA),
                #mean_UDA_target = mean(UDA_financial_half_target),
                YTD_UDA_delivered = sum(UDA_total)) %>%
      mutate(mean_Q3_UDA_target = mean_annual_contracted_UDA * 0.65 / 4) %>%
      count(YTD_UDA_delivered >= (mean_Q3_UDA_target) * month_factor / 3)
  }else{
    #count number of contracts meeting target
    data <- data %>%
      group_by(contract_number) %>%
      summarise(mean_annual_contracted_UOA = mean(annual_contracted_UOA),
                #mean_Q3_UOA_target = mean(UOA_financial_half_target),
                YTD_UOA_delivered = sum(UOA_total)) %>%
      mutate(mean_Q3_UOA_target = mean_annual_contracted_UOA * 0.85 / 4) %>%
      count(YTD_UOA_delivered >= (mean_Q3_UOA_target) * month_factor / 3)
  }
  
  
  
  no_on_target <- data[2, "n"]
  as.integer(no_on_target)
  
}


################################################################################
get_SOF_data <- function(data = UDA_scheduled_data){
  SOF_data <- data %>%
    select(month, contract_number, commissioner_name, annual_contracted_UDA, UDA_delivered) %>%
    filter(annual_contracted_UDA > 100 & !(contract_number %in% prototype_contracts)) %>%
    group_by(month, commissioner_name) %>%
    summarise(annual_contracted_UDA = sum(annual_contracted_UDA, na.rm = T), UDA_delivered = sum(UDA_delivered, na.rm = T)) %>%
    mutate(annual_contracted_UDA_scaled_monthly = annual_contracted_UDA / 12) %>%
    #change names to match ICS names
    mutate(commissioner_name = if_else(commissioner_name == "South East London STP", 
                                       "OUR HEALTHIER SOUTH EAST LONDON ICS", 
                                       commissioner_name)) %>%
    mutate(commissioner_name = if_else(commissioner_name == "Lancashire and South Cumbria STP", 
                                       "HEALTHIER LANCASHIRE AND SOUTH CUMBRIA ICS", 
                                       commissioner_name))
  
  #write.csv(SOF_data, "SOF_data.csv", row.names = F)
}



################################################################################
#hand back scatter
plot_delivery_vs_contract_size_scatter <- function(data = UDA_scheduled_data,
                                                   remove_prototypes = T,
                                                   plot_month = as.Date("2021-11-01"),
                                                   UDAorUOA = "UDA"){
  
  
  #create not in function
  `%notin%` = Negate(`%in%`)
  
  #remove prototype contracts if specified
  if(remove_prototypes & UDAorUOA == "UDA"){
    data <- data %>%
      filter(contract_number %notin% prototype_contracts$prototype_contract_number)%>%
      filter(annual_contracted_UDA > 100)
  }else if(remove_prototypes & UDAorUOA == "UOA"){
    data <- data %>%
      filter(contract_number %notin% prototype_contracts$prototype_contract_number)#%>%
    #filter(annual_contracted_UOA > 100)
  }
  
  data <- data %>%
    filter(month == plot_month) %>%
    select(contract_number, annual_contracted_UDA, UDA_delivered) %>%
    mutate(UDA_delivery = UDA_delivered * 12 / annual_contracted_UDA) 
  
  
  ggplot(data, aes(x = annual_contracted_UDA, y = UDA_delivery)) +
    geom_point(colour = "steelblue") +
    theme_bw() +
    geom_hline(yintercept = 0.65,
               colour = "orangered4",
               linetype = "dashed") +
    annotate(geom = "text",
             x = 150000,
             y = 0.59,
             label = "65% Q3 threshold",
             size = 3,
             #label.size = 0,
             colour = "orangered4") +
    geom_hline(yintercept = 0.85,
               colour = "grey40",
               linetype = "dashed") +
    annotate(geom = "text",
             x = 150000,
             y = 0.79,
             label = "85%",
             size = 3,
             #label.size = 0,
             colour = "grey40") +
    geom_hline(yintercept = 1,
               colour = "grey40",
               linetype = "dashed") +
    annotate(geom = "text",
             x = 150000,
             y = 0.94,
             label = "100%",
             size = 3,
             #label.size = 0,
             colour = "grey40") +
    scale_x_continuous(breaks = seq(0, 175000, 20000),
                       #limits = c(0, 1.2),
                       #labels = scales::percent_format(accuracy = 1)
                       labels=function(x) format(x, big.mark = ",", scientific = FALSE)
    ) +
    scale_y_continuous(breaks = seq(0, 3, 0.5),
                       limits = c(0, 3),
                       labels = scales::percent_format(accuracy = 1)) +
    labs(title = "UDA contract size Vs UDA delivery scaled up 12 months",
         subtitle = "November 2021 delivery",
         x = "Annual contracted UDAs",
         y = "Percentage of annual contracted UDAs delivered \n scaled up 12 months",
         caption = "*Excluding prototype contracts and contracts with annual contracted UDA < 100.
         24 contracts with delivery > 300% have been excluded. All of which have contract size below 2560."
    )
  
}

################################################################################
#hand back scatter
plot_delivery_vs_contract_size_scatter_dental_group <- function(data = UDA_scheduled_data,
                                                                demographics_data = contract_demographics,
                                                                remove_prototypes = T,
                                                                plot_month = as.Date("2021-11-01"),
                                                                UDAorUOA = "UDA"){
  
  demographics_data <- demographics_data %>%
    select(contract_number, Region, Dental.Group, Corporate.Status, Average.UDA.value)
  
  data <- data %>%
    left_join(demographics_data)
  
  #create not in function
  `%notin%` = Negate(`%in%`)
  
  #remove prototype contracts if specified
  if(remove_prototypes & UDAorUOA == "UDA"){
    data <- data %>%
      filter(contract_number %notin% prototype_contracts$prototype_contract_number)%>%
      filter(annual_contracted_UDA > 100)
  }else if(remove_prototypes & UDAorUOA == "UOA"){
    data <- data %>%
      filter(contract_number %notin% prototype_contracts$prototype_contract_number)#%>%
    #filter(annual_contracted_UOA > 100)
  }
  
  data <- data %>%
    filter(month == plot_month) %>%
    select(contract_number, annual_contracted_UDA, UDA_delivered,
           Region, Dental.Group, Corporate.Status, Average.UDA.value) %>%
    mutate(UDA_delivery = UDA_delivered * 12 / annual_contracted_UDA) %>%
    mutate(Corporate.Status = if_else(Corporate.Status == 1, T, F)) %>%
    filter(!is.na(Dental.Group)) %>%
    arrange(Dental.Group)
  
  
  ggplot(data, aes(x = annual_contracted_UDA, y = UDA_delivery)) +
    geom_point(aes(colour = Dental.Group)) +
    theme_bw() +
    geom_hline(yintercept = 0.65,
               colour = "orangered4",
               linetype = "dashed") +
    annotate(geom = "text",
             x = 150000,
             y = 0.59,
             label = "65% Q3 threshold",
             size = 3,
             #label.size = 0,
             colour = "orangered4") +
    geom_hline(yintercept = 0.85,
               colour = "grey40",
               linetype = "dashed") +
    annotate(geom = "text",
             x = 150000,
             y = 0.79,
             label = "85%",
             size = 3,
             #label.size = 0,
             colour = "grey40") +
    geom_hline(yintercept = 1,
               colour = "grey40",
               linetype = "dashed") +
    annotate(geom = "text",
             x = 150000,
             y = 0.94,
             label = "100%",
             size = 3,
             #label.size = 0,
             colour = "grey40") +
    #scale_colour_manual(values = c("steelblue", "coral"), labels = c("non-corporate", "corporate")) +
    scale_x_continuous(breaks = seq(0, 175000, 20000),
                       #limits = c(0, 1.2),
                       #labels = scales::percent_format(accuracy = 1)
                       labels=function(x) format(x, big.mark = ",", scientific = FALSE)
    ) +
    scale_y_continuous(breaks = seq(0, 3, 0.5),
                       limits = c(0, 3),
                       labels = scales::percent_format(accuracy = 1)) +
    labs(title = "UDA contract size Vs UDA delivery scaled up 12 months",
         subtitle = "November 2021 delivery",
         x = "Annual contracted UDAs",
         y = "Percentage of annual contracted UDAs delivered \n scaled up 12 months",
         caption = "*Excluding prototype contracts and contracts with annual contracted UDA < 100.
         24 contracts with delivery > 300% have been excluded. All of which have contract size below 2560.",
         colour = "Dental Group"
    )
  
}



################################################################################
#hand back scatter
plot_delivery_vs_contract_size_scatter_region <- function(data = UDA_scheduled_data,
                                                          demographics_data = contract_demographics,
                                                          remove_prototypes = T,
                                                          plot_month = as.Date("2021-11-01"),
                                                          UDAorUOA = "UDA"){
  
  demographics_data <- demographics_data %>%
    select(contract_number, Region, Dental.Group, Corporate.Status, Average.UDA.value)
  
  data <- data %>%
    left_join(demographics_data)
  
  #create not in function
  `%notin%` = Negate(`%in%`)
  
  #remove prototype contracts if specified
  if(remove_prototypes & UDAorUOA == "UDA"){
    data <- data %>%
      filter(contract_number %notin% prototype_contracts$prototype_contract_number)%>%
      filter(annual_contracted_UDA > 100)
  }else if(remove_prototypes & UDAorUOA == "UOA"){
    data <- data %>%
      filter(contract_number %notin% prototype_contracts$prototype_contract_number)#%>%
    #filter(annual_contracted_UOA > 100)
  }
  
  data <- data %>%
    filter(month == plot_month) %>%
    select(contract_number, annual_contracted_UDA, UDA_delivered,
           Region, Dental.Group, Corporate.Status, Average.UDA.value) %>%
    mutate(UDA_delivery = UDA_delivered * 12 / annual_contracted_UDA) %>%
    mutate(Corporate.Status = if_else(Corporate.Status == 1, T, F)) %>%
    filter(!is.na(Average.UDA.value)) %>%
    arrange(Average.UDA.value)
  
  
  ggplot(data, aes(x = annual_contracted_UDA, y = UDA_delivery)) +
    geom_point(aes(colour = Average.UDA.value)) +
    theme_bw() +
    geom_hline(yintercept = 0.65,
               colour = "orangered4",
               linetype = "dashed") +
    annotate(geom = "text",
             x = 150000,
             y = 0.59,
             label = "65% Q3 threshold",
             size = 3,
             #label.size = 0,
             colour = "orangered4") +
    geom_hline(yintercept = 0.85,
               colour = "grey40",
               linetype = "dashed") +
    annotate(geom = "text",
             x = 150000,
             y = 0.79,
             label = "85%",
             size = 3,
             #label.size = 0,
             colour = "grey40") +
    geom_hline(yintercept = 1,
               colour = "grey40",
               linetype = "dashed") +
    annotate(geom = "text",
             x = 150000,
             y = 0.94,
             label = "100%",
             size = 3,
             #label.size = 0,
             colour = "grey40") +
    #scale_colour_manual(values = c("steelblue", "coral"), labels = c("non-corporate", "corporate")) +
    scale_x_continuous(breaks = seq(0, 175000, 20000),
                       #limits = c(0, 1.2),
                       #labels = scales::percent_format(accuracy = 1)
                       labels=function(x) format(x, big.mark = ",", scientific = FALSE)
    ) +
    scale_y_continuous(breaks = seq(0, 3, 0.5),
                       limits = c(0, 3),
                       labels = scales::percent_format(accuracy = 1)) +
    labs(title = "UDA contract size Vs UDA delivery scaled up 12 months",
         subtitle = "November 2021 delivery",
         x = "Annual contracted UDAs",
         y = "Percentage of annual contracted UDAs delivered \n scaled up 12 months",
         caption = "*Excluding prototype contracts and contracts with annual contracted UDA < 100.
         24 contracts with delivery > 300% have been excluded. All of which have contract size below 2560.",
         colour = "Average UDA value"
    )
  
}




################################################################################
#function to plot unique patients by band
plot_unique_patients_bar <- function(data = unique_patients_static,
                                     calendar_data = UDA_calendar_data,
                                     scheduled_data = UDA_scheduled_data,
                                     level = "National",
                                     region_STP_name = NULL,
                                     plotChart = TRUE,
                                     remove_prototypes = TRUE){
  
  #avoid standard form notation
  options(scipen = 5)
  
  
  # unique_patients_static <- rename(unique_patients_static, contract_number = "Contract Number", unique_patients_rolling_12M = "Unique Patient Count Rolling 12M",
  #                                                                    band1_unique_patients_rolling_12M = "Band 1 Unique Patient Count Rolling 12M",
  #                                                                    band2_or_3_unique_patients_rolling_12M = "Band 2 or Band 3 Unique Patient Count Rolling 12M",
  #                                                                    band1_urgent_unique_patients_rolling_12M = "Band 1 Urgent Unique Patient Count Rolling 12M",
  #                                                                    band_other_unique_patients_rolling_12M = "Other Unique Patient Count Rolling 12M")
  
  
  
  #join in region and STP data
  region_STP_lookup <- calendar_data %>%
    select(contract_number, commissioner_name, region_name) %>%
    unique()
  
  #join annual contracted UDAs
  contracted_UDAs <- scheduled_data %>%
    filter(month == max(scheduled_data$month, na.rm = TRUE)) %>%
    select(contract_number, annual_contracted_UDA)
  
  data <- data %>%
    left_join(region_STP_lookup) %>%
    left_join(contracted_UDAs)
  
  #remove prototype contracts if specified
  if(remove_prototypes){
    #create not in function
    `%notin%` = Negate(`%in%`)
    data <- data %>%
      filter(contract_number %notin% prototype_contracts$prototype_contract_number) %>%
      filter(annual_contracted_UDA > 100)
  }
  
  #filter for region or STP
  if(level == "Regional"){
    data <- data %>%
      filter(region_name == region_STP_name)
    subtitle <- region_STP_name
  }else if(level == "STP"){
    data <- data %>%
      filter(commissioner_name == region_STP_name)
    subtitle <- region_STP_name
  }else{
    subtitle <- "England"
  }
  
  data <- data %>%
    summarise(unique_patients_rolling_12M = sum(unique_patients_rolling_12M, na.rm = TRUE),
              band1_unique_patients_rolling_12M = sum(band1_unique_patients_rolling_12M, na.rm = TRUE),
              band2_or_3_unique_patients_rolling_12M = sum(band2_or_3_unique_patients_rolling_12M, na.rm = TRUE),
              band1_urgent_unique_patients_rolling_12M = sum(band1_urgent_unique_patients_rolling_12M, na.rm = TRUE),
              band_other_unique_patients_rolling_12M = sum(band_other_unique_patients_rolling_12M, na.rm = TRUE)) %>%
    pivot_longer(c(unique_patients_rolling_12M,
                   band1_unique_patients_rolling_12M,
                   band2_or_3_unique_patients_rolling_12M,
                   band1_urgent_unique_patients_rolling_12M,
                   band_other_unique_patients_rolling_12M)) %>%
    rename(band = name, total_unique_patients = value) %>%
    mutate(band = case_when(band == "unique_patients_rolling_12M" ~ "Any band",
                            band == "band1_unique_patients_rolling_12M" ~ "Band 1",
                            band == "band2_or_3_unique_patients_rolling_12M" ~ "Band 2 or 3",
                            band == "band1_urgent_unique_patients_rolling_12M" ~ "Urgent band 1",
                            band == "band_other_unique_patients_rolling_12M" ~ "Other band"))
  
  data$band <- factor(data$band,
                      levels = c(
                        "Any band",
                        "Band 1",
                        "Band 2 or 3",
                        "Urgent band 1",
                        "Other band"
                      ))
  
  
  
  if(plotChart == TRUE){
    #plot code
    ggplot(data) +
      theme_bw() +
      geom_bar(aes(x =band,
                   y = total_unique_patients),
               position = "dodge",
               stat = "identity",
               fill = "steelblue") +
      geom_text(aes(x =band,
                    y = total_unique_patients + 0.05*total_unique_patients,
                    label = format( round(total_unique_patients), big.mark = ",", scientific = FALSE)
      ),
      size = 2.5) +
      scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
      labs(title = "Total number of unique patients seen over \n12 month period Apr-2018 to Apr-2019 by band",
           x = "Band",
           y = "Unique patients",
           subtitle = subtitle,
           caption = "*N.B. this analysis uses unique patients per contract** and does not take \ninto account patients who have been seen at more than one dental practice. \n**Excluding prototype contracts and those with annual contracted UDAs < 100."
      ) #+
    #theme(axis.text.x = element_text(angle = 90))
    #theme(legend.position = "bottom")
  }else{
    data
  }
  
  
  
}


################################################################################
get_slide7_table <- function(data = UOA_scheduled_data, remove_prototypes = T){
  
  #remove spaces from column names
  colnames(data) <- make.names(colnames(data), unique = T)
  
  #remove prototype contracts if specified
  if(remove_prototypes){
    #create not in function
    `%notin%` = Negate(`%in%`)
    
    
    data <- data %>%
      filter(contract_number %notin% prototype_contracts$prototype_contract_number)#%>%
    #filter(annual_contracted_UOA > 0)
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
