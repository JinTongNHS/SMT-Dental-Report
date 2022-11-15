################################################################################
plot_UDA_UOA_delivery_MY_cat <- function(data = UDA_scheduled_data, 
                                           contractor_cats = contractor_categories,
                                           UDAorUOA = "UDA",
                                           level = "National",
                                           region_STP_name = NULL,
                                           remove_prototypes = T,
                                           regional_lines = F, 
                                           STP_lines = F,
                                           cat_lines = F,
                                           plotChart = T){

  #join in MY categories
  data <- data %>%
    left_join(contractor_cats, by = "contract_number")
  
  
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
  
  if(UDAorUOA == "UDA"){
    #get data into the right format
    data <- get_delivery_data_MY_cat(calendar_data = calendar_data, scheduled_data = data, remove_prototypes, UDAorUOA = "UDA", regional_lines, STP_lines, cat_lines)
    title <- "Calendar monthly percentage of usual annual contracted UDAs \ndelivered across all contracts* scaled up to 12 months"
    ylab <- "% of contracted UDAs delivered"
    captionTitle <- "*Excluding contracts with annual contracted UDA < 100. Excluding prototype contracts up until April 2022."
    lineCol <- "coral"
    septemberTarget <- 60
    decemberTarget <- 65
    marchTarget <- 85
    juneTarget <- 95
  }else{
    
    #get data into the right format
    data <- get_delivery_data_MY_cat(calendar_data = calendar_data, scheduled_data = data, remove_prototypes, UDAorUOA = "UOA", regional_lines, STP_lines, cat_lines)
    title <- "Calendar monthly percentage of usual annual contracted UOAs \ndelivered across all contracts* scaled up to 12 months"
    ylab <- "% of contracted UOAs delivered"
    captionTitle <- "*Excluding contracts with no annual contracted UOAs. Excluding prototype contracts up until April 2022."
    lineCol <- "#009E73"
    septemberTarget <- 80
    decemberTarget <- 85
    marchTarget <- 90
    juneTarget <- 100
  }
  
  
  subtitle_addition <- ""
  
  if(regional_lines){
    g <- 
      ggplot(data) +
      theme_bw() +
      geom_line(aes(x = month, 
                    y = scaled_perc_UDA_UOA_delivered,
                    colour = region_name), 
                size = 1) +
      geom_point(aes(x = month, 
                     y = scaled_perc_UDA_UOA_delivered, 
                     colour = region_name)
      )
    
    legendTitle <- "Region"
    
  }else if(STP_lines){
    g <- 
      ggplot(data) +
      theme_bw() +
      geom_line(aes(x = month, 
                    y = scaled_perc_UDA_UOA_delivered,
                    colour = commissioner_name), 
                size = 1) +
      geom_point(aes(x = month, 
                     y = scaled_perc_UDA_UOA_delivered, 
                     colour = commissioner_name)
      )
    
    legendTitle <- "STP"
    
  }else if(cat_lines){
    data <- data %>%
      filter(!is.na(category_sub_type))
    
    g <- 
      ggplot(data) +
      theme_bw() +
      geom_line(aes(x = month, 
                    y = scaled_perc_UDA_UOA_delivered,
                    colour = category_sub_type), 
                size = 1) +
      geom_point(aes(x = month, 
                     y = scaled_perc_UDA_UOA_delivered, 
                     colour = category_sub_type)
      )
    
    legendTitle <- "MY Category"
    subtitle_addition <- if_else(remove_prototypes, " - *Excluding prototypes and contracts with annual contracted UDA < 100",
                                 " - *Including prototypes and contracts with annual contracted UDA < 100")
    
    captionTitle <- ""
    
  }else{
    g <- 
      ggplot(data) +
      theme_bw() +
      geom_line(aes(x = month, 
                    y = scaled_perc_UDA_UOA_delivered),
                colour = lineCol, 
                size = 1) +
      geom_point(aes(x = month, 
                     y = scaled_perc_UDA_UOA_delivered), 
                 colour = lineCol
      )+
      annotate(geom = "text", 
               x = data$month, 
               y = data$scaled_perc_UDA_UOA_delivered + 5, 
               label = paste0(round(data$scaled_perc_UDA_UOA_delivered), "%"), 
               size = 3) 
    
  }
  
  g <- g +
    geom_segment(aes(x = as.Date("2021-04-01"), 
                     y = septemberTarget, 
                     xend = as.Date("2021-09-01"), 
                     yend = septemberTarget),
                 colour = "#0072B2",
                 linetype = "dashed") +
    
    geom_segment(aes(x = as.Date("2021-10-01"), 
                     y = decemberTarget, 
                     xend = as.Date("2021-12-01"), 
                     yend = decemberTarget),
                 colour = "#0072B2",
                 linetype = "dashed") +
    
    geom_segment(aes(x = as.Date("2022-01-01"), 
                     y = marchTarget, 
                     xend = as.Date("2022-03-01"), 
                     yend = marchTarget),
                 colour = "#0072B2",
                 linetype = "dashed") +
    
    geom_segment(aes(x = as.Date("2022-04-01"), 
                     y = juneTarget, 
                     xend = as.Date("2022-06-01"), 
                     yend = juneTarget),
                 colour = "#0072B2",
                 linetype = "dashed") +
    
    
    annotate(geom = "text", 
             x = as.Date("2021-04-01") + lubridate::weeks(2), 
             y = septemberTarget - 3, 
             label = "H1 threshold", 
             size = 3,
             colour = "#0072B2") + 
    
    annotate(geom = "text", 
             x = as.Date("2021-10-01") + lubridate::weeks(2), 
             y = decemberTarget - 3, 
             label = "Q3 threshold", 
             size = 3,
             colour = "#0072B2") +
    
    annotate(geom = "text", 
             x = as.Date("2022-01-01") + lubridate::weeks(2), 
             y = marchTarget - 3, 
             label = "Q4 threshold", 
             size = 3,
             colour = "#0072B2") +
    
    annotate(geom = "text", 
             x = as.Date("2022-04-01") + lubridate::weeks(2), 
             y = juneTarget - 3, 
             label = "Q1 threshold", 
             size = 3,
             colour = "#0072B2") +
    
    scale_x_date(date_breaks = "1 month", 
                 date_labels = "%b-%y") +
    scale_y_continuous(limits = c(0, max(c(data$scaled_perc_UDA_UOA_delivered, 90), na.rm = T) + 10),
                       breaks = scales::breaks_pretty()) 
  
  
  if(regional_lines == F & cat_lines == F){
    g <- g +
      labs(title = title, 
           x = "Month",
           y = ylab, 
           subtitle = paste0(subtitle, subtitle_addition),
           caption = captionTitle)
    
  }else{
    g <- g +
      labs(title = title, 
           x = "Month",
           y = ylab, 
           subtitle = paste0(subtitle, subtitle_addition),
           caption = captionTitle,
           colour = legendTitle) 
    
  }
  
  
  if(plotChart){
    g
  }else{
    
    if(UDAorUOA == "UDA"){
      new_col_names <- c(Month = "month", 
                         `Region Name` = "region_name", 
                         `Commissioner Name` = "commissioner_name", 
                         `Monthly UDAs delivered` = "monthly_UDA_UOAs_delivered", 
                         `Annual contracted UDAs` = "annual_contracted_UDA_UOA", 
                         `Scaled monthly UDAs delivered` = "scaled_monthly_UDA_UOAs_delivered", 
                         `Scaled percentage of UDAs delivered` = "scaled_perc_UDA_UOA_delivered",
                         `Category Sub-type` = "category_sub_type")
    }else{
      new_col_names <- c(Month = "month", 
                         `Region Name` = "region_name", 
                         `Commissioner Name` = "commissioner_name", 
                         `Monthly UOAs delivered` = "monthly_UDA_UOAs_delivered", 
                         `Annual contracted UOAs` = "annual_contracted_UDA_UOA", 
                         `Scaled monthly UOAs delivered` = "scaled_monthly_UDA_UOAs_delivered", 
                         `Scaled percentage of UOAs delivered` = "scaled_perc_UDA_UOA_delivered",
                         `Category Sub-type` = "category_sub_type")
    }
    
    data <- data %>%
      rename(any_of(new_col_names))
  }
}
