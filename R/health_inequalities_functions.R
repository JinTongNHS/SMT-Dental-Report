#functions to create dental health inequalities plots

#supress summarise info
options(dplyr.summarise.inform = FALSE)

#function to plot percentage of FP17 forms that are band 2 or have flouride varnish for 3-13 year olds
plot_child_band2_FV <- function(data = child_band2_FV){
  
  
  #melt data into long format
  data <- reshape2::melt(child_band2_FV)
  
  data[1,1] <- "2018/2019"
  data[2,1] <- "2019/2020"
  data[3,1] <- "2020/2021"
  data[4,1] <- "2021/2022"
  
  data$financial_year <- factor(data$financial_year, levels = c("2018/2019", 
                                                                "2019/2020",
                                                                "2020/2021",
                                                                "2021/2022"))
  
  #plot code
  ggplot(data, 
         aes(x = financial_year,
             y = value,
             fill = variable
             )) +
    geom_bar(stat = "identity",
             position = "dodge"
             #aes(fill = "variable")
             ) +
    theme_bw() +
    # annotate(geom = "text", 
    #          x = as.Date("2021-09-01"),
    #          y = 17.7, 
    #          label = "16.7% target", 
    #          size = 3) +
    # annotate(geom = "text", 
    #          x = data$month, 
    #          y = data$perc_of_UDA_UOA_target_delivered + 1, 
    #          label = round(data$perc_of_UDA_UOA_target_delivered, 2), 
    #          size = 3) +
    scale_fill_manual(labels = c("Band 2", "Flouride Varnish"), values = c("red", "blue")) +
    labs(title = "Percentage of FP17 forms for 3-16 year olds that included a \nBand 2 treatment or Flouride Varnish treatment",
         x = "Financial year", 
         y = "Percentage of FP17 forms",
         fill = "Treatment type")
  
}


################################################################################
#function to get monthly health inequality data into the right format
clean_HE_data <- function(data = monthly_HE){
  
  #convert characters to numerics and convert dates
  data <- data %>%
    rename(year_month = month) %>%
    #rename(ethnic_group = ethinc_group) %>%
    mutate(band1 = if_else(band1 == "<5", "5", band1)) %>%
    mutate(band2 = if_else(band2 == "<5", "5", band2)) %>%
    mutate(band3 = if_else(band3 == "<5", "5", band3)) %>%
    mutate(urgent = if_else(urgent == "<5", "5", urgent)) %>%
    mutate(total_FP17 = if_else(total_FP17 == "<5", "5", total_FP17)) %>%
    mutate(band1 = as.numeric(band1), 
           band2 = as.numeric(band2),
           band3 = as.numeric(band3),
           urgent = as.numeric(urgent),
           total_FP17 = as.numeric(total_FP17)) %>%
    mutate(year = substring(year_month, 1, 4)) %>%
    mutate(month = substring(year_month, 5)) %>%
    mutate(date = as.Date(paste0(year,"-",month,"-","01")))

}


################################################################################
#function to plot ethnicity health inequality line chart
#2018-08 to 2021-07
plot_HE_ethnicity <- function(data = monthly_HE, band = band1, band_name = "Band 1", numOrPerc = "num", 
                              level = "National", 
                              region = NULL, 
                              STP = NULL,
                              override_subtitle = NULL,
                              caption = NULL){
  
  #filter data based on region or STP
  if(level == "Regional"){
    data <- data %>%
      filter(region_name == region)
    subtitle <- region
  }else if(level == "STP"){
    data <- data %>%
      filter(commissioner_name == STP)
    subtitle <- STP
  }else{
    subtitle <- "England"
  }
  
  #override subtitle if specified
  if(!is.null(override_subtitle) & override_subtitle == "Grouped top 20 most deprived local authorities by contract*"){
    subtitle <- override_subtitle
    caption <- 
      "*Contracts that fall within the following Local Authorities based on 2019 deprivation data: 
    Middlesbrough, Liverpool, Knowsley, Kingdton upon Hull, Manchester, Blackpool, Birmingham, 
    Burnley, Blackburn with Darwen, Hartlepool, Bradford, Stoke-on-Trent,Halton, Pendle, 
    Nottingham, Oldham, North East Lincolnshire, Hastings, Salford, Rochdale."
  }else{
    subtitle <- override_subtitle
    caption <- "*Patients resident within the following Local Authorities based on 2019 deprivation data: 
    Middlesbrough, Liverpool, Knowsley, Kingdton upon Hull, Manchester, Blackpool, Birmingham, 
    Burnley, Blackburn with Darwen, Hartlepool, Bradford, Stoke-on-Trent,Halton, Pendle, 
    Nottingham, Oldham, North East Lincolnshire, Hastings, Salford, Rochdale."
  }
  
  #avoid standard form
  options(scipen = 999)
  
  #get data in the right format to plot
  data <- clean_HE_data(data)
  
  data <- data %>%
    group_by(date, ethnic_group) %>%
    summarise(band1 = sum(band1), 
              band2 = sum(band2), 
              band3 = sum(band3), 
              urgent = sum(urgent), 
              total_FP17 = sum(total_FP17, na.rm = T)) %>%
    group_by(date) %>%
    mutate(total_band1 = sum(band1),
           total_band2 = sum(band2),
           total_band3 = sum(band3),
           total_urgent = sum(urgent),
           total_total_FP17 = sum(total_FP17)) %>%
    mutate(perc_band1 = band1 * 100 / total_band1,
           perc_band2 = band2 * 100 / total_band2,
           perc_band3 = band3 * 100 / total_band3,
           perc_urgent = urgent * 100 / total_urgent,
           perc_total_FP17 = total_FP17 * 100 / total_total_FP17)
  
  #set correct title
  if(numOrPerc == "num"){
    title <- paste("Monthly number of", band_name, "forms by ethnic group")
    ytitle <- "Number of FP17 forms"
    limits <- c(0, max(data$band1, na.rm = T))
  }else if(numOrPerc == "perc"){
    title <- paste("Monthly percentage of", band_name, "forms by ethnic group")
    ytitle <- paste("Percentage of", band_name, "FP17 forms")
    limits <- c(0, 100)
  }else{
    title <- paste("Monthly number of", band_name, "forms by ethnic group")
    ytitle <- "Number of FP17 forms"
    limits <- c(0, max(data$total_FP17, na.rm = T))
  }

  
  #order ethnic groups 
  data$ethnic_group <- factor(data$ethnic_group, 
                              levels = c("White",
                              "Black",
                              "Asian",
                              "Mixed",
                              "Other",
                              "Patient declined",
                              "N/A")
                              )
  
  #plot_code
  ggplot(data, 
         aes(x = date,
             y = {{ band }},
             colour = ethnic_group
         )) +
    geom_line(size = 1) +
    scale_colour_manual(labels = c("White",
                                   "Black",
                                   "Asian",
                                   "Mixed",
                                   "Other",
                                   "Patient declined",
                                   "N/A"), 
                        values = c("coral3",
                                   "orange",
                                   "yellow3",
                                   "green",
                                   "blue",
                                   "blueviolet",
                                   "grey45")
                        ) +
    scale_x_date(date_breaks = "1 month", 
                 date_labels = "%b-%y") +
    scale_y_continuous(label = scales::comma,
                       limits = limits,
                       breaks = scales::pretty_breaks()
                       ) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90)) +
    labs(title = title, 
         x = "Month", 
         y = ytitle,
         colour = "Ethnic group",
         subtitle = subtitle, 
         caption = caption) 

}



################################################################################
#function to plot gender health inequality line chart
plot_HE_gender <- function(data = monthly_HE, band = band1, band_name = "Band 1", numOrPerc = "num", 
                           level = "National", 
                           region = NULL, 
                           STP = NULL,                               
                           override_subtitle= NULL,                               
                           caption = NULL){
  
  #filter data based on region or STP
  if(level == "Regional"){
    data <- data %>%
      filter(region_name == region)
    subtitle <- region
  }else if(level == "STP"){
    data <- data %>%
      filter(commissioner_name == STP)
    subtitle <- STP
  }else{
    subtitle <- "England"
  }
  
  #override subtitle if specified
  if(!is.null(override_subtitle) & override_subtitle == "Grouped top 20 most deprived local authorities by contract*"){
    subtitle <- override_subtitle
    caption <- 
      "*Contracts that fall within the following Local Authorities based on 2019 deprivation data: 
    Middlesbrough, Liverpool, Knowsley, Kingdton upon Hull, Manchester, Blackpool, Birmingham, 
    Burnley, Blackburn with Darwen, Hartlepool, Bradford, Stoke-on-Trent,Halton, Pendle, 
    Nottingham, Oldham, North East Lincolnshire, Hastings, Salford, Rochdale."
  }else{
    subtitle <- override_subtitle
    caption <- "*Patients resident within the following Local Authorities based on 2019 deprivation data: 
    Middlesbrough, Liverpool, Knowsley, Kingdton upon Hull, Manchester, Blackpool, Birmingham, 
    Burnley, Blackburn with Darwen, Hartlepool, Bradford, Stoke-on-Trent,Halton, Pendle, 
    Nottingham, Oldham, North East Lincolnshire, Hastings, Salford, Rochdale."
  }
  
  #avoid standard form
  options(scipen = 999)
  
  #get data in the right format to plot
  data <- clean_HE_data(data)
  
  data <- data %>%
    group_by(date, gender) %>%
    summarise(band1 = sum(band1), 
              band2 = sum(band2), 
              band3 = sum(band3), 
              urgent = sum(urgent), 
              total_FP17 = sum(total_FP17, na.rm = T)) %>%
    group_by(date) %>%
    mutate(total_band1 = sum(band1),
           total_band2 = sum(band2),
           total_band3 = sum(band3),
           total_urgent = sum(urgent),
           total_total_FP17 = sum(total_FP17)) %>%
    mutate(perc_band1 = band1 * 100 / total_band1,
           perc_band2 = band2 * 100 / total_band2,
           perc_band3 = band3 * 100 / total_band3,
           perc_urgent = urgent * 100 / total_urgent,
           perc_total_FP17 = total_FP17 * 100 / total_total_FP17)
  
  #set correct title
  if(numOrPerc == "num"){
    title <- paste("Monthly number of", band_name, "forms by gender")
    ytitle <- "Number of FP17 forms"
    limits <- c(0, max(data$band1, na.rm = T))
    breaks <- seq(0, max(data$band1, na.rm = T),by = 200000)
  }else if(numOrPerc == "perc"){
    title <- paste("Monthly percentage of", band_name, "forms by gender")
    ytitle <- paste("Percentage of", band_name, "FP17 forms")
    limits <- c(0, 100)
    breaks <- seq(0, 100, by = 20)
  }else{
    title <- paste("Monthly number of", band_name, "forms by gender")
    ytitle <- "Number of FP17 forms"
    limits <- c(0, max(data$total_FP17, na.rm = T))
    breaks <- seq(0, max(data$total_FP17, na.rm = T),by = 200000)
  }
  

  #plot_code
  ggplot(data, 
         aes(x = date,
             y = {{ band }},
             colour = gender
         )) +
    geom_line(size = 1) +
    scale_colour_manual(labels = c("Male",
                                   "Female"
                                   ), 
                        values = c("steelblue",
                                   "coral3"
                                   )
    ) +
    labs(title = title, 
         x = "Month", 
         y = ytitle,
         colour = "Gender",
         subtitle = subtitle,
         caption = caption) +
    scale_x_date(date_breaks = "1 month", 
                 date_labels = "%b-%y") +
    scale_y_continuous(label = scales::comma,
                       limits = limits,
                       breaks = scales::breaks_pretty()
    ) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90)) 
  
}


################################################################################
#function to plot charge status health inequality line chart
plot_HE_charge_status <- function(data = monthly_HE, band = band1, band_name = "Band 1", numOrPerc = "num", 
                                  level = "National", 
                                  region = NULL, 
                                  STP = NULL,                               
                                  override_subtitle= NULL,                               
                                  caption = NULL){
  
  #filter data based on region or STP
  if(level == "Regional"){
    data <- data %>%
      filter(region_name == region)
    subtitle <- region
  }else if(level == "STP"){
    data <- data %>%
      filter(commissioner_name == STP)
    subtitle <- STP
  }else{
    subtitle <- "England"
  }
  
  #override subtitle if specified
  if(!is.null(override_subtitle) & override_subtitle == "Grouped top 20 most deprived local authorities by contract*"){
    subtitle <- override_subtitle
    caption <- 
      "*Contracts that fall within the following Local Authorities based on 2019 deprivation data: 
    Middlesbrough, Liverpool, Knowsley, Kingdton upon Hull, Manchester, Blackpool, Birmingham, 
    Burnley, Blackburn with Darwen, Hartlepool, Bradford, Stoke-on-Trent,Halton, Pendle, 
    Nottingham, Oldham, North East Lincolnshire, Hastings, Salford, Rochdale."
  }else{
    subtitle <- override_subtitle
    caption <- "*Patients resident within the following Local Authorities based on 2019 deprivation data: 
    Middlesbrough, Liverpool, Knowsley, Kingdton upon Hull, Manchester, Blackpool, Birmingham, 
    Burnley, Blackburn with Darwen, Hartlepool, Bradford, Stoke-on-Trent,Halton, Pendle, 
    Nottingham, Oldham, North East Lincolnshire, Hastings, Salford, Rochdale."
  }
  
  #avoid standard form
  options(scipen = 999)
  
  #get data in the right format to plot
  data <- clean_HE_data(data)
  
  data <- data %>%
    group_by(date, patient_charge_status) %>%
    summarise(band1 = sum(band1), 
              band2 = sum(band2), 
              band3 = sum(band3), 
              urgent = sum(urgent), 
              total_FP17 = sum(total_FP17, na.rm = T)) %>%
    group_by(date) %>%
    mutate(total_band1 = sum(band1),
           total_band2 = sum(band2),
           total_band3 = sum(band3),
           total_urgent = sum(urgent),
           total_total_FP17 = sum(total_FP17)) %>%
    mutate(perc_band1 = band1 * 100 / total_band1,
           perc_band2 = band2 * 100 / total_band2,
           perc_band3 = band3 * 100 / total_band3,
           perc_urgent = urgent * 100 / total_urgent,
           perc_total_FP17 = total_FP17 * 100 / total_total_FP17)
  
  #set correct title
  if(numOrPerc == "num"){
    title <- paste("Monthly number of", band_name, "forms by patient charge status")
    ytitle <- "Number of FP17 forms"
    limits <- c(0, max(data$band1, na.rm = T))
    breaks <- seq(0, max(data$band1, na.rm = T),by = 200000)
  }else if(numOrPerc == "perc"){
    title <- paste("Monthly percentage of", band_name, "forms by patient charge status")
    ytitle <- paste("Percentage of", band_name, "FP17 forms")
    limits <- c(0, 100)
    breaks <- seq(0, 100, by = 20)
  }else{
    title <- paste("Monthly number of", band_name, "forms by patient charge status")
    ytitle <- "Number of FP17 forms"
    limits <- c(0, max(data$total_FP17, na.rm = T))
    breaks <- seq(0, max(data$total_FP17, na.rm = T),by = 200000)
  }
  
  
  #plot_code
  ggplot(data, 
         aes(x = date,
             y = {{ band }},
             colour = patient_charge_status
         )) +
    geom_line(size = 1) +
    scale_colour_manual(labels = c("Child",
                                   "Exempt",
                                   "Non-Exempt"
    ), 
    values = c("seagreen4",
               "violetred4",
               "royalblue"
    )
    ) +
    labs(title = title, 
         x = "Month", 
         y = ytitle,
         colour = "Patient charge status",
         subtitle = subtitle,
         caption = caption) +
    scale_x_date(date_breaks = "1 month", 
                 date_labels = "%b-%y") +
    scale_y_continuous(label = scales::comma,
                       limits = limits,
                       breaks = scales::breaks_pretty()
    ) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90)) 
  
}


################################################################################
#function to plot age health inequality line chart
plot_HE_age <- function(data = monthly_HE, band = band1, band_name = "Band 1", numOrPerc = "num", 
                        level = "National", 
                        region = NULL, 
                        STP = NULL,                               
                        override_subtitle= NULL,                               
                        caption = NULL){
  
  #filter data based on region or STP
  if(level == "Regional"){
    data <- data %>%
      filter(region_name == region)
    subtitle <- region
  }else if(level == "STP"){
    data <- data %>%
      filter(commissioner_name == STP)
    subtitle <- STP
  }else{
    subtitle <- "England"
  }
  
  #override subtitle if specified
  if(!is.null(override_subtitle) & override_subtitle == "Grouped top 20 most deprived local authorities by contract*"){
    subtitle <- override_subtitle
    caption <- 
    "*Contracts that fall within the following Local Authorities based on 2019 deprivation data: 
    Middlesbrough, Liverpool, Knowsley, Kingdton upon Hull, Manchester, Blackpool, Birmingham, 
    Burnley, Blackburn with Darwen, Hartlepool, Bradford, Stoke-on-Trent,Halton, Pendle, 
    Nottingham, Oldham, North East Lincolnshire, Hastings, Salford, Rochdale."
  }else{
    subtitle <- override_subtitle
    caption <- "*Patients resident within the following Local Authorities based on 2019 deprivation data: 
    Middlesbrough, Liverpool, Knowsley, Kingdton upon Hull, Manchester, Blackpool, Birmingham, 
    Burnley, Blackburn with Darwen, Hartlepool, Bradford, Stoke-on-Trent,Halton, Pendle, 
    Nottingham, Oldham, North East Lincolnshire, Hastings, Salford, Rochdale."
  }
  
  #avoid standard form
  options(scipen = 999)
  
  #get data in the right format to plot
  data <- clean_HE_data(data)
  
  data <- data %>%
    group_by(date, age_group) %>%
    summarise(band1 = sum(band1), 
              band2 = sum(band2), 
              band3 = sum(band3), 
              urgent = sum(urgent), 
              total_FP17 = sum(total_FP17, na.rm = T)) %>%
    group_by(date) %>%
    mutate(total_band1 = sum(band1),
           total_band2 = sum(band2),
           total_band3 = sum(band3),
           total_urgent = sum(urgent),
           total_total_FP17 = sum(total_FP17)) %>%
    mutate(perc_band1 = band1 * 100 / total_band1,
           perc_band2 = band2 * 100 / total_band2,
           perc_band3 = band3 * 100 / total_band3,
           perc_urgent = urgent * 100 / total_urgent,
           perc_total_FP17 = total_FP17 * 100 / total_total_FP17)
  
  #set correct title
  if(numOrPerc == "num"){
    title <- paste("Monthly number of", band_name, "forms by age group")
    ytitle <- "Number of FP17 forms"
    limits <- c(0, max(data$band1, na.rm = T))
    breaks <- seq(0, max(data$band1, na.rm = T),by = 200000)
  }else if(numOrPerc == "perc"){
    title <- paste("Monthly percentage of", band_name, "forms by age group")
    ytitle <- paste("Percentage of", band_name, "FP17 forms")
    limits <- c(0, 100)
    breaks <- seq(0, 100, by = 20)
  }else{
    title <- paste("Monthly number of", band_name, "forms by age group")
    ytitle <- "Number of FP17 forms"
    limits <- c(0, max(data$total_FP17, na.rm = T))
    breaks <- seq(0, max(data$total_FP17, na.rm = T),by = 200000)
  }
  
  #order ethnic groups 
  data$age_group <- factor(data$age_group, 
                              levels = c("0-5",
                                         "06-17",
                                         "18-24",
                                         "25-44",
                                         "45-64",
                                         "65+",
                                         "Other")
  )
  
  #plot_code
  ggplot(data, 
         aes(x = date,
             y = {{ band }},
             colour = age_group
         )) +
    geom_line(size = 1) +
    scale_colour_manual(labels = c("0-5",
                                   "06-17",
                                   "18-24",
                                   "25-44",
                                   "45-64",
                                   "65+",
                                   "Other"), 
                        values = c("coral3",
                                   "orange",
                                   "yellow3",
                                   "green",
                                   "blue",
                                   "blueviolet",
                                   "grey45")
    ) +
    labs(title = title, 
         x = "Month", 
         y = ytitle,
         colour = "Age group",
         subtitle = subtitle,
         caption = caption) +
    scale_x_date(date_breaks = "1 month", 
                 date_labels = "%b-%y") +
    scale_y_continuous(label = scales::comma,
                       limits = limits,
                       breaks = scales::breaks_pretty()
    ) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90)) 
  
}







