library (tidyverse)
library(DBI)
library(odbc)
library(formattable)

################################################################################
pull_UDA_scheduled_data <- function(){
  
  con <- dbConnect(odbc::odbc(), "NCDR")
  
  sql <- "SELECT *
  FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[UDA_scheduled]"
  result <- dbSendQuery(con, sql)
  UDA_scheduled_data <- dbFetch(result)
  dbClearResult(result)
  
  UDA_scheduled_data %>%
  rename( month = data_month) 
}

pull_new_vs_returning_patients <- function(){
  
  con <- dbConnect(odbc::odbc(), "NCDR")
  sql <- "SELECT * FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[New_vs_Returning_Patients]"
  
  result <- dbSendQuery (con, sql)
  new_vs_return <- dbFetch(result)
  dbClearResult(result)
  new_vs_return
}
###UDA data

##colnames(new_vs_return_data)

new_vs_return_data <- pull_new_vs_returning_patients()

UDA_scheduled_data <-pull_UDA_scheduled_data()

plot_number_new_vs_returning_patients <- function(data = new_vs_return_data,
                                                  level = "National",
                                                  stp_region_name = NULL, 
                                                  scheduled_data = UDA_scheduled_data,
                                                  as_percentage = FALSE){
  
  
  #filter for region or STP if specified
  if(level == "National"){
    
    subtitle <- "England"
    
  }else if(level == "Regional"){
    
    data <- data %>%
      filter(Latest_Region_Description == stp_region_name)
    
    subtitle <- stp_region_name
    
  }else{
    
    data <- data %>%
      filter("Latest Commissioner_Name" == stp_region_name)
    
    subtitle <- stp_region_name
    
  }


  scheduled_data <- scheduled_data %>%
    filter(month >= as.Date("2022-04-01"))

  # scheduled_data <- scheduled_data %>% filter(data_month >= as.Date("2022-04-01")) 
  # 
  #  
  # contractors_number_all_patients <- scheduled_data %>% 
  #   group_by(data_month) %>%
  #   dplyr::count(data_month, name = "total_number_of_contractors_submitted_FP17")
  # 
  contractors_number_england <- scheduled_data %>%
    dplyr::count(month, name = "total_number_of_contractors_submitted_FP17")
  # 
  ## Find out the number of contractors seen new patients in regions and Nationally by age
  
  ##new_patients_seen_contractors_regional
  
  
  ##new_vs_return_data
  
  new_vs_return_data$Contract_Number<- as.numeric(new_vs_return_data$Contract_Number)
  
  data_longer <- new_vs_return_data 
  
  colnames(data_longer) <- c(
    "Contract.Number", "Latest_Provider_Name", "Latest_Region",            
    "Latest_Region_Description", "Latest_Commissioner_Code",  "Latest_Commissioner_Name" ,
    "adult_2022-04",             "adult_2022-05" ,            "adult_2022-06",            
    "adult_2022-07" ,            "adult_2022-08"  ,           "adult_2022-09" ,           
    "adult_2022-10"  ,           "adult_2022-11"   ,          "adult_2022-12"  ,          
    "adult_2023-01"   ,          "adult_2023-02"    ,         "child_2022-04"   ,         
    "child_2022-05"    ,         "child_2022-06"     ,        "child_2022-07"    ,        
    "child_2022-08"     ,        "child_2022-09"      ,       "child_2022-10"     ,       
    "child_2022-11"      ,       "child_2022-12"       ,      "child_2023-01"      ,      
    "child_2023-02")
  
  
  test_1<- data_longer<- data_longer %>% 
    # select(!c(##Contract_Number, 
    # Latest_Provider_Name, Latest_Region, 
    # Latest_Commissioner_Code, Latest_Commissioner_Name)) %>% 
    pivot_longer(cols = -c(Contract.Number, Latest_Provider_Name, Latest_Region,            
    Latest_Region_Description, Latest_Commissioner_Code,  Latest_Commissioner_Name), 
                 names_to = c('.value', 'month'), 
                 names_sep="_") 
  
  year_x <- substr(test_1$month, 1, 4)
  month_x <- substr(test_1$month, 6, 7)
  test_1$month <- as.Date(paste0(year_x, "-", month_x, "-01"))
  
  #####################
  #####################
  #####################  patients' numbers ################
  
  patients_number_summary <- test_1 %>% group_by(month) %>% 
    dplyr::summarise (new_adult_patients_number = sum(adult, na.rm = TRUE),
                      new_child_patients_number = sum(child, na.rm = TRUE)) 
  
  patients_number_summary_longer <- patients_number_summary %>% 
    pivot_longer (cols = c('new_adult_patients_number', 'new_child_patients_number'),
                                      names_to='category',
                                      values_to='number')
              
  ##%>% select(Latest_Region_Description, month, new_adult_patients_number, new_child_patients_number )
  
  
  # new_all_patients_regional_summary <- new_adult_patients_regional_summary %>%  
  #   pivot_longer(cols=c('new_adult_patients_number', 'new_child_patients_number'),
  #                names_to='category',
  #                values_to='number')
  # 
  
  if(as_percentage == FALSE){
    new_patient_line_chart_england <- ggplot(patients_number_summary_longer, 
                                             aes(x = month, y = number,
                                            group =category )) +
      geom_line(aes(color=category),
                linewidth = 1.5)+
      geom_point(aes(color=category),
                 size = 3) +
      scale_x_date(date_labels = "%b-%Y", breaks = "1 month") +
      ##expand_limits(y=0) +
      geom_text(aes(label = number), vjust=-.5)+
      theme_classic() +
      ##theme(legend.position="bottom") +
      theme(legend.position="top") +
      labs(title = "New Patient (no previous visit at all or before 24 months or more) Numbers",
           subtitle = subtitle)
    
    new_patient_line_chart_england
    
  }
  else{
    
    ### providers percentage 
    
    new_patients_provider_number <- test_1 %>% 
      group_by (month) %>%
      summarise(served_new_child_patients = sum(!is.na(child)),
                served_new_adult_patients = sum(!is.na(adult))) 
    
    all_provider <- left_join(new_patients_provider_number, 
                              contractors_number_all_patients, by = c("month" = "data_month"))
    
    
    all_provider <- all_provider %>% 
      mutate(percent_served_new_child_patients = formattable::percent (served_new_child_patients /
               total_number_of_contractors_submitted_FP17, digits =0),
             percent_served_new_adult_patients = formattable::percent (served_new_adult_patients /
               total_number_of_contractors_submitted_FP17, digits =0))
    
    all_provider_longer <- all_provider %>% 
      pivot_longer(cols = c("percent_served_new_child_patients", 
                            "percent_served_new_adult_patients"),
                   names_to ='category',
                   values_to='Percentage') %>% 
      select (month, category, Percentage) %>%
      mutate(month = as.Date(month))
    
    
    provider_chart <- ggplot(all_provider_longer, 
                                         aes(x = month, y = Percentage,
                                             group =category )) +
      geom_line(aes(color=category),
                linewidth = 1.5)+
      geom_point(aes(color=category),
                 size = 3) +
     scale_x_date(date_labels = "%b-%Y", breaks = "1 month") +
      expand_limits(y=0) +
      geom_text(aes(label = Percentage), vjust=-.5)+
      theme_classic() +
      ##theme(legend.position="bottom") +
      theme(legend.position="top") +
      labs(title = "% of Providers Served New Patients",
           subtitle = subtitle)
    
    provider_chart 
    
  }


    ###colnames(all_provider)
    
    
    
    
    
    
    #   mutate(category ="child_new_patients_provider") %>%
    #   select(month, category, number_of_provider )
    # 
    # new_adult_patients_provider_number_summary <- test_1 %>% select(!(child)) %>%
    #   group_by (month) %>%
    #   summarise(number_of_provider = sum(!is.na(adult))) %>%
    #   mutate(category ="adult_new_patients_provider") %>%
    #   select(month, category, number_of_provider )
    # 
    # new_all_patients_provider_regional_summary <- bind_rows (new_child_patients_provider_number_summary,
    #                                                          new_adult_patients_provider_number_summary)
    # 
    # 
    # ### England providers
    # 
    # new_child_patients_provider_england_summary <- test_1 %>% select(!(adult)) %>%
    #   group_by (month) %>%
    #   summarise(number_of_provider = sum(!is.na(child))) %>%
    #   mutate(category ="% of Providers Served New Child Patients") %>%
    #   select(month,category, number_of_provider )
    # 
    # new_adult_patients_provider_england_summary <- test_1 %>% select(!(child)) %>%
    #   group_by (month) %>%
    #   summarise(number_of_provider = sum(!is.na(adult))) %>%
    #   mutate(category ="% of Providers Served New Adult Patients") %>%
    #   select(month,category, number_of_provider )
    # 
    # new_all_patients_provider_england_summary <- bind_rows (new_child_patients_provider_england_summary,
    #                                                         new_adult_patients_provider_england_summary)
    # 
    # 
    # 
    # child_patients_comparision_provider_regional<- left_join(contractors_number_regional,
    #                                                          new_child_patients_provider_number_summary,
    #                                                          by = c("month" = "month")) %>%
    #   mutate (Percentage = formattable::percent (number_of_provider/ total_number_of_contractors_submitted_FP17, digits = 0)) %>%
    #   mutate (category = "% of Providers Served New Child Patients") %>%
    #   select(region_name, month, category, Percentage )
    # 
    # adult_patients_comparision_provider_regional<- left_join(contractors_number_regional,
    #                                                          new_adult_patients_provider_number_summary,
    #                                                          by = c("month" = "month")) %>%
    #   mutate (Percentage = formattable::percent(number_of_provider/ total_number_of_contractors_submitted_FP17, digits = 0)) %>%
    #   mutate (category = "% of Providers Served New Adult Patients") %>%
    #   select(region_name, month, category, Percentage )
    # 
    # 
    # 
    # ###table 1 for CHART -regional providers
    # regional_provider_chart_1 <- bind_rows(child_patients_comparision_provider_regional,
    #                                        adult_patients_comparision_provider_regional)
    # # 
    # all_england_provider <- left_join (new_all_patients_provider_england_summary, contractors_number_england,
    #                                    by = c("month" = "month")) %>%
    #   mutate (Percentage = formattable::percent (number_of_provider/ total_number_of_contractors_submitted_FP17, digits = 0)) %>%
    #   select (month, category, Percentage)
    # 
    # 
    
  # # subtitle = subtitle) +
  # # scale_fill_manual(values = c("#009E73", "#F0E442"),
  # #                   label = c("Average % of completed FP17s indicating no oral health risk",
  # #                             "% of all FP17s completed with BPE scores"))
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # # new_patients_summary_england <- test_1 %>% group_by(month) %>% 
  # #   dplyr::summarise (adult_new_patients = sum(adult, na.rm = TRUE),
  # #                     child_new_patients = sum(child, na.rm = TRUE))
  # 
  # ##sum(new_patients_summary_england$adult_new_patients)/1000000
  # 
  # 
  # #####################
  # #####################
  # #####################  provider ################
  # 
}
####table 1 for chart -england
# new_patients_provider_regional <- left_join (new_child_patients_provider_number_summary,
#                                              new_adult_patients_provider_number_summary,
#                                              by = c("month", "Latest_Region_Description"))

# colnames(new_patients_provider_regional) <- c("month",  "Latest_Region_Description",
#                                           "number_of_new_child_patients_provider",
#                                         "number_of_new_adult_patients_provider")
# new_child_patients_provider_number_england <- test_1 %>% select(!(adult)) %>% 
#   group_by (month) %>% 
#   summarise(total = sum(!is.na(child)))
# 
# new_adult_patients_provider_number_england <- test_1 %>% select(!(child)) %>% 
#   group_by (month) %>% 
#   summarise(total = sum(!is.na(adult)))
# 
# new_patients_provider_england <- left_join (new_child_patients_provider_number_england,
#                                             new_adult_patients_provider_number_england,
#                                              by = c("month"))
# 
# colnames(new_patients_provider_england) <- c("month", "number_of_new_child_patients_provider",
#                                               "number_of_new_adult_patients_provider")
#