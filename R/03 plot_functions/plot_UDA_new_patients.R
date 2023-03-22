library (tidyverse)
library(DBI)
library(odbc)

###UDA data

#new_vs_return_data <- pull_new_vs_returning_patient()

new_vs_return_data$Contract_Number<- as.numeric(new_vs_return_data$Contract_Number)

plot_number_new_vs_returning_patients <- function(data = new_vs_return_data,
                                                  scheduled_data = UDA_scheduled_data,
                                                  as_percentage = FALSE){
  
  ## Find out the number of contractors submitted FP17 in regions and Nationally
  scheduled_data <- scheduled_data %>%
    filter(month >= as.Date("2022-04-01")) 
  
  contractors_number_regional <- scheduled_data %>% 
    group_by(region_name) %>%
    dplyr::count(month, name = "total_number_of_contractors_submitted_FP17")
  
  contractors_number_england <- scheduled_data %>% 
    dplyr::count(month, name = "total_number_of_contractors_submitted_FP17")
  
  ## Find out the number of contractors seen new patients in regions and Nationally by age
  
  ##new_patients_seen_contractors_regional
  
  
  ##new_vs_return_data
  
  data_longer <- new_vs_return_data 
  
  colnames(data_longer) <- c(
    "Contract.Number", "Latest_Provider_Name", "Latest_Region",            
    "Latest_Region_Description", "Latest_Commissioner_Code",  "Latest Commissioner_Name" ,
    "adult_2022-04",             "adult_2022-05" ,            "adult_2022-06",            
    "adult_2022-07" ,            "adult_2022-08"  ,           "adult_2022-09" ,           
    "adult_2022-10"  ,           "adult_2022-11"   ,          "adult_2022-12"  ,          
    "adult_2023-01"   ,          "adult_2023-02"    ,         "child_2022-04"   ,         
    "child_2022-05"    ,         "child_2022-06"     ,        "child_2022-07"    ,        
    "child_2022-08"     ,        "child_2022-09"      ,       "child_2022-10"     ,       
    "child_2022-11"      ,       "child_2022-12"       ,      "child_2023-01"      ,      
    "child_2023-02")
  
  
  test_1<- data_longer<- data_longer %>% select(!c(##Contract_Number, 
    Latest_Provider_Name, Latest_Region, 
    Latest_Commissioner_Code, "Latest Commissioner_Name")) %>% 
    pivot_longer(cols = -c(Latest_Region_Description, Contract.Number), 
                 names_to = c('.value', 'month'), 
                 names_sep="_") 
  
  test_1$month 
  
  ##x <- "202210"
  year_x <- substr(test_1$month, 1, 4)
  month_x <- substr(test_1$month, 6, 7)
  test_1$month <- as.Date(paste0(year_x, "-", month_x, "-01"))
  
  
  
  #####################
  #####################
  #####################  patients' numbers ################
  
  ##### number of new patients' regional 
  new_adult_patients_regional_summary <- test_1 %>% group_by(Latest_Region_Description, month) %>% 
    dplyr::summarise (number = sum(adult, na.rm = TRUE)) %>% 
    mutate(category ="adult_new_patients") %>% 
    select(Latest_Region_Description, month,category, number )
  
  
  new_child_patients_regional_summary <- test_1 %>% group_by(Latest_Region_Description, month) %>% 
    dplyr::summarise (number = sum(child, na.rm = TRUE)) %>% 
    mutate(category ="child_new_patients") %>% 
    select(Latest_Region_Description, month,category, number )
  
  ##combined --for CHART
  new_all_patients_regional_summary<- bind_rows(new_adult_patients_regional_summary,
                                                new_child_patients_regional_summary)
  ##Latest_Region_Description                  
  # 
  # new_patient_line_chart <- ggplot(new_all_patients_regional_summary, aes(x = month, y = number,
  #                                  group =Latest_Region_Description )) +
  #   geom_line(aes(color=category),
  #             linewidth = 1.5)+
  #   geom_point(aes(color=category),
  #              size = 3) +
  #   expand_limits(y=0) + 
  #   geom_text(aes(label = category), vjust=-.5)+
  #   theme_classic() + 
  #   ##theme(legend.position="bottom") +
  #   theme(legend.position="top") +
  #   labs(title = "Average % of completed FP17s with BPE scores and average % of FP17s indicating no oral health risk")+
  #        
  #        # subtitle = subtitle) +
  #   scale_fill_manual(values = c("#009E73", "#F0E442"),
  #                     label = c("Average % of completed FP17s indicating no oral health risk",
  #                               "% of all FP17s completed with BPE scores"))
  
  
  
  
  ##### number of new patients England 
  new_adult_patients_england_summary <- test_1 %>% group_by(month) %>% 
    dplyr::summarise (number = sum(adult, na.rm = TRUE)) %>% 
    mutate(category ="adult_new_patients") %>% 
    select(month,category, number )
  
  
  new_child_patients_england_summary <- test_1 %>% group_by( month) %>% 
    dplyr::summarise (number = sum(child, na.rm = TRUE)) %>% 
    mutate(category ="child_new_patients") %>% 
    select( month,category, number )
  
  ##combined --for CHART
  new_all_patients_england_summary<- bind_rows(new_adult_patients_england_summary,
                                               new_child_patients_england_summary)
  
  ##month      category           
  
  str(new_all_patients_england_summary)
  
  if(as_percentage == FALSE){
    new_patient_line_chart_england <- ggplot(new_all_patients_england_summary, aes(x = month, y = number,
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
      labs(title = "New Patient (no previous visit at all or before 24 months or more) Numbers- England")
    
    new_patient_line_chart_england
    
  }else{
    
    ###regional providers
    
    new_child_patients_provider_number_summary <- test_1 %>% select(!(adult)) %>%
      group_by (month, Latest_Region_Description) %>%
      summarise(number_of_provider = sum(!is.na(child))) %>%
      mutate(category ="child_new_patients_provider") %>%
      select(month, Latest_Region_Description,category, number_of_provider )
    
    new_adult_patients_provider_number_summary <- test_1 %>% select(!(child)) %>%
      group_by (month, Latest_Region_Description) %>%
      summarise(number_of_provider = sum(!is.na(adult))) %>%
      mutate(category ="adult_new_patients_provider") %>%
      select(month, Latest_Region_Description,category, number_of_provider )
    
    new_all_patients_provider_regional_summary <- bind_rows (new_child_patients_provider_number_summary,
                                                             new_adult_patients_provider_number_summary)
    
    
    ### England providers
    
    new_child_patients_provider_england_summary <- test_1 %>% select(!(adult)) %>%
      group_by (month) %>%
      summarise(number_of_provider = sum(!is.na(child))) %>%
      mutate(category ="% of Providers Served New Child Patients") %>%
      select(month,category, number_of_provider )
    
    new_adult_patients_provider_england_summary <- test_1 %>% select(!(child)) %>%
      group_by (month) %>%
      summarise(number_of_provider = sum(!is.na(adult))) %>%
      mutate(category ="% of Providers Served New Adult Patients") %>%
      select(month,category, number_of_provider )
    
    new_all_patients_provider_england_summary <- bind_rows (new_child_patients_provider_england_summary,
                                                            new_adult_patients_provider_england_summary)
    
    
    child_patients_comparision_provider_regional<- left_join(contractors_number_regional,
                                                             new_child_patients_provider_number_summary,
                                                             by = c("month" = "month", "region_name" = "Latest_Region_Description")) %>%
      mutate (Percentage = number_of_provider/ total_number_of_contractors_submitted_FP17) %>%
      mutate (category = "% of Providers Served New Child Patients") %>%
      select(region_name, month, category, Percentage )
    
    adult_patients_comparision_provider_regional<- left_join(contractors_number_regional,
                                                             new_adult_patients_provider_number_summary,
                                                             by = c("month" = "month", "region_name" = "Latest_Region_Description")) %>%
      mutate (Percentage = number_of_provider/ total_number_of_contractors_submitted_FP17) %>%
      mutate (category = "% of Providers Served New Adult Patients") %>%
      select(region_name, month, category, Percentage )
    
    
    
    ####table 1 for CHART -regional providers
    regional_provider_chart_1 <- bind_rows(child_patients_comparision_provider_regional,
                                           adult_patients_comparision_provider_regional)
    
    all_england_provider <- left_join (new_all_patients_provider_england_summary, contractors_number_england,
                                       by = c("month" = "month")) %>%
      mutate (Percentage = formattable::percent (number_of_provider/ total_number_of_contractors_submitted_FP17, digits =  0)) %>%
      select (month, category, Percentage)
    
    
    
    all_england_provider_chart <- ggplot(all_england_provider, aes(x = month, y = Percentage,
                                                                   group =category )) +
      geom_line(aes(color=category),
                linewidth = 1.5)+
      geom_point(aes(color=category),
                 size = 3) +
      ##not working ---------------------scale_x_date(date_labels = "%b-%Y") +
      ##expand_limits(y=0) +
      geom_text(aes(label = Percentage), vjust=-.5)+
      theme_classic() +
      ##theme(legend.position="bottom") +
      theme(legend.position="top") +
      labs(title = "% of Providers Served New Patients")
    
    all_england_provider_chart 
    
  }
 
  
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