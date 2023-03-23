
library (tidyverse)
##library(readxl)
library(DBI)
library(odbc)
##library(scales)
library(formattable)
##library(stringr)
con <- dbConnect(odbc::odbc(), "NCDR")
# 
# bpe_data_pull <- function(){
# 
#   con <- dbConnect(odbc::odbc(), "NCDR")
#   sql <- "SELECT * FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[BPE]"
#   result <- dbSendQuery (con, sql)
#   bpe_all <- dbFetch(result)
#   dbClearResult(result)
#   bpe_all
# }
# 
# 

#data_org_main <- bpe_data_pull()

plot_BPE_no_oral_health_risk_line <- function(level = "National",
                                              stp_region_name = NULL){
  
  ###setwd("N:/_Everyone/Mohammed_Emran/BPE_sexton/BSA_Data")
  ##bpe_data_upload <- read_excel ("BPE_Sextant_Score_Untreated_Decayed_Teeth_and_RRIv2ALL_England.xlsx")
  ###replace all NA with 0
  data_org_main[is.na(data_org_main)] <- 0
  
  #filter for region or STP if specified
  if(level == "National"){
    
    subtitle <- "England"
    
  }else if(level == "Regional"){
    
    data_org_main <- data_org_main %>%
      filter(Latest.Region.Description == stp_region_name)
    
    subtitle <- stp_region_name
    
  }else{
    
    data_org_main <- data_org_main %>%
      filter(Latest.Commissioner.Name == stp_region_name)
    
    subtitle <- stp_region_name
    
  }
  
  ###filtering and selecting the right columns
  ########str(data_org_main)
  
  data_org <- data_org_main %>%
    group_by(Latest.Region.Description, Year_Month ) %>%
    rename("Total_Form_Count_Highest_BPE_Sextant_Score_0_or_1_and_UDT_0_and_RRI_less_than_1_year" =
             "Total_Form_Count_Highest_BPE_Sextant_Score_0_or_1_and_UDT_0_and_RRI_<1_year") %>%
    
    # filter (Year_Month == max(data_org_main$Year_Month)) %>%
    
    mutate (percentage_low_risk_recall_interval_less_than_1_year	 = 
              as.numeric(Total_Form_Count_Highest_BPE_Sextant_Score_0_or_1_and_UDT_0_and_RRI_less_than_1_year) /
              as.numeric(Total_Form_Count_Highest_BPE_Sextant_Score_0_or_1_and_0_UDT)) %>%
    
    mutate (percentage_complete_forms	 = 
              as.numeric (Forms_with_Highest_BPE_Sextant_Score) /
              as.numeric (Total.Form.Count)) %>%
    
    mutate (average_percentage_low_risk_recall_interval_less_than_1_year_average_over_practices
            = round (mean(percentage_low_risk_recall_interval_less_than_1_year, na.rm = TRUE), digits = 2)) %>%
    
    mutate (average_risk_score_complete_average_over_practices = 
              round (mean(percentage_complete_forms, na.rm = TRUE), digits = 2)) %>%
    
    mutate (average_percentage_low_risk_recall_interval_less_than_1_year_average_over_patients_seen	 = 
              round (as.numeric(sum (Total_Form_Count_Highest_BPE_Sextant_Score_0_or_1_and_UDT_0_and_RRI_less_than_1_year))  /
                       as.numeric(sum (Total_Form_Count_Highest_BPE_Sextant_Score_0_or_1_and_0_UDT)), digits = 2)) %>%
    
    mutate (average_percentage_low_risk_score_complete_average_over_patients_seen = 
              round (as.numeric (sum (Forms_with_Highest_BPE_Sextant_Score)) /
                       as.numeric (sum (Total.Form.Count)), digits = 2)) 
  
}
  

plot_BPE_no_oral_health_risk <- function(data = data_org_main,#BPE_data,
                                         level = "National",
                                         region_STP_name = NULL,
                                         ICB_lookup = STP_ICB_lookup_codes){
  
  data[is.na(data)] <- 0
  
  #join in standardised ICB column
  ICB_lookup <- ICB_lookup %>%
    select(commissioner_name = commissioner_name_ICB,
           Latest_Commissioner_Code = commissioner_ONS_boundary_code_ICB)
  
  data <- data %>%
    left_join(ICB_lookup, by = "Latest_Commissioner_Code")
  
  #filter for STP or region
  if(level == "Regional"){
    data <- data %>%
      filter(Latest.Region.Description == region_STP_name )
    subtitle <- region_STP_name
  }else if(level == "STP"){
    data <- data %>%
      filter(commissioner_name == region_STP_name)
    subtitle <- region_STP_name
  }else{
    subtitle <- "England"
  }
  
  ###filtering and selecting the right columns
  ########str(data)
  
  data_org <- data %>%
    group_by(Latest.Region.Description) %>%
    rename("Total_Form_Count_Highest_BPE_Sextant_Score_0_or_1_and_UDT_0_and_RRI_less_than_1_year" =
             "Total_Form_Count_Highest_BPE_Sextant_Score_0_or_1_and_UDT_0_and_RRI_<1_year") %>%
    
    # filter (Year_Month == max(data$Year_Month)) %>%
    
    mutate (percentage_low_risk_recall_interval_less_than_1_year	 = 
              as.numeric(Total_Form_Count_Highest_BPE_Sextant_Score_0_or_1_and_UDT_0_and_RRI_less_than_1_year) /
              as.numeric(Total_Form_Count_Highest_BPE_Sextant_Score_0_or_1_and_0_UDT)) %>%
    
    mutate (percentage_complete_forms	 = 
              as.numeric (Forms_with_Highest_BPE_Sextant_Score) /
              as.numeric (Total.Form.Count)) %>%
    
    mutate (average_percentage_low_risk_recall_interval_less_than_1_year_average_over_practices
            = round (mean(percentage_low_risk_recall_interval_less_than_1_year, na.rm = TRUE), digits = 2)) %>%
    
    mutate (average_risk_score_complete_average_over_practices = 
              round (mean(percentage_complete_forms, na.rm = TRUE), digits = 2)) %>%
    
    mutate (average_percentage_low_risk_recall_interval_less_than_1_year_average_over_patients_seen	 = 
              round (as.numeric(sum (Total_Form_Count_Highest_BPE_Sextant_Score_0_or_1_and_UDT_0_and_RRI_less_than_1_year))  /
                       as.numeric(sum (Total_Form_Count_Highest_BPE_Sextant_Score_0_or_1_and_0_UDT)), digits = 2)) %>%
    
    mutate (average_percentage_low_risk_score_complete_average_over_patients_seen = 
              round (as.numeric (sum (Forms_with_Highest_BPE_Sextant_Score)) /
                       as.numeric (sum (Total.Form.Count)), digits = 2)) 
  
  
  

  ##### plot the bar graph
  
  plot_1 <- data_org %>% group_by(Latest.Region.Description, Year_Month ) %>%
    summarise(average_percentage_low_risk_recall_interval_less_than_1_year_average_over_practices=
                formattable::percent (mean (average_percentage_low_risk_recall_interval_less_than_1_year_average_over_practices), digits =  0),
              average_risk_score_complete_average_over_practices =
                formattable::percent (mean (average_risk_score_complete_average_over_practices), digits =  0),
              average_percentage_low_risk_recall_interval_less_than_1_year_average_over_patients_seen =
                formattable::percent (mean (average_percentage_low_risk_recall_interval_less_than_1_year_average_over_patients_seen), digits =  0),
              average_percentage_low_risk_score_complete_average_over_patients_seen = 
                formattable::percent  (mean (average_percentage_low_risk_score_complete_average_over_patients_seen),digits =  0) )
  
  
  plot_2 <- plot_1 %>% pivot_longer(cols=c('average_percentage_low_risk_recall_interval_less_than_1_year_average_over_practices', 
                                           'average_percentage_low_risk_recall_interval_less_than_1_year_average_over_patients_seen',
                                           'average_risk_score_complete_average_over_practices',
                                           'average_percentage_low_risk_score_complete_average_over_patients_seen'),
                                    names_to='Description',
                                    values_to= 'Percentage')
  
  plot_2 [plot_2 == 'average_percentage_low_risk_recall_interval_less_than_1_year_average_over_practices'] <-
    "Average % of completed record with no indication of oral health risk"
  
  
  plot_2 [plot_2 == 'average_risk_score_complete_average_over_practices'] <-
    "Average % of completed risk scores"
  
  filtered_plot_2_1 = filter(plot_2, Description %in% c("Average % of completed record with no indication of oral health risk",
                                                        "Average % of completed risk scores"))
  

  filtered_plot_2_2 = filter(plot_2, Description %in% c("Average % of completed record with no indication of oral health risk"))
  
  
  filtered_plot_2_1$Year_Month 
  
  x <- "202210"
  year_x <- substr(filtered_plot_2_1$Year_Month, 1, 4)
  month_x <- substr(filtered_plot_2_1$Year_Month, 5, 6)
  filtered_plot_2_1$Year_Month <- as.Date(paste0(year_x, "-", month_x, "-01"))
  
  
  bpe_england <- filtered_plot_2_1 %>%  group_by(Year_Month, Description) %>% 
    summarise(Percentage = mean(Percentage))
  
  
  p_test<- ggplot(bpe_england, aes(x = Year_Month, y = Percentage,
                                   group =Description )) +
    geom_line(aes(color=Description),
              linewidth = 1.5)+
    geom_point(aes(color=Description),
               size = 3) +
    expand_limits(y=0) + 
    geom_text(aes(label = Percentage), vjust=-.5)+
    theme_classic() + 
    ##theme(legend.position="bottom") +
    scale_y_continuous(labels = scales::percent) +
    theme(legend.position="top") +
    scale_x_date(date_labels = "%b-%Y") +
    labs(title = "Average % of completed FP17s with BPE scores and average % of FP17s indicating no oral health risk",
       subtitle = subtitle) +
    scale_fill_manual(values = c("#009E73", "#F0E442"),
                      label = c("Average % of completed FP17s indicating no oral health risk",
                                "% of all FP17s completed with BPE scores"))
  p_test
  
  
}
