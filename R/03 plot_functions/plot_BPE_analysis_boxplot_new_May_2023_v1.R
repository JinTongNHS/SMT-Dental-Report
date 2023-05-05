library (tidyverse)
##library(readxl)
##library(DBI)
##library(odbc)
##library(scales)
library(formattable)
library(cowplot)

source("N:/_Everyone/Mohammed_Emran/From_document_folder/R_Projects/SMT-Dental-Report/R/03 plot_functions/boxplot_explanation_function.R")

plot_BPE_no_oral_health_risk <- function(data = BPE_data,
                                         level = "National",
                                         region_STP_name = NULL,
                                         ICB_lookup = STP_ICB_lookup_codes,
                                         plotChart = TRUE,
                                         quartileOutput = "LQ"){
  
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
  
  ##data <- BPE_data ###need to comment out
  
  year_x <- substr(data$Year_Month, 1, 4)
  month_x <- substr(data$Year_Month, 5, 6)
  data$Year_Month <- as.Date(paste0(year_x, "-", month_x, "-01"))
  
  data_org <- data %>%
    group_by(Year_Month, Contract.Number) %>%
    summarise(Nforms = sum(Total.Form.Count),
              NcompYear_Monthedforms = sum(Forms_with_Highest_BPE_Sextant_Score)) %>% 
    mutate (percentcompYear_Montheform = formattable::percent(NcompYear_Monthedforms/ Nforms, digits =0))
  
  ##colnames(n_bpe_data)
  
  data <- data %>% 
    dplyr::rename("Total_Form_Count_Highest_BPE_Sextant_Score_0_or_1_and_UDT_0_and_RRI_less_than_1_year" =
                    "Total_Form_Count_Highest_BPE_Sextant_Score_0_or_1_and_UDT_0_and_RRI_<1_year") %>% 
    group_by(Year_Month, Contract.Number) %>%
    filter(Highest.BPE.Sextant.Score != 'No BPE Assessment Possible') %>% 
    summarise (compYear_Monthe_forms = sum(Forms_with_Highest_BPE_Sextant_Score), 
               nlow_risk = 
                 sum (as.numeric(Total_Form_Count_Highest_BPE_Sextant_Score_0_or_1_and_0_UDT), na.rm = TRUE),
               low_risk_less1year = 
                 sum(as.numeric(Total_Form_Count_Highest_BPE_Sextant_Score_0_or_1_and_UDT_0_and_RRI_less_than_1_year), na.rm = TRUE)) %>%  
    mutate (percent_low_risk_whic_are1_year = 
              formattable::percent (low_risk_less1year/nlow_risk, digits =0)) %>% 
    mutate (percentlowrisk= formattable::percent (nlow_risk/ compYear_Monthe_forms, digits =0) ) %>% 
    filter(!is.na(percent_low_risk_whic_are1_year))
  
   p_meds <- data %>% 
    group_by(Year_Month) %>% 
    summarise (med = median(percent_low_risk_whic_are1_year))
  
  max_month <- max (data$Year_Month)
  
  p_meds_latest <- data %>% 
    group_by (Year_Month) %>% 
    summarise(med = median(percent_low_risk_whic_are1_year)) %>% 
    filter(Year_Month ==max_month)
  
  p_meds_latest_p <- formattable::percent (p_meds_latest$med, digits = 0)
  
  if(plotChart == TRUE){
    p_test <- ggplot(data = data,
                     aes(x = Year_Month, y = percent_low_risk_whic_are1_year)) +
      geom_boxplot(aes(group = Year_Month)) +
      labs(x = "Month",
           y = "",
           title = "Percentrage of Low Risk Patients Recalled within a Year by Contractors",
           subtitle = subtitle
      ) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      scale_x_date(date_labels = "%b-%y") +
      theme_bw() +
      geom_text(data = p_meds, aes(x = Year_Month, y = med, label = med), 
                size = 3, vjust = -.7) 
    
    legend_plot<- ggplot_box_legend()
    
    p <- plot_grid(p_test,
                   legend_plot,
                   nrow = 1, rel_widths = c(.6,.4))
    
    ###to be added at the bottom of chart
    # description <- c("50% of all the contractors are recalling", formattable::percent (p_meds_latest$med, digits = 0),
    #                  " of the patients with low oral health risk ") ##+ "of"
    
    p
  }else if(!is.null(quartileOutput)){
    
    #get latest year of data
    numbers_calc <- data %>%  
      filter(Year_Month == max_month)

    #output differentquartile based on input
    if(quartileOutput == "LQ"){
      quartile <- quantile(numbers_calc$percent_low_risk_whic_are1_year, prob = 0.25)
    }else if(quartileOutput == "UQ"){
      quartile <- quantile(numbers_calc$percent_low_risk_whic_are1_year, prob = 0.75)
    }else{
      quartile <- quantile(numbers_calc$percent_low_risk_whic_are1_year, prob = 0.5)
    }

    quartile
  }else{
    
    #return max date
    max_month
  }
  
}


# ##### plot the bar graph
# 
# data_org <- data %>%
#   group_by(Year_Month) %>%
#   summarise(Nforms = sum(Total.Form.Count),
#             Ncompletedforms = sum(Forms_with_Highest_BPE_Sextant_Score)) %>% 
#   mutate (percentcompleteform = formattable::percent(Ncompletedforms/ Nforms, digits =0))
# 
# 
# data <- data %>% 
#   rename("Total_Form_Count_Highest_BPE_Sextant_Score_0_or_1_and_UDT_0_and_RRI_less_than_1_year" =
#            "Total_Form_Count_Highest_BPE_Sextant_Score_0_or_1_and_UDT_0_and_RRI_<1_year") %>% 
#   group_by(Year_Month) %>%
#   filter(Highest.BPE.Sextant.Score != 'No BPE Assessment Possible') %>% 
#   summarise (complete_forms = sum(Forms_with_Highest_BPE_Sextant_Score), 
#              nlow_risk = sum (as.numeric(Total_Form_Count_Highest_BPE_Sextant_Score_0_or_1_and_0_UDT), na.rm = TRUE),
#              low_risk_less1year = sum(as.numeric(Total_Form_Count_Highest_BPE_Sextant_Score_0_or_1_and_UDT_0_and_RRI_less_than_1_year), na.rm = TRUE)) %>%  
#   mutate (percent_low_risk_whic_are1_year = formattable::percent (low_risk_less1year/nlow_risk, digits =0)) %>% 
#   mutate (percentlowrisk= formattable::percent (nlow_risk/ complete_forms, digits =0) )
# 
# 
# year_x <- substr(data_org$Year_Month, 1, 4)
# month_x <- substr(data_org$Year_Month, 5, 6)
# data_org$Year_Month <- as.Date(paste0(year_x, "-", month_x, "-01"))
# 
# 
# year_y <- substr(data$Year_Month, 1, 4)
# month_y <- substr(data$Year_Month, 5, 6)
# data$Year_Month <- as.Date(paste0(year_y, "-", month_x, "-01"))
# 
# 
# colnames (bpe_england)
# 
# bpe_plot <-data_org %>%  left_join(data, by = c("Year_Month"= "Year_Month")) %>% 
#   select (Year_Month, percentcompleteform, percent_low_risk_whic_are1_year) ##, percentlowrisk)
# 
# bpe_england <- bpe_plot %>% pivot_longer(cols = c ("percentcompleteform", "percent_low_risk_whic_are1_year"),
#                                          names_to='Description', values_to= 'Percentage')
# # 
# # bpe_england <- filtered_plot_2_1 %>%  group_by(Year_Month, Description) %>% 
# #   summarise(Percentage = mean(Percentage))
# # 
# 
# p_test<- ggplot(bpe_england, aes(x = Year_Month, y = Percentage,
#                                  group =Description )) +
#   geom_line(aes(color=Description),
#             linewidth = 1.5)+
#   geom_point(aes(color=Description),
#              size = 3) +
#   expand_limits(y=0) + 
#   geom_text(aes(label = Percentage), vjust=-.5)+
#   theme_classic() + 
#   ##theme(legend.position="bottom") +
#   scale_y_continuous(labels = scales::percent,
#                      limits = c(0, 1.1)) +
#   theme(legend.position="top") +
#   scale_x_date(date_labels = "%b-%Y") +
#   labs(title = "Average % of completed FP17s with BPE scores and average % of FP17s indicating no oral health risk",
#        subtitle = subtitle) +
#   scale_color_manual(values = c("#009E73", "#F0E442"),
#                      label = c("% of FP17 forms indicating no oral health risk with recal intervals <12 months",
#                                "% of all FP17 forms completed with BPE scores data")
#   )
# plot_1 <- data_org %>% group_by(Latest.Region.Description, Year_Month ) %>%
#   summarise(average_percentage_low_risk_recall_interval_less_than_1_year_average_over_practices=
#               formattable::percent (mean (average_percentage_low_risk_recall_interval_less_than_1_year_average_over_practices), digits =  0),
#             average_risk_score_complete_average_over_practices =
#               formattable::percent (mean (average_risk_score_complete_average_over_practices), digits =  0),
#             average_percentage_low_risk_recall_interval_less_than_1_year_average_over_patients_seen =
#               formattable::percent (mean (average_percentage_low_risk_recall_interval_less_than_1_year_average_over_patients_seen), digits =  0),
#             average_percentage_low_risk_score_complete_average_over_patients_seen = 
#               formattable::percent  (mean (average_percentage_low_risk_score_complete_average_over_patients_seen),digits =  0) )
# 
# 
# plot_2 <- plot_1 %>% pivot_longer(cols=c('average_percentage_low_risk_recall_interval_less_than_1_year_average_over_practices', 
#                                          'average_percentage_low_risk_recall_interval_less_than_1_year_average_over_patients_seen',
#                                          'average_risk_score_complete_average_over_practices',
#                                          'average_percentage_low_risk_score_complete_average_over_patients_seen'),
#                                   names_to='Description',
#                                   values_to= 'Percentage')
# 
# plot_2 [plot_2 == 'average_percentage_low_risk_recall_interval_less_than_1_year_average_over_practices'] <-
#   "Average % of completed record with no indication of oral health risk"
# 
# 
# plot_2 [plot_2 == 'average_risk_score_complete_average_over_practices'] <-
#   "Average % of completed risk scores"
# 
# filtered_plot_2_1 = filter(plot_2, Description %in% c("Average % of completed record with no indication of oral health risk",
#                                                       "Average % of completed risk scores"))
# 
# 
# filtered_plot_2_2 = filter(plot_2, Description %in% c("Average % of completed record with no indication of oral health risk"))
# 
# 
# filtered_plot_2_1$Year_Month 
# 
##x <- "202210"
