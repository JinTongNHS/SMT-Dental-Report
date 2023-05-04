# plot_BPE_no_oral_health_risk <- function(data = BPE_data,
#                                          level = "National",
#                                          region_STP_name = NULL,
#                                          ICB_lookup = STP_ICB_lookup_codes){
#   
#   data[is.na(data)] <- 0
#   
#   #join in standardised ICB column
#   ICB_lookup <- ICB_lookup %>%
#     select(commissioner_name = commissioner_name_ICB,
#            Latest_Commissioner_Code = commissioner_ONS_boundary_code_ICB)
#   
#   data <- data %>%
#     left_join(ICB_lookup, by = "Latest_Commissioner_Code")
#   
#   #filter for STP or region
#   if(level == "Regional"){
#     data <- data %>%
#       filter(Latest.Region.Description == region_STP_name )
#     subtitle <- region_STP_name
#   }else if(level == "STP"){
#     data <- data %>%
#       filter(commissioner_name == region_STP_name)
#     subtitle <- region_STP_name
#   }else{
#     subtitle <- "England"
#   }
#   
#   ###filtering and selecting the right columns
#   ########str(data)
#   
#   ##data <- BPE_data ###need to comment out
#   
#   ##colnames((data_org))
#   
#   data_org <- data %>%
#     group_by(Year_Month) %>%
#     summarise(Nforms = sum(Total.Form.Count),
#               Ncompletedforms = sum(Forms_with_Highest_BPE_Sextant_Score)) %>% 
#     mutate (percentcompleteform = formattable::percent(Ncompletedforms/ Nforms, digits =0))
#   
#   
#   data_org_2 <- data %>% 
#     rename("Total_Form_Count_Highest_BPE_Sextant_Score_0_or_1_and_UDT_0_and_RRI_less_than_1_year" =
#              "Total_Form_Count_Highest_BPE_Sextant_Score_0_or_1_and_UDT_0_and_RRI_<1_year") %>% 
#              group_by(Year_Month) %>%
#     filter(Highest.BPE.Sextant.Score != 'No BPE Assessment Possible') %>% 
#     summarise (complete_forms = sum(Forms_with_Highest_BPE_Sextant_Score), 
#                nlow_risk = sum (as.numeric(Total_Form_Count_Highest_BPE_Sextant_Score_0_or_1_and_0_UDT), na.rm = TRUE),
#                low_risk_less1year = sum(as.numeric(Total_Form_Count_Highest_BPE_Sextant_Score_0_or_1_and_UDT_0_and_RRI_less_than_1_year), na.rm = TRUE)) %>%  
#     mutate (percent_low_risk_whic_are1_year = formattable::percent (low_risk_less1year/nlow_risk, digits =0)) %>% 
#     mutate (percentlowrisk= formattable::percent (nlow_risk/ complete_forms, digits =0) )
#     
# 
#   year_x <- substr(data_org$Year_Month, 1, 4)
#   month_x <- substr(data_org$Year_Month, 5, 6)
#   data_org$Year_Month <- as.Date(paste0(year_x, "-", month_x, "-01"))
#   
#   
#   year_y <- substr(data_org_2$Year_Month, 1, 4)
#   month_y <- substr(data_org_2$Year_Month, 5, 6)
#   data_org_2$Year_Month <- as.Date(paste0(year_y, "-", month_x, "-01"))
#   
#   
#   colnames (bpe_england)
#   
#   bpe_plot <-data_org %>%  left_join(data_org_2, by = c("Year_Month"= "Year_Month")) %>% 
#     select (Year_Month, percentcompleteform, percent_low_risk_whic_are1_year) ##, percentlowrisk)
#   
#   bpe_england <- bpe_plot %>% pivot_longer(cols = c ("percentcompleteform", "percent_low_risk_whic_are1_year"),
#                                      names_to='Description', values_to= 'Percentage')
#   # 
#   # bpe_england <- filtered_plot_2_1 %>%  group_by(Year_Month, Description) %>% 
#   #   summarise(Percentage = mean(Percentage))
#   # 
#   
#   p_test<- ggplot(bpe_england, aes(x = Year_Month, y = Percentage,
#                                    group =Description )) +
#     geom_line(aes(color=Description),
#               linewidth = 1.5)+
#     geom_point(aes(color=Description),
#                size = 3) +
#     expand_limits(y=0) + 
#     geom_text(aes(label = Percentage), vjust=-.5)+
#     theme_classic() + 
#     ##theme(legend.position="bottom") +
#     scale_y_continuous(labels = scales::percent,
#                        limits = c(0, 1.1)) +
#     theme(legend.position="top") +
#     scale_x_date(date_labels = "%b-%Y") +
#     labs(title = "Average % of completed FP17s with BPE scores and average % of FP17s indicating no oral health risk",
#     subtitle = subtitle) +
#     scale_color_manual(values = c("#009E73", "#F0E442"),
#                       label = c("% of FP17 forms indicating no oral health risk with recal intervals <12 months",
#                                 "% of all FP17 forms completed with BPE scores data")
#                       )
#   p_test
#   
#   
# }
# 
# 
# # ##### plot the bar graph
# # 
# # plot_1 <- data_org %>% group_by(Latest.Region.Description, Year_Month ) %>%
# #   summarise(average_percentage_low_risk_recall_interval_less_than_1_year_average_over_practices=
# #               formattable::percent (mean (average_percentage_low_risk_recall_interval_less_than_1_year_average_over_practices), digits =  0),
# #             average_risk_score_complete_average_over_practices =
# #               formattable::percent (mean (average_risk_score_complete_average_over_practices), digits =  0),
# #             average_percentage_low_risk_recall_interval_less_than_1_year_average_over_patients_seen =
# #               formattable::percent (mean (average_percentage_low_risk_recall_interval_less_than_1_year_average_over_patients_seen), digits =  0),
# #             average_percentage_low_risk_score_complete_average_over_patients_seen = 
# #               formattable::percent  (mean (average_percentage_low_risk_score_complete_average_over_patients_seen),digits =  0) )
# # 
# # 
# # plot_2 <- plot_1 %>% pivot_longer(cols=c('average_percentage_low_risk_recall_interval_less_than_1_year_average_over_practices', 
# #                                          'average_percentage_low_risk_recall_interval_less_than_1_year_average_over_patients_seen',
# #                                          'average_risk_score_complete_average_over_practices',
# #                                          'average_percentage_low_risk_score_complete_average_over_patients_seen'),
# #                                   names_to='Description',
# #                                   values_to= 'Percentage')
# # 
# # plot_2 [plot_2 == 'average_percentage_low_risk_recall_interval_less_than_1_year_average_over_practices'] <-
# #   "Average % of completed record with no indication of oral health risk"
# # 
# # 
# # plot_2 [plot_2 == 'average_risk_score_complete_average_over_practices'] <-
# #   "Average % of completed risk scores"
# # 
# # filtered_plot_2_1 = filter(plot_2, Description %in% c("Average % of completed record with no indication of oral health risk",
# #                                                       "Average % of completed risk scores"))
# # 
# # 
# # filtered_plot_2_2 = filter(plot_2, Description %in% c("Average % of completed record with no indication of oral health risk"))
# # 
# # 
# # filtered_plot_2_1$Year_Month 
# # 
# ##x <- "202210"