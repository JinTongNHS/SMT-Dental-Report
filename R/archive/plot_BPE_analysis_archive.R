## create table in SQL Server
library(readxl)
library(DBI)
library(odbc)
library (tidyverse)
library(formattable)


#
con <- dbConnect(odbc::odbc(), "NCDR")

bpe_data_pull <- function(){

  con <- dbConnect(odbc::odbc(), "NCDR")
  sql <- "SELECT * FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[BPE]"
  result <- dbSendQuery (con, sql)
  bpe_all <- dbFetch(result)
  dbClearResult(result)
  bpe_all
}



data_org_main <- bpe_data_pull()

###setwd("N:/_Everyone/Mohammed_Emran/BPE_sexton/BSA_Data")
##bpe_data_upload <- read_excel ("BPE_Sextant_Score_Untreated_Decayed_Teeth_and_RRIv2ALL_England.xlsx")
###replace all NA with 0
data_org_main[is.na(data_org_main)] <- 0

###filtering and selecting the right columns
########str(data_org_main)

data_org <- data_org_main %>%
  group_by(Latest.Region.Description) %>%
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

p1<- ggplot(filtered_plot_2_1, aes(x = Latest.Region.Description, 
                                   y = Percentage, fill= Description)) +
  geom_bar(stat="identity", position = "dodge") +
  ###facet_grid(cols = vars(Year_Month), labeller = label_value) +
  geom_text(aes(label = Percentage), 
            check_overlap = TRUE,
            colour = "black", size= 3.5,
            position = position_dodge(width = 1), vjust= -0.5) +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position="top") +
  labs(title = "Average % of completed FP17s with BPE scores and average % of FP17s indicating no oral health risk") +
  scale_fill_manual(values = c("#009E73", "#F0E442"),
                    label = c("Average % of completed FP17s indicating no oral health risk", 
                              "% of all FP17s completed with BPE scores"))
p1

