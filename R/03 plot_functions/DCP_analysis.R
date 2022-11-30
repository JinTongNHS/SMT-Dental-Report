library (tidyverse)
##library(readxl)
library(DBI)
library(odbc)
##library(reactable) #make sure you have the latest version by doing install.packages("reactable")
##library(downloadthis)
##library(scales)
library(formattable)
##library(stringr)


UDA_scheduled_delivery_pull <- function(){
  
  con <- dbConnect(odbc::odbc(), "NCDR")
  sql <- "SELECT * FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[UDA_scheduled]
--where [data_month] = '2022-10-01'"
  
  result <- dbSendQuery (con, sql)
  UDA_deliver_all <- dbFetch(result)
  dbClearResult(result)
  UDA_deliver_all
}


UDA_scheduled_delivery <- UDA_scheduled_delivery_pull () 

UDA_scheduled_delivery <- UDA_scheduled_delivery %>%
  filter(data_month == max(UDA_scheduled_delivery$data_month))

dcp_main <- read.csv("N:/_Everyone/Primary Care Group/SMT_Dental DENT 2022_23-008/DCP_data/DPC_v1_Oct_2022.csv") 


###setwd("N:/_Everyone/Mohammed_Emran/New_indicators_DPC")

delivery_total <-  UDA_scheduled_delivery %>% 
  group_by(data_month) %>%
  dplyr::summarise( total_FP17 = sum(general_FP17s, na.rm = TRUE),
                    total_B1 = sum(UDA_band_1, na.rm = TRUE),
                    total_B2 = sum(UDA_band_2, na.rm = TRUE),
                    total_B3 = sum(UDA_band_3, na.rm = TRUE),
                    total_urgent = sum(UDA_urgent, na.rm = TRUE)) %>%
  mutate (DCP_description = "Total_dentist_only_and_DCP_assisted") %>%
  select (DCP_description, total_FP17,total_B1, total_B2, total_B3, total_urgent)

###write.csv(UDA_scheduled_delivery, "N:/_Everyone/Mohammed_Emran/New_indicators_DPC/delivery_total_october.csv")


###UDA Chart
pull_dcp<- dcp_main %>%
  group_by(DCP_description) %>%
  summarise (total_FP17 = sum(FP17_Current_Year_total, na.rm = TRUE),
             total_B1 = sum(Band_1._UDA, na.rm = TRUE),
             total_B2 = sum(Band_2._UDA, na.rm = TRUE),
             total_B3 = sum(Band_3._UDA, na.rm = TRUE),
             total_urgent = sum(Urgent_UDA, na.rm = TRUE))

dcp_summary <- pull_dcp %>%
  mutate(DCP_description=replace(DCP_description, DCP_description== "Hygienist", "Hygienist_assisted"),
         DCP_description=replace(DCP_description, DCP_description== "Therapist", "Therapist_assisted"),
         DCP_description=replace(DCP_description, DCP_description== "Dental Nurse", "Dental_Nurse_assisted")) %>%
  mutate_if(is.numeric, round, 0)

joined <- rbind(dcp_summary, delivery_total) %>%mutate_if(is.numeric, round, 2)

test_UDA <- group_by(joined, DCP_description) %>%
  mutate (Assisted_FP17_Percentage = 
            formattable::percent (total_FP17 / joined %>% with(total_FP17[DCP_description == 'Total_dentist_only_and_DCP_assisted']), digits=2))%>%
  mutate (Assisted_B1_Percentage = 
            formattable::percent (total_B1 / joined %>% with(total_B1[DCP_description == 'Total_dentist_only_and_DCP_assisted']), digits=2)) %>%
  mutate (Assisted_B2_Percentage = 
            formattable::percent (total_B2  / joined %>% with(total_B2[DCP_description == 'Total_dentist_only_and_DCP_assisted']), digits=2)) %>%
  mutate (Assisted_B3_Percentage = 
            formattable::percent (total_B3  / joined %>% with(total_B3[DCP_description == 'Total_dentist_only_and_DCP_assisted']), digits=2)) %>%
  mutate (Assisted_Urgent_Percentage = 
            formattable::percent (total_urgent   / joined %>% with(total_urgent[DCP_description == 'Total_dentist_only_and_DCP_assisted']), digits=2)) %>%
  select(DCP_description, Assisted_B1_Percentage, Assisted_B2_Percentage, Assisted_B3_Percentage, Assisted_Urgent_Percentage)



data_long_UDA <- pivot_longer(test_UDA, cols = -DCP_description, 
                              names_to = 'UDAs_in_Bands', 
                              values_to = 'UDAs')

filtered_data_UDA <- dplyr::filter(data_long_UDA, DCP_description %in% c("Dental_Nurse_assisted", "Hygienist_assisted", 
                                                                         "Therapist_assisted"))

plot_1 <- filtered_data_UDA %>%  ggplot(aes(fill = UDAs_in_Bands , y = UDAs, x = DCP_description)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = UDAs),
            colour = "black",
            position = position_dodge(width = 1), vjust=-0.25) +
  theme(legend.position="bottom") +
  ##coord_flip() +
  ggtitle("Percentage of Bandwise DCP Assisted UDAs in October 2022") 
# +
#   theme(axis.title.x=element_blank(),
#         axis.text.x=element_blank(),
#         axis.ticks.x=element_blank())
plot_1



test_FP17 <- group_by(joined, DCP_description) %>%
  mutate (Assisted_FP17_Percentage = 
            formattable::percent (total_FP17 / joined %>% with(total_FP17[DCP_description == 'Total_dentist_only_and_DCP_assisted']), digits=2)) %>%
  select(DCP_description, Assisted_FP17_Percentage)



data_long_FP17 <- pivot_longer(test_FP17, cols = -DCP_description, 
                               names_to = 'UDAs_in_Bands', 
                               values_to = 'UDAs')

filtered_data_FP17 <- dplyr::filter(data_long_FP17, DCP_description %in% c("Dental_Nurse_assisted", "Hygienist_assisted", 
                                                                           "Therapist_assisted"))

plot_2 <- filtered_data_FP17 %>%  ggplot(aes(fill = UDAs_in_Bands , y = UDAs, x = DCP_description)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = UDAs),
            colour = "black",
            position = position_dodge(width = 1), vjust=-0.25) +
  theme(legend.position="bottom") +
  ##coord_flip() +
  ggtitle("Percentage of DCP Assisted Course of Treatement in October 2022") 
# +
#   theme(axis.title.x=element_blank(),
#         axis.text.x=element_blank(),
#         axis.ticks.x=element_blank())
plot_2
