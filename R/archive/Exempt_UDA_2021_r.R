setwd("N:/_Everyone/Mohammed_Emran/Exempt_patients_data")


######Connecting to SQL

library (tidyverse)
library(readxl)
library(DBI)
library(odbc)
library(reactable) #make sure you have the latest version by doing install.packages("reactable")
library(downloadthis)
library(dplyr)
library(ggplot2)
library(scales)
library(ggplot2)
library(magrittr)

pull_exempt_data_2021 <- function(){
  
  con <- dbConnect(odbc::odbc(), "NCDR")
  sql <- "SELECT [Treatment Charge Band] as Treatment_Charge_Band 
      ,[Regions]
      ,[FY]
      ,[Child]
      ,[Adult-Exempt] as adult_exempt
      ,[Adult Non-Exempt] as adult_non_exempt
   FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[GEM\\MEmran].[Dental.[Exempt_UDA_2021]" 
  ##(problem with / solved by adding \\)
  
  result <- dbSendQuery (con, sql)
  exempt_data_2021 <- dbFetch(result)
  dbClearResult(result)
  
  exempt_data_2021
}


exeempt_data_2021 <- pull_exempt_data_2021 ()


data_long <- gather(exeempt_data_2021, patient_type, number_uda,
                    Child:adult_non_exempt, 
                    factor_key=TRUE)


###to remove comma's from data column
data_long <- data_long %>%
  mutate(number_uda = str_replace(number_uda, ",", "")) %>%
  mutate(number_uda = str_replace(number_uda, ",", "")) %>%
  mutate(number_uda = as.numeric(number_uda))


##colnames(data_long) ##] "Treatment_Charge_Band" "Regions", "FY", "patient_type",
                  ###"number_uda"

##filtering all national

exempt_data_2021_process_england <- data_long %>% 
                filter(Regions == "England") %>%
                group_by(Treatment_Charge_Band, patient_type) %>%
                summarise(number_uda)

plot_exempt_Engliand <- ggplot(data = exempt_data_2021_process_england, aes (Treatment_Charge_Band, number_uda, group = patient_type )) +
  geom_col(aes(fill = patient_type)) +
  geom_text(aes(label = number_uda), position = position_stack(vjust = 0.3))  + 
  theme(legend.position="top")  + ## + scale_y_continuous(breaks = seq(0, 78000000, by = 50000))
  ggtitle("Patient Charge Status by Treatment Charge Band - UDA Delivered - England (2020-21)")


plot_exempt_Engliand

##Barplot- London

exempt_data_2021_process_london <- data_long %>% 
  filter(Regions == "London") %>%
  group_by(Treatment_Charge_Band, patient_type) %>%
  summarise(number_uda)

plot_exempt_london <- ggplot(data = exempt_data_2021_process_london, aes (Treatment_Charge_Band, number_uda, group = patient_type )) +
  geom_col(aes(fill = patient_type)) +
  geom_text(aes(label = number_uda), position = position_stack(vjust = 0.3))  + 
  theme(legend.position="top") + scale_y_continuous(breaks = seq(0, 78000000, by = 250000)) +
  ggtitle("Patient Charge Status by Treatment Charge Band - UDA Delivered - London (2020-21)")

plot_exempt_london



##Barplot- South West

exempt_data_2021_process_South_West <- data_long %>% 
  filter(Regions == "South West") %>%
  group_by(Treatment_Charge_Band, patient_type) %>%
  summarise(number_uda)

plot_exempt_South_West <- ggplot(data = exempt_data_2021_process_South_West, 
                                 aes (Treatment_Charge_Band, number_uda, group = patient_type )) +
  geom_col(aes(fill = patient_type)) +
  geom_text(aes(label = number_uda), position = position_stack(vjust = 0.3))  + 
  theme(legend.position="top") + ### + scale_y_continuous(breaks = seq(0, 78000000, by = 500000)) +
  ggtitle("Patient Charge Status by Treatment Charge Band - UDA Delivered - South West (2020-21)")


plot_exempt_South_West



##Barplot- South East

exempt_data_2021_process_South_East <- data_long %>% 
  filter(Regions == "South East") %>%
  group_by(Treatment_Charge_Band, patient_type) %>%
  summarise(number_uda)

plot_exempt_South_East <- ggplot(data = exempt_data_2021_process_South_East, aes (Treatment_Charge_Band, number_uda, group = patient_type )) +
  geom_col(aes(fill = patient_type)) +
  geom_text(aes(label = number_uda), position = position_stack(vjust = 0.3))  + 
  theme(legend.position="top") + ##+ scale_y_continuous(breaks = seq(0, 78000000, by = 500000)) +
  ggtitle("Patient Charge Status by Treatment Charge Band - UDA Delivered - South East (2020-21)")


plot_exempt_South_East



##Barplot- Midlands

exempt_data_2021_process_Midlands <- data_long %>% 
  filter(Regions == "Midlands") %>%
  group_by(Treatment_Charge_Band, patient_type) %>%
  summarise(number_uda)

plot_exempt_Midlands <- ggplot(data = exempt_data_2021_process_Midlands, aes (Treatment_Charge_Band, number_uda, group = patient_type )) +
  geom_col(aes(fill = patient_type)) +
  geom_text(aes(label = number_uda), position = position_stack(vjust = 0.3))  + 
  theme(legend.position="top") + scale_y_continuous(breaks = seq(0, 78000000, by = 500000)) +
  ggtitle("Patient Charge Status by Treatment Charge Band - UDA Delivered - Midlands (2020-21)")



plot_exempt_Midlands




##Barplot- East of England

exempt_data_2021_process_East_of_England <- data_long %>% 
  filter(Regions == "East of England") %>%
  group_by(Treatment_Charge_Band, patient_type) %>%
  summarise(number_uda)

plot_exempt_East_of_England <- ggplot(data = exempt_data_2021_process_East_of_England, aes (Treatment_Charge_Band, number_uda, group = patient_type )) +
  geom_col(aes(fill = patient_type)) +
  geom_text(aes(label = number_uda), position = position_stack(vjust = 0.3))  + 
  theme(legend.position="top") + scale_y_continuous(breaks = seq(0, 78000000, by = 250000)) +
  ggtitle("Patient Charge Status by Treatment Charge Band - UDA Delivered - East of England (2020-21)")


plot_exempt_East_of_England



##Barplot- North West

exempt_data_2021_process_North_West <- data_long %>% 
  filter(Regions == "North West") %>%
  group_by(Treatment_Charge_Band, patient_type) %>%
  summarise(number_uda)

plot_exempt_North_West <- ggplot(data = exempt_data_2021_process_North_West, aes (Treatment_Charge_Band, number_uda, group = patient_type )) +
  geom_col(aes(fill = patient_type)) +
  geom_text(aes(label = number_uda), position = position_stack(vjust = 0.3))  + 
  theme(legend.position="top") + scale_y_continuous(breaks = seq(0, 78000000, by = 250000)) +
  ggtitle("Patient Charge Status by Treatment Charge Band - UDA Delivered - North West (2020-21)")



plot_exempt_North_West



##Barplot- North East and Yorkshire

exempt_data_2021_process_North_East_and_Yorkshire <- data_long %>% 
  filter(Regions == "North East and Yorkshire") %>%
  group_by(Treatment_Charge_Band, patient_type) %>%
  summarise(number_uda)

plot_exempt_North_East_and_Yorkshire <- ggplot(data = exempt_data_2021_process_North_East_and_Yorkshire, aes (Treatment_Charge_Band, number_uda, group = patient_type )) +
  geom_col(aes(fill = patient_type)) +
  geom_text(aes(label = number_uda), position = position_stack(vjust = 0.3))  + 
  theme(legend.position="top") + scale_y_continuous(breaks = seq(0, 78000000, by = 500000)) +
  ggtitle("Patient Charge Status by Treatment Charge Band - UDA Delivered - North East and Yorkshire (2020-21)")


plot_exempt_North_East_and_Yorkshire

