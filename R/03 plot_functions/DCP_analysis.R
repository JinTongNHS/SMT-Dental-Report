# 
# getwd()
# setwd("N:/_Everyone/Mohammed_Emran/New_indicators_DPC/DPC")
library (tidyverse)
##library(readxl)
library(DBI)
library(odbc)
##library(scales)
library(formattable)
##library(stringr)

###UDA data

UDA_scheduled_data_pull <- function(){
  
  con <- dbConnect(odbc::odbc(), "NCDR")
  sql <- "SELECT * FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[UDA_scheduled]"
  
  result <- dbSendQuery (con, sql)
  UDA_deliver_all <- dbFetch(result)
  dbClearResult(result)
  UDA_deliver_all
}

UDA_scheduled_data <- UDA_scheduled_data_pull () %>%
  filter(data_month >= as.Date("2022-10-01")) %>%
  rename (Month = data_month)
###filter(data_month == max(UDA_scheduled_data$data_month))

###DCP data

dcp_data_pull <- function(){
  con <- dbConnect(odbc::odbc(), "NCDR")
  sql <- "SELECT * FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[DCP]"
  result <- dbSendQuery (con, sql)
  DCP_all <- dbFetch(result)
  dbClearResult(result)
  DCP_all
}

dcp_main_new <- dcp_data_pull () %>%
  rename (Contract_Number = "Contract Number",
          Contract.Type = "Contract Type",
          Name.or.Company.Name = "Name or Company Name", commissioner_name = "Commissioner Name", 
          Paid.by.BSA ="Paid by BSA", Contract.Start.Date = "Contract Start Date", 
          Contract.End.Date = "Contract End Date",  DCP_description = "DCP Description",
          UDA_Delivered_Current_Year =  "@{pv_mth} UDA Delivered Current Year",               
          FP17_Current_Year_total = "@{pv_mth} General FP17s Current Year", 
          UDA_Delivered_Previous.Year_NN = "@{pv_mth} UDA Delivered Previous Year", 
          General.FP17s.Previous.Year_NN = "@{pv_mth} General FP17s Previous Year", 
          Band_1._UDA = "UDA - Band 1 @{pv_mth} (Current Year)",          
          Band_2._UDA = "UDA - Band 2 @{pv_mth} (Current Year)" ,        
          Band_3._UDA = "UDA - Band 3 @{pv_mth} (Current Year)",
          Urgent_UDA = "UDA - Urgent @{pv_mth} (Current Year)",
          other_UDA = "UDA - Other (General) @{pv_mth} (Current Year)", 
          FP17_Current_Year_B1= "FP17s - Band 1 @{pv_mth} (Current Year)",       
          FP17_Current_Year_B2 = "FP17s - Band 2 @{pv_mth} (Current Year)",       
          FP17_Current_Year_B3 = "FP17s - Band 3 @{pv_mth} (Current Year)",       
          FP17_Current_Year_urgent = "FP17s - Band Urgent @{pv_mth} (Current Year)",
          FP17_Current_Year_other= "FP17s - Band Other @{pv_mth} (Current Year)")

###colnames(dcp_main_new)
###dcp_main <- read.csv("N:/_Everyone/Primary Care Group/SMT_Dental DENT 2022_23-008/DCP_data/DPC_v1_Oct_2022.csv") 


delivery_total <-  UDA_scheduled_data %>% 
  group_by(Month) %>%
  dplyr::summarise( total_FP17 = sum(general_FP17s, na.rm = TRUE),
                    total_B1 = sum(UDA_band_1, na.rm = TRUE),
                    total_B2 = sum(UDA_band_2, na.rm = TRUE),
                    total_B3 = sum(UDA_band_3, na.rm = TRUE),
                    total_urgent = sum(UDA_urgent, na.rm = TRUE)) %>%
  mutate (DCP_description = "Total_dentist_only_and_DCP_assisted") %>%
  select (Month, DCP_description, total_FP17,total_B1, total_B2, total_B3, total_urgent)

###write.csv(UDA_scheduled_data, "N:/_Everyone/Mohammed_Emran/New_indicators_DPC/delivery_total_october.csv")
###UDA Chart
pull_dcp<- dcp_main_new %>%
  group_by(Month, DCP_description) %>%
  summarise (total_FP17 = sum(FP17_Current_Year_total, na.rm = TRUE),
             total_B1 = sum(Band_1._UDA, na.rm = TRUE),
             total_B2 = sum(Band_2._UDA, na.rm = TRUE),
             total_B3 = sum(Band_3._UDA, na.rm = TRUE),
             total_urgent = sum(Urgent_UDA, na.rm = TRUE))

dcp_summary <- pull_dcp %>%
  mutate(DCP_description=replace(DCP_description, DCP_description== "Hygienist", "Hygienist_assisted"),
         DCP_description=replace(DCP_description, DCP_description== "Therapist", "Therapist_assisted"),
         DCP_description=replace(DCP_description, DCP_description== "Dental Nurse", "Dental_Nurse_assisted"),
         DCP_description=replace(DCP_description, DCP_description== "Clinical Technician", "Clinical_Technician_assisted")) %>%
  mutate_if(is.numeric, round, 0)


##colnames(dcp_summary_longer)
##colnames (delivery_total_longer)
dcp_summary_longer <- dcp_summary %>% pivot_longer (
  cols = starts_with("total"),
  names_to = "Bands",
  names_prefix = "dcp",
  values_to = "numbers",
  values_drop_na = TRUE
) 

delivery_total_longer <- delivery_total %>% pivot_longer(
  cols = starts_with("total"),
  names_to = "Bands",
  names_prefix = "dcp",
  values_to = "all_numbers",
  values_drop_na = TRUE
)

all_lookup <- left_join(dcp_summary_longer, delivery_total_longer, by = 
                          c("Month"="Month", "Bands"="Bands"))
total<- all_lookup %>% 
  mutate (asissted_percent = formattable::percent (numbers / all_numbers, digits=2))

filtered_data_UDA = filter(total, Bands %in% c("total_B1",	"total_B2",	"total_B3", "total_urgent")) %>%
  select(Month, "DCP_description.x", "Bands", "asissted_percent") %>%
  rename (DCP_description = DCP_description.x) %>%
  mutate(Month = strftime(Month, "%B-%y"))

UDA_plot <- ggplot(filtered_data_UDA, aes(x=DCP_description, y= asissted_percent, fill= Bands)) +
  geom_bar(stat="identity") +
  facet_grid(cols = vars(Month), labeller = label_value) +
  theme_minimal()  +
  geom_text(aes(label= asissted_percent),hjust=0.5, 
            size=3.5,check_overlap = TRUE,
            position = position_stack(vjust = 0.6)) +
  ##theme(axis.text.x = element_text(angle = -90)) +
  scale_fill_manual(values = c("#009E73", "#F0E442", "#D55E00", "#CC79A7"),
                    labels = c("Band 1", "Band 2", "Band 3", "Urgent")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = -90))+
  labs(title = "Percentage of total UDAs delivered which had DCP* assistance by band",
       ## subtitle = paste0(subtitle, ": ",format(data_month, "%b-%Y")),
       x = "DCP description",
       y = "Percentage of total UDAs delivered",
       fill = "Band",
       caption = "*Dental Care Practitioner")

UDA_plot

filtered_data_FP17 = filter(total, Bands %in% c("total_FP17")) %>% 
  select("Month", "DCP_description.x", "Bands", "asissted_percent") %>%
  rename ( Percent_of_FP17 = Bands, DCP_description = DCP_description.x) %>%
  mutate(Month = strftime(Month, "%B-%y"))


# filtered_data_FP17 = filter(total, Bands %in% c("total_B1",	"total_B2",	"total_B3", "total_urgent")) %>%
#   select(Month, "DCP_description.x", "Bands", "asissted_percent") %>%
#   rename (DCP_description = DCP_description.x) %>%


FP17_plot <- ggplot(filtered_data_FP17, aes(x=DCP_description, y= asissted_percent)) +
  geom_bar(stat="identity", position = "dodge", fill = "steelblue") +
  facet_grid(cols = vars(Month), labeller = label_value) +
  geom_text(aes(label = asissted_percent), 
            colour = "black", size= 3.5,
            position = position_dodge(width = 1), vjust=-0.25) +
  ##scale_y_continuous(labels = scales::percent) +
  theme(legend.position="bottom") +
  theme_bw() + theme(axis.text.x = element_text(angle = -90)) +
  labs(title = "Percentage of total Courses of Treatment (CoTs) delivered which had DCP* assistance",
       ##subtitle = paste0(subtitle, ": ", format(data_month, "%b-%Y")),
       x = "DCP description",
       y = "Percentage of total CoTs delivered",
       caption = "*Dental Care Practitioner")

FP17_plot
UDA_plot