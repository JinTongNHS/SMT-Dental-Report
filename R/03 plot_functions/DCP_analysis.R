###############DPC porject

library(tidyverse)
library(data.table)
library(readxl)
library(DBI)
library(odbc)
###library(reactable) #make sure you have the latest version by doing install.packages("reactable")
library(dplyr)
library(ggplot2)
library(scales)
library(ggplot2)
library(magrittr)
getwd()


##### Import Data

dpc_main <- read.csv("N:/_Everyone/Primary Care Group/SMT_Dental DENT 2022_23-008/DCP_data/DPC_v1_Oct_2022.csv") 

colnames(dpc_main)
# Barplot- All with no regions

for_plot_all_t <- dpc_main %>%
  group_by(DCP_description) %>%
  summarise (total_FP17 = sum(FP17_Current_Year_total, na.rm = TRUE),
             total_B1 = sum(Band_1._UDA, na.rm = TRUE),
             total_B2 = sum(Band_2._UDA, na.rm = TRUE),
             total_B3 = sum(Band_3._UDA, na.rm = TRUE),
             total_urgent = sum(Urgent_UDA, na.rm = TRUE))

for_plot_all_t %>% pivot_longer(-DCP_description, names_to = "description", values_to = "value")%>%
  group_by(DCP_description) %>% 
  ggplot(., aes(fill = description, y = value, x = DCP_description)) +
  geom_bar(position = "dodge", stat = "identity")

# ###dpc_main <- read_xlsx("Live E - Country UDA 22-23 monthly data DCPs breakdown.xlsx")

 ####for regional data
 UDA_Projection_data <- function(){

  con <- dbConnect(odbc::odbc(), "NCDR")
  sql <- "/****** Script for SelectTopNRows command from SSMS  ******/
SELECT DISTINCT convert (date, a.[data_month], 105)  as date
      , A.[contract_number]
      , A.[name_or_company_name]
      , A.commissioner_name
		, A.region_name
		, A.[contract_end_date]
     , A.[annual_contracted_UDA]

  FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[UDA_scheduled] A

  WHERE a.[data_month] between '2022-04-01' and '2022-10-01'"

  result <- dbSendQuery (con, sql)
  UDA_Data_pull <- dbFetch(result)
  dbClearResult(result)

  UDA_Data_pull
}


names(dpc_main)[1] <- "contract_number"

colnames(oct_data)
colnames(dpc_main)

oct_data <- UDA_Projection_data ()

dpc_merge_region<- left_join(dpc_main, oct_data, by= "contract_number", all.x=TRUE)

