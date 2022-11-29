###############DPC porject

library(tidyverse)
library(ggpubr)
library("scales")
library(formattable)

##### Import Data

dcp_main <- read.csv("N:/_Everyone/Primary Care Group/SMT_Dental DENT 2022_23-008/DCP_data/DPC_v1_Oct_2022.csv") 


###UDA Chart
test_1 <- dcp_main %>%
  group_by(DCP_description, ) %>%
  summarise (B1 = sum(Band_1._UDA, na.rm = TRUE),
             B2 = sum(Band_2._UDA, na.rm = TRUE),
             B3 = sum(Band_3._UDA, na.rm = TRUE),
             urgent = sum(Urgent_UDA, na.rm = TRUE))


data_long <- pivot_longer(test_1, cols = -DCP_description, 
                          names_to = 'Total_UDA_Each_Band', 
                          values_to = 'UDAs')
plot_test_1 <- group_by(data_long, DCP_description) %>% 
  mutate(percent = formattable::percent (UDAs/sum(UDAs)))

d_test <-
  plot_test_1 %>%
  mutate(new_o = " (",
         new_c = ")",
         label_n = paste (UDAs, new_o, percent, new_c, sep = ''))

plot_1 <- d_test %>%  ggplot(aes(fill = Total_UDA_Each_Band , y = UDAs, x = DCP_description)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = label_n), ###paste0(UDAs, sep="~", percent)),
            colour = "black",
            position = position_dodge(width = .9)) +
  theme(legend.position="bottom") +
  coord_flip() +
  ggtitle("UDAs Delivered by DCPs in October 2022") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


###FP17 Chart
FP17_plot_all_t <- dcp_main %>%
  group_by(DCP_description) %>%
  summarise (c_total_FP17 = sum(FP17_Current_Year_total, na.rm = TRUE)) %>%
  mutate (FP17_percent = sum(c_total_FP17), 
          ratio_FP17 = c_total_FP17/FP17_percent, 
          label_FP17 = percent(ratio_FP17 %>% round(2)), 
          new_o_FP17 = " (",
          new_c_FP17 = ")",
          total_FP17 = paste (c_total_FP17, new_o_FP17, label_FP17, new_c_FP17, sep = '')) %>%
  select (DCP_description, total_FP17) 

plot_2 <- FP17_plot_all_t %>% pivot_longer(-DCP_description, names_to = "FP17", values_to = "total_percent")%>%
  group_by(DCP_description) %>% 
  ggplot(., aes(fill = "FP17" , y = total_percent, x = DCP_description)) +
  geom_bar(position = "dodge", stat = "identity", fill = "grey") +
  geom_text (aes(label = total_percent), position = position_dodge(width = .8)) +
  coord_flip() +
  ggtitle("FP17 Delivered by DCPs in October 2022") +
  theme(legend.position="bottom") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y = element_blank())

<<<<<<< HEAD
combined_plot <- ggarrange(plot_1,
                           plot_2,
                           nrow = 1,
                           ncol = 2) #nrow & ncol depend on how you want to 

combined_plot
=======
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

>>>>>>> 712fcf909cdd940e7477e5d3d11807e5e6273a91
