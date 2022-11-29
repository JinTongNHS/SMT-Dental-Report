###############DPC porject

library(tidyverse)
library(ggpubr)

##### Import Data

dpc_main <- read.csv("N:/_Everyone/Primary Care Group/SMT_Dental DENT 2022_23-008/DCP_data/DPC_v1_Oct_2022.csv") 


###UDA Chart
UDA_plot_all_t <- dpc_main %>%
  group_by(DCP_description) %>%
  summarise (c_total_B1 = sum(Band_1._UDA, na.rm = TRUE),
             c_total_B2 = sum(Band_2._UDA, na.rm = TRUE),
             c_total_B3 = sum(Band_3._UDA, na.rm = TRUE),
             c_total_urgent = sum(Urgent_UDA, na.rm = TRUE)) %>%
             mutate (ratio_total_B1 = c_total_B1 / (c_total_B1 + c_total_B2 + c_total_B3 +c_total_urgent),
                    label = percent(ratio_total_B1 %>% round(2)), 
                    new_o = " (",
                    new_c = ")",
                    total_B1 = paste (c_total_B1, new_o, label, new_c, sep = '')) %>%
  
            mutate (ratio_total_B2 = c_total_B2 / (c_total_B1 + c_total_B2 + c_total_B3 + c_total_urgent),
            label_2 = percent(ratio_total_B2 %>% round(2)), 
            new_o_2 = " (",
            new_c_2 = ")",
            total_B2 = paste (c_total_B2, new_o_2, label_2, new_c_2, sep = ''))%>%
  
  
          mutate (ratio_total_B3 = c_total_B3 / (c_total_B1 + c_total_B2 + c_total_B3 +c_total_urgent),
          label_3 = percent(ratio_total_B3 %>% round(2)), 
          new_o_3 = " (",
          new_c_3 = ")",
          total_B3 = paste (c_total_B3, new_o_3, label_3, new_c_3, sep = '')) %>%
    
    mutate (ratio_total_urgent = c_total_urgent / (c_total_B1 + c_total_B2 + c_total_B3 +c_total_urgent),
          label_urgent = percent(ratio_total_urgent %>% round(2)), 
          new_o_urgent = " (",
          new_c_urgent = ")",
          total_urgent = paste (c_total_urgent, new_o_urgent, label_urgent, new_c_urgent, sep = '')) %>%
    select (DCP_description, total_B1, total_B2, total_B3, total_urgent )

###FP17 Chart
FP17_plot_all_t <- dpc_main %>%
  group_by(DCP_description) %>%
  summarise (c_total_FP17 = sum(FP17_Current_Year_total, na.rm = TRUE)) %>%
  mutate (FP17_percent = sum(c_total_FP17), 
          ratio_FP17 = c_total_FP17/FP17_percent, 
          label_FP17 = percent(ratio_FP17 %>% round(2)), 
          new_o_FP17 = " (",
          new_c_FP17 = ")",
          total_FP17 = paste (c_total_FP17, new_o_FP17, label_FP17, new_c_FP17, sep = '')) %>%
          select (DCP_description, total_FP17) 



plot_1<- UDA_plot_all_t %>% pivot_longer(-DCP_description, names_to = "UDAs", values_to = "total_percent_each_DCP")%>%
  group_by(DCP_description) %>% 
  ggplot(., aes(fill = UDAs, y = reorder (UDAs, desc(total_percent_each_DCP)), x = DCP_description)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text (aes(label = total_percent_each_DCP), position = position_dodge(width = .8)) +
  coord_flip() +
  ggtitle("UDAs Delivered by DCPs in October 2022") + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


plot_2 <- FP17_plot_all_t %>% pivot_longer(-DCP_description, names_to = "FP17", values_to = "total_percent")%>%
  group_by(DCP_description) %>% 
  ggplot(., aes(fill = "FP17" , y = total_percent, x = DCP_description)) +
  geom_bar(position = "dodge", stat = "identity", fill = "grey") +
  geom_text (aes(label = total_percent), position = position_dodge(width = .8)) +
  coord_flip() +
  ggtitle("FP17 Delivered by DCPs in October 2022") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y = element_blank())

combined_plot <- ggarrange(plot_1,
                           plot_2,
                           nrow = 1,
                           ncol = 2) #nrow & ncol depend on how you want to 

combined_plot
