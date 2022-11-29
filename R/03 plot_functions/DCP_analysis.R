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

data_long_UDA <- pivot_longer(test_1, cols = -DCP_description, 
                          names_to = 'Total_UDA_Each_Band', 
                          values_to = 'UDAs') %>% 
  mutate_if(is.numeric, round, 0)

# 
# plot_test_1 <- group_by(data_long, DCP_description) %>% 
#   mutate(percent = formattable::percent (UDAs/sum(UDAs)))
  
plot_test_1 <- group_by(data_long_UDA, DCP_description) %>% 
  mutate(percent = formattable::percent (UDAs/sum(UDAs), digits = 0))
 
# 
# d_test <-
#   plot_test_1 %>%
#   mutate(new_o = " (",
#          new_c = ")",
#          label_n = paste (UDAs, new_o, percent, new_c, sep = ''))

plot_1 <- plot_test_1 %>%  ggplot(aes(fill = Total_UDA_Each_Band , y = UDAs, x = DCP_description)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(UDAs, "\n", percent)),
            colour = "black",
            position = position_dodge(width = .9)) +
  theme(legend.position="bottom") +
  ##coord_flip() +
  ggtitle("UDAs Delivered by DCPs in October 2022") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
plot_1

###FP17 Chart

FP17_plot_all_t <- dcp_main %>%
  group_by(DCP_description) %>%
  summarise (Total_FP17 = sum(FP17_Current_Year_total, na.rm = TRUE)) %>%  
  mutate(percent_FP = formattable::percent (Total_FP17/sum(Total_FP17), digits = 0))

###select (DCP_description, total_FP17) 

plot_2 <- FP17_plot_all_t %>% 
  ggplot(., aes(fill = Total_FP17 , y = percent_FP, x = DCP_description)) +
  geom_bar(position = "dodge", stat = "identity", fill = "grey") +
  ##geom_text (aes(label = total_percent), position = position_dodge(width = 1)) +
  geom_text(aes(label = paste0(Total_FP17, "\n", percent_FP)),
            colour = "black",
            position = position_dodge(width = .9)) +
##  coord_flip() +
  ggtitle("FP17 Delivered by DCPs in October 2022") +
  theme(legend.position="middle") +
  theme(axis.title.y.left =element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.y = element_blank())
plot_2

# 
# combined_plot <- ggarrange(plot_1,
#                            plot_2,
#                            nrow = 1,
#                            ncol = 2) #nrow & ncol depend on how you want to 
# 
# combined_plot
