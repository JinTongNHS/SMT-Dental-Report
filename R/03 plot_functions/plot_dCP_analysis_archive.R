#DCP_data_October22 <- read.csv("N:/_Everyone/Primary Care Group/SMT_Dental DENT 2022_23-008/DCP_data/DPC_v1_Oct_2022.csv") 
#DCP_data <- pull_dcp_data()

plot_DCP_analysis <- function(data = UDA_scheduled_data,
                              dcp_data = DCP_data,
                              UDA_or_FP17 = "UDA",
                              level = "National",
                              region_STP_name = NULL){
  
  
  #filter for region or STP
  if(level == "Regional"){
    data <- data %>% 
      filter(region_name == region_STP_name )
    
    dcp_data <- dcp_data %>%
      filter(Region == region_STP_name)
    
    subtitle <- region_STP_name 
  }else if(level == "STP"){
    data <- data %>% 
      filter(commissioner_name == region_STP_name)
    
    dcp_data <- dcp_data %>%
      filter(commissioner_name == region_STP_name)
    
    subtitle <- region_STP_name
  }else{
    subtitle <- "England"
  }
  
  delivery_total <-  data %>% 
    group_by(month) %>%
    dplyr::summarise( total_FP17 = sum(general_FP17s, na.rm = TRUE),
                      total_B1 = sum(UDA_band_1, na.rm = TRUE),
                      total_B2 = sum(UDA_band_2, na.rm = TRUE),
                      total_B3 = sum(UDA_band_3, na.rm = TRUE),
                      total_urgent = sum(UDA_urgent, na.rm = TRUE)) %>%
    mutate(DCP_description = "Total_dentist_only_and_DCP_assisted") %>%
    #select(DCP_description, total_FP17,total_B1, total_B2, total_B3, total_urgent)
    select(Month = month, DCP_description, total_FP17,total_B1, total_B2, total_B3, total_urgent)
  
  pull_dcp <- dcp_data %>%
    group_by(Month, DCP_description) %>%
    summarise(total_FP17 = sum(FP17_Current_Year_total, na.rm = TRUE),
              total_B1 = sum(Band_1._UDA, na.rm = TRUE),
              total_B2 = sum(Band_2._UDA, na.rm = TRUE),
              total_B3 = sum(Band_3._UDA, na.rm = TRUE),
              total_urgent = sum(Urgent_UDA, na.rm = TRUE))
  
  dcp_summary <- pull_dcp %>%
    mutate(DCP_description = replace(DCP_description, DCP_description == "Hygienist", "Hygienist assisted"),
           DCP_description = replace(DCP_description, DCP_description == "Therapist", "Therapist assisted"),
           DCP_description = replace(DCP_description, DCP_description == "Dental Nurse", "Dental Nurse assisted"),
           DCP_description=replace(DCP_description, DCP_description== "Clinical Technician", "Clinical Technician assisted")) %>%
    mutate_if(is.numeric, round, 0)
  
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
  
  
  if(UDA_or_FP17 == "UDA"){
    
    filtered_data_UDA <- total %>%
      filter(Bands %in% c("total_B1",	"total_B2",	"total_B3", "total_urgent")) %>%
      select(Month, "DCP_description.x", "Bands", "asissted_percent") %>%
      rename (DCP_description = DCP_description.x) %>%
      mutate(Month = strftime(Month, "%B-%y"))
    
    filtered_data_UDA$Month <- factor(filtered_data_UDA$Month,
                                      levels = c("October-22",
                                                 "November-22",
                                                 "December-22",
                                                 "January-23",
                                                 "February-23",
                                                 "March-23",
                                                 "April-23",
                                                 "May-23",
                                                 "June-23",
                                                 "July-23",
                                                 "August-23",
                                                 "September-23"))
    
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
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
      theme_bw() +
      labs(title = "Percentage of total UDAs delivered which had DCP* assistance by band",
           subtitle = subtitle,
           x = "DCP description",
           y = "Percentage of total UDAs delivered",
           fill = "Band",
           caption = "*Dental Care Practitioner")
    
    UDA_plot
  }else{
    
    filtered_data_FP17 <- total %>%
      filter( Bands %in% c("total_FP17")) %>% 
      select("Month", "DCP_description.x", "Bands", "asissted_percent") %>%
      rename ( Percent_of_FP17 = Bands, DCP_description = DCP_description.x) %>%
      mutate(Month = strftime(Month, "%B-%y"))
    
    filtered_data_FP17$Month <- factor(filtered_data_FP17$Month,
                                       levels = c("October-22",
                                                  "November-22",
                                                  "December-22",
                                                  "January-23",
                                                  "February-23",
                                                  "March-23",
                                                  "April-23",
                                                  "May-23",
                                                  "June-23",
                                                  "July-23",
                                                  "August-23",
                                                  "September-23"))
    
    FP17_plot <- 
      ggplot(filtered_data_FP17, 
             aes(x=DCP_description, y= asissted_percent)) +
      geom_bar(stat="identity", position = "dodge", fill = "steelblue") +
      facet_grid(cols = vars(Month), labeller = label_value) +
      geom_text(aes(label = asissted_percent), 
                colour = "black", size= 3.5,
                position = position_dodge(width = 1), vjust=-0.25) +
      theme(legend.position="bottom") +
      theme_bw() + 
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
      #theme(axis.text.x = element_text(angle = 45, vjust = -1)) +
      labs(title = "Percentage of total Courses of Treatment (CoTs) delivered which had DCP* assistance",
           subtitle = subtitle,
           x = "DCP description",
           y = "Percentage of total CoTs delivered",
           caption = "*Dental Care Practitioner")
    
    FP17_plot
  }
  
}