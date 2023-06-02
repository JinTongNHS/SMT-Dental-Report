library(formattable)

plot_UDA_yearly_delivery <- function(data = UDA_scheduled_data, 
                                       level = "National",
                                       region_STP_name = NULL,
                                       remove_prototypes = TRUE, 
                                       plotChart = TRUE){
  
  if(remove_prototypes == TRUE){
    data <- data %>%
      filter(annual_contracted_UDA > 100)
    
    chartCaption <- "Excludes contracts with annual contracted UDAs < 100."
  }else{
    
    chartCaption <- "Includes contracts with annual contracted UDAs < 100."
  }
  
  data <- data %>% 
    filter(as.Date(month)>= "2023-04-01" ####needs to be updated each year
           ) %>% 
    select(month, contract_number, name_or_company_name,
           commissioner_name, commissioner_ods_code_icb, region_name,
           contract_type, paid_by_BSA, contract_start_date, contract_end_date, annual_contracted_UDA,
           ##annual_contracted_UOA, annual_contracted_UOA,
           UDA_delivered)
  
  
  data_wide <- data %>%
    # select(month, contract_number, annual_contracted_UDA, #name_or_company_name, commissioner_name, region_name, contract_start_date, contract_end_date, annual_contracted_UDA, 
    #        UDA_delivered) %>%
    filter(month >= as.Date("2023-04-01")) %>% #### Needs to be updated each year
    ##left_join(UDA_value_data, by = "contract_number") %>%
    arrange(month) %>%
    mutate(month = format(month, "%B-%Y")) %>%
    pivot_wider(names_from = month,
                values_from = UDA_delivered,
                names_prefix = "Delivered UDA in ",
                values_fill = 0) %>%
    mutate_at(vars(starts_with("Delivered UDA")), ~replace_na(.,0)) %>% 
    mutate (delivered_whole_year = rowSums(across(starts_with("Delivered UDA")), na.rm = TRUE),
            Delivered_percent = formattable::percent(delivered_whole_year/annual_contracted_UDA, digits =0)) %>% 
    mutate(performance_category = case_when(Delivered_percent == 0 ~ "ignore",
                                            Delivered_percent < 0.96 ~ 'Delivered less than 96%',
                                            Delivered_percent >= 0.96 ~ 'Delivered 96% or more')) ##%>% # changed to >=96 from >95
  
  
  
  data_plot <- data_wide %>%
    group_by(performance_category) %>%
    count() %>%
    filter(performance_category %in% c('Delivered less than 96%', 
                                       "Delivered 96% or more")) %>%
    mutate(region_name = "National")
  
  data_regional <- data_wide %>%
    group_by(region_name, performance_category) %>%
    count() %>%
    filter(performance_category %in% c('Delivered less than 96%', 
                                       "Delivered 96% or more")) %>%
    bind_rows(data_plot)
  
  data_regional$region_name <- factor(data_regional$region_name, levels = c("National", "East of England", "London", "Midlands", "North East and Yorkshire", 
                                                                            "North West", "South East", "South West"))
  
  
  if(level == "National"){

    if(plotChart == TRUE){

      bar_plot_national <- ggplot(data_plot,
                                  aes(x=performance_category,
                                      y= n,
                                      fill= performance_category)) +
        geom_bar(stat="identity") +
        theme_minimal() +
        theme(legend.position="none") +
        geom_text(aes(label= n), vjust=-0.3, size=3.5) +
        labs(title = "Number of contracts delivered (in FY 2023/24) more or less than 96% of contracted UDA",
             subtitle = paste0("National", "\nDelivered from Apr-23 to ", format(max(data$month), "%b-%y")),
             x = "Performance category",
             y = "Number of contracts",
             caption = chartCaption)

      bar_plot_national
    }else{
      data_wide
    }


  }else{

    if(plotChart == TRUE){
      d_test <- data_regional %>%
        group_by(region_name) %>%
        mutate(percent_n = sum(n),
               ratio = n/percent_n,
               label = round(ratio,3) * 100,
               label_n = paste0(n, "\n(", label, "%)"))


      bar_plot_regional_percent_n <- ggplot(data = d_test,
                                            aes(x = region_name,
                                                y = n,
                                                group = performance_category)) +
        geom_col(aes(fill = performance_category)) +
        geom_text(aes(label = label_n),
                  position = position_stack(vjust = 0.5))  +
        theme(legend.position="top") +
        scale_fill_brewer(palette="Reds") +
        labs(title = "Number of contracts delivered (in FY 2023/24) more or less than 96% of contracted UDAs",
             subtitle = paste0("Delivered from Apr-23 to ", format(max(data$month), "%b-%y")),
             x = "Region",
             y = "Number of contracts",
             fill = "",
             caption = chartCaption) +
        theme_bw() +
        theme(legend.position="bottom")

      bar_plot_regional_percent_n

    }else{
      d_test
    }

  }
  
  
}
