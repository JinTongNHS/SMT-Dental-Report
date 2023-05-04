plot_UDA_projection <- function(data = UDA_scheduled_data,
                                UDA_value_data = UDA_UOA_value_data, #september_BSA_UDA_value_data, 
                                level = "National",
                                region_STP_name = NULL,
                                remove_prototypes = TRUE, 
                                plotChart = TRUE){
  
  projection_data <- get_UDA_projection_data(data = data,
                                             UDA_value_data = UDA_value_data, 
                                             level = "National",
                                             region_STP_name = region_STP_name,
                                             remove_prototypes = remove_prototypes, 
                                             plotChart = FALSE)
  
  if(remove_prototypes == TRUE){
    projection_data <- projection_data %>%
      filter(annual_contracted_UDA > 100)
  }

  data <- projection_data %>%
    group_by(performance_category) %>%
    count() %>%
    filter(performance_category %in% c("Projected to deliver 96% or more", "Projected to deliver less than 96%"))

  data_regional <- projection_data %>%
    group_by(region_name, performance_category) %>%
    count() %>%
    filter(performance_category %in% c("Projected to deliver 96% or more", "Projected to deliver less than 96%"))


  if(level == "National"){
    bar_plot_national <- ggplot(data,
                                aes(x=performance_category,
                                    y= n,
                                    fill= performance_category)) +
      geom_bar(stat="identity") +
      theme_minimal() +
      theme(legend.position="none") +
      geom_text(aes(label= n), vjust=-0.3, size=3.5) +
      labs(title = "Number of contracts projected to deliver (in FY 2022/23) more or less than 96% of contracted UDA",
           subtitle = paste0("National", "\nProjection based on delivery from Apr-22 to ", format(Sys.Date() - lubridate::duration("4 weeks"), "%b-%y")),
           x = "Performance projectiion category",
           y = "Number of contracts")

    bar_plot_national

  }else{

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
      labs(title = "Number of contracts projected to deliver (in FY 2022/23) more or less than 96% of contracted UDAs",
           subtitle = paste0("Projection based on delivery from Apr-22 to ", format(Sys.Date() - lubridate::duration("4 weeks"), "%b-%y")),
           x = "Region",
           y = "Number of contracts",
           fill = "") +
      theme_bw() +
      theme(legend.position="bottom")

    bar_plot_regional_percent_n
  }
  
  
}
