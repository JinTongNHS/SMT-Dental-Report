plot_prototype_delivery <- function(data){
  
  data <- data %>%
    select(PrototypeRef, 
           delivery_2014.15, delivery_2015.16, delivery_2016.17, delivery_2017.18,
           delivery_2018.19, delivery_2019.20, delivery_2020.21, scaled_delivery_2021.22) 
  
  data <- reshape2::melt(data, id = "PrototypeRef")
  
  ggplot(data, aes(x = variable, y = value)) +
    geom_point()
}



plot_prototype_scatter <- function(data = prototype_delivery){
  
  data_gds <- data %>%
    select(PrototypeRef, 
           gds_perf_2014.15,
           gds_perf_2015.16,
           gds_perf_2016.17,
           gds_perf_2017.18,
           gds_perf_2019.20,
           gds_perf_2020.21,
           gds_perf_2021.22) %>%
    mutate(gds_perf_2021.22_scaled = gds_perf_2021.22 * 12/6) %>%
    reshape2::melt(id = "PrototypeRef") %>%
    rename(delivery_year = variable, gds_performance = value) %>%
    mutate(delivery_year = substr(delivery_year, 10, 16))
  
  data_dcr <- data %>%
    select(PrototypeRef,
           dcr_perf_2016.17,
           dcr_perf_2017.18,
           dcr_perf_2019.20,
           dcr_perf_2020.21) %>%
    reshape2::melt(id = "PrototypeRef") %>%
    rename(delivery_year = variable, dcr_performance = value) %>%
    mutate(delivery_year = substr(delivery_year, 10, 16))
  
  data <- left_join(data_gds, data_dcr) %>%
    filter(!is.na(dcr_performance)) 

  ggplot(data, aes(x = gds_performance, y = dcr_performance)) +
     geom_point(aes(colour = delivery_year)) +
    theme_bw() +
    scale_x_continuous(breaks = seq(0, 1.2, 0.1),
                       limits = c(0, 1.2),
                       labels = scales::percent_format(accuracy = 1)) +
    scale_y_continuous(breaks = seq(0, 1.6, 0.1),
                       limits = c(0, 1.2),
                       labels = scales::percent_format(accuracy = 1)) +
    labs(title = "GDS and DCR Performance for Prototype Contracts",
         #subtitle = "2018/19 vs 2020/21",
         x = "GDS Performance",
         y = "DCR Performance",
         #caption = "Wave 1-4 contracts included"
         colour = "Financial Year"
         )
    
  
  #data
}


plot_prototype_scatter2 <- function(data = prototype_delivery, year = "2016.17", col = "#F8766D", sub = "2016/17"){
  
  data_gds <- data %>%
    select(PrototypeRef, 
           gds_perf_2014.15,
           gds_perf_2015.16,
           gds_perf_2016.17,
           gds_perf_2017.18,
           gds_perf_2019.20,
           gds_perf_2020.21,
           gds_perf_2021.22) %>%
    mutate(gds_perf_2021.22_scaled = gds_perf_2021.22 * 12/6) %>%
    reshape2::melt(id = "PrototypeRef") %>%
    rename(delivery_year = variable, gds_performance = value) %>%
    mutate(delivery_year = substr(delivery_year, 10, 16))
  
  data_dcr <- data %>%
    select(PrototypeRef,
           dcr_perf_2016.17,
           dcr_perf_2017.18,
           dcr_perf_2019.20,
           dcr_perf_2020.21) %>%
    reshape2::melt(id = "PrototypeRef") %>%
    rename(delivery_year = variable, dcr_performance = value) %>%
    mutate(delivery_year = substr(delivery_year, 10, 16))
  
  data <- left_join(data_gds, data_dcr) %>%
    filter(!is.na(dcr_performance)) %>%
    filter(delivery_year == year)
  
  ggplot(data, aes(x = gds_performance, y = dcr_performance)) +
    geom_point(colour = col) +
    theme_bw() +
    scale_x_continuous(breaks = seq(0, 1.2, 0.1),
                       limits = c(0, 1.2),
                       labels = scales::percent_format(accuracy = 1)) +
    scale_y_continuous(breaks = seq(0, 1.6, 0.1),
                       limits = c(0, 1.2),
                       labels = scales::percent_format(accuracy = 1)) +
    labs(title = "GDS and DCR Performance for Prototype Contracts",
         subtitle = sub,
         x = "GDS Performance",
         y = "DCR Performance"#,
         #caption = "Wave 1-4 contracts included"
         #colour = "Financial Year"
    )
  
  
  #data
}








plot_scatter_protos <- function(data = prototype_delivery){
  
  data <- data %>%
    filter(Wave == "Wave 1" | Wave == "Wave 2" | Wave == "Wave 3") %>%
    mutate(mean_delivery_pre_pan = rowMeans(select(., `delivery_2016-17`, 
                                                   `delivery_2017-18`,
                                                   `delivery_2018-19`,
                                                   `delivery_2019-20`))) %>%
    mutate(mean_scaled_delivery_post_pan = rowMeans(select(., `delivery_2020-21`,
                                                           `scaled_delivery_2021-22`)))
  
  ggplot(data, aes(x = mean_delivery_pre_pan, y = mean_scaled_delivery_post_pan)) +
    geom_point() + 
    geom_point(aes(x = mean(mean_delivery_pre_pan), y = mean(mean_scaled_delivery_post_pan)),
               colour = "red",
               #shape = 4,
               size = 3
    ) +
    theme_bw() +
    geom_smooth(method = "lm"
                , se = F
    ) +
    scale_x_continuous(breaks = seq(0.2, 1.2, 0.1), 
                       limits = c(0.25, 1.2),
                       labels = scales::percent_format(accuracy = 1)) +
    scale_y_continuous(breaks = seq(0, 0.6, 0.1),
                       limits = c(0, 0.6),
                       labels = scales::percent_format(accuracy = 1)) +
    labs(title = "GDS UDA Delivery of Contracted Value for Prototype Contracts",
         subtitle = "2016-2020 mean delivery Vs 2020-2022 scaled* mean delivery",
         x = "Mean delivery pre-pandemic",
         y = "Mean delivery post-pandemic",
         caption = "*Data for 2021/22 has been scaled up to a full year for comparison. Wave 4 contracts excluded.") 
  
}


plot_scatter_protos2 <- function(data = prototype_delivery){
  
  
  ggplot(data, aes(x = `delivery_2019-20`, y = `delivery_2020-21`)) +
    geom_point() +
    geom_point(aes(x = mean(`delivery_2019-20`), y = mean(`delivery_2020-21`)),
               colour = "red",
               #shape = 4,
               size = 3
    ) +
    theme_bw() +
    geom_smooth(method = "lm"
                , se = F
    ) +
    scale_x_continuous(breaks = seq(0.2, 1.2, 0.1),
                       limits = c(0.25, 1.2),
                       labels = scales::percent_format(accuracy = 1)) +
    scale_y_continuous(breaks = seq(0, 0.6, 0.1),
                       limits = c(0, 0.6),
                       labels = scales::percent_format(accuracy = 1)) +
    labs(title = "GDS UDA Delivery of Contracted Value for Prototype Contracts",
         subtitle = "2019/20 vs 2020/21",
         x = "2019/20 delivery",
         y = "2020/21 delivery",
         caption = "Wave 1-4 contracts included")
}


plot_scatter_protos3 <- function(data = prototype_delivery){
  
  
  ggplot(data, aes(x = `delivery_2018-19`, y = `delivery_2020-21`)) +
    geom_point() +
    geom_point(aes(x = mean(`delivery_2018-19`), y = mean(`delivery_2020-21`)),
               colour = "red",
               #shape = 4,
               size = 3
    ) +
    theme_bw() +
    geom_smooth(method = "lm"
                , se = F
    ) +
    scale_x_continuous(breaks = seq(0.2, 1.2, 0.1),
                       limits = c(0.25, 1.2),
                       labels = scales::percent_format(accuracy = 1)) +
    scale_y_continuous(breaks = seq(0, 0.6, 0.1),
                       limits = c(0, 0.6),
                       labels = scales::percent_format(accuracy = 1)) +
    labs(title = "GDS UDA Delivery of Contracted Value for Prototype Contracts",
         subtitle = "2018/19 vs 2020/21",
         x = "2018/19 delivery",
         y = "2020/21 delivery",
         caption = "Wave 1-4 contracts included")
}


################################################################################
#function to plot the density of contracts based on performance 
plot_density <- function(data = prototype_delivery, 
                         gdsCol,
                         dcrCol,
                         subtitle = "2020/21"){
  
  
  
  data <- data %>% 
    #select({{ gdsCol }}, {{ dcrCol }}) %>%
    select(gds_perf_2020.21, dcr_perf_2020.21) %>%
    reshape2::melt() %>%
    rename(GDSorDCR = variable,
           perf = value) %>%
    mutate(perf = perf * 100)
  
  
  data_GDS <- data %>%
    filter(GDSorDCR == "gds_perf_2020.21")
  
  data_GDS <- summary(data_GDS$perf)
  
  data_DCR <- data %>%
    filter(GDSorDCR == "dcr_perf_2020.21")
  
  data_DCR <- summary(data_DCR$perf)
  
  
  ggplot(data,
         aes(x = perf)) +
    geom_density(size = 1,
                 aes(colour = GDSorDCR,
                     fill = GDSorDCR),
                 alpha = 0.1) +
    #GDS mean
    geom_vline(xintercept = as.numeric(data_GDS["Mean"]),
               size = 0.5,
               colour = "coral",
               linetype = "solid"
    ) +
    annotate(geom = "text",
             x = as.numeric(data_GDS["Mean"]),
             y = 0,
             label = "mean",
             size = 3,
             colour = "coral") +
    
    #GDS LQ
    geom_vline(xintercept = as.numeric(data_GDS["1st Qu."]),
               size = 0.5,
               colour = "coral",
               linetype = "dashed"
    ) +
    annotate(geom = "text",
             x = as.numeric(data_GDS["1st Qu."]),
             y = 0,
             label = "LQ.",
             size = 3,
             colour = "coral") +
    
    #GDS UQ
    geom_vline(xintercept = as.numeric(data_GDS["3rd Qu."]),
               size = 0.5,
               colour = "coral",
               linetype = "dashed"
    ) +
    annotate(geom = "text",
             x = as.numeric(data_GDS["3rd Qu."]),
             y = 0,
             label = "UQ.",
             size = 3,
             colour = "coral") +
    
    #DCR mean
    geom_vline(xintercept = as.numeric(data_DCR["Mean"]),
               size = 0.5,
               colour = "seagreen",
               linetype = "solid"
    ) +
    annotate(geom = "text",
             x = as.numeric(data_DCR["Mean"]),
             y = 0,
             label = "mean",
             size = 3,
             colour = "seagreen") +
    
    #DCR LQ
    geom_vline(xintercept = as.numeric(data_DCR["1st Qu."]),
               size = 0.5,
               colour = "seagreen",
               linetype = "dashed"
    ) +
    annotate(geom = "text",
             x = as.numeric(data_DCR["1st Qu."]),
             y = 0,
             label = "LQ.",
             size = 3,
             colour = "seagreen") +
    
    #DCR UQ
    geom_vline(xintercept = as.numeric(data_DCR["3rd Qu."]),
               size = 0.5,
               colour = "seagreen",
               linetype = "dashed"
    ) +
    annotate(geom = "text",
             x = as.numeric(data_DCR["3rd Qu."]),
             y = 0,
             label = "UQ.",
             size = 3,
             colour = "seagreen") +
    
    scale_x_continuous(limits = c(0,150),
                       breaks = seq(0,150, 10)) +
    scale_colour_manual(labels = c("GDS", "DCR"), values = c("coral", "seagreen")) +
    scale_fill_manual(labels = c("GDS", "DCR"), values = c("coral", "seagreen")) +
    labs(title = "Performance distribution for prototype contracts in GDS and DCR systems",
         x = "Delivery in given time period (%)",
         y = "Density of contracts",
         colour = "Measure of performance",
         fill = "Measure of performance",
         subtitle = subtitle) +
    theme_bw()
  
  
  
}