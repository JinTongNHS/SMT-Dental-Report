plot_band2_split <- function(data = band2_split_data){
  
  data <- data %>%
    select(month, band_2a_UDAs, band_2b_UDAs, band_2c_UDAs) %>%
    group_by(month) %>%
    summarise(band_2a_UDAs = sum(band_2a_UDAs, na.rm = TRUE), 
              band_2b_UDAs = sum(band_2b_UDAs, na.rm = TRUE), 
              band_2c_UDAs = sum(band_2c_UDAs, na.rm = TRUE)) %>%
    pivot_longer(cols = !month, names_to = "band", values_to = "UDAs") %>%
    mutate(band = case_when(band == "band_2a_UDAs" ~ "Band 2A",
                            band == "band_2b_UDAs" ~ "Band 2B",
                            band == "band_2c_UDAs" ~ "Band 2C"))
  
  ggplot(data,
         aes(x = month, 
             y = UDAs,
             colour = band)) +
    geom_line() +
    geom_point() +
    theme_bw() +
    labs(title = "Band 2 UDA split",
         x = "Month",
         y = "UDAs",
         colour = "") +
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
    scale_x_date(date_breaks = "1 month", 
                 date_labels = "%b-%y") +
    theme(axis.text.x = element_text(angle = 90))
}