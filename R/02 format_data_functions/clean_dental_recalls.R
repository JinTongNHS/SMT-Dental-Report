################################################################################
clean_dental_recalls <- function(data = dental_recalls_STP_2021_22){
  
  data <- data %>%
    pivot_longer(cols = c("Band_1", "Band_2", "Band_3", "Other", "Urgent"),
                 names_to = "Band",
                 values_to = "forms")
  
}