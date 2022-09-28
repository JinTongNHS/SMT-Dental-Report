################################################################################
clean_workforce_returns <- function(data = dental_workforce_returns){
  
  data <- data %>%
    rename(total_workforce_returns_due = workforce_returns_due) %>%
    pivot_longer(cols = starts_with("workforce_returns"),
                 names_to = "old_column_names") %>%
    mutate(year = substr(old_column_names, 19, 22)) %>%
    mutate(month = substr(old_column_names, 23, 24)) %>%
    mutate(date = as.Date(paste0(year,"-", month,"-01"))) %>%
    rename(workforce_returns = value)
}