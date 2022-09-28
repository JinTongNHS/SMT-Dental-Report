################################################################################
get_change_in_commissioned_UDAs <- function(data = UDA_scheduled_data,
                                            historical_data = historical_UDA_scheduled_data,
                                            protos = prototype_contracts$prototype_contract_number,
                                            level = "National",
                                            region_STP_name = NULL,
                                            all_regions_and_STPs = FALSE,
                                            plotChart = TRUE){
  
  data <- data %>%
    select(month, contract_number, annual_contracted_UDA) %>%
    filter(!(contract_number %in% protos)) %>%
    arrange(month) %>%
    pivot_wider(names_from = month, values_from = annual_contracted_UDA) %>%
    rowwise() %>%
    # mutate(min_contracted = min(`2021-04-01`, `2021-05-01`, `2021-06-01`, `2021-07-01`, `2021-08-01`, `2021-09-01`,
    #            `2021-10-01`, `2021-11-01`, `2021-12-01`, `2022-01-01`, `2022-02-01`, `2022-03-01`,
    #            `2022-04-01`, `2022-05-01`, `2022-06-01`, `2022-07-01`, `2022-08-01`, na.rm = TRUE)) %>%
    # mutate(max_contracted = max(`2021-04-01`, `2021-05-01`, `2021-06-01`, `2021-07-01`, `2021-08-01`, `2021-09-01`,
    #                             `2021-10-01`, `2021-11-01`, `2021-12-01`, `2022-01-01`, `2022-02-01`, `2022-03-01`,
    #                             `2022-04-01`, `2022-05-01`, `2022-06-01`, `2022-07-01`, `2022-08-01`, na.rm = TRUE)) %>%
    # mutate(min_equals_max = min_contracted == max_contracted) %>%
    mutate(mean_contracted_2021_22 = mean(c(`2021-04-01`, `2021-05-01`, `2021-06-01`, `2021-07-01`, `2021-08-01`, `2021-09-01`,
                                            `2021-10-01`, `2021-11-01`, `2021-12-01`, `2022-01-01`, `2022-02-01`, `2022-03-01`), na.rm = TRUE)) %>%
    mutate(mean_contracted_2022_23 = mean(c(`2022-04-01`, `2022-05-01`, `2022-06-01`, `2022-07-01`, `2022-08-01`), na.rm = TRUE)) %>%
    #filter(mean_contracted_2021_22 != mean_contracted_2022_23) %>%
    #filter(mean_contracted_2021_22 > 100) %>%
    mutate(change_in_contracted_UDA = mean_contracted_2022_23 - mean_contracted_2021_22) %>%
    mutate(change_in_contracted_UDA_category = case_when(mean_contracted_2021_22 < mean_contracted_2022_23 ~ "Increased",
                                                         mean_contracted_2021_22 == mean_contracted_2022_23 ~ "No change",
                                                         mean_contracted_2021_22 > mean_contracted_2022_23 ~ "Decreased",
                                                         TRUE ~ "Contract ended"))
  
  
}
