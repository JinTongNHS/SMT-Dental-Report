get_UOAs_delivered_2021_22 <- function(data = UOA_scheduled_data){
  
  data <- data %>%
    filter(month >= as.Date("2021-04-01") & month < as.Date("2022-04-01")) %>%
    summarise(UOA_delivered = sum(UOA_delivered, na.rm = TRUE))
  
}


payments_2021_22 <- payments_to_dentists %>%
  filter(year == "2021/22") 

num_contracts <- payments_2021_22 %>%
  count()

num_contracts_with_UOAs <- payments_2021_22 %>%
  filter(Total_Contracted_UOA > 0) %>%
  count()

num_contracts_with_UOAs_no_UDAs <- payments_2021_22 %>%
  filter(Total_Contracted_UOA > 0 & Total_Contracted_UDA == 0) %>%
  count()

total_contracted_UOAs <- payments_2021_22 %>%
  summarise(Total_Contracted_UOA = sum(as.numeric(Total_Contracted_UOA), na.rm = T))

total_contracted_UOAs_UOA_only_contracts <- payments_2021_22 %>%
  filter(Total_Contracted_UOA > 0 & Total_Contracted_UDA == 0) %>%
  summarise(Total_Contracted_UOA = sum(as.numeric(Total_Contracted_UOA), na.rm = T))

total_baseline_contract <- payments_2021_22 %>%
  summarise(total_baseline_contract = sum(as.numeric(Baseline_Contract), na.rm = T))

total_baseline_contract_UOA_only_contracts <- payments_2021_22 %>%
  filter(Total_Contracted_UOA > 0 & Total_Contracted_UDA == 0) %>%
  summarise(total_baseline_contract = sum(as.numeric(Baseline_Contract), na.rm = T))