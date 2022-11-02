
get_ICB_level_dental_access <- function(){
  dental_access_data <- read_excel("data/ward level unique patient dental access standardisation update 1 v3.xlsx")
  
  names(dental_access_data) <- names(dental_access_data) %>% make.names()
  
  dental_access_data_ICB <- dental_access_data %>%
    group_by(Year, ICB.code, ICB.name) %>%
    summarise(ICB_population = sum(Ward.population, na.rm = TRUE),
              ICB_unique_patient_count = sum(Ward.unique.patient.count, na.rm = TRUE),
              ICB_expected_unique_patient_count = sum(Ward.expected.unique.patients, na.rm = TRUE)) %>%
    mutate(proportion_of_ICB_pop_accessing_NHS_dentists = ICB_unique_patient_count / ICB_population,
           expected_proportion_of_ICB_population_accessing_NHS_dentists = ICB_expected_unique_patient_count / ICB_population) %>%
    mutate(dental_access_standardised = proportion_of_ICB_pop_accessing_NHS_dentists / expected_proportion_of_ICB_population_accessing_NHS_dentists)
}
