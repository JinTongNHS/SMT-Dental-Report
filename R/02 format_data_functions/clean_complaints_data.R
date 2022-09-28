################################################################################
clean_complaints_data <- function(data = Dental_Access_Complaints_Data_June_22){
  
  data <- data %>%
    select(date_received = "Date Received (Case) (Case)",
           reference_number = "Reference Number",
           provider_name = "Provider Name",
           region_name = "Region",
           service = "KO41 Service",
           subject = "KO41 Subject",
           isUrgent = "Urgent",
           isChildren = "Children",
           isOrthodontics = "Orthodontics")
  
  #filter_out_dummy referrals
  data <- data %>%
    filter(grepl("Dummy", provider_name, ignore.case = TRUE) == FALSE) %>%
    mutate(isUrgent = case_when(isUrgent == "Y" ~ TRUE,
                                isUrgent == "y" ~ TRUE,
                                isUrgent == "N" ~ FALSE,
                                isUrgent == "n" ~ FALSE)) %>%
    mutate(isChildren = case_when(isChildren == "Y" ~ TRUE,
                                  isChildren == "y" ~ TRUE,
                                  isChildren == "N" ~ FALSE,
                                  isChildren == "n" ~ FALSE)) %>%
    mutate(isOrthodontics = case_when(isOrthodontics == "Y" ~ TRUE,
                                      isOrthodontics == "y" ~ TRUE,
                                      isOrthodontics == "N" ~ FALSE,
                                      isOrthodontics == "n" ~ FALSE)) %>%
    mutate(service = sub(".*- ", "", service)) %>%
    mutate(subject = sub(".*- ", "", subject))
  
}