library(tidyverse)

#remove spaces from column names
colnames(dental_data_all_months) <- make.names(colnames(dental_data_all_months), unique = T)
colnames(dental_data_august) <- make.names(colnames(dental_data_august), unique = T)
colnames(orthodontic_data_august) <- make.names(colnames(orthodontic_data_august), unique = T)

#create not in function
`%notin%` = Negate(`%in%`)

#filter out prototype contracts
dental_data <- filter(dental_data_august, Contract.Number %notin% prototype_contracts$proto_contracts)
orth_data <- filter(orthodontic_data_august, Contract.Number %notin% prototype_contracts$proto_contracts)


annual_contracted_UDA_august <- sum(dental_data$Annual.contracted.UDA, na.rm = T)
UDA_delivered_august <- sum(dental_data$AUG.UDA.Delivered, na.rm = T)

annual_contracted_UOA_august <- sum(orth_data$Annual.contracted.UOA, na.rm = T)
UOA_delivered_august <- sum(orth_data$UOA...AUG..2021., na.rm = T)


#function to get dental data into the right format for slide 4
get_into_slide4_format <- function(data = dental_data_all_months, remove_prototypes = T){
  
  #remove spaces from column names
  colnames(data) <- make.names(colnames(data), unique = T)
  data
  
  #remove prototype contracts if specified
  if(remove_prototypes){
    #create not in function
    `%notin%` = Negate(`%in%`)
    data <- data %>%
      filter(Contract.Number %notin% prototype_contracts$proto_contracts)
  }

  #group by month and sum UDAs delivered
  UDAs_delivered <- data %>%
    group_by(Year.Month) %>%
    summarise(monthly_UDA_UOAs_delivered = sum(UDA.Delivered, na.rm = T))

  #group by month and sum contracted UDAs
  UDAs_contracted <- data %>%
    group_by(Year.Month) %>%
    summarise(annual_contracted_UDA_UOA = sum(Annual.contracted.UDA, na.rm = T))
  
  slide4_UDA <- full_join(UDAs_delivered, UDAs_contracted, by = "Year.Month")
  slide4_UDA <- slide4_UDA %>%
    mutate(month = if_else(Year.Month == 202104, as.Date("2021-04-01"), 
                           if_else(Year.Month == 202105, as.Date("2021-05-01"),
                                   if_else(Year.Month == 202106, as.Date("2021-06-01"),
                                           if_else(Year.Month == 202107, as.Date("2021-07-01"),
                                                   if_else(Year.Month == 202108, as.Date("2021-08-01"),
                                                           as.Date(NA))))))
    )
}