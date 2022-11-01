setwd("N:/_Everyone/Mohammed_Emran/UDA_Projection_Monthly")


######Connecting to SQL

library (tidyverse)
library(readxl)
library(DBI)
library(odbc)
library(reactable) #make sure you have the latest version by doing install.packages("reactable")
library(downloadthis)
library(dplyr)
library(ggplot2)
library(scales)
library(ggplot2)
library(magrittr)
library(corrr)

UDA_Projection <- function(){
  
  con <- dbConnect(odbc::odbc(), "NCDR")
  sql <- "Select Distinct Y.[Contract Number] as 'Contract Number'
	, Y.[Latest Provider Name] as 'Name of the Company'
	, Y.[Commissioner Name ] as 'Commissioner Name'
	, Y.[Latest Region Description] as 'Regions'
	, Y.[UDA Performance Target] as 'Annual_Contracted_UDA'
	, Y.[UDA Financial Value]
	, Y.[Cost per UDA] as cost_per_uda
	, Y.[Contract End Date]	
	, AP.UDA_delivered as 'Delivered UDA in April-22'
	, My.UDA_Delivered as 'Delivered UDA in May-2022'
	, JN.UDA_Delivered as 'Delivered UDA in June-22'
	, JL.UDA_Delivered as 'Delivered UDA in July-22'
	, AG.UDA_Delivered as 'Delivered UDA in August-2022'
	, SP.UDA_Delivered as 'Delivered UDA in September-2022'
	, OC.UDA_Delivered as 'Delivered UDA in October-2022'		
   
   from [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[September_UDA_BSA] Y
  
	left join  [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[UDA_scheduled] AP
   on Y.[Contract Number]  = AP.contract_number and ap.data_month= '2022-04-01'
	
	
	left join  [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[UDA_scheduled] MY
     on Y.[Contract Number]  = MY.contract_number and MY.data_month= '2022-05-01'
	
	
	left join  [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[UDA_scheduled] JN
      on Y.[Contract Number] = JN.contract_number and JN.data_month= '2022-06-01'
	
		left join  [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[UDA_scheduled] JL
      on Y.[Contract Number] = JL.contract_number and JL.data_month= '2022-07-01'

	left join  [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[UDA_scheduled] AG
	on Y.[Contract Number] = AG.contract_number and AG.data_month= '2022-08-01'

	left join  [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[UDA_scheduled] SP
   on Y.[Contract Number]  = SP.contract_number and SP.data_month= '2022-09-01'
	
	left join  [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[UDA_scheduled] OC
   on Y.[Contract Number]  = OC.contract_number and oc.data_month= '2022-10-01'

   WHERE Y.[UDA Performance Target] >=100"
  
  result <- dbSendQuery (con, sql)
  UDA_Data_pull <- dbFetch(result)
  dbClearResult(result)
  
  UDA_Data_pull
}


UDA_Data <- UDA_Projection ()

##sum(is.na(new_2$Regions))

###Pulling proto_type
###calling  prototype


pull_proto_type <- function(){
  
  con <- dbConnect(odbc::odbc(), "NCDR")
  
  sql <- "
    SELECT [prototype_contract_number]
    FROM [NHSE_Sandbox_PrimaryCareNHSContracts].[Dental].[prototype_contracts]"
  result <- dbSendQuery(con, sql)
  proto_type <- dbFetch(result)
  dbClearResult(result)
  
  proto_type
}

proto_type <- pull_proto_type ()


##removing the proto type ones
uda_delivery_prot_removed <- anti_join(UDA_Data, proto_type, 
                                 by = c("Contract Number" = "prototype_contract_number"))

## (uda_delivery_prot_removed$"Contract number" == '7460880001') ---to check if the removal is okay.

uda_delivery_prot_removed_over_hundred <- uda_delivery_prot_removed %>% 
                            filter (Annual_Contracted_UDA >100)

############################
#####adding Average / Year Based on average of 3 months' annualised Delivered UDA

new_2 <- uda_delivery_prot_removed_over_hundred %>%
 mutate (avrg_last_6_month_delivered_UDA
           = rowMeans(select(uda_delivery_prot_removed_over_hundred, 
                             "Delivered UDA in May-2022", "Delivered UDA in June-22", 
                             "Delivered UDA in July-22", 
                             "Delivered UDA in August-2022",
                             "Delivered UDA in September-2022",
                             "Delivered UDA in October-2022"), na.rm = TRUE)) %>%
         mutate(rest_year_delivery = avrg_last_6_month_delivered_UDA *5) %>%
         mutate(projected_tota_year = rowSums(across(c(rest_year_delivery, "Delivered UDA in April-22",
           "Delivered UDA in May-2022", "Delivered UDA in June-22", "Delivered UDA in July-22", 
           "Delivered UDA in August-2022", "Delivered UDA in September-2022", "Delivered UDA in October-2022")), na.rm = TRUE)) %>%
       mutate(projection_percent = projected_tota_year / Annual_Contracted_UDA) %>%
        mutate(Performer = case_when ( is.na(avrg_last_6_month_delivered_UDA) ~ 'ignore',
                                       avrg_last_6_month_delivered_UDA == 0 ~ 'ignore',
                                      projection_percent <0.96 ~ 'Projected to deliver less than 96%',
                                      projection_percent >0.95 ~ 'Projected to deliver 96% or more')) 

###write.csv(test_new,"N:/_Everyone/Mohammed_Emran/UDA_Projection_Monthly/test_new.csv")


# corr_test <- new_2 %>% select(projection_percent, "Cost per UDA") 
# 
# corr_test_plot <- new_2 %>% select(projection_percent, "Cost per UDA") %>% cor %>% corrplot::corrplot(., method="number")
# 
# correlate(corr_test)


count_of_all_no_region <- new_2 %>% count(Performer, name = "Number of Contractors")

whole_data_no_ignore <- new_2 %>% filter(Performer != "ignore")                             

count_of_all_no_region_no_ignore <- whole_data_no_ignore %>% 
  count(Performer) ##, name = "Number of Contractors")  ## does not come by using name

count_of_all_with_region_no_ignore <- whole_data_no_ignore %>%
  group_by(Regions) %>%
  count(Performer) ##, name = "Number of Contractors")  ## does not come by using name


# Barplot- All with no regions

bar_plot_national <- ggplot(count_of_all_no_region_no_ignore, aes(x=Performer, y= n, fill= Performer)) +
  geom_bar(stat="identity") + theme_minimal() + theme(legend.position="none") +
  geom_text(aes(label= n), vjust=-0.3, size=3.5) +
  ggtitle("Number of contractors projected to deliver (in FY 2022/23) more or less than 96% of contracted UDA - National")

bar_plot_national

bar_plot_regional_n <- ggplot(data = count_of_all_with_region_no_ignore, aes (Regions, n, group = Performer)) +
  geom_col(aes(fill = Performer)) +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5))  + 
  theme(legend.position="top") + 
  scale_fill_brewer(palette="Reds") +
  ggtitle("Number of contractors projected to deliver (in FY 2022/23) more or less than 96% of contracted UDA - All Regions")
  
bar_plot_regional_n

### with percentage


d <- whole_data_no_ignore %>%
  group_by (Regions) %>%
  count (Performer) %>%
  mutate(percent_n = sum(n),
         ratio = n/percent_n,
         label = percent(ratio %>% round(2)))

bar_plot_regional_percent <- ggplot(data = d, aes (Regions, n, group = Performer)) +
  geom_col(aes(fill = Performer)) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5))  + 
  theme(legend.position="top") + 
  scale_fill_brewer(palette="Reds") +
  ggtitle("Number of contractors projected to deliver (in FY 2022/23) more or less than 96% of contracted UDA - All Regions")

bar_plot_regional_percent


d_test <- whole_data_no_ignore %>%
  group_by (Regions) %>%
  count (Performer) %>%
  mutate(percent_n = sum(n),
         ratio = n/percent_n,
         label = percent(ratio %>% round(2)),
         label_n = paste (n, label, sep = ' - '))

d_test <- whole_data_no_ignore %>%
  group_by (Regions) %>%
  count (Performer) %>%
  mutate(percent_n = sum(n),
         ratio = n/percent_n,
         label = percent(ratio %>% round(2)), 
           new_o = " (",
         new_c = ")",
            label_n = paste (n, new_o, label, new_c, sep = ''))



bar_plot_regional_percent_n <- ggplot(data = d_test, aes (Regions, n, group = Performer)) +
  geom_col(aes(fill = Performer)) +
  geom_text(aes(label = label_n), position = position_stack(vjust = 0.5))  + 
  theme(legend.position="top") + 
  scale_fill_brewer(palette="Reds") +
  ggtitle("Number of contractors projected to deliver (in FY 2022/23) more or less than 96% of contracted UDA - All Regions")

bar_plot_regional_percent_n

##colnames(new_2)

##str(new_2)

### add projection of rest of the months "Projected UDA (Unit) October-2022"

###new_2 %>% filter_all(any_vars(. %in% c(1018630000, 7636750002, 1048170001, 3396790001))) ###find a specific row

###https://www.youtube.com/watch?v=Y0zE9AWBVfg







################dont use yet



test_new <- new_2 %>% select('Contract Number', cost_per_uda,  
                             projection_percent, Performer ) %>%
  mutate(uda_value_banding = case_when (cost_per_uda < 23 ~ 'less than ?23/UDA' ,
                                        cost_per_uda >= 23 & cost_per_uda <= 50 ~ '?23-?50/UDA',
                                        cost_per_uda >= 50.1 & cost_per_uda <= 75 ~ '?51-?75/UDA',
                                        cost_per_uda >= 75.1 & cost_per_uda <= 100 ~ '?76-?100/UDA',
                                        cost_per_uda >= 100 ~ '?100+/UDA'))

test_new_no_ignore <- test_new %>% filter(Performer != "ignore")

test_new_no_ignore_sumry <- test_new_no_ignore %>% group_by(Performer) %>% count(uda_value_banding)

ggplot(test_new_no_ignore_sumry, aes(x=uda_value_banding, y= n, fill= Performer)) +
  geom_bar(stat="identity") + theme_minimal() + theme(legend.position="none") +
  geom_text(aes(label= n), vjust=-0.3, size=3.5) +
  ggtitle("Number of contractors projected to deliver (in FY 2022/23) more or less than 96% of contracted UDA - National")
