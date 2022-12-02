plot_UDA_projection <- function(level = "National"){
  
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
  
  ount_of_all_no_region <- new_2 %>% count(Performer, name = "Number of Contractors")
  
  whole_data_no_ignore <- new_2 %>% filter(Performer != "ignore")                             
  
  count_of_all_no_region_no_ignore <- whole_data_no_ignore %>% 
    count(Performer) ##, name = "Number of Contractors")  ## does not come by using name
  
  count_of_all_with_region_no_ignore <- whole_data_no_ignore %>%
    group_by(Regions) %>%
    count(Performer) ##, name = "Number of Contractors")  ## does not come by using name
  
  if(level == "National"){
    bar_plot_national <- ggplot(count_of_all_no_region_no_ignore, aes(x=Performer, y= n, fill= Performer)) +
      geom_bar(stat="identity") + theme_minimal() + theme(legend.position="none") +
      geom_text(aes(label= n), vjust=-0.3, size=3.5) +
      labs(title = "Number of contracts projected to deliver (in FY 2022/23) more or less than 96% of contracted UDA",
           subtitle = paste0("National", "\nProjection based on delivery from Apr-22 to ", format(Sys.Date() - lubridate::duration("4 weeks"), "%b-%y")),
           x = "",
           y = "Number of contracts")
    
    bar_plot_national
  }else{
    
    d_test <- whole_data_no_ignore %>%
      group_by (Regions) %>%
      count (Performer) %>%
      mutate(percent_n = sum(n),
             ratio = n/percent_n,
             label = round(ratio,3) * 100, 
             label_n = paste0(n, "\n(", label, "%)"))
    
    
    bar_plot_regional_percent_n <- ggplot(data = d_test, aes (Regions, n, group = Performer)) +
      geom_col(aes(fill = Performer)) +
      geom_text(aes(label = label_n), position = position_stack(vjust = 0.5))  + 
      theme(legend.position="top") + 
      scale_fill_brewer(palette="Reds") +
      labs(title = "Number of contracts projected to deliver (in FY 2022/23) more or less than 96% of contracted UDAs",
           subtitle = paste0("Projection based on delivery from Apr-22 to ", format(Sys.Date() - lubridate::duration("4 weeks"), "%b-%y")),
           x = "Region",
           y = "Number of contracts",
           fill = "") +
      theme_bw() +
      theme(legend.position="bottom") 
    
    bar_plot_regional_percent_n
  }
  
}