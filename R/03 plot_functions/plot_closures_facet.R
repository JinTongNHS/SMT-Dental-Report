################################################################################
plot_closures_facet <- function(data = contract_handbacks,
                                level = "National",
                                region_STP_name = NULL,
                                all_regions_and_STPs = FALSE,
                                plotChart = TRUE){
  
  p1 <- plot_practice_closures(data = data,
                               level = level,
                               region_STP_name = region_STP_name,
                               all_regions_and_STPs = all_regions_and_STPs,
                               plotChart = plotChart)
  
  p2 <- plot_re_and_de_commissioned_UDAs_UOAs(data = data,
                                              level = level,
                                              region_STP_name = region_STP_name,
                                              all_regions_and_STPs = all_regions_and_STPs,
                                              plotChart = plotChart)
  
  
  ggpubr::ggarrange(p1, p2, nrow = 2)
}
