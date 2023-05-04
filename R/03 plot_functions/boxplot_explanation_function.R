
library (tidyverse)

ggplot_box_legend <- function(family = "serif"){
  
  # Create data to use in the boxplot legend:
  ##set.seed(100)
  
  sample_df <- data.frame(parameter = "test",
                          values = sample(500))
  
  # Extend the top whisker a bit:
  sample_df$values[1:100] <- 701:800
  # Make sure there's only 1 lower outlier:
  sample_df$values[1] <- -350
  
  # Function to calculate important values:
  ggplot2_boxplot <- function(x){
    
    quartiles <- as.numeric(quantile(x,
                                     probs = c(0.25, 0.5, 0.75)))
    
    names(quartiles) <- c("25th percentile",
                          "50th percentile\n(median)",
                          "75th percentile")
    
    IQR <- diff(quartiles[c(1,3)])
    
    upper_whisker <- max(x[x < (quartiles[3] + 1.5 * IQR)])
    lower_whisker <- min(x[x > (quartiles[1] - 1.5 * IQR)])
    
    upper_dots <- x[x > (quartiles[3] + 1.5*IQR)]
    lower_dots <- x[x < (quartiles[1] - 1.5*IQR)]
    
    return(list("quartiles" = quartiles,
                "25th percentile" = as.numeric(quartiles[1]),
                "50th percentile\n(median)" = as.numeric(quartiles[2]),
                "75th percentile" = as.numeric(quartiles[3]),
                "IQR" = IQR,
                "upper_whisker" = upper_whisker,
                "lower_whisker" = lower_whisker,
                "upper_dots" = upper_dots,
                "lower_dots" = lower_dots))
  }
  
  # Get those values:
  ggplot_output <- ggplot2_boxplot(sample_df$values)
  
  # Lots of text in the legend, make it smaller and consistent font:
  update_geom_defaults("text",
                       list(size = 3,
                            hjust = 0,
                            family = family))
  # Labels don't inherit text:
  update_geom_defaults("label",
                       list(size = 3,
                            hjust = 0,
                            family = family))
  
  # Create the legend:
  # The main elements of the plot (the boxplot, error bars, and count)
  # are the easy part.
  # The text describing each of those takes a lot of fiddling to
  # get the location and style just right:
  explain_plot <- ggplot() +
    stat_boxplot(data = sample_df,
                 aes(x = parameter, y=values),
                 geom ='errorbar', width = 0.3) +
    geom_boxplot(data = sample_df,
                 aes(x = parameter, y=values),
                 width = 0.3, fill = "lightgrey") +
    # geom_text(aes(x = 1, y = 950, label = "500"), hjust = 0.5) +
    # geom_text(aes(x = 1.17, y = 950,
    #               label = "Number of values"),
    #           fontface = "bold", vjust = 0.4) +
    theme_minimal(base_size = 5, base_family = family) +
    geom_segment(aes(x = 2.3, xend = 2.3,
                     y = ggplot_output[["25th percentile"]],
                     yend = ggplot_output[["75th percentile"]])) +
    geom_segment(aes(x = 1.2, xend = 2.3,
                     y = ggplot_output[["25th percentile"]],
                     yend = ggplot_output[["25th percentile"]])) +
    geom_segment(aes(x = 1.2, xend = 2.3,
                     y = ggplot_output[["75th percentile"]],
                     yend = ggplot_output[["75th percentile"]])) +
    geom_text(aes(x = 2.4, y = ggplot_output[["50th percentile\n(median)"]]),
              label = "Interquartile\nrange", fontface = "bold",
              vjust = 0.4) +
    geom_text(aes(x = c(1.17,1.17),
                  y = c(ggplot_output[["upper_whisker"]],
                        ggplot_output[["lower_whisker"]]),
                  label = c("Largest value within 1.5 times\ninterquartile range above\n75th percentile",
                            "Smallest value within 1.5 times\ninterquartile range below\n25th percentile")),
              fontface = "bold", vjust = 0.9) +
    geom_text(aes(x = c(1.17),
                  y =  ggplot_output[["lower_dots"]],
                  label = "Outside value"),
              vjust = 0.5, fontface = "bold") +
    geom_text(aes(x = c(1.9),
                  y =  ggplot_output[["lower_dots"]],
                  label = "-Value is >1.5 times and"),
              vjust = 0.5) +
    geom_text(aes(x = 1.17,
                  y = ggplot_output[["lower_dots"]],
                  label = "<3 times the interquartile range\nbeyond either end of the box"),
              vjust = 1.5) +
    geom_label(aes(x = 1.17, y = ggplot_output[["quartiles"]],
                   label = names(ggplot_output[["quartiles"]])),
               vjust = c(0.4,0.85,0.4),
               fill = "white", label.size = 0) +
    ylab("") + xlab("") +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          aspect.ratio = 4/3,
          plot.title = element_text(hjust = 0.5, size = 10)) +
    coord_cartesian(xlim = c(1.4,3.1), ylim = c(-600, 900)) +
    labs(title = "Boxplot Explanation")
  
  return(explain_plot)
  
}








# 
# 
# 
# 
# 
# 
# 
# boxplot_framework <- function(upper_limit,
#                               family_font = "serif",
#                               lower_limit = 0,
#                               logY = FALSE,
#                               fill_var = NA,
#                               fill = "lightgrey", width = 0.6){
# 
#   update_geom_defaults("text",
#                        list(size = 3,
#                             family = family_font))
# 
#   n_fun <- function(x, lY = logY){
#     return(data.frame(y = ifelse(logY, 0.95*log10(upper_limit), 0.95*upper_limit),
#                       label = length(x)))
#   }
# 
#   prettyLogs <- function(x){
#     pretty_range <- range(x[x > 0])
#     pretty_logs <- 10^(-10:10)
#     log_index <- which(pretty_logs < pretty_range[2] &
#                          pretty_logs > pretty_range[1])
#     log_index <- c(log_index[1]-1,log_index,
#                    log_index[length(log_index)]+1)
#     pretty_logs_new <-  pretty_logs[log_index]
#     return(pretty_logs_new)
#   }
# 
#   fancyNumbers <- function(n){
#     nNoNA <- n[!is.na(n)]
#     x <-gsub(pattern = "1e",replacement = "10^",
#              x = format(nNoNA, scientific = TRUE))
#     exponents <- as.numeric(sapply(strsplit(x, "\\^"), function(j) j[2]))
# 
#     base <- ifelse(exponents == 0, "1", ifelse(exponents == 1, "10","10^"))
#     exponents[base == "1" | base == "10"] <- ""
#     textNums <- rep(NA, length(n))
#     textNums[!is.na(n)] <- paste0(base,exponents)
# 
#     textReturn <- parse(text=textNums)
#     return(textReturn)
#   }
# 
#   if(!is.na(fill_var)){
#     basic_elements <- list(stat_boxplot(geom ='errorbar', width = width),
#                            geom_boxplot(width = width),
#                            stat_summary(fun.data = n_fun,
#                                         geom = "text",
#                                         position = position_dodge(width),
#                                         hjust =0.5,
#                                         aes_string(group=fill_var)),
#                            expand_limits(y = lower_limit))
#   } else {
#     basic_elements <- list(stat_boxplot(geom ='errorbar', width = width),
#                            geom_boxplot(width = width, fill = fill),
#                            stat_summary(fun.data = n_fun,
#                                         geom = "text", hjust =0.5),
#                            expand_limits(y = lower_limit))
#   }
# 
#   if(logY){
#     return(c(basic_elements,
#              scale_y_log10(limits = c(lower_limit, upper_limit),
#                            expand = expand_scale(mult = c(0, 0)),
#                            labels=fancyNumbers,
#                            breaks=prettyLogs),
#              annotation_logticks(sides = c("rl"))))
#   } else {
#     return(c(basic_elements,
#              scale_y_continuous(sec.axis = dup_axis(label = NULL,
#                                                     name = NULL),
#                                 expand = expand_scale(mult = c(0, 0)),
#                                 breaks = pretty(c(lower_limit,upper_limit), n = 5),
#                                 limits = c(lower_limit,upper_limit))))
#   }
# }
