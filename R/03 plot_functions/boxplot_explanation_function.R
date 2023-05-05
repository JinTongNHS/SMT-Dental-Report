ggplot_box_legend <- function(family = "serif"){
  
  # Create data to use in the boxplot legend:
  ##set.seed(100)
  #sample data - simulated
  sample_df <- structure(list(parameter = c("test", "test", "test", "test", 
                                            "test", "test", "test", "test", "test", "test", "test", "test", 
                                            "test", "test", "test", "test", "test", "test", "test", "test", 
                                            "test", "test", "test", "test", "test", "test", "test", "test", 
                                            "test", "test", "test", "test", "test", "test", "test", "test", 
                                            "test", "test", "test", "test", "test", "test", "test", "test", 
                                            "test", "test", "test", "test", "test", "test", "test", "test", 
                                            "test", "test", "test", "test", "test", "test", "test", "test", 
                                            "test", "test", "test", "test", "test", "test", "test", "test", 
                                            "test", "test", "test", "test", "test", "test", "test", "test", 
                                            "test", "test", "test", "test", "test", "test", "test", "test", 
                                            "test", "test", "test", "test", "test", "test", "test", "test", 
                                            "test", "test", "test", "test", "test", "test", "test", "test", 
                                            "test", "test", "test", "test", "test", "test", "test", "test", 
                                            "test", "test", "test", "test", "test", "test", "test", "test", 
                                            "test", "test", "test", "test", "test", "test", "test", "test", 
                                            "test", "test", "test", "test", "test", "test", "test", "test", 
                                            "test", "test", "test", "test", "test", "test", "test", "test", 
                                            "test", "test", "test", "test", "test", "test", "test", "test", 
                                            "test", "test", "test", "test", "test", "test", "test", "test", 
                                            "test", "test", "test", "test", "test", "test", "test", "test", 
                                            "test", "test", "test", "test", "test", "test", "test", "test", 
                                            "test", "test", "test", "test", "test", "test", "test", "test", 
                                            "test", "test", "test", "test", "test", "test", "test", "test", 
                                            "test", "test", "test", "test", "test", "test", "test", "test", 
                                            "test", "test", "test", "test", "test", "test", "test", "test", 
                                            "test", "test", "test", "test", "test", "test", "test", "test", 
                                            "test", "test", "test", "test", "test", "test", "test", "test", 
                                            "test", "test", "test", "test", "test", "test", "test", "test", 
                                            "test", "test", "test", "test", "test", "test", "test", "test", 
                                            "test", "test", "test", "test", "test", "test", "test", "test", 
                                            "test", "test", "test", "test", "test", "test", "test", "test", 
                                            "test", "test", "test", "test", "test", "test", "test", "test", 
                                            "test", "test", "test", "test", "test", "test", "test", "test", 
                                            "test", "test", "test", "test", "test", "test", "test", "test", 
                                            "test", "test", "test", "test", "test", "test", "test", "test", 
                                            "test", "test", "test", "test", "test", "test", "test", "test", 
                                            "test", "test", "test", "test", "test", "test", "test", "test", 
                                            "test", "test"), values = c(101.381279076347, 115.024814126526, 
                                                                        116.283770505484, 143.388940411906, 151.999901025432, 156.622212688133, 
                                                                        167.971909254246, 167.975928256918, 185.656196551564, 200.721262428664, 
                                                                        205.884701802087, 208.609431664086, 224.04854677466, 226.537748300792, 
                                                                        243.569052436479, 244.710723946974, 253.313105443309, 255.654255064006, 
                                                                        256.359903718326, 262.435560596752, 267.370415184079, 270.70517136183, 
                                                                        274.136610226043, 282.036420276192, 282.806452569069, 284.1138771535, 
                                                                        291.787596806806, 305.646756553035, 337.178463545062, 339.383282271944, 
                                                                        347.809732933174, 358.842456281873, 379.882758161983, 391.689891691191, 
                                                                        397.703660359347, 428.193871395939, 432.205655027086, 437.114464360898, 
                                                                        439.392195483166, 460.618211569135, 467.356925953403, 474.030927247335, 
                                                                        485.513674168092, 495.634219469987, 512.519251787173, 514.224542733771, 
                                                                        514.693570299948, 516.410405342209, 522.300033712531, 522.919708208392, 
                                                                        530.770759111017, 534.912277770809, 537.303232086797, 545.537809758848, 
                                                                        558.77807435316, 560.885542387988, 566.719715116402, 569.057924070856, 
                                                                        585.900346431118, 585.996222387789, 586.154794672012, 593.290157437601, 
                                                                        593.886668852477, 595.464355965594, 595.552915631989, 597.267954078576, 
                                                                        599.209121404167, 603.928970154412, 614.47454397526, 616.415676088337, 
                                                                        618.47090677731, 618.704726202617, 620.299065852282, 621.221796533576, 
                                                                        624.639638497392, 626.772857447509, 628.334875566717, 639.094823663395, 
                                                                        639.968631215323, 640.789016031981, 647.86515793056, 647.90606038772, 
                                                                        652.948477648821, 656.370007099207, 668.866815705447, 669.578524490795, 
                                                                        671.671673436733, 673.24499857676, 677.790770668105, 683.301120546819, 
                                                                        685.405277371257, 688.388394984508, 688.756570941441, 688.928355456651, 
                                                                        689.825774057133, 694.109040353333, 699.003170218909, 699.391874564446, 
                                                                        701.845833489368, 702.156088188828, 703.414538846097, 703.717557990373, 
                                                                        707.133144940064, 711.32659908449, 712.600100190538, 713.90995895672, 
                                                                        715.18943768072, 716.348938943006, 716.452479473385, 716.789364537938, 
                                                                        720.115391933364, 723.22508691772, 724.356781810046, 725.061036296253, 
                                                                        728.995060434243, 729.024673785406, 730.057942590606, 732.696038067907, 
                                                                        733.87118848259, 735.23204178383, 735.36325187445, 735.620389740959, 
                                                                        737.400789595488, 739.196780226085, 740.760038601803, 742.625751092172, 
                                                                        744.387471223431, 745.43692260011, 748.377605076564, 748.483378405703, 
                                                                        748.946473385801, 749.239269710773, 749.570767803624, 750.753430715183, 
                                                                        750.866658078687, 754.505701797762, 754.708816582747, 754.778611042176, 
                                                                        758.918637545384, 763.240353751243, 763.594295187488, 764.227214040214, 
                                                                        764.675009493987, 767.923202337959, 769.104199280041, 769.198002782303, 
                                                                        770.864258217758, 771.245068528652, 775.68458268862, 776.191355091034, 
                                                                        776.622660094456, 778.439680566319, 780.61257973965, 782.686294143935, 
                                                                        783.833622168979, 785.622718474961, 785.664705633629, 789.089835284999, 
                                                                        789.368815915373, 789.781550602816, 790.720630614129, 791.133909192817, 
                                                                        793.286182889057, 793.62319861646, 793.799199543982, 793.983661259417, 
                                                                        794.099164443092, 795.781476129251, 797.730174228802, 797.908767812294, 
                                                                        798.09320876846, 798.549811012831, 798.853496608602, 801.915271342597, 
                                                                        803.043973304202, 803.985054914929, 808.251411531591, 809.211301815105, 
                                                                        811.58898968812, 811.762560229475, 812.555187886039, 812.912001783236, 
                                                                        812.940676870946, 813.701510658822, 816.127151208242, 816.244124834601, 
                                                                        818.916770111271, 819.004826073983, 821.299415351309, 821.354965651233, 
                                                                        822.148527247848, 822.313774028719, 822.613220920305, 822.940756774194, 
                                                                        823.111669486417, 823.370443637396, 823.869797658288, 824.116925771577, 
                                                                        824.526693430928, 825.452425511469, 825.78366595808, 825.929640767449, 
                                                                        827.575015784075, 827.624516536892, 828.749517809072, 830.110624046318, 
                                                                        832.013989616188, 832.404615177555, 833.383643671484, 833.655225531589, 
                                                                        834.384866865168, 835.018567502374, 835.057038015697, 835.630439813405, 
                                                                        836.449892710687, 837.459634311606, 837.589503405449, 838.918908336881, 
                                                                        838.960900051588, 839.991529791905, 841.773647578269, 842.629964815763, 
                                                                        842.788390345861, 844.252591847587, 845.511390862768, 845.946854348749, 
                                                                        847.116378007613, 847.198322859833, 848.053391670685, 849.461118597542, 
                                                                        850.46684253868, 850.909229431484, 851.738321960513, 852.721710145407, 
                                                                        855.297924582755, 855.486209558917, 856.635743527592, 857.022899241954, 
                                                                        857.238178176625, 857.391579692183, 858.200908962615, 858.961211156616, 
                                                                        860.21232127793, 860.307145421317, 861.289715410476, 861.834001874589, 
                                                                        862.09832139584, 862.238839074866, 863.819888772296, 863.931573757037, 
                                                                        865.791646068525, 867.297260794333, 867.972987049342, 868.422309130561, 
                                                                        868.77022856648, 868.884700108128, 869.164033347798, 870.17492018175, 
                                                                        871.826570668779, 872.979335904177, 872.997131769676, 873.26620715233, 
                                                                        874.098208163662, 874.45425208402, 876.818697093937, 877.486917892597, 
                                                                        878.821527552586, 879.733135117064, 880.266147317443, 882.866928873962, 
                                                                        883.628741289641, 884.253810920936, 884.587458463697, 886.70477378607, 
                                                                        886.750231890577, 887.302929102579, 887.588720623001, 887.589151296421, 
                                                                        888.16114158397, 888.415649328028, 888.721881555583, 890.10961499265, 
                                                                        890.279353233173, 892.303491966205, 893.497868151659, 893.657480565551, 
                                                                        894.225420638859, 894.285149677904, 895.423961897341, 897.631224882414, 
                                                                        898.347048276826, 90, 91.5, 93, 94.5, 96, 97.5, 99, 100.5, 102, 
                                                                        103.5, 105)), row.names = c(NA, -302L), groups = structure(list(
                                                                          parameter = "test", .rows = structure(list(1:302), ptype = integer(0), class = c("vctrs_list_of", 
                                                                                                                                                           "vctrs_vctr", "list"))), row.names = c(NA, -1L), .drop = TRUE, class = c("tbl_df", 
                                                                                                                                                                                                                                    "tbl", "data.frame")), class = c("grouped_df", "tbl_df", "tbl", 
                                                                                                                                                                                                                                                                     "data.frame"))
  # # Extend the top whisker a bit:
  # sample_df$values[1:100] <- 701:800
  # # Make sure there's only 1 lower outlier:
  # sample_df$values[1] <- -350
  # 
  # Function to calculate important values:
  ggplot2_boxplot <- function(x){
    
    quartiles <- as.numeric(quantile(x,
                                     probs = c(0.25, 0.5, 0.75)))
    
    names(quartiles) <- c("25th percentile - three quarters of contracts \nbring back at least this percentage of low risk patients",
                          "50th percentile\n(median)",
                          "75th percentile - one quarter of contracts \nbring back at least this percentage of low risk patients")
    
    IQR <- diff(quartiles[c(1,3)])
    

    upper_dots <- x[x > (quartiles[3] + 1.5*IQR)]
    lower_dots <- x[x < (quartiles[1] - 1.5*IQR)][1]
    
    return(list("quartiles" = quartiles,
                "25th percentile - three quarters of contracts \nbring back at least this percentage of low risk patients" = as.numeric(quartiles[1]),
                "50th percentile\n(median)" = as.numeric(quartiles[2]),
                "75th percentile - one quarter of contracts \nbring back at least this percentage of low risk patients" = as.numeric(quartiles[3]),
                "IQR" = IQR,
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
    # stat_boxplot(data = sample_df,
    #              aes(x = parameter, y=values),
    #              geom ='errorbar', width = 0.3) +
    geom_boxplot(data = sample_df,
                 aes(x = parameter, y=values),
                 width = 0.3, fill = "lightgrey") +
    # geom_text(aes(x = 1, y = 950, label = "500"), hjust = 0.5) +
    # geom_text(aes(x = 1.17, y = 950,
    #               label = "Number of values"),
    #           fontface = "bold", vjust = 0.4) +
    theme_minimal(base_size = 5, base_family = family) +
    geom_segment(aes(x = 3.7, xend = 3.7,
                     y = ggplot_output[["25th percentile - three quarters of contracts \nbring back at least this percentage of low risk patients"]],
                     yend = ggplot_output[["75th percentile - one quarter of contracts \nbring back at least this percentage of low risk patients"]])) +
    geom_segment(aes(x = 1.2, xend = 3.7,
                     y = ggplot_output[["25th percentile - three quarters of contracts \nbring back at least this percentage of low risk patients"]],
                     yend = ggplot_output[["25th percentile - three quarters of contracts \nbring back at least this percentage of low risk patients"]])) +
    geom_segment(aes(x = 1.2, xend = 3.7,
                     y = ggplot_output[["75th percentile - one quarter of contracts \nbring back at least this percentage of low risk patients"]],
                     yend = ggplot_output[["75th percentile - one quarter of contracts \nbring back at least this percentage of low risk patients"]])) +
    geom_text(aes(x = 3, y = ggplot_output[["50th percentile\n(median)"]]),
              label = "Interquartile\nrange", fontface = "bold",
              vjust = 0.85) +
    # geom_text(aes(x = c(1.17,1.17),
    #               y = c(ggplot_output[["upper_whisker"]],
    #                     ggplot_output[["lower_whisker"]]),
    #               label = c("Largest value within 1.5 times\ninterquartile range above\n75th percentile",
    #                         "Smallest value within 1.5 times\ninterquartile range below\n25th percentile")),
    #           fontface = "bold", vjust = 0.9) +
    geom_text(aes(x = c(1.17),
                  y =  ggplot_output[["lower_dots"]],
                  label = "Outside values"),
              vjust = 0.0, fontface = "bold") +
    geom_text(aes(x = c(1.9),
                  y =  ggplot_output[["lower_dots"]],
                  label = "- Indicator that there is a relatively"),
              vjust = 0.5) +
    geom_text(aes(x = 1.17,
                  y = ggplot_output[["lower_dots"]],
                  label = "high number of contracts that recall \nsignificantly lower proportions of low risk patients \nwithin a year"),
              vjust = 1.1) +
    geom_label(aes(x = 1.17, y = ggplot_output[["quartiles"]],
                   label = names(ggplot_output[["quartiles"]])),
               vjust = c(0.5,0.8,0.3),
               fill = "white", label.size = 0) +
    ylab("") + xlab("") +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          aspect.ratio = 4/3,
          plot.title = element_text(hjust = 0.5, size = 10)) +
    coord_cartesian(xlim = c(1.4,3.1), ylim = c(-600, 900)) +
    labs(title = "Boxplot Explanation")

  explain_plot
}




# ggplot_box_legend <- function(family = "serif"){
#   
#   # Create data to use in the boxplot legend:
#   ##set.seed(100)
#   
  # sample_df <- data.frame(parameter = "test",
  #                         values = sample(500))
#   
#   # Extend the top whisker a bit:
#   sample_df$values[1:100] <- 701:800
#   # Make sure there's only 1 lower outlier:
#   sample_df$values[1] <- -350
#   
#   # Function to calculate important values:
#   ggplot2_boxplot <- function(x){
#     
#     quartiles <- as.numeric(quantile(x,
#                                      probs = c(0.25, 0.5, 0.75)))
#     
#     names(quartiles) <- c("25th percentile - three quarters of contracts \nbring back at least this percentage of low risk patients",
#                           "50th percentile\n(median)",
#                           "75th percentile - one quarter of contracts \nbring back at least this percentage of low risk patients")
#     
#     IQR <- diff(quartiles[c(1,3)])
#     
#     upper_whisker <- max(x[x < (quartiles[3] + 1.5 * IQR)])
#     lower_whisker <- min(x[x > (quartiles[1] - 1.5 * IQR)])
#     
#     upper_dots <- x[x > (quartiles[3] + 1.5*IQR)]
#     lower_dots <- x[x < (quartiles[1] - 1.5*IQR)]
#     
#     return(list("quartiles" = quartiles,
#                 "25th percentile - three quarters of contracts \nbring back at least this percentage of low risk patients" = as.numeric(quartiles[1]),
#                 "50th percentile\n(median)" = as.numeric(quartiles[2]),
#                 "75th percentile - one quarter of contracts \nbring back at least this percentage of low risk patients" = as.numeric(quartiles[3]),
#                 "IQR" = IQR,
#                 "upper_whisker" = upper_whisker,
#                 "lower_whisker" = lower_whisker,
#                 "upper_dots" = upper_dots,
#                 "lower_dots" = lower_dots))
#   }
#   
#   # Get those values:
#   ggplot_output <- ggplot2_boxplot(sample_df$values)
#   
#   # Lots of text in the legend, make it smaller and consistent font:
#   update_geom_defaults("text",
#                        list(size = 3,
#                             hjust = 0,
#                             family = family))
#   # Labels don't inherit text:
#   update_geom_defaults("label",
#                        list(size = 3,
#                             hjust = 0,
#                             family = family))
#   
#   # Create the legend:
#   # The main elements of the plot (the boxplot, error bars, and count)
#   # are the easy part.
#   # The text describing each of those takes a lot of fiddling to
#   # get the location and style just right:
#   explain_plot <- ggplot() +
#     stat_boxplot(data = sample_df,
#                  aes(x = parameter, y=values),
#                  geom ='errorbar', width = 0.3) +
#     geom_boxplot(data = sample_df,
#                  aes(x = parameter, y=values),
#                  width = 0.3, fill = "lightgrey") +
#     # geom_text(aes(x = 1, y = 950, label = "500"), hjust = 0.5) +
#     # geom_text(aes(x = 1.17, y = 950,
#     #               label = "Number of values"),
#     #           fontface = "bold", vjust = 0.4) +
#     theme_minimal(base_size = 5, base_family = family) +
#     geom_segment(aes(x = 3.7, xend = 3.7,
#                      y = ggplot_output[["25th percentile - three quarters of contracts \nbring back at least this percentage of low risk patients"]],
#                      yend = ggplot_output[["75th percentile - one quarter of contracts \nbring back at least this percentage of low risk patients"]])) +
#     geom_segment(aes(x = 1.2, xend = 3.7,
#                      y = ggplot_output[["25th percentile - three quarters of contracts \nbring back at least this percentage of low risk patients"]],
#                      yend = ggplot_output[["25th percentile - three quarters of contracts \nbring back at least this percentage of low risk patients"]])) +
#     geom_segment(aes(x = 1.2, xend = 3.7,
#                      y = ggplot_output[["75th percentile - one quarter of contracts \nbring back at least this percentage of low risk patients"]],
#                      yend = ggplot_output[["75th percentile - one quarter of contracts \nbring back at least this percentage of low risk patients"]])) +
#     geom_text(aes(x = 3, y = ggplot_output[["50th percentile\n(median)"]]),
#               label = "Interquartile\nrange", fontface = "bold",
#               vjust = 0.4) +
#     geom_text(aes(x = c(1.17,1.17),
#                   y = c(ggplot_output[["upper_whisker"]],
#                         ggplot_output[["lower_whisker"]]),
#                   label = c("Largest value within 1.5 times\ninterquartile range above\n75th percentile",
#                             "Smallest value within 1.5 times\ninterquartile range below\n25th percentile")),
#               fontface = "bold", vjust = 0.9) +
#     geom_text(aes(x = c(1.17),
#                   y =  ggplot_output[["lower_dots"]],
#                   label = "Outside value"),
#               vjust = 0.5, fontface = "bold") +
#     geom_text(aes(x = c(1.9),
#                   y =  ggplot_output[["lower_dots"]],
#                   label = "- Indicator that there is a relatively"),
#               vjust = 0.5) +
#     geom_text(aes(x = 1.17,
#                   y = ggplot_output[["lower_dots"]],
#                   label = "higher number of contracts that recall \nsignificantly lower proportions of low risk patients \nwithin a year"),
#               vjust = 1.5) +
#     geom_label(aes(x = 1.17, y = ggplot_output[["quartiles"]],
#                    label = names(ggplot_output[["quartiles"]])),
#                vjust = c(0.4,0.5,0.4),
#                fill = "white", label.size = 0) +
#     ylab("") + xlab("") +
#     theme(axis.text = element_blank(),
#           axis.ticks = element_blank(),
#           panel.grid = element_blank(),
#           aspect.ratio = 4/3,
#           plot.title = element_text(hjust = 0.5, size = 10)) +
#     coord_cartesian(xlim = c(1.4,3.1), ylim = c(-600, 900)) +
#     labs(title = "Boxplot Explanation")
#   
#   return(explain_plot)
#   
# }


