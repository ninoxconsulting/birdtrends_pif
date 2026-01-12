
# this script is to regenerate the plot level species outputs 
# they take the outputs and recreate the plots required size 

# note need to rerun the data first (10_pif_smooths_workflow and 10_pif_audobon.R)
# note audobon version needs to review the input1 data type for plots 



###########################################################
# update species specific plots 

library(jpeg)
library(magrittr)
library(dplyr)
library(stringr)
library(ggplot2)
library(birdtrends)


paper_filepath <- "C:/Users/genev/OneDrive/Documents/02.Contracts/2024_ECCC_birdtrends/06.Journal_paper/Paper_tables_figures"

files <- list.files(file.path("02_outputs"), recursive = T, pattern = "outputs.rds")



# audobon 
foi <- c("5250", "5260", "5530", "3760")
files <- files[str_detect(files, paste(foi,collapse = "|"))]


for(i in files){
 
  print(i)
 # i = files[1]
  
  filename <- str_sub(i, 1,4)
  
  # extract componts to plot 
  aa <-  readRDS(file.path("02_outputs", i))
  
  fitted_data <- aa$fitted_smooths
  preds_sm <- aa$pred_sm
  index_baseline <- aa$index_baseline
  input1 <- aa$input1
  common_name <- aa$targ$english
  
  if(filename %in% foi) {
    end_yr = 2021
  } else {
    end_yr = 2022
  }
  
  
  # sm_plots with targets 
  sm_plot_target <- plot_trend(raw_indices = input1 , 
                               model_indices = fitted_data, 
                               pred_indices = preds_sm,
                               start_yr = 2014, 
                               end_yr = end_yr,  
                               ref_yr = 2014,
                               targets = index_baseline,
                               set_upperlimit = TRUE,
                               upperlimit = 1.5,
                               annual_variation = FALSE)
  
  p <- sm_plot_target + 
    theme(
      plot.caption.position = "plot",
      axis.text = element_text(size=25),
      axis.title = element_text(size=25),
      plot.title = element_text(size=30)
      )+
     scale_x_continuous(expand = c(0.03, 0.1),
                        breaks=seq(1980,2040,20),
                        limits=c(1968, 2050)) +
    ggplot2::labs(title = common_name)
  
   # p
  
  ggsave(plot = p, 
         file.path(paper_filepath, "species_plots", paste0(common_name,"_single_plot.jpg")),
         width = 25,
         height = 20,
         units = c("cm"),
         dpi = 300)
  
  
}
  

#############################################################################

# Figure 4 

files <- list.files(file.path("02_outputs"), recursive = T, pattern = "outputs.rds")

#foi <- paste(c("6860", "4060", "6580", "6420"), collapse = '|')
#a) 6860
#b) 4060
#c) 6580
#d) 6420

# canada warbler
i <- "6860/6860_outputs.rds"
  filename <- str_sub(i, 1, 4)
  # extract componts to plot
  aa <- readRDS(file.path("02_outputs", i))

  fitted_data <- aa$fitted_smooths
  preds_sm <- aa$pred_sm
  index_baseline <- aa$index_baseline
  input1 <- aa$input1
  common_name <- aa$targ$english

  # sm_plots with targets
  sm_plot_target <- plot_trend(
    raw_indices = input1,
    model_indices = fitted_data,
    pred_indices = preds_sm,
    start_yr = 2014,
    end_yr = 2021,
    ref_yr = 2014,
    targets = index_baseline,
    set_upperlimit = TRUE,
    upperlimit = 1.5,
    annual_variation = FALSE
  )

  pa <- sm_plot_target +
    theme(
      plot.caption.position = "plot",
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 12),
      axis.title.x = element_blank(),
      plot.title = element_text(size = 12)
    ) #+
  #  scale_x_continuous(
  #    expand = c(0.03, 0.1),
  #    breaks = seq(1980, 2040, 20),
  #    limits = c(1968, 2050)
  #  ) #+
   # ggplot2::labs(title = common_name)


  
#B Red headed woodpeckert  
  i <- "4060/4060_outputs.rds"
  filename <- str_sub(i, 1, 4)
  # extract componts to plot
  aa <- readRDS(file.path("02_outputs", i))
  
  fitted_data <- aa$fitted_smooths
  preds_sm <- aa$pred_sm
  index_baseline <- aa$index_baseline
  input1 <- aa$input1
  common_name <- aa$targ$english
  
  # sm_plots with targets
  sm_plot_target <- plot_trend(
    raw_indices = input1,
    model_indices = fitted_data,
    pred_indices = preds_sm,
    start_yr = 2014,
    end_yr = 2021,
    ref_yr = 2014,
    targets = index_baseline,
    set_upperlimit = TRUE,
    upperlimit = 1.5,
    annual_variation = FALSE
  )
  
  pb <- sm_plot_target +
    theme(
      plot.caption.position = "plot",
      axis.text = element_text(size = 12),
      #axis.title = element_text(size = 12),
      axis.title = element_blank(),
      plot.title = element_text(size = 12)
    ) #+
  #  scale_x_continuous(
  #    expand = c(0.03, 0.1),
  #    breaks = seq(1980, 2040, 20),
  #    limits = c(1968, 2050)
  #  ) #+
   # ggplot2::labs(title = common_name)
  

  
  #C Cerulean warbler 
  i <- "6580/6580_outputs.rds"
  filename <- str_sub(i, 1, 4)
  # extract componts to plot
  aa <- readRDS(file.path("02_outputs", i))
  
  fitted_data <- aa$fitted_smooths
  preds_sm <- aa$pred_sm
  index_baseline <- aa$index_baseline
  input1 <- aa$input1
  common_name <- aa$targ$english
  
  # sm_plots with targets
  sm_plot_target <- plot_trend(
    raw_indices = input1,
    model_indices = fitted_data,
    pred_indices = preds_sm,
    start_yr = 2014,
    end_yr = 2021,
    ref_yr = 2014,
    targets = index_baseline,
    set_upperlimit = TRUE,
    upperlimit = 1.5,
    annual_variation = FALSE
  )
  
  pc <- sm_plot_target +
    theme(
      plot.caption.position = "plot",
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 12),
      plot.title = element_text(size = 12)
    ) +
    scale_x_continuous(
      expand = c(0.03, 0.1),
      breaks = seq(1980, 2040, 20),
      limits = c(1968, 2050)
    )# +
    #ggplot2::labs(title = paste0 ("a) ",common_name))
  
  
  
  #D Goldern winged  warbler 
  i <- "6420/6420_outputs.rds"
  filename <- str_sub(i, 1, 4)
  # extract componts to plot
  aa <- readRDS(file.path("02_outputs", i))
  
  fitted_data <- aa$fitted_smooths
  preds_sm <- aa$pred_sm
  index_baseline <- aa$index_baseline
  input1 <- aa$input1
  common_name <- aa$targ$english
  
  # sm_plots with targets
  sm_plot_target <- plot_trend(
    raw_indices = input1,
    model_indices = fitted_data,
    pred_indices = preds_sm,
    start_yr = 2014,
    end_yr = 2021,
    ref_yr = 2014,
    targets = index_baseline,
    set_upperlimit = TRUE,
    upperlimit = 1.5,
    annual_variation = FALSE
  )
  
  pd <- sm_plot_target +
    theme(
      plot.caption.position = "plot",
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 12),
      plot.title = element_text(size = 12),
      axis.title.y = element_blank(),
    ) +
    scale_x_continuous(
      expand = c(0.03, 0.1),
      breaks = seq(1980, 2040, 20),
      limits = c(1968, 2050)
    ) #+
    #ggplot2::labs(title = common_name)
  
  
  pa
  pb
  pc
  pd  
  #install.packages("cowplot")
  
  library("cowplot")
  fig4<- ggdraw() +
    draw_plot(pa, x = 0, y = .5, width = .5, height = .5) +
    draw_plot(pb, x = .5, y = .5, width = .5, height = .5) +
    draw_plot(pc, x = 0, y = 0, width = 0.5, height = 0.5) +
    draw_plot(pd, x = 0.5, y = 0, width = 0.5, height = 0.5) +
    draw_plot_label(label = c("a) Canada Warbler", " b) Red-headed Woodpecker", "c) Cerulean Warbler", "d) Goldern-winged Warbler"), size = 14,
                    x = c(0.05, 0.5, 0.05, 0.5), y = c(0.99, 0.99, 0.49, 0.49))    

  
  fig4
  
  ggsave(plot =  fig4, 
         file.path(paper_filepath, "Figure4.jpg"),
         width = 25,
         height = 20,
         units = c("cm"),
         dpi = 300)
  
  
  
  ########################################################################
  ## figure 5 
  
  files <- list.files(file.path("02_outputs"), recursive = T, pattern = "outputs.rds")

  # Pinyon Jay 
  i <- "4920/4920_outputs.rds"
  filename <- str_sub(i, 1, 4)
  # extract componts to plot
  aa <- readRDS(file.path("02_outputs", i))
  
  fitted_data <- aa$fitted_smooths
  preds_sm <- aa$pred_sm
  index_baseline <- aa$index_baseline
  input1 <- aa$input1
  common_name <- aa$targ$english
  
  # sm_plots with targets
  sm_plot_target <- plot_trend(
    raw_indices = input1,
    model_indices = fitted_data,
    pred_indices = preds_sm,
    start_yr = 2014,
    end_yr = 2021,
    ref_yr = 2014,
    targets = index_baseline,
    set_upperlimit = TRUE,
    upperlimit = 1.5,
    annual_variation = FALSE
  )
  
  pa <- sm_plot_target +
    theme(
      plot.caption.position = "plot",
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 12),
      #axis.title.x = element_blank(),
      plot.title = element_text(size = 12)
    ) #+
  #  scale_x_continuous(
  #    expand = c(0.03, 0.1),
  #    breaks = seq(1980, 2040, 20),
  #    limits = c(1968, 2050)
  #  ) #+
  # ggplot2::labs(title = common_name)
  
  
  
  #B Kentucky Warbler 
  i <- "6770/6770_outputs.rds"
  filename <- str_sub(i, 1, 4)
  # extract componts to plot
  aa <- readRDS(file.path("02_outputs", i))
  
  fitted_data <- aa$fitted_smooths
  preds_sm <- aa$pred_sm
  index_baseline <- aa$index_baseline
  input1 <- aa$input1
  common_name <- aa$targ$english
  
  # sm_plots with targets
  sm_plot_target <- plot_trend(
    raw_indices = input1,
    model_indices = fitted_data,
    pred_indices = preds_sm,
    start_yr = 2014,
    end_yr = 2021,
    ref_yr = 2014,
    targets = index_baseline,
    set_upperlimit = TRUE,
    upperlimit = 1.5,
    annual_variation = FALSE
  )
  
  pb <- sm_plot_target +
    theme(
      plot.caption.position = "plot",
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 12),
      axis.title.y = element_blank(),
      plot.title = element_text(size = 12)
    ) #+
  #  scale_x_continuous(
  #    expand = c(0.03, 0.1),
  #    breaks = seq(1980, 2040, 20),
  #    limits = c(1968, 2050)
  #  ) #+
  # ggplot2::labs(title = common_name)
  
  
  
  #C Black swift 
  i <- "4200/4200_outputs.rds"
  filename <- str_sub(i, 1, 4)
  # extract componts to plot
  aa <- readRDS(file.path("02_outputs", i))
  
  fitted_data <- aa$fitted_smooths
  preds_sm <- aa$pred_sm
  index_baseline <- aa$index_baseline
  input1 <- aa$input1
  common_name <- aa$targ$english
  
  # sm_plots with targets
  sm_plot_target <- plot_trend(
    raw_indices = input1,
    model_indices = fitted_data,
    pred_indices = preds_sm,
    start_yr = 2014,
    end_yr = 2021,
    ref_yr = 2014,
    targets = index_baseline,
    set_upperlimit = TRUE,
    upperlimit = 1.5,
    annual_variation = FALSE
  )
  
  pc <- sm_plot_target +
    theme(
      plot.caption.position = "plot",
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 12),
      plot.title = element_text(size = 12)
    ) +
    scale_x_continuous(
      expand = c(0.03, 0.1),
      breaks = seq(1980, 2040, 20),
      limits = c(1968, 2050)
    )# +
  #ggplot2::labs(title = paste0 ("a) ",common_name))
  

  pa
  pb
  pc
  
  #install.packages("cowplot")
  
  library("cowplot")
  fig5<- ggdraw() +
    draw_plot(pa, x = 0, y = .5, width = .5, height = .5) +
    draw_plot(pb, x = .5, y = .5, width = .5, height = .5) +
    draw_plot(pc, x = 0, y = 0, width = 0.5, height = 0.5) +
    #draw_plot(pd, x = 0.5, y = 0, width = 0.5, height = 0.5) +
    draw_plot_label(label = c("a) Pinyon Jay", " b) Kentucky Warbler", "c) Black Swift"), size = 14,
                    x = c(0.05, 0.5, 0.05), y = c(0.99, 0.99, 0.49))    
  
  fig5
  
  ggsave(plot =  fig5, 
         file.path(paper_filepath, "Figure5.jpg"),
         width = 25,
         height = 20,
         units = c("cm"),
         dpi = 300)
  
############################################################################################  
  
  
  # Figure 8
  
  files <- list.files(file.path("02_outputs"), recursive = T, pattern = "outputs.rds")
  
  # rufous humminbird 
  i <- "4330/4330_outputs.rds"
  filename <- str_sub(i, 1, 4)
  # extract componts to plot
  aa <- readRDS(file.path("02_outputs", i))
  
  fitted_data <- aa$fitted_smooths
  preds_sm <- aa$pred_sm
  index_baseline <- aa$index_baseline
  input1 <- aa$input1
  common_name <- aa$targ$english
  
  # sm_plots with targets
  sm_plot_target <- plot_trend(
    raw_indices = input1,
    model_indices = fitted_data,
    pred_indices = preds_sm,
    start_yr = 2014,
    end_yr = 2021,
    ref_yr = 2014,
    targets = index_baseline,
    set_upperlimit = TRUE,
    upperlimit = 1.5,
    annual_variation = FALSE
  )
  
  pa <- sm_plot_target +
    theme(
      plot.caption.position = "plot",
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 12),
      axis.title.x = element_blank(),
      plot.title = element_text(size = 12)
    ) #+
  #  scale_x_continuous(
  #    expand = c(0.03, 0.1),
  #    breaks = seq(1980, 2040, 20),
  #    limits = c(1968, 2050)
  #  ) #+
  # ggplot2::labs(title = common_name)
  
  
  
  #B Bobolink
  i <- "4940/4940_outputs.rds"
  filename <- str_sub(i, 1, 4)
  # extract componts to plot
  aa <- readRDS(file.path("02_outputs", i))
  
  fitted_data <- aa$fitted_smooths
  preds_sm <- aa$pred_sm
  index_baseline <- aa$index_baseline
  input1 <- aa$input1
  common_name <- aa$targ$english
  
  # sm_plots with targets
  sm_plot_target <- plot_trend(
    raw_indices = input1,
    model_indices = fitted_data,
    pred_indices = preds_sm,
    start_yr = 2014,
    end_yr = 2021,
    ref_yr = 2014,
    targets = index_baseline,
    set_upperlimit = TRUE,
    upperlimit = 1.5,
    annual_variation = FALSE
  )
  
  pb <- sm_plot_target +
    theme(
      plot.caption.position = "plot",
      axis.text = element_text(size = 12),
      #axis.title = element_text(size = 12),
      axis.title = element_blank(),
      plot.title = element_text(size = 12)
    ) #+
  #  scale_x_continuous(
  #    expand = c(0.03, 0.1),
  #    breaks = seq(1980, 2040, 20),
  #    limits = c(1968, 2050)
  #  ) #+
  # ggplot2::labs(title = common_name)
  
  
  
  #C Le conte sparrow 
  i <- "5480/5480_outputs.rds"
  filename <- str_sub(i, 1, 4)
  # extract componts to plot
  aa <- readRDS(file.path("02_outputs", i))
  
  fitted_data <- aa$fitted_smooths
  preds_sm <- aa$pred_sm
  index_baseline <- aa$index_baseline
  input1 <- aa$input1
  common_name <- aa$targ$english
  
  # sm_plots with targets
  sm_plot_target <- plot_trend(
    raw_indices = input1,
    model_indices = fitted_data,
    pred_indices = preds_sm,
    start_yr = 2014,
    end_yr = 2021,
    ref_yr = 2014,
    targets = index_baseline,
    set_upperlimit = TRUE,
    upperlimit = 1.5,
    annual_variation = FALSE
  )
  
  pc <- sm_plot_target +
    theme(
      plot.caption.position = "plot",
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 12),
      plot.title = element_text(size = 12)
    ) +
    scale_x_continuous(
      expand = c(0.03, 0.1),
      breaks = seq(1980, 2040, 20),
      limits = c(1968, 2050)
    )# +
  #ggplot2::labs(title = paste0 ("a) ",common_name))
  
  
  
  #D Greater Sage grouse
  i <- "3090/3090_outputs.rds"
  filename <- str_sub(i, 1, 4)
  # extract componts to plot
  aa <- readRDS(file.path("02_outputs", i))
  
  fitted_data <- aa$fitted_smooths
  preds_sm <- aa$pred_sm
  index_baseline <- aa$index_baseline
  input1 <- aa$input1
  common_name <- aa$targ$english
  
  # sm_plots with targets
  sm_plot_target <- plot_trend(
    raw_indices = input1,
    model_indices = fitted_data,
    pred_indices = preds_sm,
    start_yr = 2014,
    end_yr = 2021,
    ref_yr = 2014,
    targets = index_baseline,
    set_upperlimit = TRUE,
    upperlimit = 1.5,
    annual_variation = FALSE
  )
  
  pd <- sm_plot_target +
    theme(
      plot.caption.position = "plot",
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 12),
      plot.title = element_text(size = 12),
      axis.title.y = element_blank(),
    ) +
    scale_x_continuous(
      expand = c(0.03, 0.1),
      breaks = seq(1980, 2040, 20),
      limits = c(1968, 2050)
    ) #+
  #ggplot2::labs(title = common_name)
  
  
  pa
  pb
  pc
  pd  
  #install.packages("cowplot")
  
  library("cowplot")
  fig8<- ggdraw() +
    draw_plot(pa, x = 0, y = .5, width = .5, height = .5) +
    draw_plot(pb, x = .5, y = .5, width = .5, height = .5) +
    draw_plot(pc, x = 0, y = 0, width = 0.5, height = 0.5) +
    draw_plot(pd, x = 0.5, y = 0, width = 0.5, height = 0.5) +
    draw_plot_label(label = c("a) Rufous Hummingbird", " b) Bobolink", "c) LeConte's Sparrow", "d) Greater Sage-Grouse"), size = 14,
                    x = c(0.05, 0.55, 0.05, 0.5), y = c(0.99, 0.99, 0.49, 0.49))    
  fig8
  
  ggsave(plot =  fig8, 
         file.path(paper_filepath, "Figure8.jpg"),
         width = 25,
         height = 20,
         units = c("cm"),
         dpi = 300)
  
  
  
  
  ############################################################################
  
  # figure 10
  
  files <- list.files(file.path("02_outputs"), recursive = T, pattern = "outputs.rds")
  
  # Cassin's Finch 
  i <- "5180/5180_outputs.rds"
  filename <- str_sub(i, 1, 4)
  # extract componts to plot
  aa <- readRDS(file.path("02_outputs", i))
  
  fitted_data <- aa$fitted_smooths
  preds_sm <- aa$pred_sm
  index_baseline <- aa$index_baseline
  input1 <- aa$input1
  common_name <- aa$targ$english
  
  # sm_plots with targets
  sm_plot_target <- plot_trend(
    raw_indices = input1,
    model_indices = fitted_data,
    pred_indices = preds_sm,
    start_yr = 2014,
    end_yr = 2021,
    ref_yr = 2014,
    targets = index_baseline,
    set_upperlimit = TRUE,
    upperlimit = 1.5,
    annual_variation = FALSE
  )
  
  pa <- sm_plot_target +
    theme(
      plot.caption.position = "plot",
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 12),
      #axis.title.x = element_blank(),
      plot.title = element_text(size = 12)
    ) #+
  #  scale_x_continuous(
  #    expand = c(0.03, 0.1),
  #    breaks = seq(1980, 2040, 20),
  #    limits = c(1968, 2050)
  #  ) #+
  # ggplot2::labs(title = common_name)
  
  
  
  #B Eastern Whip-poor-will

  i <- "4171/4171_outputs.rds"
  filename <- str_sub(i, 1, 4)
  # extract componts to plot
  aa <- readRDS(file.path("02_outputs", i))
  
  fitted_data <- aa$fitted_smooths
  preds_sm <- aa$pred_sm
  index_baseline <- aa$index_baseline
  input1 <- aa$input1
  common_name <- aa$targ$english
  
  # sm_plots with targets
  sm_plot_target <- plot_trend(
    raw_indices = input1,
    model_indices = fitted_data,
    pred_indices = preds_sm,
    start_yr = 2014,
    end_yr = 2021,
    ref_yr = 2014,
    targets = index_baseline,
    set_upperlimit = TRUE,
    upperlimit = 1.5,
    annual_variation = FALSE
  )
  
  pb <- sm_plot_target +
    theme(
      plot.caption.position = "plot",
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 12),
      axis.title.y = element_blank(),
      plot.title = element_text(size = 12)
    ) #+
  #  scale_x_continuous(
  #    expand = c(0.03, 0.1),
  #    breaks = seq(1980, 2040, 20),
  #    limits = c(1968, 2050)
  #  ) #+
  # ggplot2::labs(title = common_name)
  pa
  pb
# 
#   fig10<- ggdraw() +
#     draw_plot(pa, x = 0, y = 0, width = .5, height = .5) +
#     draw_plot(pb, x = .5, y = 0, width = .5, height = .5) +
#     draw_plot_label(label = c("a) Cassin's Finch", " b) Eastern Whip-poor-will"), size = 14,
#                     x = c(0.05, 0.55), y = c(0.99, 0.99 ))    
#   
  
  fig10 <- plot_grid(pb, pa) +
    draw_plot_label(label = c("a) Eastern Whip-poor-will", " b) Cassin's Finch"), size = 14,
                    x = c(0.05, 0.55), y = c(0.99, 0.99 ))  
 
  ggsave(plot =  fig10, 
         file.path(paper_filepath, "Figure10.jpg"),
         width = 25,
         height = 15,
         units = c("cm"),
         dpi = 300)
  
  
  ############################################################################
  
  # figure 11
  
  files <- list.files(file.path("02_outputs"), recursive = T, pattern = "outputs.rds")
  
#Black-billed Cuckoo
  i <- "3880/3880_outputs.rds"
  filename <- str_sub(i, 1, 4)
  # extract componts to plot
  aa <- readRDS(file.path("02_outputs", i))
  
  fitted_data <- aa$fitted_smooths
  preds_sm <- aa$pred_sm
  index_baseline <- aa$index_baseline
  input1 <- aa$input1
  common_name <- aa$targ$english
  
  # sm_plots with targets
  sm_plot_target <- plot_trend(
    raw_indices = input1,
    model_indices = fitted_data,
    pred_indices = preds_sm,
    start_yr = 2014,
    end_yr = 2021,
    ref_yr = 2014,
    targets = index_baseline,
    set_upperlimit = TRUE,
    upperlimit = 1.5,
    annual_variation = FALSE
  )
  
  pa <- sm_plot_target +
    theme(
      plot.caption.position = "plot",
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 12),
      #axis.title.x = element_blank(),
      plot.title = element_text(size = 12)
    ) #+
  #  scale_x_continuous(
  #    expand = c(0.03, 0.1),
  #    breaks = seq(1980, 2040, 20),
  #    limits = c(1968, 2050)
  #  ) #+
  # ggplot2::labs(title = common_name)
  
  
  
  #B Rufous-winged Sparrow

  i <- "5790/5790_outputs.rds"
  filename <- str_sub(i, 1, 4)
  # extract componts to plot
  aa <- readRDS(file.path("02_outputs", i))
  
  fitted_data <- aa$fitted_smooths
  preds_sm <- aa$pred_sm
  index_baseline <- aa$index_baseline
  input1 <- aa$input1
  common_name <- aa$targ$english
  
  # sm_plots with targets
  sm_plot_target <- plot_trend(
    raw_indices = input1,
    model_indices = fitted_data,
    pred_indices = preds_sm,
    start_yr = 2014,
    end_yr = 2021,
    ref_yr = 2014,
    targets = index_baseline,
    set_upperlimit = TRUE,
    upperlimit = 1.5,
    annual_variation = FALSE
  )
  
  pb <- sm_plot_target +
    theme(
      plot.caption.position = "plot",
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 12),
      axis.title.y = element_blank(),
      plot.title = element_text(size = 12)
    ) #+
  #  scale_x_continuous(
  #    expand = c(0.03, 0.1),
  #    breaks = seq(1980, 2040, 20),
  #    limits = c(1968, 2050)
  #  ) #+
  # ggplot2::labs(title = common_name)
  pa
  pb
  
  fig11 <- plot_grid(pa, pb) +
    draw_plot_label(label = c(" a) Black-billed Cuckoo" ,"b) Rufous-winged Sparrow"), size = 14,
                    x = c(0.04, 0.49), y = c(0.99, 0.99 ))  
  
  fig11
  
  ggsave(plot =  fig11, 
         file.path(paper_filepath, "Figure11.jpg"),
         width = 25,
         height = 15,
         units = c("cm"),
         dpi = 300)
  
  
  
  