
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

# Figure 4 
#foi <- paste(c("6860", "4060", "6580", "6420"), collapse = '|')
#a) 6860
#b) 4060
#c) 6580
#d) 6420

# these are all fitted models 
#files <- files[str_detect(files, foi)]

for(i in files){
 
  i = files[1]
  
  filename <- str_sub(i, 1,4)
  
  # extract componts to plot 
  aa <-  readRDS(file.path("02_outputs", i))
  
  fitted_data <- aa$fitted_smooths
  preds_sm <- aa$pred_sm
  index_baseline <- aa$index_baseline
  input1 <- aa$input1
  common_name <- aa$targ$english
  
  # sm_plots with targets 
  sm_plot_target <- plot_trend(raw_indices = input1 , 
                               model_indices = fitted_data, 
                               pred_indices = preds_sm,
                               start_yr = 2014, 
                               end_yr = 2022,  # 
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
  
    p
  
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

# these are all fitted models 
#files <- files[str_detect(files, foi)]

  