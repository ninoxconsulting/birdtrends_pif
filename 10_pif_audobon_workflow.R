# workflow for pif data files # OBTAINED FROM aUDUBON 
# Note this data set is the baysian posterier distribution, not smooths as completed
# for other species. Hence different method used. 


library(dplyr)
library(readr)
library(janitor)
#devtools::install_github("ninoxconsulting/birdtrends")
library(bbsBayes2)
library(birdtrends)
library(ggplot2)

# notes files recieved directly from Audbon and downlowed on a locatl copy. A copy of the data 
# is saved on the main google drive. Due to the number f files this is not currently automated in the 
# script and can be done manually. 
## https://drive.google.com/drive/folders/1AwjONN_eTZP-IXmzKItjyYKS_NTA1psV
# 

outputs <- "02_outputs"

pifs <- read.csv("sp_key_bbs_full.csv") |> 
  filter(pif_rank %in% c("d", "r", "red")) |> 
  dplyr::select(aou, english, unid_combined, pif_rank, spcode,
                st_pop_pc_lower, st_pop_pc_uppper,
                lt_pop_pc_lower, lt_pop_pc_uppper, model_present) |> 
  filter(model_present == "Yv2")


# pif species list 
aous <- pifs$aou

# Step 1: 
# check if downloaded and if not download - see note above 



##########################################

aous <- sort(pifs$aou)

for(i in aous){
  
  i = aous[1]
  
  aou_id <- i
  
  ## prepare data inputs : example 1 
  
  aou_file <- paste0("C:/r_repo/2024_ECCC_birdtrends/birdtrends_pif/fitted_models/fit_",aou_id,".rds")
  
  if(file.exists(aou_file)) {
    
    dir <- file.path("outputs", i) 
    if (!dir.exists(dir)) dir.create(dir)
    
    # loading the fitted model object
    fit <- readRDS(aou_file)
    
    # Generate indicies (data options 1 and 2)
    inds <- generate_indices(fit, regions = "continent")
    
    # Generate indicies with smooth (data option 3) 
    
    indsmooth <- generate_indices(fit,regions = "continent",
                                  alternate_n = "n_smooth")
    
    plot_indices(inds)
    
    # Input option 1 - annual indices of relative abundance with CI estimates
    # data-frame with at a minimum, columns - we will use this for our plots 
    
    input1 <- inds$indices %>%
      select(year, index, index_q_0.025,index_q_0.975)%>%
      dplyr::mutate(yearn = as.integer(year) - min(as.integer(year)))  
    
    
    # Input option 3 - matrix of posterior draws (rows) for each year (columns)
    # representing the smoothed annual indices of relative abundance 
    input_option_3 <- indsmooth$samples$continent_continent
    
    
    ###########################################
    # 2. Generate trends for each data set 
    ###########################################
    
    indat3 <- as.data.frame(input_option_3)
    
    # fit the Heirachial GAM model using all years: 
    
    fitted_smooths <- fit_smooths(indat3, start_yr = NA, end_yr = NA)
    
    sel_bbssmooths <- fitted_smooths %>% 
      dplyr::slice_sample(., n = 100) %>%  
      dplyr::mutate(draw = seq(1, 100, 1)) %>% 
      tidyr::pivot_longer(., cols = !starts_with("d")) |> 
      dplyr::mutate(yearn = as.integer(name) - min(as.integer(name)))
    
    
    
    comp_plot_3 <- ggplot2::ggplot(data =  sel_bbssmooths,
                                   ggplot2::aes(x = yearn,y = value,
                                                group = draw, colour = draw))+
      ggplot2::geom_pointrange(data = input1,
                               ggplot2::aes(x = yearn, y = index,
                                            ymin = index_q_0.025,
                                            ymax = index_q_0.975),
                               inherit.aes = FALSE,
                               alpha = 0.3)+
      ggplot2::geom_line(alpha = 0.3)+
      ggplot2::scale_colour_viridis_c() +
      ggplot2::scale_y_continuous(trans = "log10")+
      ggplot2::theme_bw()
    
    comp_plot_3
    
    
    
    ######################################################################
    # 3. calculate trend 
    ##########################################################
    
    ldf_smooths <- tibble::rowid_to_column(fitted_smooths, "draw") %>%
      tidyr::pivot_longer(., cols = !starts_with("d")) %>%
      dplyr::rename('year' = name, "proj_y" = value)%>%
      mutate(year = as.integer(year))
    
    
    trend_sm <- get_trend(ldf_smooths, start_yr = 2014, end_yr = 2022, method = "gmean")
    
    ######################################################################
    # 4. predict trend 
    ##########################################################
    
    preds_sm <- predict_trend(ldf_smooths, trend_sm, start_yr = 2023, proj_yr = 2050)
    
    
    ######################################################################
    # 5. plot graphs 
    ##########################################################
    # 
    # smooth_plot <- plot_trend(raw_indices = input_option_1, 
    #                           model_indices = ldf_smooths, 
    #                           pred_indices = preds_sm,
    #                           start_yr = 2014, 
    #                           end_yr = 2022)
    # 
    # smooth_plot
    
    
    ###########################################
    
    # Summarise average trends percent (2014 - 2022) 
    
    trend_sm_summary <- trend_sm  %>%
      summarize(annualpc_q_0.025 = quantile(perc_trend, 0.025),
                annualpc = quantile(perc_trend,0.5),
                annualpc_q_0.975 = quantile(perc_trend,0.975))
    
    
    trend_sm_summary 
    
    
    
    ## Get the predicted trends from the excel sheet 
    targ <- pifs |>  filter(aou == aou_id)
    
    index_baseline <- get_targets(model_indices = ldf_smooths, 
                                  ref_year = 2014, 
                                  st_year = 2026, 
                                  st_lu_target_pc = targ$st_pop_pc_lower,
                                  st_up_target_pc = targ$st_pop_pc_uppper, 
                                  lt_year = 2046, 
                                  lt_lu_target_pc = targ$lt_pop_pc_lower,
                                  lt_up_target_pc = targ$lt_pop_pc_uppper)
    
    
    # sm_plots with targets 
    sm_plot_target <- plot_trend(raw_indices = input1 , 
                                 model_indices = ldf_smooths, 
                                 pred_indices = preds_sm,
                                 start_yr = 2014, 
                                 end_yr = 2022, 
                                 ref_yr = 2014,
                                 targets = index_baseline)
    
    
    sm_plot_target <- sm_plot_target + ggplot2::labs(title = fit$meta_data$species)
    saveRDS(sm_plot_target, file.path(dir, paste0("trend_",aou_id,"_plot.rds")))
    
    # 
    # plot = sm_plot_target, 
    # width = 25,
    # height = 18,
    # units = "cm")
    # 
    # 
    
    ########################################################
    
    # estimate the probability that the targets will be met
    
    st_inc_dec <- targ %>% 
      select(contains("st_pop_pc")) %>%
      tidyr::pivot_longer(everything())%>%
      select(value) |> 
      pull()
    
    
    # # repeat for long term trends 
    lt_inc_dec <- targ %>% 
      select(contains("lt_pop_pc")) %>%
      tidyr::pivot_longer(everything())%>%
      select(value) |> 
      pull()
    
    
    # note need to figure out a way to automate these plots 
    
    
    prob_st <- calculate_probs(predicted_trends = preds_sm,
                               ref_year = 2014, 
                               targ_year = 2026, 
                               prob_decrease = NULL, 
                               prob_increase = st_inc_dec)
    
    
    prob_lt <- calculate_probs(predicted_trends = preds_sm,
                               ref_year = 2014, 
                               targ_year = 2046, 
                               prob_decrease = NULL, 
                               prob_increase = lt_inc_dec) 
    
    
    
    outdata <- list(ldf_smooths, trend_sm, preds_sm,  targ, index_baseline, prob_st, prob_lt  )
    names(outdata)<- c("ldf_smooths", "trend_sm", "pred_sm", "targ", "index_baseline", "prob_st", "prob_lt")
    
    
    saveRDS(outdata, file.path(dir, paste0(aou_id,"_outputs.rds")))
    
  } else {
    
    message("no model present")
    
  }
}

