# workflow for pif data files # OBTAINED FROM aUDUBON 
# Note this data set is the baysian posterier distribution, not smooths as completed
# for other species. Hence different method used. 

devtools::install_github("ninoxconsulting/birdtrends", ref= "annual_trend_projections" )
#devtools::install_github("bbsBayes/bbsBayes2")
library(bbsBayes2)
library(birdtrends)
library(ggplot2)
library(readr)
library(dplyr)
library(janitor)
# notes files received directly from Audbon and downlowed on a locatl copy. A copy of the data 
# is saved on the main google drive. Due to the number f files this is not currently automated in the 
# script and can be done manually. 
## https://drive.google.com/drive/folders/1AwjONN_eTZP-IXmzKItjyYKS_NTA1psV
# 

# read in the updated plots function 
#source("functions/plot_trend_temp.r")

outputs <- "02_outputs"

pifs <- read.csv("sp_key_bbs_full.csv") |> 
  filter(pif_rank %in% c("d", "r", "red")) |> 
  dplyr::select(aou, english, genus, species, unid_combined, pif_rank, spcode,
                st_pop_pc_lower, st_pop_pc_uppper,
                lt_pop_pc_lower, lt_pop_pc_uppper, model_present) |> 
  filter(model_present == "Yv2")%>% 
  mutate(bbs = paste0(genus, "_", species))


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
  
  bname <- pifs %>% 
    filter(aou == i) |> 
    select(bbs) |> 
    pull()
    
  english_name <- pifs |> 
    filter(aou == i) |> 
    select(english) |>
    pull()
  
  aou_file <- paste0("C:/r_repo/2024_ECCC_birdtrends/birdtrends_pif/01_fitted_models/CBC_posteriors/",bname,"/",bname ,"_coda.csv")
  
  if(file.exists(aou_file)) {
    
    dir <- file.path("02_outputs", i) 
    if (!dir.exists(dir)) dir.create(dir)
    
    # loading the fitted model object
    fit <- read_csv(aou_file, show_col_types = FALSE)
    
    # format the files to a suitable method 
    
    tt <- t(fit)
    t <- unlist(tt)
    t <- as.data.frame(t)
    names(t) <- as.vector(unlist(t[1,])) 
    t <- t[-1,]
    
    row.names(t) = NULL
    indata2 <- t
    
    
    # option 2: Fit GAM to each draw : data input type 2
    
    fitted_data <- fit_gam(t)
    
#    > fitted_data
#    # A tibble: 560,000 × 3
#    draw  year   proj_y
#    <int> <int>    <dbl>
#      1     1  1966 3120130.
#    2     1  1967 3017288.
#    3     1  1968 2921924.
#    4     1  1969 2838121.
#    5     1  1970 2768596.
    
    
    
    
  #   fitted_data_wide <- fit_gam(t, longform = FALSE)
  #   
  #   sel_hgams <- fitted_data_wide %>%
  #     dplyr::slice_sample(., n = 100) %>%  
  #     dplyr::mutate(draw = seq(1, 100, 1)) %>% 
  #     tidyr::pivot_longer(., cols = !starts_with("d")) |> 
  #     dplyr::mutate(yearn = as.integer(name) - min(as.integer(name)))
  #   
  # 
  #   comp_plot_3 <- ggplot2::ggplot(data =  sel_hgams,
  #                              ggplot2::aes(x = yearn,y = value,
  #                                           group = draw, colour = draw))+
  # # ggplot2::geom_pointrange(data = input1,
  # #                          ggplot2::aes(x = yearn, y = index,
  # #                                       ymin = index_q_0.025,
  # #                                       ymax = index_q_0.975),
  # #                          inherit.aes = FALSE,
  # #                          alpha = 0.3)+
  #   ggplot2::geom_line(alpha = 0.3)+
  #   ggplot2::scale_colour_viridis_c() +
  #   ggplot2::scale_y_continuous(trans = "log10")+
  #   ggplot2::theme_bw()
  # 
  #   comp_plot_3
  # 


######################################################################
# 3. calculate trend 
##########################################################

# ldf_hgams <- tibble::rowid_to_column(fitted_data, "draw") %>%
#   tidyr::pivot_longer(., cols = !starts_with("d")) %>%
#   dplyr::rename('year' = name, "proj_y" = value) %>%
#   mutate(year = as.integer(year))
# 
#     
    
    
# # convert the ldf_hams into indupt data for the plot 
#   i1 <- fitted_data %>% 
#     group_by(year)%>% 
#     mutate(index = median(proj_y), 
#            index_q_0.025 = stats::quantile(proj_y, probs = 0.05),
#            index_q_0.975 = stats::quantile(proj_y, probs = 0.95))%>%
#     select(-draw, -proj_y)
#   
  

#trend_sm <- get_trend(fitted_data , start_yr = 2014, end_yr = 2022, method = "gmean")
trend_sm <- get_trend(fitted_data , start_yr = 2014, end_yr = 2021, method = "gmean", annual_variation = TRUE)

    
#    draw trend_log perc_trend
#    <int>     <dbl>      <dbl>
#      1     1  -0.0425      -4.16 
#    2     2  -0.0101      -1.00 
#    3     3  -0.0114      -1.13 
    
#NEW version
#draw trend_log perc_trend trend_start_year trend_end_year
#<int>     <dbl>      <dbl>            <dbl>          <dbl>
#  1     1  -0.0425      -4.16              2014           2021
#2     2  -0.0101      -1.00              2014           2021
#3     3  -0.0114      -1.13              2014           2021
#4     4  -0.0108      -1.07              2014           2021
#5     5  -0.00926     -0.922             2014           2021
    
    

######################################################################
# 4. predict trend 
##########################################################

preds_sm <- proj_trend(fitted_data, trend_sm, start_yr = 2022, proj_yr = 2050)

#    > preds_sm 
#    # A tibble: 850,000 × 4
#    draw  year   proj_y pred_ind
#    <int> <dbl>    <dbl>    <dbl>
#      1     1  1966 3120130. 3120130.
#    2     1  1967 3017288. 3017288.
#    3     1  1968 2921924. 2921924.
#    4     1  1969 2838121. 2838121.
#    5     1  1970 2768596. 2768596.    
    
    
    
# > head(preds_sm)
# draw year trend_end_year    trend_log perc_trend trend_start_year  proj_y
# 1    1 1966           2021 -0.042523266 -4.1631832             2014 3120130
# 2    2 1966           2021 -0.010075209 -1.0024624             2014 2614742
# 3    3 1966           2021 -0.011403699 -1.1338924             2014 2784599
# 4    4 1966           2021 -0.010794054 -1.0736007             2014 2696888
# 5    5 1966           2021 -0.009262627 -0.9219861             2014 2865505
# 6    6 1966           2021 -0.009625319 -0.9579144             2014 3363375
    # starting_pred_ind pred_ind
    # 1           1229435  3120130
    # 2           1349836  2614742
    # 3           1235278  2784599
    # 4           1225231  2696888
    # 5           1206272  2865505
    # 6           1160341  3363375    
    # 
    
    
    
    
######################################################################
# 5. plot graphs 
##########################################################
# 
# smooth_plot <- plot_trend(raw_indices = NULL,
#                           model_indices = fitted_data,
#                           pred_indices = preds_sm,
#                           start_yr = 2014,
#                           end_yr = 2021)
# 
# smooth_plot


###########################################

# Summarise average trends percent (2014 - 2022) 

trend_sm_summary <- trend_sm  %>%
  summarize(annualpc_q_0.025 = quantile(perc_trend, 0.025),
            annualpc = quantile(perc_trend,0.5),
            annualpc_q_0.975 = quantile(perc_trend,0.975))


#trend_sm_summary 


## Get the predicted trends from the excel sheet 
targ <- pifs |>  filter(aou == aou_id)

index_baseline <- get_targets(model_indices = fitted_data, 
                              ref_year = 2014, 
                              st_year = 2026, 
                              st_lu_target_pc = targ$st_pop_pc_lower,
                              st_up_target_pc = targ$st_pop_pc_uppper, 
                              lt_year = 2046, 
                              lt_lu_target_pc = targ$lt_pop_pc_lower,
                              lt_up_target_pc = targ$lt_pop_pc_uppper)


# sm_plots with targets 
sm_plot_target <- plot_trend(raw_indices = NULL, 
                             model_indices = fitted_data, 
                             pred_indices = preds_sm,
                             start_yr = 2014, 
                             end_yr = 2021, 
                             ref_yr = 2014,
                             targets = index_baseline,
                             set_upperlimit = TRUE,
                             upperlimit = 1.5,
                             annual_variation = FALSE)


sm_plot_target <- sm_plot_target + ggplot2::labs(title = english_name)
saveRDS(sm_plot_target, file.path(dir, paste0("trend_",aou_id,"_plot.rds")))


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

prob_st <- calculate_probs(projected_trends = preds_sm,
                           ref_year = 2014, 
                           targ_year = 2026, 
                           prob_decrease = NULL, 
                           prob_increase = st_inc_dec)


prob_lt <- calculate_probs(projected_trends = preds_sm,
                           ref_year = 2014, 
                           targ_year = 2046, 
                           prob_decrease = NULL, 
                           prob_increase = lt_inc_dec) 

#short term trends
trend_st <- trend_change(projected_trends = preds_sm,
                         ref_year = 2014, 
                         targ_year = 2026) |> 
  select(ch_pc) |> 
  mutate(aou = i) |> 
  rename("st_ch_pc" = ch_pc)


# long term trends
trend_lt <- trend_change(projected_trends = preds_sm, ref_year = 2014, targ_year = 2046) |> 
  select(ch_pc) |> 
  mutate(aou = i) |> 
  rename("lt_ch_pc" = ch_pc)


trends <- cbind( trend_st ,  trend_lt ) 
trends <- trends[,-4]
#probs


outdata <- list(fitted_data, trend_sm, preds_sm,  targ, index_baseline, trends, prob_st, prob_lt, input1 = NULL )
names(outdata)<- c("fitted_smooths", "trend_sm", "pred_sm", "targ", "index_baseline", "trends", "prob_st", "prob_lt", "input1")


saveRDS(outdata, file.path(dir, paste0(aou_id,"_outputs.rds")))

  } else {
    
    message("no model present")
    
  }
}

