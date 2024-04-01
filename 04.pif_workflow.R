# PIF workflow using birdtrends

#devtools::install_github("ninoxconsulting/birdtrends")
library(mgcv)
library(tidyverse)
library(birdtrends)
library(bbsBayes2)


## prepare data inputs 

aou <- bbsBayes2::search_species("Pacific Wren")$aou[1]

# loading the fitted model object
fit <- readRDS(paste0("C:/r_repo/2024_ECCC_birdtrends/birdtrends_pif/fitted_models/fit_",aou,".rds"))


# Generate indicies (data options 1 and 2)
inds <- generate_indices(fit,regions = "country")

# Generate indicies with smooth (data option 3) 

indsmooth <- generate_indices(fit,regions = "country",
                              alternate_n = "n_smooth")

plot_indices(inds)

ttemp <- generate_trends(indsmooth)
#aa <- ttemp$trends


# Input option 1 - annual indices of relative abundance with CI estimates
# data-frame with at a minimum, columns
# year, 
# index, # estimated annual relative abundance
# index_q_0.025, # lower 95% uncertainty limit on estimated annual relative abundance
# index_q_0.975 # upper 95% uncertainty limit on estimated annual relative abundance

input_option_1 <- inds$indices %>% 
  filter(region == "Canada") %>% 
  select(year, index, index_q_0.025,index_q_0.975)
#saveRDS(input_option_1, file = file.path("data", "input_data1.rds"))


# Input option 2 - matrix of posterior draws (rows) for each year (columns)
# representing the full annual indices of relative abundance - i.e., full posterior of option 1
# column names = year
# row names = draw | iteration
input_option_2 <- as.data.frame(inds$samples$country_Canada)

#saveRDS(input_option_2, file = file.path("data", "input_data2.rds"))



# Input option 3 - matrix of posterior draws (rows) for each year (columns)
# representing the smoothed annual indices of relative abundance 
# column names = year
# row names = draw | iteration
# identical structure to option 2, but not requiring the extra step of fitting an additional GAM to smooth through time
input_option_3 <- indsmooth$samples$country_Canada

#saveRDS(input_option_3, file = file.path("data", "input_data3.rds"))





###########################################
# 2. Generate trends for each data set 
###########################################


# data option 1: annual indices

indat1 <- input_option_1

# fit the Heirachial GAM model using all years: 

fitted_data <- fit_hgam(indat1, start_yr = NA, end_yr = NA, n_knots = 5)

# plot the datasets 
sel_hgams <- fitted_data %>%
  dplyr::slice_sample(., n = 100) %>%  
  dplyr::mutate(draw = seq(1, 100, 1)) %>% 
  tidyr::pivot_longer(., cols = !starts_with("d")) |> 
  dplyr::mutate(yearn = as.integer(name) - min(as.integer(name)))

indat1 <- indat1 |> 
  dplyr::mutate(yearn = as.integer(year) - min(as.integer(year)))  

comp_plot <- ggplot2::ggplot(data = sel_hgams,
                             ggplot2::aes(x = yearn,y = value,
                                          group = draw, colour = draw))+
  ggplot2::geom_pointrange(data = indat1,
                           ggplot2::aes(x = yearn, y = index,
                                        ymin = index_q_0.025,
                                        ymax = index_q_0.975),
                           inherit.aes = FALSE,
                           alpha = 0.3)+
  ggplot2::geom_line(alpha = 0.3)+
  ggplot2::scale_colour_viridis_c() +
  ggplot2::scale_y_continuous(trans = "log10")+
  ggplot2::theme_bw()

  comp_plot


# data option 2: GAm posterior draw from BBS model 
  
  indat2 <- input_option_2
  
  # fit the Heirachial GAM model using all years: 
  
  fitted_gams <- fit_gam(indat2, start_yr = NA, end_yr = NA, n_knots = 14)
  
  
  sel_gams <- fitted_gams %>%
    dplyr::slice_sample(., n = 100) %>%  
    dplyr::mutate(draw = seq(1, 100, 1)) %>% 
    tidyr::pivot_longer(., cols = !starts_with("d")) |> 
    dplyr::mutate(yearn = as.integer(name) - min(as.integer(name)))
  
  comp_plot <- ggplot2::ggplot(data = sel_gams,
                               ggplot2::aes(x = yearn,y = value,
                                            group = draw, colour = draw))+
    ggplot2::geom_pointrange(data = indat1,
                             ggplot2::aes(x = yearn, y = index,
                                          ymin = index_q_0.025,
                                          ymax = index_q_0.975),
                             inherit.aes = FALSE,
                             alpha = 0.3)+
    ggplot2::geom_line(alpha = 0.3)+
    ggplot2::scale_colour_viridis_c() +
    ggplot2::scale_y_continuous(trans = "log10")+
    ggplot2::theme_bw()
  
  comp_plot


  
  
  
  
  # data option 3: Use smooth from generated BBS model 
  
  indat3 <- as.data.frame(input_option_3)
  
  # fit the Heirachial GAM model using all years: 
  
  fitted_smooths <- fit_smooths(indat3, start_yr = NA, end_yr = NA)
 
  sel_bbssmooths <- fitted_smooths %>% 
    dplyr::slice_sample(., n = 100) %>%  
    dplyr::mutate(draw = seq(1, 100, 1)) %>% 
    tidyr::pivot_longer(., cols = !starts_with("d")) |> 
    dplyr::mutate(yearn = as.integer(name) - min(as.integer(name)))
  
          # approach = "3.GAMYE original bbsBayes smooth")

  
  comp_plot <- ggplot2::ggplot(data =  sel_bbssmooths,
                               ggplot2::aes(x = yearn,y = value,
                                            group = draw, colour = draw))+
    ggplot2::geom_pointrange(data = indat1,
                             ggplot2::aes(x = yearn, y = index,
                                          ymin = index_q_0.025,
                                          ymax = index_q_0.975),
                             inherit.aes = FALSE,
                             alpha = 0.3)+
    ggplot2::geom_line(alpha = 0.3)+
    ggplot2::scale_colour_viridis_c() +
    ggplot2::scale_y_continuous(trans = "log10")+
    ggplot2::theme_bw()
  
  comp_plot
  
  
  
  
  
  
######################################################################
  # 3. calculate trend 
##########################################################
  
  ldf_hgam <- tibble::rowid_to_column(fitted_data, "draw") %>%
    tidyr::pivot_longer(., cols = !starts_with("d")) %>%
    dplyr::rename('year' = name, "proj_y" = value)%>%
    mutate(year = as.integer(year))
  
  # convert data to long form? 
  
  ldf_smooths <- tibble::rowid_to_column(fitted_smooths, "draw") %>%
    tidyr::pivot_longer(., cols = !starts_with("d")) %>%
    dplyr::rename('year' = name, "proj_y" = value)%>%
    mutate(year = as.integer(year))
  
  
 # ldf <- nest(fitted_smooths)

  ldf_gams <- tibble::rowid_to_column(fitted_gams, "draw") %>%
    tidyr::pivot_longer(., cols = !starts_with("d")) %>%
    dplyr::rename('year' = name, "proj_y" = value)%>%
    mutate(year = as.integer(year))
  
  
  
trend_hgam <- get_trend(ldf_hgam, start_yr = 2014, end_yr = 2022, method = "gmean")
trend_sm <- get_trend(ldf_smooths, start_yr = 2014, end_yr = 2022, method = "gmean")
trend_gam <- get_trend(ldf_gams, start_yr = 2014, end_yr = 2022, method = "gmean")

#trend_sm <- get_trend(ldf_smooths, start_yr = 2014, end_yr = 2022, method = "lm")
  
  
  


######################################################################
# 4. predict trend 
##########################################################

preds_hgam <- predict_trend(ldf_hgam, trend_hgam, start_yr = 2023, proj_yr = 2050)
preds_sm   <- predict_trend(ldf_smooths, trend_sm, start_yr = 2023, proj_yr = 2050)
preds_gams <- predict_trend(ldf_gams, trend_gam,start_yr = 2023, proj_yr = 2050)

  


######################################################################
# 5. plot graphs 
##########################################################


hgams_plot <- trend_plot(raw_indices = input_option_1, 
                         model_indices = ldf_hgam, 
                         pred_indices = preds_hgam,
                         start_yr = 2014, 
                         end_yr = 2022)

smooth_plot <- trend_plot(raw_indices = input_option_1, 
                          model_indices = ldf_smooths, 
                          pred_indices = preds_sm,
                          start_yr = 2014, 
                          end_yr = 2022)

gam_plot <- trend_plot(raw_indices = input_option_1, 
                       model_indices = ldf_gams, 
                       pred_indices =  preds_gams,
                       start_yr = 2014, 
                       end_yr = 2022)

# 
#   # ----------------------------------------------------
#   # Summarize indices each year
#   # ----------------------------------------------------
# 
#   trend_plot <- function(raw_indices = indat1, 
#                          model_indices = ldf_hgam, 
#                          pred_indices =  preds_hgam ,
#                          start_yr = 2014, 
#                          end_yr = 2022){
#   
#   indices_summarized <- raw_indices #%>%
#   #   tibble::rowid_to_column(., "draw") %>%
#   #   tidyr::pivot_longer(., cols = !starts_with("d")) %>%
#   #   dplyr::rename('year' = name, "obs_y" = value)%>%
#   #   mutate(year = as.integer(year)) %>%
#   #   group_by(year) %>%
#   #   summarize(Index_q_0.025 = quantile(obs_y,0.025),
#   #             Index = quantile(obs_y,0.5),
#   #             Index_q_0.975 = quantile(obs_y,0.975)) 
#   # 
#   
#   gam_summarized <- model_indices %>%
#     group_by(year) %>%
#     summarize(gam_q_0.025 = quantile(proj_y,0.025),
#               gam_index = quantile(proj_y,0.5),
#               gam_q_0.975 = quantile(proj_y,0.975)) 
#   
#   predict_summarized <- pred_indices |> 
#     group_by(year)%>%
#     summarize(pred_q_0.025 = quantile(pred_ind,0.025),
#               pred_index = quantile(pred_ind,0.5),
#               pred_q_0.975 = quantile(pred_ind,0.975)) %>% 
#     filter(year >= end_yr)
#     
#   #Index_baseline <- pred_indices %>% 
#   #  filter(year == 2014)
#   
#   ref_year = 2014
#   
#   
#   baseline_years <- gam_summarized %>%
#     filter(year %in% c(start_yr, end_yr))
#   
#   
#   sp_plot_index <- ggplot() +
#     
#     # Vertical line showing the year goals were set
#     geom_vline(xintercept =  ref_year, size=2, col = "black", alpha = 0.2)+
#     #geom_text(aes(x =  ref_year+1, y = 0.01), 
#     #          label = "<- reference year", col = "black", alpha = 0.2,
#     #          hjust=0, fontface = "bold", size = 2)+
#     
#     geom_vline(xintercept = 2024, size=1, col = "black", alpha = 0.4,  linetype="dotted")+
#     geom_vline(xintercept = 2046, size=1, col = "black", alpha = 0.4, linetype="dotted")+
#     
#     # Observed indices
#     geom_errorbar(data = subset(indices_summarized, year <= max(indices_summarized$year)),aes(x = year, ymin = index_q_0.025, ymax = index_q_0.975), width = 0, col = "gray30")+
#     geom_point(data = subset(indices_summarized, year <= max(indices_summarized$year)),aes(x = year, y = index), col = "gray30")+
#     
#   # gam smooth
#     geom_ribbon(data = gam_summarized, aes(x = year, ymin = gam_q_0.025, ymax = gam_q_0.975), alpha = 0.4, fill = "gray50")+
#     geom_line(data = gam_summarized, aes(x = year, y = gam_index), col = "gray50", linewidth = 1)+
#     
#   # predicted projection
#     geom_ribbon(data = subset(predict_summarized, year >= end_yr), aes(x = year, ymin = pred_q_0.025, ymax = pred_q_0.975), alpha = 0.2, fill = "orangered")+
#     geom_line(data = subset(predict_summarized, year >= end_yr), aes(x = year, y = pred_index), col = "orangered", linewidth = 1)+
#   
#   
#     #geom_line(data = baseline_years, aes(x = year, y = gam_index), col = "black", linewidth = 1)+
#     
#     ylab("Annual Index of Abundance")+
#     xlab("Year")+
#     theme_bw()
#     #coord_cartesian(ylim=c(0,max(apply(sp_projection$indices,2,function(x) quantile(x, 0.975)))),
#      #               xlim=c(1970,2050))+
#     #scale_x_continuous(breaks = seq(1970,sp_projection$end_of_projection,10))
#   
#   print(sp_plot_index)
#   
#   } 
# 
 #  
 # 
 #  
 #  percent_of_Baseline_StatusQuo <- indices_StatusQuo
 #  percent_of_Baseline_Recovery <- indices_Recovery
 #  
 #  for (j in 1:ncol(percent_of_Baseline_StatusQuo)){
 #    percent_of_Baseline_StatusQuo[,j] <- 100* percent_of_Baseline_StatusQuo[,j]/Index_baseline
 #    percent_of_Baseline_Recovery[,j] <- 100* percent_of_Baseline_Recovery[,j]/Index_baseline
 #  }
 #  
 #  percent_of_Baseline_Obs <- indicies
 #  for (j in 1:ncol(percent_of_Baseline_Obs)) percent_of_Baseline_Obs[,j] <- 100* percent_of_Baseline_Obs[,j]/Index_baseline
 #  
 #  
 #  
 #  
 #  indices <- input_option_3
 #  min_year = NULL
 #  max_year = NULL
 #  quantiles = c(0.025, 0.05, 0.25, 0.75, 0.95, 0.975)
 #  #slope = FALSE
 #  #prob_decrease = NULL
 #  #prob_increase = NULL
 # # hpdi = FALSE
 #  calc_quantiles <- stats::quantile
 #  
 #  
 #  # generate trends 
 #  yr_seq <- seq(min(as.integer(colnames(indices))), max(as.integer(colnames(indices))))
 #  
 #  start_year <- min(as.integer(colnames(indices)))
 #  n_years <- length(yr_seq)
 #  indx <- indices
 #  
 #  
 #  if(is.null(min_year)) {
 #    min_year <- start_year
 #  } else {
 #    
 #    if(min_year < start_year) {
 #      message("`min_year` is before the date range, using minimum year of ",
 #              "the data (", min_year <- start_year, ") instead.")
 #    }
 #  }
 #  
 #  if (is.null(max_year)) {
 #    max_year <- max(as.integer(colnames(indices)))
 #  } else if(max_year > max(as.integer(colnames(indices)))) {
 #    message("`max_year` is beyond the date range, using maximum year of ",
 #            "the data (", max_year <- max(as.integer(colnames(indices))), ") instead.")
 #  }
 #  
 #  
 #  # For indexing
 #  min_year_num <- min_year - start_year + 1
 #  max_year_num <- max_year - start_year + 1
 #  
 #  
 #  library(purrr)
 #  
 #  
 #  
 #  
 #  trends <- indices %>%
 #    #dplyr::filter(.data$year %in% min_year:max_year) %>%
 #    #dplyr::group_by(.data$region, .data$region_type,
 #    #                .data$strata_included, .data$strata_excluded) %>%
 #    dplyr::summarize(
 #      # Basic statistics
 #      #rel_abundance = mean(.data$index),
 #      #obs_rel_abundance = mean(.data$obs_mean),
 #      #mean_n_routes = mean(.data$n_routes),
 #      #n_routes = mean(.data$n_routes_total),
 #      #backcast_flag = mean(.data$backcast_flag),
 #      
 #      # Metadata
 #      start_year = .env$min_year,
 #      end_year = .env$max_year) %>%
 #    
 #    dplyr::mutate(
 #      #n_strata_included = purrr::map_dbl(
 #      #  .data$strata_included, ~length(unlist(stringr::str_split(.x, " ; ")))),
 #      
 #      # Add in samples
 #      n = purrr::map2(.data,
 #                      ~indices[[paste0(.x, "_", .y)]]))#,
 #      # Calculate change start to end for each iteration
 #      ch = purrr::map(.data$n,
 #                      ~.x[, .env$max_year_num] / .x[, .env$min_year_num]),
 #      # Calculate change as trend for each iteration
 #      tr = purrr::map(
 #        .data$ch,
 #        ~100 * ((.x^(1/(.env$max_year_num - .env$min_year_num))) - 1)),
 #      
 #      # Median and percentiles of trend per region
 #      trend = purrr::map_dbl(.data$tr, stats::median),
 #      trend_q = purrr::map_df(
 #        .data$tr,
 #        ~stats::setNames(calc_quantiles(.x, quantiles, names = FALSE),
 #                         paste0("trend_q_", quantiles))),
 #      
 #      # Percent change and quantiles thereof per region
 #      percent_change = purrr::map_dbl(.data$ch, ~100 * (stats::median(.x) - 1)),
 #      pc_q = purrr::map_df(
 #        .data$ch, ~stats::setNames(
 #          100 * (calc_quantiles(.x, quantiles, names = FALSE) - 1),
 #          paste0("percent_change_q_", quantiles)))) %>%
 #    dplyr::ungroup() %>%
 #    tidyr::unnest(cols = c("trend_q", "pc_q")) %>%
 #    dplyr::arrange(.data$region_type, .data$region)
 #  
 #  
 #  
 #  

