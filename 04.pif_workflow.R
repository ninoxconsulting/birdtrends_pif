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




# 2. Generate trends for each data set 


# data option 1: annual indices

indat1 <- input_option_1

# fit the Heirachial GAM model using all years: 

fitted_data <- fit_hgam(indat1, start_yr = NA, end_yr = NA, n_knots = 5)


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
  
  
  
  
  # calculate trend 
  
  # convert data to long form? 
  
  ldf <-tibble::rowid_to_column(fitted_smooths, "draw") %>%
    tidyr::pivot_longer(., cols = !starts_with("d")) %>%
    dplyr::rename('year' = name, "proj_y" = value)%>%
    mutate(year = as.integer(year),
           log_proj_y = log(proj_y))
  
  
  trend <- function(proj_data, start_yr = NA, end_yr = NA, method = "geomean"){
    
    # testing 
    proj_data <-ldf 
    
    if(is.na(start_yr)){start_yr = min(proj_data$year)}
    
    if(is.na(end_yr)){end_yr = max(proj_data$year)}
    
    
    trend_dat <- subset(proj_data, year %in% seq(start_yr, end_yr))
    
    
    
      if(method == "geomean") {
      
      #i_dat_for_trend <- subset(i_dat, Year %in% seq(year_goals_are_set,year_goals_are_set - length_current_trend))
      #trend_log <- mean(diff(i_dat_for_trend$gam_pred))
      #trend_percent <- 100*(exp(trend_log)-1) # Convert to percent change per year
      
    
    trend_sum <- trend_dat %>%
      group_by(draw) %>%
      summarise(trend_log = mean(diff(proj_y)),
                trend_loglog = mean(diff(log_proj_y)))%>%
      mutate(perc_trend = 100*(exp(trend_log)-1),
             perc_trendlog = 100*(exp(trend_loglog)-1))
    
  
    
    
        } ifelse(method == "lm"){
      
      # method for lm 
      
      #trendlls_log <- lm(gam_pred ~ Year, data = i_dat)
      #trendlls_log <- as.numeric(trendlls_log$coefficients[2])
      #trendlls_percent <- 100*(exp(trendlls_log)-1)
      
      }
    
    return(trend_sum)
    
    }
  
 
  
  
  
  
  

