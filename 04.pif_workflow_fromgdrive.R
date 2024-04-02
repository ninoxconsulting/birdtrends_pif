library(googlesheets4)
library(googledrive)
library(dplyr)
library(readr)
library(janitor)


# location of shared files: 
## https://drive.google.com/drive/folders/1AwjONN_eTZP-IXmzKItjyYKS_NTA1psV
# 
#
# To authorize the googlesheets download, set your google auth email with:
# options(
#   gargle_oauth_email = "email.which.gives.you.access.to.these.files@gmail.com"
# )
# If this is different from your normal google auth email you can add this to a
# project-specific .Rprofile file to cache


#aou <- 4330
#aou <- 2920
aou <- 2930


# get list of id files in shared google drive: 

bfiles <- drive_ls(as_id("1AwjONN_eTZP-IXmzKItjyYKS_NTA1psV"))


bboi <- bfiles %>%
filter(name %in% paste0("fit_",aou,".rds"))


# select the first file and download a local copy

# this example is for the first bird: 
bboi$id[1]

drive_download(as_id(bboi$id[1]), path = file.path("fitted_models", bboi$name[1]))

# read in file: 


## prepare data inputs : example 1 

#aou <- 4370


# loading the fitted model object
fit <- readRDS(paste0("C:/r_repo/2024_ECCC_birdtrends/birdtrends_pif/fitted_models/fit_",aou,".rds"))


# Generate indicies (data options 1 and 2)
inds <- generate_indices(fit, regions = "continent")

# Generate indicies with smooth (data option 3) 

indsmooth <- generate_indices(fit,regions = "continent",
                              alternate_n = "n_smooth")

plot_indices(inds)

#ttemp <- generate_trends(indsmooth)
#aa <- ttemp$trends


# Input option 1 - annual indices of relative abundance with CI estimates
# data-frame with at a minimum, columns
input_option_1 <- inds$indices %>% 
  select(year, index, index_q_0.025,index_q_0.975)

# Input option 2 - matrix of posterior draws (rows) for each year (columns)
# representing the full annual indices of relative abundance - i.e., full posterior of option 1
# column names = year
# row names = draw | iteration
input_option_2 <- as.data.frame(inds$samples$continent_continent)

# Input option 3 - matrix of posterior draws (rows) for each year (columns)
# representing the smoothed annual indices of relative abundance 
# column names = year
# row names = draw | iteration
# identical structure to option 2, but not requiring the extra step of fitting an additional GAM to smooth through time
input_option_3 <- indsmooth$samples$continent_continent





###########################################
# 2. Generate trends for each data set 
###########################################


# data option 1: annual indices

indat1 <- input_option_1

# fit the Heirachial GAM model using all years: 

fitted_data <- fit_hgam(indat1, start_yr = NA, end_yr = NA, n_knots = 14)

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



# estimate the average trends percent

trend_hgam_summary <- trend_hgam  %>%
  summarize(annualpc_q_0.025 = quantile(perc_trend, 0.025),
            annualpc = quantile(perc_trend,0.5),
            annualpc_q_0.975 = quantile(perc_trend,0.975))



######################################################################
# 4. predict trend 
##########################################################

preds_hgam <- predict_trend(ldf_hgam, trend_hgam, start_yr = 2023, proj_yr = 2050) %>% 
  mutate(type = "1.hgam")
preds_sm   <- predict_trend(ldf_smooths, trend_sm, start_yr = 2023, proj_yr = 2050)%>% 
  mutate(type = "3.gamye")
preds_gams <- predict_trend(ldf_gams, trend_gam,start_yr = 2023, proj_yr = 2050)%>% 
  mutate(type = "2.gam")


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


cowplot::plot_grid(hgams_plot,gam_plot, smooth_plot, nrow = 3)







               