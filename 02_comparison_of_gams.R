### this is a copy of the script written by Adam Smith (see here: 
#https://github.com/AdamCSmithCWS/Interim_Goals_Framework/blob/main/comparison_of_gams.R
#)


library(bbsBayes2)
library(mgcv)
library(tidyverse)
library(bayesplot)
library(cmdstanr)

source("functions/GAM_basis_function_mgcv.R")
#fetch_bbs_data()

q2_5 <- function(x)c(q2_5 = quantile(x,probs = c(0.025),
                                     names = FALSE))
q97_5 <- function(x)c(q97_5 = quantile(x,probs = c(0.975),
                                       names = FALSE))

aou <- bbsBayes2::search_species("Pacific Wren")$aou[1]

#fit <- readRDS(paste0("C:/GitHub/CWS_2022_BBS_Analyses/output/fit_",aou,".rds"))

# note this is the full bayes output 
fit <- readRDS(paste0("C:/r_repo/2024_ECCC_birdtrends/birdtrends_pif/fitted_models/fit_",aou,".rds"))

# 
# ##### based on canada ##############################
# 
# inds <- generate_indices(fit, regions = "country")
# inds_samples_can <- inds$samples$country_Canada
# 
# inds_can <- inds$indices %>% 
#   filter(region == "Canada") %>% 
#   mutate(ln_index = log(index),
#          ln_index_lci = log(index_q_0.025),
#          ln_index_uci = log(index_q_0.975),
#          ln_index_sd = (ln_index_uci - ln_index_lci)/(1.96*2),
#          yearn = year-(min(year)-1))
# 
# 
# indsmooth <- generate_indices(fit, regions = "country",
#                               alternate_n = "n_smooth")
# indsmooth_samples_can <- indsmooth$samples$country_Canada
# 
# 
# # fitting hierarchical gam to the estimated indices -----------------------
# 
# n_years <- as.integer(length(min(inds_can$year):max(inds_can$year)))
# n_indices <- as.integer(nrow(inds_can))
# n_knots <- as.integer(round(n_indices/4))
# 
# 
# gam_data <- gam_basis(inds_can$year,
#                       nknots = n_knots,
#                       sm_name = "year")
# stan_data <- list(
#   n_years = n_years,
#   n_indices = n_indices,
#   n_knots_year = gam_data$nknots_year,
#   year = inds_can$yearn,
#   ln_index = inds_can$ln_index,
#   ln_index_sd = inds_can$ln_index_sd,
#   year_basis = gam_data$year_basis
# )
# ## fit model with cmdstanr
# file <- "models/GAM_smooth_model.stan"
# mod <- cmdstan_model(file)
# 
# fit_gam <- mod$sample(data = stan_data,
#                       parallel_chains = 4,
#                       refresh = 0,
#                       adapt_delta = 0.95)
# 
# sum <- fit_gam$summary(variables = NULL,
#                        "mean",
#                        "sd",
#                        "ess_bulk",
#                        "rhat",
#                        q2_5 = q2_5,
#                        q97_5 = q97_5)
# 
# mx_rhat <- max(sum$rhat,na.rm = TRUE)
# 
# if(mx_rhat > 1.05){stop("High Rhat value")}
# 
# 
# smooths <- posterior::as_draws_df(fit_gam) %>%
#   select(matches("^smooth_n([[:punct:]])"),
#          .draw)
# 
# 
# 
# 
# # fitting GAMs to the posterior draws -------------------------------------
# 
# 
# year_seq <- colnames(inds_samples_can) %>% as.numeric()
# gam_preds <- smooths[1,] # setting up same object structure as the draws df above
# bbs_smooth <- smooths[1,]
# 
# for (i in 1:nrow(inds_samples_can)){
#   
#   #i = 1
#   
#   i_dat <- data.frame(log_y = log(inds_samples_can[i,]),Year = year_seq) 
#   
#   gam <- gam(log_y~s(Year, k = n_knots, bs = "tp"),
#              data = i_dat)
#
# note this uses thin plate regression 
#   
#   # Smoothed predictions from gam; these smoothed indices are used to calculate trends
#   i_dat$gam_pred <- predict(gam, newdata = i_dat)
#   predvals <- exp(i_dat$gam_pred)
#   gam_preds[i,1:(ncol(gam_preds)-1)] <- t(predvals) 
#   gam_preds[i,ncol(gam_preds)] <- i 
#   
#   
#   bbs_smooth[i,1:(ncol(bbs_smooth)-1)] <- t(indsmooth_samples_can[i,]) 
#   bbs_smooth[i,ncol(bbs_smooth)] <- i 
#   
#   
# }
# 
# 
# sel_gams <- gam_preds %>% 
#   slice_sample(.,n = 100) %>% 
#   pivot_longer(., cols = starts_with("smooth")) %>% 
#   mutate(yearn = as.integer(str_extract_all(name,
#                                             "[[:digit:]]{1,}",
#                                             simplify = TRUE)),
#          approach = "GAM fit to draws")
# 
# sel_hgams <- smooths %>% 
#   slice_sample(.,n = 100) %>% 
#   pivot_longer(., cols = starts_with("smooth")) %>% 
#   mutate(yearn = as.integer(str_extract_all(name,
#                                             "[[:digit:]]{1,}",
#                                             simplify = TRUE)),
#          approach = "HGAM fit to indices")
# 
# sel_bbssmooths <- bbs_smooth %>% 
#   slice_sample(.,n = 100) %>% 
#   pivot_longer(., cols = starts_with("smooth")) %>% 
#   mutate(yearn = as.integer(str_extract_all(name,
#                                             "[[:digit:]]{1,}",
#                                             simplify = TRUE)),
#          approach = "Original bbsBayes smooth")
# 
# 
# comp_smooths <- sel_gams %>% 
#   bind_rows(.,sel_hgams) %>% 
#   bind_rows(.,sel_bbssmooths)
# 
# 
# comp_plot <- ggplot(data = comp_smooths,
#                     aes(x = yearn,y = value,
#                         group = .draw, colour = .draw))+
#   geom_pointrange(data = inds_can,
#                   aes(x = yearn,y = index,
#                       ymin = index_q_0.025,
#                       ymax = index_q_0.975),
#                   inherit.aes = FALSE,
#                   alpha = 0.3)+
#   geom_line(alpha = 0.3)+
#   scale_colour_viridis_c() +
#   scale_y_continuous(trans = "log10")+
#   facet_wrap(vars(approach))+
#   theme_bw()
# 
# comp_plot
# 
# 
# comp_trends <- comp_smooths %>% 
#   select(-name) %>% 
#   filter(yearn %in% c(10,n_years)) %>% 
#   pivot_wider(names_from = yearn,
#               names_prefix = "Y",
#               values_from = value) %>% 
#   mutate(p_change = Y55/Y10,
#          trend = 100*(p_change^(1/(n_years-10))-1),
#          p_change = (p_change-1)*100)
# 
# p_ch_comp <- ggplot(data = comp_trends,
#                     aes(x = approach,y = trend))+
#   geom_point(alpha = 0.5,position = position_jitter(width = 0.3))
# p_ch_comp
# 
# 
# 




####################################################
### based on continent  ############################


# calculate the annual indices of relative abundance by year 
inds <- generate_indices(fit, regions = "continent") #, start_year = 2014)


# method 2 : draw from the posterior distribution of the indices for the continent for GAM (posterior)
inds_samples_con <- inds$samples$continent_continent

inds_con <- inds$indices %>% 
  mutate(ln_index = log(index),
         ln_index_lci = log(index_q_0.025),
         ln_index_uci = log(index_q_0.975),
         ln_index_sd = (ln_index_uci - ln_index_lci)/(1.96*2),
         yearn = year-(min(year)-1))

# extract the smooth from the model (method 3) 
indsmooth <- generate_indices(fit, regions = "continent",
                              alternate_n = "n_smooth")#,
#start_year = 2014)
indsmooth_samples_con <- indsmooth$samples$continent_continent





# fitting hierarchical gam to the estimated indices -----------------------

n_years <- as.integer(length(min(inds_con$year):max(inds_con$year)))
n_indices <- as.integer(nrow(inds_con))
n_knots <- as.integer(round(n_indices/4))


gam_data <- gam_basis(inds_con$year,
                      nknots = n_knots,
                      sm_name = "year")
stan_data <- list(
  n_years = n_years,
  n_indices = n_indices,
  n_knots_year = gam_data$nknots_year,
  year = inds_con$yearn,
  ln_index = inds_con$ln_index,
  ln_index_sd = inds_con$ln_index_sd,
  year_basis = gam_data$year_basis
)

## fit model with cmdstanr
file <- "models/GAM_smooth_model.stan"
mod <- cmdstan_model(file)

fit_gam <- mod$sample(data = stan_data,
                      parallel_chains = 4,
                      refresh = 0,
                      adapt_delta = 0.95)

sum <- fit_gam$summary(variables = NULL,
                       "mean",
                       "sd",
                       "ess_bulk",
                       "rhat",
                       q2_5 = q2_5,
                       q97_5 = q97_5)

mx_rhat <- max(sum$rhat,na.rm = TRUE)

if(mx_rhat > 1.05){stop("High Rhat value")}


smooths <- posterior::as_draws_df(fit_gam) %>%
  select(matches("^smooth_n([[:punct:]])"),
         .draw)




# fitting GAMs to the posterior draws -------------------------------------

year_seq <- colnames(inds_samples_con) %>% as.numeric()
gam_preds <- smooths[1,] # setting up same object structure as the draws df above
bbs_smooth <- smooths[1,]

for (i in 1:nrow(inds_samples_con)){
  
    i = 1
  
  i_dat <- data.frame(log_y = log(inds_samples_con[i,]),Year = year_seq) 
  
  gam <- gam(log_y~s(Year, k = n_knots, bs = "tp"),
             data = i_dat)
  
  # Smoothed predictions from gam; these smoothed indices are used to calculate trends
  i_dat$gam_pred <- predict(gam, newdata = i_dat)
  predvals <- exp(i_dat$gam_pred)
  gam_preds[i,1:(ncol(gam_preds)-1)] <- t(predvals) 
  gam_preds[i,ncol(gam_preds)] <- i 
  
  
  i = 1
  
  bbs_smooth[i,1:(ncol(bbs_smooth)-1)] <- t(indsmooth_samples_con[i,]) 
  bbs_smooth[i,ncol(bbs_smooth)] <- i 
  
  
}


sel_gams <- gam_preds %>% 
  slice_sample(.,n = 100) %>% 
  pivot_longer(., cols = starts_with("smooth")) %>% 
  mutate(yearn = as.integer(str_extract_all(name,
                                            "[[:digit:]]{1,}",
                                            simplify = TRUE)),
         approach = "2.GAM fit to draws")

sel_hgams <- smooths %>% 
  slice_sample(.,n = 100) %>% 
  pivot_longer(., cols = starts_with("smooth")) %>% 
  mutate(yearn = as.integer(str_extract_all(name,
                                            "[[:digit:]]{1,}",
                                            simplify = TRUE)),
         approach = "1.HGAM fit to indices")

sel_bbssmooths <- bbs_smooth %>% 
  slice_sample(.,n = 100) %>% 
  pivot_longer(., cols = starts_with("smooth")) %>% 
  mutate(yearn = as.integer(str_extract_all(name,
                                            "[[:digit:]]{1,}",
                                            simplify = TRUE)),
         approach = "3.GAMYE original bbsBayes smooth")


comp_smooths <- sel_gams %>% 
  bind_rows(.,sel_hgams) %>% 
  bind_rows(.,sel_bbssmooths)


comp_plot <- ggplot(data = comp_smooths,
                    aes(x = yearn,y = value,
                        group = .draw, colour = .draw))+
  geom_pointrange(data = inds_con,
                  aes(x = yearn, y = index,
                      ymin = index_q_0.025,
                      ymax = index_q_0.975),
                  inherit.aes = FALSE,
                  alpha = 0.3)+
  geom_line(alpha = 0.3)+
  scale_colour_viridis_c() +
  scale_y_continuous(trans = "log10")+
  facet_wrap(vars(approach))+
  theme_bw()

comp_plot


comp_trends <- comp_smooths %>% 
  select(-name) %>% 
  filter(yearn %in% c(10,n_years)) %>% 
  pivot_wider(names_from = yearn,
              names_prefix = "Y",
              values_from = value) %>% 
  mutate(p_change = Y55/Y10,
         trend = 100*(p_change^(1/(n_years-10))-1),
         p_change = (p_change-1)*100)

p_ch_comp <- ggplot(data = comp_trends,
                    aes(x = approach,y = trend))+
  geom_point(alpha = 0.5,position = position_jitter(width = 0.3))
p_ch_comp
