

# Package layout 


## helper functions 
# quartiles create 

library(mgcv)
library(tidyverse)

## pre step 


# Generate indicies from full bbs out put (if required)

# TODO: add precurser functions


## step 1: Read in data files 

# option 1: dataframe (year, index, LCI, UCI)
# option 2: dataframe (cols = years, rows = draws from posterior dis, nrow is variable)
# option 3: dataframe (cols = years, rows = draws from posterior smooth, nrow is variable)


indat1 <- read_rds(file.path("data", "input_data1.rds"))
indat2 <- read_rds(file.path("data", "input_data2.rds"))
indat3 <- read_rds(file.path("data", "input_data3.rds"))


## Step 2: Select methods for modelling 

# params - entire time series (or limited time series)

# option 1: HGAM - fit a bayesian Generalized Additive Model (HGAM) : data input type 1
# option 2: Fit GAM to each draw : data input type 2
# option 3: Use smooth to fit trend (no modelling required)



# option 1: 




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



fit_hgam <- function(indata, start_yr = NA, end_yr = NA,  n_knots = 5){
  
  # testing 
  indata = indat1
  start_yr = NA#1990
  end_yr = NA#1995
  n_knots = 5
  
  
  allyr_seq <- indata$year
  
  # if years are to be filtered do this here:
  if(is.na(start_yr)) {
    start_year <- min(allyr_seq)
  } else {
    start_year <- start_yr
  }
  
  if(is.na(end_yr)) {
    end_year <- max(allyr_seq)
  } else {
    end_year <- end_yr
  }

  
  # create a list of year to use 
  year_seq = allyr_seq
  year_seq = year_seq[year_seq >= start_year]
  year_seq = year_seq[year_seq <= end_year]
  
  # filter years of interest
  out <- indata |> 
    dplyr::filter(year %in% year_seq)
  
  
  
  n_years <- as.integer(length(min(out$year):max(out$year)))
  n_indices <- as.integer(nrow(out))
  
  if(is.na(n_knots)){
  n_knots <- as.integer(round(n_indices/4))
  } 
  
  
  gam_data <- prep_hgam(out$year,
                        nknots = n_knots,
                        sm_name = "year")
  
  ### up to here 
  
  stan_data <- list(
    n_years = n_years,
    n_indices = n_indices,
    n_knots_year = gam_data$nknots_year,
    
    year = inds_can$yearn,
    ln_index = inds_can$ln_index,
    ln_index_sd = inds_can$ln_index_sd,
    year_basis = gam_data$year_basis
  )
  
  
}





# 
gam_data <- gam_basis(out$year,
                      nknots = n_knots,
                      sm_name = "year")
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




















# option 3: 
#Fit smooth
indat3 <- as.data.frame(indat3)


fit_smooths <- function(indata, start_yr = NA, end_yr = NA){
  
  #indata <- indat3 
  
  # if a double - convert to dataframe
  
  allyr_seq <- colnames(indata) %>% as.numeric()
  
  # if years are to be filtered do this here: 
  if(is.na(start_yr)) {
    start_year <- min(allyr_seq)
  } else {
    start_year <- start_yr
  }
  
  if(is.na(end_yr)) {
    end_year <- max(allyr_seq)
  } else {
    end_year <- end_yr
  }
  
  # create a list of year to use 
  year_seq = allyr_seq
  year_seq = year_seq[year_seq >= start_year]
  year_seq = year_seq[year_seq <= end_year]
  
  # filter years of interest
  smooth_out <- indata %>%
    dplyr::select(dplyr::all_of(as.character(year_seq))) 
  
  return(smooth_out)
}
  
  
#a <- fit_smooths(indat3, start_yr = 1990, end_yr = 2020)



  
  
  
  
  








# option 2: 

indata = indat2


library(foreach)

n_knots <- 14




run_gam <- function(indata, start_yr = NA, end_yr = NA, n_knots = 5) {
  
  # perhaps add a default number of knots or some approximate calculation for number of years?
  # questions 1: - do we want to make the gam option flexible (tp vs cs)
  
 #  # testing lines 
 # indata = indat2
 # start_yr = NA
 # end_yr = NA
 # n_knots = 14
  message("hang tight, you are currently running gams.... this might take a minute")
  
  allyr_seq <- colnames(indata) %>% as.numeric()
  
  # if years are to be filtered do this here: 
  if(is.na(start_yr)) {
    start_year <- min(allyr_seq)
  } else {
    start_year <- start_yr
  }
  
  if(is.na(end_yr)) {
    end_year <- max(allyr_seq)
  } else {
    end_year <- end_yr
  }
  
  # create a list of year to use 
  year_seq = allyr_seq
  year_seq = year_seq[year_seq >= start_year]
  year_seq = year_seq[year_seq <= end_year]
  
  
  # filter per years of interest
  indata <- indata %>%
    select(all_of(as.character(year_seq)))
  
  # convert to log and create a list 
  out <- log(indata) |> split(seq(nrow(indata)))
  
  # iterate over data frame per row
  preds <- foreach(i = names(out), .combine = rbind) %do%{
    # testing line
    #i = names(out)[1]
    
    i_dat <- data.frame(log_y = as.vector(unlist(out[i])), Year = year_seq)
    
    gam <- gam(log_y~s(Year, k = n_knots, bs = "tp"),
               data = i_dat) ## or 
    
    #gam <- gam(log_y~s(year, bs = 'cs', k = length(knots)),
    #           knots = list(Year = knots),
    #           data = i_dat)
    
    
    i_dat$gam_pred <- predict(gam, newdata = i_dat)
    predvals <- exp(i_dat$gam_pred)
    
    predvals 
    
  } 
  
  pred_df <- data.frame(preds)
  colnames(pred_df) = year_seq
  rownames(pred_df) <- NULL
  
  return(pred_df)
  
  } 
  
    

pred_dataset <- run_gam(indat2, start_yr = NA, end_yr = NA, n_knots = 14)







## Step 3: generate trend 

# option 1 : geometric mean
# option 2 : log linear LM 
# option 3 : use smooth directly (may also need raw data to model?)






