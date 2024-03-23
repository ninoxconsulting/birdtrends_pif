

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






# option 2: 

indata = indat2


library(foreach)

n_knots <- 14




run_gam <- function(indata, start_yr = NA, end_yr = NA, n_knots = 5) {
  
  # perhaps add a default number of knots or some approximate calculation for number of years?

  # testing lines 
 # indata = indat2
#  start_yr = NA
#  end_yr = NA
#  n_knots = 14
  
  
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
  out <- logindata |> split(seq(nrow(indata)))
    

  # iterate over data frame per row
  preds <- foreach(i = names(out), .combine = rbind) %do%{
    
    # testing line
   # i = names(out)[1]
    
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






