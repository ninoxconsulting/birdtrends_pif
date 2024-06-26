## script written by Adam Smith (Adam.Smith@ec.gc.ca)
## March 22nd 2024


### three alternate data inputs for PIF trend assessment package
# examples provided by bbsBayes2 package, but input formats generalised to
# data frames or matrices to allow option of inputing from other monitoring programs

library(bbsBayes2)
library(tidyverse)

aou <- bbsBayes2::search_species("Pacific Wren")$aou[1]

# loading the fitted model object
#fit <- readRDS(paste0("C:/GitHub/CWS_2022_BBS_Analyses/output/fit_",aou,".rds"))
fit <- readRDS(paste0("C:/r_repo/2024_ECCC_birdtrends/birdtrends_pif/01_fitted_models/fit_",aou,".rds"))



indsmooth <- generate_indices(fit,regions = "country",
                              alternate_n = "n_smooth")

inds <- generate_indices(fit,regions = "country")



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

saveRDS(input_option_2, file = file.path("data", "input_data2.rds"))


# Input option 3 - matrix of posterior draws (rows) for each year (columns)
# representing the smoothed annual indices of relative abundance 
# column names = year
# row names = draw | iteration
# identical structure to option 2, but not requiring the extra step of fitting an additional GAM to smooth through time
input_option_3 <- indsmooth$samples$country_Canada

saveRDS(input_option_3, file = file.path("data", "input_data3.rds"))
