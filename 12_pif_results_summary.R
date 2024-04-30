
library(dplyr)
library(readr)
library(janitor)
library(bbsBayes2)
library(birdtrends)
library(ggplot2)


# read in the final plot data 

outputs <- "outputs"

pifs <- read.csv("sp_key_bbs_full.csv") |> 
  filter(pif_rank %in% c("d", "r", "red")) |> 
  dplyr::select(aou, english, unid_combined, pif_rank, spcode,
                st_pop_pc_lower, st_pop_pc_uppper,
                lt_pop_pc_lower, lt_pop_pc_uppper)


redpifs <- pifs |> 
  filter(pif_rank %in% c("red", "d", "r"))