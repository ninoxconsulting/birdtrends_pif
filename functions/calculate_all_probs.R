# 
# calculate_all_probs <- function(
#     predicted_trends = preds_sm,
#     ref_year = 2014,
#     targ_year = 2026){#,
#    # prob_decrease = NULL,
#   #  prob_increase = NULL){
#   
#   # # testing lines
#   # predicted_trends = preds_sm
#   # ref_year = 2014
#   # targ_year = 2026
#   # prob_decrease = NULL
#   # prob_increase = st_inc_dec
#   
#   
#   # from bbsbayes2 get trend function
#   calc_prob_crease <- function(x, p, type = "decrease") {
#     if(type == "decrease") f <- function(p) length(x[x < (-1 * p)]) / length(x)
#     if(type == "increase") f <- function(p) length(x[x > p]) / length(x)
#     
#     vapply(p, FUN = f, FUN.VALUE = 1.1) %>%
#       stats::setNames(paste0("prob_", type, "_", p, "_percent"))
#   }
#   
#   
#   # for each draw subtract target year from ref year (2024) - 2014
#   tyears <- dplyr::filter(predicted_trends, year %in% c(ref_year, targ_year)) |>
#     dplyr::select(-proj_y)
#   
#   tyears <-  tidyr::pivot_wider(tyears, values_from = pred_ind, names_from = year)
#   colnames(tyears)= c("draw", "ref_yr", "targ_year")
#   
#   ltyears <- tyears |>
#     dplyr::mutate(ch = targ_year/ref_yr) |>
#     dplyr::rowwise() |>
#     dplyr::mutate(ch_pc = 100 * (stats::median(ch) -1)) |>
#     dplyr::ungroup() 
#  
#   return(ltyears)
#   
# }
# 
