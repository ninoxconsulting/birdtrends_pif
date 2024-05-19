
calculate_all_probs <- function(
    predicted_trends = preds_sm,
    ref_year = 2014,
    targ_year = 2026){#,
   # prob_decrease = NULL,
  #  prob_increase = NULL){
  
  # # testing lines
  # predicted_trends = preds_sm
  # ref_year = 2014
  # targ_year = 2026
  # prob_decrease = NULL
  # prob_increase = st_inc_dec
  
  
  # from bbsbayes2 get trend function
  calc_prob_crease <- function(x, p, type = "decrease") {
    if(type == "decrease") f <- function(p) length(x[x < (-1 * p)]) / length(x)
    if(type == "increase") f <- function(p) length(x[x > p]) / length(x)
    
    vapply(p, FUN = f, FUN.VALUE = 1.1) %>%
      stats::setNames(paste0("prob_", type, "_", p, "_percent"))
  }
  
  
  # for each draw subtract target year from ref year (2024) - 2014
  tyears <- dplyr::filter(predicted_trends, year %in% c(ref_year, targ_year)) |>
    dplyr::select(-proj_y)
  
  tyears <-  tidyr::pivot_wider(tyears, values_from = pred_ind, names_from = year)
  colnames(tyears)= c("draw", "ref_yr", "targ_year")
  
  ltyears <- tyears |>
    dplyr::mutate(ch = targ_year/ref_yr) |>
    dplyr::rowwise() |>
    dplyr::mutate(ch_pc = 100 * (stats::median(ch) -1)) |>
    dplyr::ungroup() #%>%
  # dplyr::mutate(percent_change_ave = 100 * (stats::median(ch)-1),
  #        percent_change_q_0.025 = 100 *  (stats::quantile(ch, probs = 0.025)-1),
  #        percent_change_q_0.05 = 100 *  (stats::quantile(ch, probs = 0.05)-1),
  #        percent_change_q_0.25 = 100 *  (stats::quantile(ch, probs = 0.25)-1),
  #        percent_change_q_0.75 = 100 *  (stats::quantile(ch, probs = 0.75)-1),
  #        percent_change_q_0.95 = 100 *  (stats::quantile(ch, probs = 0.95)-1),
  #        percent_change_q_0.975 = 100 *  (stats::quantile(ch, probs = 0.975)-1))
  #hist(ltyears$ch_pc,  breaks = 50)
  
  # 
  # # Model conditional probabilities of population change during trends period
  # if(!is.null(prob_decrease)) {
  #   ltyears <- ltyears %>%
  #     dplyr::mutate(
  #       pch_pp = purrr::map_df(.data$ch_pc, calc_prob_crease,
  #                              .env$prob_decrease, type = "decrease")) %>%
  #     tidyr::unnest("pch_pp")
  # }
  # 
  # if(!is.null(prob_increase)){
  #   ltyears <- ltyears %>%
  #     dplyr::mutate(
  #       pch_pp = purrr::map_df(.data$ch_pc, calc_prob_crease,
  #                              .env$prob_increase, type = "increase")) %>%
  #     tidyr::unnest("pch_pp")
  # }
  
 
  return(ltyears)
  
}

