#' Calculate change in trend between years
#'
#' @param projected_trends tibble with all draws, years, projected trends, output of proj_trend function
#' @param ref_year numeric year in which change is measured from i.e. 2014
#' @param targ_year numeric year in which target will be reached i.e. 2026
#'
#' @return tibble with the average percent change in addition to probability of increase or decrease
#' @export
#'
#' @examples
#' \dontrun{
#'   test_st <- trend_change(predicted_trends = preds_sm,
#'         ref_year = 2014,
#'         targ_year = 2026)
#'}
trend_change <- function(
    projected_trends = preds_sm,
    ref_year = 2014,
    targ_year = 2026){
  
  
  # check in input values
  uyrs <-  projected_trends$year
  if(!ref_year %in% uyrs){
    stop("reference year is not within input data, please re-select")
  }
  if(!targ_year %in% uyrs){
    stop("target year is not within input data, please re-select")
  }
  
  if(!targ_year %in% uyrs){
    stop("target year is not within input data, please re-select")
  }
  
  # from bbsbayes2 get trend function
  calc_prob_crease <- function(x, p, type = "decrease") {
    if(type == "decrease") f <- function(p) length(x[x < (-1 * p)]) / length(x)
    if(type == "increase") f <- function(p) length(x[x > p]) / length(x)
    
    vapply(p, FUN = f, FUN.VALUE = 1.1) %>%
      stats::setNames(paste0("prob_", type, "_", p, "_percent"))
  }
  
  
  # for each draw subtract target year from ref year (2024) - 2014
  tyears <- dplyr::filter(projected_trends, year %in% c(ref_year, targ_year)) |>
    dplyr::select(-proj_y)
  
  
  tyears <-  tidyr::pivot_wider(tyears, values_from = pred_ind, names_from = year)
  colnames(tyears)= c("draw", "ref_yr", "targ_year")
  
  ltyears <- tyears |>
    dplyr::mutate(ch = targ_year/ref_yr) |>
    dplyr::rowwise() |>
    dplyr::mutate(ch_pc = 100 * (stats::median(ch) -1)) |>
    dplyr::ungroup()
  
  
  return(ltyears)
  
}

