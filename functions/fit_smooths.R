#' Extract Smooths from BBS model run
#'
#' @param indata dataframe of generated smooths. Each column is a year, each row is a predicted smooth
#' @param start_yr numeric year at which to start, if subset is required. Default is the last year available
#' @param end_yr numeric year at which to end, if subset is required. Default is the last year available
#' @param longform TRUE/FALSE the output will be converted to a longform tibble with columns draw, year, proj_y. Default = TRUE
#' @return dataframe with smooths generated for each year of the selected range
#' @export
#'
#' @examples
#' \dontrun{
#'smooths <- fit_smooths(indat3, start_yr = 1990, end_yr = 2020)
#'}
fit_smooths <- function(indata, start_yr = NA, end_yr = NA, longform = TRUE){
  
  #indata <- indat3
##  start_yr = NA
#  end_yr = NA
#  longform = TRUE
  
  # if a double - convert to dataframe
  
  allyr_seq <- colnames(indata) |>  as.numeric()
  
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
  smooth_out <- indata |>
    dplyr::select(dplyr::all_of(as.character(year_seq)))
  
  if(longform){
    
    smooth_out <- tibble::rowid_to_column( smooth_out, "draw") %>%
      tidyr::pivot_longer(., cols = !starts_with("d")) %>%
      dplyr::rename('year' = name, "proj_y" = value)%>%
      mutate(year = as.integer(year))
    
  }
  
  return(smooth_out)
  
}


fit_gam <- function(indata, start_yr = NA, end_yr = NA, n_knots = NA,longform = TRUE) {
  
  # questions 1: - do we want to make the gam option flexible (tp vs cs)
  
  #  # testing lines
#  indata = t
#  start_yr = NA
#  end_yr = NA
#  n_knots = NA
#  longform = FALSE
  #  # testing end
  
  message("hang tight, you are currently running gams.... this might take a minute")
  
  allyr_seq <- colnames(indata) |>  as.numeric()
  
  min_yr <- min(allyr_seq)
  max_yr <- max(allyr_seq)
  
  # if years are to be filtered do this here:
  if(is.na(start_yr)) {
    start_yr <- min_yr
  } else {
    if(start_yr < min_yr) {
      message("`start_yr` is before the date range, using minimum year of ",
              "the data (", start_yr <- min_yr, ") instead.")
    }
  }
  
  if (is.na(end_yr)) {
    end_yr <- max_yr
  } else {
    if(end_yr > max_yr) {
      message("`max_year` is beyond the date range, using maximum year of ",
              "the data (", end_yr <- max_year, ") instead.")
    }
  }
  
  
  # create a list of year to use
  year_seq = allyr_seq
  year_seq = year_seq[year_seq >= start_yr]
  year_seq = year_seq[year_seq <= end_yr]
  
  
  #estimate n_knots if not specified
  n_years <- as.integer(length(year_seq))
  
  if(is.na(n_knots)){
    n_knots <- as.integer(round(n_years/4))
    message("`n_knots` is not defined, using default of one knot per 4 years of data ",
            "using n_knots = ",  n_knots, ".")
  }
  
  # filter per years of interest
  indata <- indata |>
    dplyr::select(dplyr::all_of(as.character(year_seq)))
  
  # convert to log and create a list
  out <- log(indata) |> split(seq(nrow(indata)))
  
  # iterate over data frame per row
  preds <- foreach::foreach(i = names(out), .combine = rbind)%do%{
    # testing line
    #i = names(out)[1]
    
    i_dat <- data.frame(log_y = as.vector(unlist(out[i])), Year = year_seq)
    
    gam <- mgcv::gam(log_y~s(Year, k = n_knots, bs = "tp"),
                     data = i_dat) ## or
    
    #gam <- gam(log_y~s(year, bs = 'cs', k = length(knots)),
    #           knots = list(Year = knots),
    #           data = i_dat)
    
    i_dat$gam_pred <- stats::predict(gam, newdata = i_dat)
    predvals <- exp(i_dat$gam_pred)
    
    predvals
  }
  
  pred_df <- data.frame(preds)
  colnames(pred_df) = year_seq
  rownames(pred_df) <- NULL
  
  
  if(longform){
    
    pred_df <- tibble::rowid_to_column( pred_df, "draw") %>%
      tidyr::pivot_longer(., cols = !starts_with("d")) %>%
      dplyr::rename('year' = name, "proj_y" = value)%>%
      mutate(year = as.integer(year))
    
  }
  
  return( pred_df)
  

}

