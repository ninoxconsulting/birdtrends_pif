# plot_trend <- function(raw_indices = indat1,
#                        model_indices = fitted_data,
#                        pred_indices =  preds_sm,
#                        start_yr = 2014,
#                        end_yr = 2022,
#                        ref_yr = 2014,
#                        targets = NULL){
#   
#   #
#   # #   ## start testing
#      raw_indices = NULL
#      model_indices = fitted_data
#      pred_indices = preds_sm
#      start_yr = 2014
#      end_yr = 2021
#      ref_yr = 2014
#      targets = targ
#     
#    
#    ## end testing
#   
#   if(any(is.na(c(start_yr, end_yr, ref_yr)))){
#     stop("Opps - you are missing year values - please ensure start, end and ref year are defined")
#   }
#   
#   min_yr <- min(model_indices$year)
#   max_yr <- max(pred_indices$year)
#   
#   if(start_yr < min_yr) {
#     message("`start_yr` is before the date range, using minimum year of ",
#             "the data (", start_yr <- min_yr, ") instead.")
#   }
#   
#   if(end_yr > max_yr) {
#     message("`end_year` is beyond the date range, using maximum year of ",
#             "the data (", end_yr <- max_yr, ") instead.")
#   }
#   
#   
#   gam_summarized <- model_indices %>%
#     dplyr::group_by(year) %>%
#     dplyr::summarize(gam_q_0.025 = stats::quantile(proj_y,0.025),
#                      gam_index = stats::quantile(proj_y,0.5),
#                      gam_q_0.975 = stats::quantile(proj_y,0.975))
#   
#   predict_summarized <- pred_indices |>
#     dplyr::group_by(year)%>%
#     dplyr::summarize(pred_q_0.025 = stats::quantile(pred_ind,0.025),
#                      pred_index = stats::quantile(pred_ind,0.5),
#                      pred_q_0.975 = stats::quantile(pred_ind,0.975)) %>%
#     dplyr::filter(year >= end_yr)
#   
#   # estimate target recovery
#   if(!is.null(targets)){
#     
#     # generate a predicted target
#     
#     start_target_yr <-  gam_summarized %>%
#       filter(year == ref_yr)
#     
#     # generate short-term trend
#     st_year <- targets$st_year
#     st_year_seq = seq(ref_yr, st_year, 1)
#     st_index_seq = seq(start_target_yr$gam_index, ((targets$st_up_target + targets$st_lu_target)/2), length.out = length(st_year_seq))
#     
#     target_indices <- as_tibble(cbind(st_year_seq, st_index_seq))
#     
#     
#     # generate long-term trend
#     lt_year <- targets$lt_year
#     lt_year_seq = seq(tail(st_year_seq, n = 1), lt_year, 1)
#     lt_index_seq = seq(tail(st_index_seq , n = 1), ((targets$lt_up_target + targets$lt_lu_target)/2), length.out = length(lt_year_seq))
#     
#     target_indices_lt <- as_tibble(cbind(lt_year_seq, lt_index_seq))
#     colnames(target_indices_lt) <- c("st_year_seq", "st_index_seq")
#     
#     target_indices <- bind_rows(target_indices, target_indices_lt)
#     
#   }
#   
#   sp_plot_index <- ggplot2::ggplot() +
#     
#     # Vertical line showing the year goals were set
#     ggplot2::geom_vline(xintercept =  ref_yr, size=2, col = "black", alpha = 0.2)+
#     #geom_text(aes(x =  ref_year+1, y = 0.01),
#     #          label = "<- reference year", col = "black", alpha = 0.2,
#     #          hjust=0, fontface = "bold", size = 2)+
#     
#     ggplot2::geom_vline(xintercept = 2026, size=1, col = "black", alpha = 0.4, linetype="dotted")+
#     ggplot2::geom_vline(xintercept = 2046, size=1, col = "black", alpha = 0.4, linetype="dotted")+
#     
#     # Observed indices
#     #ggplot2::geom_errorbar(data = subset(raw_indices, year <= max(raw_indices$year)),aes(x = year, ymin = index_q_0.025, ymax = index_q_0.975), width = 0, col = "gray30")+
#     #ggplot2::geom_point(data = subset(raw_indices, year <= max(raw_indices$year)),aes(x = year, y = index), col = "gray30")+
#     
#     # gam smooth
#     ggplot2::geom_ribbon(data = gam_summarized, aes(x = year, ymin = gam_q_0.025, ymax = gam_q_0.975), alpha = 0.4, fill = "gray50")+
#     ggplot2::geom_line(data = gam_summarized, aes(x = year, y = gam_index), col = "gray50", linewidth = 1)+
#     
#     # predicted projection
#     ggplot2::geom_ribbon(data = subset(predict_summarized, year >= end_yr), aes(x = year, ymin = pred_q_0.025, ymax = pred_q_0.975), alpha = 0.2, fill = "orangered")+
#     ggplot2::geom_line(data = subset(predict_summarized, year >= end_yr), aes(x = year, y = pred_index), col = "orangered", linewidth = 1)+
#     
#     ggplot2::ylab("Annual Index of Abundance")+
#     ggplot2::xlab("Year")+
#     ggplot2::theme_bw()
#   
#   if(!is.null(raw_indices)){
#     
#     sp_plot_index <-  sp_plot_index +
#     
#     # Observed indices
#     ggplot2::geom_errorbar(data = subset(raw_indices, year <= max(raw_indices$year)),aes(x = year, ymin = index_q_0.025, ymax = index_q_0.975), width = 0, col = "gray30")+
#     ggplot2::geom_point(data = subset(raw_indices, year <= max(raw_indices$year)),aes(x = year, y = index), col = "gray30")
#       
#     }
#   
#   if(!is.null(targets)){
#     
#     sp_plot_index <-  sp_plot_index +
#       
#       # target range - short term
#       ggplot2::geom_errorbar(data = targets , aes(x = st_year, ymin = st_lu_target, ymax = st_up_target), width = 1, size = 0.75,col = "blue")+
#       
#       ggplot2::geom_line(data = target_indices, aes(x = st_year_seq, y = st_index_seq), col = "blue", linewidth = 1)+
#       #ggplot2::geom_line(data = target_indices_lt, aes(x = lt_year_seq, y = lt_index_seq), col = "blue", linewidth = 1)+
#       #ggplot2::geom_linerange(data = targets , aes(x = st_year, ymin = st_lu_target, ymax = st_up_target), col = "blue")+
#       
#       if(!is.na(targets$lt_year)){
#         # target range - short term
#         ggplot2::geom_errorbar(data = targets ,aes(x = lt_year, ymin = lt_lu_target, ymax =lt_up_target), width = 1, size = 0.75, col = "blue")
#         #ggplot2::geom_point(data = Index_baseline,aes(x = 2024, y = ave_ref_index), col = "blue", size = 2)+
#       }
#     
#   }
#   
#   print(sp_plot_index)
#   
# }
# 
# 
# ## 
# 
# plot_trend <- function(raw_indices = NULL,
#                        model_indices = fitted_data,
#                        pred_indices =  preds_sm,
#                        start_yr = 2014,
#                        end_yr = 2022,
#                        ref_yr = 2014,
#                        targets = NULL){
#   
#   #
# #   ## start testing
# raw_indices = NULL
#  model_indices = fitted_data
#  pred_indices = preds_sm
#  start_yr = 2014
#  end_yr = 2021
#  ref_yr = 2014
#  targets = NULL
#   #  #targets = index_baseline
#   # ## end testing
#   
#   if(any(is.na(c(start_yr, end_yr, ref_yr)))){
#     stop("Opps - you are missing year values - please ensure start, end and ref year are defined")
#   }
#   
#   min_yr <- min(model_indices$year)
#   max_yr <- max(pred_indices$year)
#   
#   if(start_yr < min_yr) {
#     message("`start_yr` is before the date range, using minimum year of ",
#             "the data (", start_yr <- min_yr, ") instead.")
#   }
#   
#   if(end_yr > max_yr) {
#     message("`end_year` is beyond the date range, using maximum year of ",
#             "the data (", end_yr <- max_yr, ") instead.")
#   }
#   
#   
#   gam_summarized <- model_indices %>%
#     dplyr::group_by(year) %>%
#     dplyr::summarize(gam_q_0.025 = stats::quantile(proj_y,0.025),
#                      gam_index = stats::quantile(proj_y,0.5),
#                      gam_q_0.975 = stats::quantile(proj_y,0.975), 
#                      gam_q_0.05 = stats::quantile(proj_y,0.05), 
#                      gam_q_0.95 = stats::quantile(proj_y,0.95))
#   
#   predict_summarized <- pred_indices |>
#     dplyr::group_by(year)%>%
#     dplyr::summarize(pred_q_0.025 = stats::quantile(pred_ind,0.025),
#                      pred_index = stats::quantile(pred_ind,0.5),
#                      pred_q_0.975 = stats::quantile(pred_ind,0.975)) %>%
#     dplyr::filter(year >= end_yr)
#   
#   # estimate target recovery
#   if(!is.null(targets)){
#     
#     # generate a predicted target
#     
#     start_target_yr <-  gam_summarized %>%
#       filter(year == ref_yr)
#     
#     # generate short-term trend
#     st_year <- targets$st_year
#     st_year_seq = seq(ref_yr, st_year, 1)
#     st_index_seq = seq(start_target_yr$gam_index, ((targets$st_up_target + targets$st_lu_target)/2), length.out = length(st_year_seq))
#     
#     target_indices <- as_tibble(cbind(st_year_seq, st_index_seq))
#     
#     
#     # generate long-term trend
#     lt_year <- targets$lt_year
#     lt_year_seq = seq(tail(st_year_seq, n = 1), lt_year, 1)
#     lt_index_seq = seq(tail(st_index_seq , n = 1), ((targets$lt_up_target + targets$lt_lu_target)/2), length.out = length(lt_year_seq))
#     
#     target_indices_lt <- as_tibble(cbind(lt_year_seq, lt_index_seq))
#     colnames(target_indices_lt) <- c("st_year_seq", "st_index_seq")
#     
#     target_indices <- bind_rows(target_indices, target_indices_lt)
#     
#   }
#   
#   sp_plot_index <- ggplot2::ggplot() +
#     
#     # Vertical line showing the year goals were set
#     ggplot2::geom_vline(xintercept =  ref_yr, size=2, col = "black", alpha = 0.2)+
#     #geom_text(aes(x =  ref_year+1, y = 0.01),
#     #          label = "<- reference year", col = "black", alpha = 0.2,
#     #          hjust=0, fontface = "bold", size = 2)+
#     
#     ggplot2::geom_vline(xintercept = 2026, size=1, col = "black", alpha = 0.4, linetype="dotted")+
#     ggplot2::geom_vline(xintercept = 2046, size=1, col = "black", alpha = 0.4, linetype="dotted")+
#     
#     # gam smooth
#     ggplot2::geom_ribbon(data = gam_summarized, aes(x = year, ymin = gam_q_0.025, ymax = gam_q_0.975), alpha = 0.4, fill = "gray50")+
#     ggplot2::geom_line(data = gam_summarized, aes(x = year, y = gam_index), col = "gray50", size = 1)+
#     
#     # predicted projection
#     ggplot2::geom_ribbon(data = subset(predict_summarized, year >= end_yr), aes(x = year, ymin = pred_q_0.025, ymax = pred_q_0.975), alpha = 0.2, fill = "orangered")+
#     ggplot2::geom_line(data = subset(predict_summarized, year >= end_yr), aes(x = year, y = pred_index), col = "orangered", size = 1)+
#     
#     ggplot2::ylab("Annual Index of Abundance")+
#     ggplot2::xlab("Year")+
#     ggplot2::theme_bw()
#   
#   
#   if(!is.null(raw_indices)){
#     
#     sp_plot_index <-  sp_plot_index +
#       
#       # Observed indices
#       ggplot2::geom_errorbar(data = subset(raw_indices, year <= max(raw_indices$year)),aes(x = year, ymin = index_q_0.025, ymax = index_q_0.975), width = 0, col = "gray30")+
#       ggplot2::geom_point(data = subset(raw_indices, year <= max(raw_indices$year)),aes(x = year, y = index), col = "gray30")
#     
#   }
#   
#   # note if no raw_indices is supplied then we can estimate these from the projected data
#   
#   if(is.null(raw_indices)){
#     
#     sp_plot_index <-  sp_plot_index +
#       
#       ggplot2::geom_errorbar(data = subset(gam_summarized, year <= max(gam_summarized$year)),aes(x = year, ymin = gam_q_0.05, ymax = gam_q_0.95), width = 0, col = "gray30")+
#       ggplot2::geom_point(data = subset(gam_summarized , year <= max(gam_summarized$year)),aes(x = year, y = gam_index), col = "gray30")
#     
#   }
#   
#   # if no targets specified we can skip this part 
#   
#   if(!is.null(targets)){
#     
#     sp_plot_index <-  sp_plot_index +
#       
#       # target range - short term
#       ggplot2::geom_errorbar(data = targets , aes(x = st_year, ymin = st_lu_target, ymax = st_up_target), width = 1, size = 0.75,col = "blue")+
#       
#       ggplot2::geom_line(data = target_indices, aes(x = st_year_seq, y = st_index_seq), col = "blue", linewidth = 1)+
#       #ggplot2::geom_line(data = target_indices_lt, aes(x = lt_year_seq, y = lt_index_seq), col = "blue", linewidth = 1)+
#       #ggplot2::geom_linerange(data = targets , aes(x = st_year, ymin = st_lu_target, ymax = st_up_target), col = "blue")+
#       
#       if(!is.na(targets$lt_year)){
#         # target range - short term
#         ggplot2::geom_errorbar(data = targets ,aes(x = lt_year, ymin = lt_lu_target, ymax =lt_up_target), width = 1, size = 0.75, col = "blue")
#         #ggplot2::geom_point(data = Index_baseline,aes(x = 2024, y = ave_ref_index), col = "blue", size = 2)+
#       }
#     
#   }
#   
#   print(sp_plot_index)
#   
# }
# 
# plot_trend <- function(raw_indices = NULL,
#                        model_indices = fitted_data,
#                        pred_indices =  preds_sm,
#                        start_yr = 2014,
#                        end_yr = 2022,
#                        ref_yr = 2014,
#                        targets = NULL){
#   
#   #
#   # #   ## start testing
#   #  raw_indices = indat1
#   #  model_indices = ldf
#   #  pred_indices = pred_sm
#   #  start_yr = 1980
#   #  end_yr = 2000
#   #  ref_yr = 1970
#   #  targets = NULL
#   #  #targets = index_baseline
#   # ## end testing
#   
#   if(any(is.na(c(start_yr, end_yr, ref_yr)))){
#     stop("Opps - you are missing year values - please ensure start, end and ref year are defined")
#   }
#   
#   min_yr <- min(model_indices$year)
#   max_yr <- max(pred_indices$year)
#   
#   if(start_yr < min_yr) {
#     message("`start_yr` is before the date range, using minimum year of ",
#             "the data (", start_yr <- min_yr, ") instead.")
#   }
#   
#   if(end_yr > max_yr) {
#     message("`end_year` is beyond the date range, using maximum year of ",
#             "the data (", end_yr <- max_yr, ") instead.")
#   }
#   
#   
#   gam_summarized <- model_indices %>%
#     dplyr::group_by(year) %>%
#     dplyr::summarize(gam_q_0.025 = stats::quantile(proj_y,0.025),
#                      gam_index = stats::quantile(proj_y,0.5),
#                      gam_q_0.975 = stats::quantile(proj_y,0.975),
#                      gam_q_0.05 = stats::quantile(proj_y,0.05), # these are only used when no actual annual indices data
#                      gam_q_0.95 = stats::quantile(proj_y,0.95))
#   
#   
#   
#   predict_summarized <- pred_indices |>
#     dplyr::group_by(year)%>%
#     dplyr::summarize(pred_q_0.025 = stats::quantile(pred_ind,0.025),
#                      pred_index = stats::quantile(pred_ind,0.5),
#                      pred_q_0.975 = stats::quantile(pred_ind,0.975)) %>%
#     dplyr::filter(year >= end_yr)
#   
#   # estimate target recovery
#   if(!is.null(targets)){
#     
#     # generate a predicted target
#     
#     start_target_yr <-  gam_summarized %>%
#       filter(year == ref_yr)
#     
#     # generate short-term trend
#     st_year <- targets$st_year
#     st_year_seq = seq(ref_yr, st_year, 1)
#     st_index_seq = seq(start_target_yr$gam_index, ((targets$st_up_target + targets$st_lu_target)/2), length.out = length(st_year_seq))
#     
#     target_indices <- as_tibble(cbind(st_year_seq, st_index_seq))
#     
#     
#     # generate long-term trend
#     lt_year <- targets$lt_year
#     lt_year_seq = seq(tail(st_year_seq, n = 1), lt_year, 1)
#     lt_index_seq = seq(tail(st_index_seq , n = 1), ((targets$lt_up_target + targets$lt_lu_target)/2), length.out = length(lt_year_seq))
#     
#     target_indices_lt <- as_tibble(cbind(lt_year_seq, lt_index_seq))
#     colnames(target_indices_lt) <- c("st_year_seq", "st_index_seq")
#     
#     target_indices <- bind_rows(target_indices, target_indices_lt)
#     
#   }
#   
#   sp_plot_index <- ggplot2::ggplot() +
#     
#     # Vertical line showing the year goals were set
#     ggplot2::geom_vline(xintercept =  ref_yr, size=2, col = "black", alpha = 0.2)+
#     #geom_text(aes(x =  ref_year+1, y = 0.01),
#     #          label = "<- reference year", col = "black", alpha = 0.2,
#     #          hjust=0, fontface = "bold", size = 2)+
#     
#     ggplot2::geom_vline(xintercept = 2026, size=1, col = "black", alpha = 0.4, linetype="dotted")+
#     ggplot2::geom_vline(xintercept = 2046, size=1, col = "black", alpha = 0.4, linetype="dotted")+
#     
#     # gam smooth
#     ggplot2::geom_ribbon(data = gam_summarized, aes(x = year, ymin = gam_q_0.025, ymax = gam_q_0.975), alpha = 0.4, fill = "gray50")+
#     ggplot2::geom_line(data = gam_summarized, aes(x = year, y = gam_index), col = "gray50", linewidth = 1)+
#     
#     # predicted projection
#     ggplot2::geom_ribbon(data = subset(predict_summarized, year >= end_yr), aes(x = year, ymin = pred_q_0.025, ymax = pred_q_0.975), alpha = 0.2, fill = "orangered")+
#     ggplot2::geom_line(data = subset(predict_summarized, year >= end_yr), aes(x = year, y = pred_index), col = "orangered", linewidth = 1)+
#     
#     ggplot2::ylab("Annual Index of Abundance")+
#     ggplot2::xlab("Year")+
#     ggplot2::theme_bw()
#   
#   
#   if(!is.null(raw_indices)){
#     
#     sp_plot_index <-  sp_plot_index +
#       
#       # Observed indices
#       ggplot2::geom_errorbar(data = subset(raw_indices, year <= max(raw_indices$year)),aes(x = year, ymin = index_q_0.025, ymax = index_q_0.975), width = 0, col = "gray30")+
#       ggplot2::geom_point(data = subset(raw_indices, year <= max(raw_indices$year)),aes(x = year, y = index), col = "gray30")
#     
#   }
#   
#   # note if no raw_indices is supplied then we can estimate these from the projected data
#   
#   if(is.null(raw_indices)){
#     
#     sp_plot_index <-  sp_plot_index +
#       
#       ggplot2::geom_errorbar(data = subset(gam_summarized, year <= max(gam_summarized$year)),aes(x = year, ymin = gam_q_0.05, ymax = gam_q_0.95), width = 0, col = "gray30")+
#       ggplot2::geom_point(data = subset(gam_summarized , year <= max(gam_summarized$year)),aes(x = year, y = gam_index), col = "gray30")
#     
#   }
#   
#   if(!is.null(targets)){
#     
#     sp_plot_index <-  sp_plot_index +
#       
#       # target range - short term
#       ggplot2::geom_errorbar(data = targets , aes(x = st_year, ymin = st_lu_target, ymax = st_up_target), width = 1, size = 0.75,col = "blue")+
#       
#       ggplot2::geom_line(data = target_indices, aes(x = st_year_seq, y = st_index_seq), col = "blue", linewidth = 1)+
#       #ggplot2::geom_line(data = target_indices_lt, aes(x = lt_year_seq, y = lt_index_seq), col = "blue", linewidth = 1)+
#       #ggplot2::geom_linerange(data = targets , aes(x = st_year, ymin = st_lu_target, ymax = st_up_target), col = "blue")+
#       
#       if(!is.na(targets$lt_year)){
#         # target range - short term
#         ggplot2::geom_errorbar(data = targets ,aes(x = lt_year, ymin = lt_lu_target, ymax =lt_up_target), width = 1, size = 0.75, col = "blue")
#         #ggplot2::geom_point(data = Index_baseline,aes(x = 2024, y = ave_ref_index), col = "blue", size = 2)+
#       }
#     
#   }
#   
#   print(sp_plot_index)
#   
# }
# 
# 


