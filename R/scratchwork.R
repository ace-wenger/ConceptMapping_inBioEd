# object_name <- function(object){
#   deparse(substitute(object))
# }
# 
# 
# file <- here::here("data", "raw_es_dataframe_v6.csv")
# 
# data_raw <- prepare_flat_data(file)
# write_csv(data_raw, here::here("data", "data_raw.csv"))
# 
# ###
# data_raw_change <- map_estimate_sd_change(data_raw, r_values = c(0.4, 0.6, 0.8))
# # walk_write_csv(data_list = data_raw_change, r_values = c(0.4, 0.6, 0.8))
# 
# # data_raw_post <- assume_postsd(data_raw)
# # write_csv(data_raw_post, here::here("data", "data_raw_post.csv"))
# 
# ###
# data_processed_change <- map_calc_es_prepost(data_raw_change)
# # walk_write_csv(data_list = data_processed_change, r_values = c(0.4, 0.6, 0.8))
# 
# # data_processed_change_test <- map_calc_es_prepost_test(data_raw_change)
# # walk_write_csv(data_list = data_processed_change_test, r_values = c(0.4, 0.6, 0.8))
# 
# # data_processed_change_test2 <- calc_es_prepost_test(data_raw)
# 
# # data_processed_post <- calc_es_postonly(data_raw_post)
# # write_csv(data_processed_post, here::here("data", "data_processed_post.csv"))
# ##
# data_vcv_matrix_change <- map_calc_vcv_matrix(
#   data_processed_change,
#   between_measure_r = 0.4,
#   within_measure_r = 0.6
# )
# 
# # data_vcv_matrix_change_test <- map_calc_vcv_matrix(
# #   data_processed_change_test,
# #   between_measure_r = 0.4,
# #   within_measure_r = 0.6
# # )
# #
# # data_vcv_matrix_post <- calc_matrix(
# #   data_processed_post,
# #   between_measure_r = 0.4,
# #   within_measure_r = 0.6
# # )
# 
# ##
# results_basemodel_change <- map_basemodel(
#   data = data_processed_change,
#   vcv = data_vcv_matrix_change
# )
# 
# # results_basemodel_change_test <- map_basemodel(
# #   data = data_processed_change_test,
# #   vcv = data_vcv_matrix_change_test
# # )
# #
# # results_basemodel_post <- basemodel(data = data_processed_post, vcv = data_vcv_matrix_post)
# 
# ##
# cooks_distance_change <- map_cooks_distance(results_basemodel_change)
# # cooks_distance_post <- cooks.distance.rma.mv(results_basemodel_post)
# 
# 
# # cooks_distance_cmb <- append(cooks_distance_change, list(post = cooks_distance_post))
# 
# plot_cooks(data = cooks_distance_change)
# 
# ##
# data_processed_out <- map(data_processed_change, ~ filter(.x, Study_ID != "32", Study_ID !="34"))
# 
# data_vcv_matrix_out <- map_calc_vcv_matrix(data_processed_out, between_measure_r = 0.4, within_measure_r = 0.6)
# 
# model_base_out <- map_basemodel(data_processed_out, data_vcv_matrix_out)
# 
# model_base_cooks_out <- map_cooks_distance(model_base_out)
# 
# plot_cooks(data = model_base_cooks_out)
# 
# ##
# targets::tar_load_everything()
# 
# tab_test <- create_model_table()
# 
# res_basemodel_test <- res_model(
#   data = data_processed_out[[2]],
#   V = data_vcv_matrix_out[[2]],
#   report_tab = tab_test
# )
# 
# ## Trying `map`
# map_basemodel <- function(data, V) {
#   map2(data, V, res_basemodel())
# }
# 
# map_basemodel(
#   data = data_processed_out,
#   V = data_vcv_matrix_out,
#   report_tab = tab_test
# )
# 
# ## Trying `loop`
# loop_basemodel <- function(data, V) {
#   model_list <- list()
# 
#   for (i in seq_along(data_processed_out)) {
#     model_list[[i]] <- res_model(
#       data = data[[i]],
#       V = V[[i]]
#     )
#   }
# 
#   model_list
# }
# 
# loop_basemodel_table <- function(data, report_tab)
#   report_tab <- create_model_table()
# 
#   for (i in seq_along(data)) {
#     report_tab[[i]] <- compile_model_table2(data = data[[i]])
# }
# 
# ## Testing...
# map_basemodel(
#   data_processed_out,
#   data_vcv_matrix_out
# )
# 
# x <- loop_basemodel(
#   data = data_processed_out,
#   V = data_vcv_matrix_out
# )
# 
# compile_model_table2(x[[1]])
# 
# 
# # loop_basemodel <- function(data, V, report_tab) {
# #   model_list <- list()
# #
# #   model_list[[1]] <- res_model(
# #     data = data[[1]],
# #     V = V[[1]],
# #     report_tab = report_tab
# #   )
# #
# #   model_list
# # }
# 
# model_x <- loop_basemodel(
#   data = data_processed_out,
#   V = data_vcv_matrix_out,
#   report_tab = tab_test
# )
# 
# x <- res_model(
#   data = data_processed_out[[1]],
#   V = data_vcv_matrix_out[[1]],
#   report_tab = tab_test
# )
# 
# ##
# res_basemodel <- function(
#   data,
#   V,
#   formula = NULL,
#   flabel = NULL,
#   slabel = flabel,
#   cat = TRUE,
#   report_tab = NULL,
#   prof = FALSE,
#   robu = TRUE) {
# 
#   ### Random-effects: standard meta-analytic 2-level model.
#   # Random "effect size" effects are nested within random "study" effects
#   # but accounting for dependent effect sizes within study via a specified
#   # variance-covariance matrix ("V").
#   # rma.mv does not support the knapp-hartung method, however, the t
#   # distribution is better than the z default.
#   # The `control` argument was set in order to avoid local maximums in the
#   # optimization algorithm.
#   # Default method is restricted maximum liklelihood (REML) - can also set as
#   # maximum likelihood (ML).
# 
#   res <- rma.mv(
#     yi = yi,
#     V = V,
#     data = data,
#     random = ~ 1 | Study_ID / ES_ID,
#     control = list(sigma2.init=0.5),
#     test = "t",
#     dfs = "contain",
#     slab = Study_ID
#   ) |> robust(res, Study_ID, clubSandwich = TRUE)
# 
#   return(res)
# }
# 
# compile_model_table2 <- function(data) {
# 
#   I2 <- multilevel_I2(data)
# 
#   c(
#     model = "RE Base Model",
#     factor = "RE Base Model",
#     level = "RE Base Model",
#     b = round(data$b, 3),
#     ci.lb = round(data$ci.lb, 3),
#     ci.ub = round(data$ci.ub, 3),
#     se = round(data$se, 3),
#     tau = round(data$sigma2[1], 3),
#     sigma = round(data$sigma2[2], 3),
#     I2_all = round(I2[1], 1),
#     I2_3 = round(I2[2], 1),
#     I2_2 = round(I2[3], 1),
#     QM = NA,
#     QMp = NA,
#     QMdf = NA,
#     k = data$s.nlevels[1],
#     j = data$s.nlevels[2]
#   )
# }

# library(wildmeta)
# 
# run_cwb(
#   model = model_exmeth_out[[1]],
#   # cluster = Study_ID,
#   R = 10,
# )
# 
# Wald_test_cwb(
#   full_model = model_exmeth_out[[2]],
#   constraints = constrain_equal(1:3),
#   R = 10
# )
# `wildmeta` does not work with `robust.rma` objects only `rma`
