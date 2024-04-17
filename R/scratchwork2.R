# create_model_table <- function() {
#   tibble(
#     model = character(),
#     term = character(),
#     b = numeric(),
#     ci.lb = numeric(),
#     ci.ub = numeric(),
#     se = numeric(),
#     tau = numeric(),
#     sigma = numeric(),
#     I2_all = numeric(),
#     I2_3 = numeric(),
#     I2_2 = numeric(),
#     QE = numeric(),
#     QEp = numeric(),
#     QEdf = character(),
#     QM = numeric(),
#     QMp = numeric(),
#     QMdf = character(),
#     k = numeric(),
#     j = numeric()
#   )
# }
# 
# # compile_model_table <- function(data, data_name) {
# #   table <- create_model_table()
# #   
# #   I2 <- multilevel_I2(data)
# #   
# #   for (i in seq_along(data$p)) {
# #     
# #     add_row(
# #       table,
# #       model = names(data),
# #       # term = 
# #       #   # match `term.label` variable names (which include factors that are included as dummy variables) to the coefficient names
# #       #   str_match(
# #       #     names(data$b[[i]]),
# #       #     attr(terms(data$formula.mods), "term.labels")
# #       #   ),
# #       #   # this will require some troubleshooting/trial-and-error
# #       # alternatively
# #       term = rownames(data$b)[i],
# #       b = round(data$b, 3),
# #       ci.lb = round(data$ci.lb[[i]], 3),
# #       ci.ub = round(data$ci.ub, 3),
# #       se = round(data$se, 3),
# #       tau = round(data$sigma2[1], 3),
# #       sigma = round(data$sigma2[2], 3),
# #       I2_all = round(I2[1], 1),
# #       I2_3 = round(I2[2], 1),
# #       I2_2 = round(I2[3], 1),
# #       QE = round(data$QE, 2),
# #       QEp = round(data$QEp, 2),
# #       QEdf = paste(data$QEdf[1], data$QEdf[2], sep = ", "),
# #       QM = round(data$QM, 2),
# #       QMp = round(data$QMp, 2),
# #       QMdf = paste(data$QMdf[1], data$QMdf[2], sep = ", "),
# #       k = data$s.nlevels[1],
# #       j = data$s.nlevels[2]
# #     )
# #   }
# # }
# 
# compile_model_table <- function(data, data_name) {
#   I2 <- multilevel_I2(data)
#   
#   cluster_count <- as_tibble(data$X) |> 
#     mutate(across(everything(), ~ if(any(. > 1)) 1 else .)) |> 
#     mutate(cluster = data$cluster)
#   
#   j <- group_by(cluster_count, cluster) |> 
#     summarize(across(everything(), ~ sum(.))) |> 
#     mutate(cluster = NULL) |> 
#     colSums() |> 
#     as.vector()
#   
#   k <- group_by(cluster_count, cluster) |> 
#     summarize(across(2:length(cluster_count) - 1 , ~ sum(. > 0))) |> 
#     summarize(across(2:length(cluster_count), ~ sum(. > 0))) |> 
#     mutate(cluster = NULL) |> 
#     as.vector(mode = "numeric")
#   
#   for (i in 1:seq_along(data$p)) {
#     
#     table <- tibble(
#       model = data_name,
#       # term = 
#       #   # match `term.label` variable names (which include factors that are included as dummy variables) to the coefficient names
#       #   str_match(
#       #     names(data$b[[i]]),
#       #     attr(terms(data$formula.mods), "term.labels")
#       #   ),
#       #   # this will require some troubleshooting/trial-and-error
#       # alternatively
#       term = rownames(data$b),
#       b = round(data$b, 3),
#       ci.lb = round(data$ci.lb, 3),
#       ci.ub = round(data$ci.ub, 3),
#       se = round(data$se, 3),
#       tau = round(data$sigma2[1], 3),
#       sigma = round(data$sigma2[2], 3),
#       I2_all = round(I2[1], 1),
#       I2_3 = round(I2[2], 1),
#       I2_2 = round(I2[3], 1),
#       QE = round(data$QE, 0),
#       QEp = round(data$QEp, 4),
#       QEdf = data$QEdf[1],
#       QM = round(data$QM, 2),
#       QMp = round(data$QMp, 2),
#       QMdf = paste(data$QMdf[1], round(data$QMdf[2], 2), sep = ", "),
#       k = k,
#       j = j
#     )
#     
#   }
# 
#   table
# }
# 
# 
# 
# 
# 
# table <- list()
# 
# model_list <- model_all_out
# 
# for (i in seq_along(model_list)) {
#   data <- model_list[[i]]
#   data_name <- names(model_list)[[i]]
#   
#   table[[i]] <- compile_model_table(data, data_name)
# }
# 
# 
# # loop_basemodel_table <- function(model_list) {
# #   table <- list(create_model_table())
# #   
# #   for (i in seq_along(model_list)) {
# #     table[[i]] <- compile_model_table(model_list[[i]], table)
# #   }
# #   
# #   names(table) <- names(model_list)
# #   
# #   table
# # }
# 
# ###
# table <- compile_model_table(model_list[[2]])
# 
# ###
# table <- create_model_table()
# data <- model_list[[2]]
# data_name <- names(model_list)[[2]]
# 
# I2 <- multilevel_I2(data)
# 
# for (i in 1:seq_along(data$p)) {
#   
#   table <- tibble(
#     model = data_name,
#     # term = 
#     #   # match `term.label` variable names (which include factors that are included as dummy variables) to the coefficient names
#     #   str_match(
#     #     names(data$b[[i]]),
#     #     attr(terms(data$formula.mods), "term.labels")
#     #   ),
#     #   # this will require some troubleshooting/trial-and-error
#     # alternatively
#     term = rownames(data$b)[i],
#     b = round(data$b, 3),
#     ci.lb = round(data$ci.lb[[i]], 3),
#     ci.ub = round(data$ci.ub, 3),
#     se = round(data$se, 3),
#     tau = round(data$sigma2[1], 3),
#     sigma = round(data$sigma2[2], 3),
#     I2_all = round(I2[1], 1),
#     I2_3 = round(I2[2], 1),
#     I2_2 = round(I2[3], 1),
#     QE = round(data$QE, 2),
#     QEp = round(data$QEp, 2),
#     QEdf = paste(data$QEdf[1], data$QEdf[2], sep = ", "),
#     QM = round(data$QM, 2),
#     QMp = round(data$QMp, 2),
#     QMdf = paste(data$QMdf[1], data$QMdf[2], sep = ", "),
#     k = data$s.nlevels[1],
#     j = data$s.nlevels[2]
#   )
#   
# }
# ### ===========================================================
# 
# for (i in seq_along(model_list)) {
#   data <- model_list[[i]]
#   data_name <- names(model_list)[[i]]
#   
#   table[[i]] <- compile_model_table(data, data_name)
# }
# 
# loop_model_table <- function(model_list) {
#   table <- list()
#   
#   for (i in seq_along(model_list)) {
#     data <- model_list[[i]]
#     data_name <- names(model_list)[[i]]
#     
#     table[[i]] <- compile_model_table(data, data_name)
#   }
#   
#   names(table) <- names(model_list)
#   
#   table
# }
# 
# model_base_table <- loop_basemodel_table(model_base)
# model_all_out_table <- loop_basemodel_table(model_all_out)
# 
# ####
# 
# loop_table_mrmodel <- function(model_list) {
#   table <- list()
#   
#   for (i in seq_along(model_list)) {
#     table[[i]] <- compile_model_table(model_list[[i]])
#   }
#   
#   names(table) <- names(model_list)
#   
#   table
# }
# 
# ####
# df <- df |> 
#   mutate(across(everything(), ~ if(any(. > threshold_value)) threshold_value else .))
# 
# data <- model_all_out[[2]]
# 
# cluster_count <- as_tibble(data$X) |> 
#   mutate(across(everything(), ~ if(any(. > 1)) 1 else .)) |> 
#   mutate(cluster = data$cluster)
# 
# j <- group_by(cluster_count, cluster) |> 
#   summarize(across(everything(), ~ sum(.))) |> 
#   mutate(cluster = NULL) |> 
#   colSums() |> 
#   as.vector()
#   
# k <- group_by(cluster_count, cluster) |> 
#   summarize(across(2:length(cluster_count) - 1 , ~ sum(. > 0))) |> 
#   summarize(across(2:length(cluster_count), ~ sum(. > 0))) |> 
#   mutate(cluster = NULL) |> 
#   as.vector(mode = "numeric")
