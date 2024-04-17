library(tidyverse)

#' Title
#'
#' @param data_list 
#' @param r_values 
#'
#' @return
#' @export
#'
#' @examples
walk_write_csv <- function(data_list, r_values) {
  data_name <- as.list(sys.call())$data_list
  paths <- str_glue("data/{data_name}_{r_values}.csv")
  
  walk2(data_list, paths, \(df, name) write_csv(df, name))
}

# walk_write_tables <- function(data_list) {
#   data_name <- as.list(sys.call())$data_list
#   paths <- str_glue("data/{data_name}.csv")
#   
#   walk2(data_list, paths, \(df, name) write_csv(df, name))
# }
# 
# table_list_myown <- list(table_model_base, table_model_base_out, table_model)

#' Title
#'
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
multilevel_I2 <- function(data = res) {
  W <- diag(1/data$vi)
  X <- model.matrix(data)
  P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
  c(
    100 * sum(data$sigma2) / (sum(data$sigma2) + (data$k-data$p)/sum(diag(P))),
    100 * data$sigma2 / (sum(data$sigma2) + (data$k-data$p)/sum(diag(P)))
  )
}
# Function for I^2 calculation (from metafor site)
# sigma^2.1 = between-study heterogeneity
# sigma^2.2 = within-study heterogeneity


#' Title
#'
#' @return
#' @export
#'
#' @examples
create_model_table <- function() {
  tibble(
    model = character(),
    term = character(),
    b = numeric(),
    ci.lb = numeric(),
    ci.ub = numeric(),
    se = numeric(),
    tau = numeric(),
    sigma = numeric(),
    I2_all = numeric(),
    I2_3 = numeric(),
    I2_2 = numeric(),
    QE = numeric(),
    QEp = numeric(),
    QEdf = character(),
    QM = numeric(),
    QMp = numeric(),
    QMdf = character(),
    k = numeric(),
    j = numeric()
  )
}

# compile_model_table <- function(data) {
#   
#   I2 <- multilevel_I2(data)
#   
#     tibble(
#       model = "names(data)",
#       factor = "names(data)",
#       level = "names(data)",
#       b = round(data$b, 3),
#       ci.lb = round(data$ci.lb, 3),
#       ci.ub = round(data$ci.ub, 3),
#       se = round(data$se, 3),
#       tau = round(data$sigma2[1], 3),
#       sigma = round(data$sigma2[2], 3),
#       I2_all = round(I2[1], 1),
#       I2_3 = round(I2[2], 1),
#       I2_2 = round(I2[3], 1),
#       QM = NA,
#       QMp = NA,
#       QMdf = NA,
#       k = data$s.nlevels[1],
#       j = data$s.nlevels[2]
#     )
# }

# compile_model_table <- function(data, table) {
#   
#   I2 <- multilevel_I2(data)
#   
#   for (i in seq_along(data$p)) {
#   
#     add_row(
#       data = table,
#       model = names(data),
#       # term = 
#       #   # match `term.label` variable names (which include factors that are included as dummy variables) to the coefficient names
#       #   str_match(
#       #     names(data$b[[i]]),
#       #     attr(terms(data$formula.mods), "term.labels")
#       #   ),
#       #   # this will require some troubleshooting/trial-and-error
#       # alternatively
#       term = rownames(data$b)[i],
#       b = round(data$b, 3),
#       ci.lb = round(data$ci.lb[[i]], 3),
#       ci.ub = round(data$ci.ub, 3),
#       se = round(data$se, 3),
#       tau = round(data$sigma2[1], 3),
#       sigma = round(data$sigma2[2], 3),
#       I2_all = round(I2[1], 1),
#       I2_3 = round(I2[2], 1),
#       I2_2 = round(I2[3], 1),
#       QE = round(data$QE, 2),
#       QEp = round(data$QEp, 2),
#       QEdf = paste(data$QEdf[1], data$QEdf[2], sep = ", "),
#       QM = round(data$QM, 2),
#       QMp = round(data$QMp, 2),
#       QMdf = paste(data$QMdf[1], data$QMdf[2], sep = ", "),
#       k = data$s.nlevels[1],
#       j = data$s.nlevels[2]
#     )
#   }
# }

compile_model_table <- function(data, data_name) {
  I2 <- multilevel_I2(data)
  
  cluster_count <- as_tibble(data$X) |> 
    mutate(across(everything(), ~ if(any(. > 1)) 1 else .)) |> 
    mutate(cluster = data$cluster)
  
  j <- group_by(cluster_count, cluster) |> 
    summarize(across(everything(), ~ sum(.))) |> 
    mutate(cluster = NULL) |> 
    colSums() |> 
    as.vector()
  
  k <- group_by(cluster_count, cluster) |> 
    summarize(across(2:length(cluster_count) - 1 , ~ sum(. > 0))) |> 
    summarize(across(2:length(cluster_count), ~ sum(. > 0))) |> 
    mutate(cluster = NULL) |> 
    as.vector(mode = "numeric")
  
  for (i in 1:seq_along(data$p)) {
    
    table <- tibble(
      model = data_name,
      # term = 
      #   # match `term.label` variable names (which include factors that are included as dummy variables) to the coefficient names
      #   str_match(
      #     names(data$b[[i]]),
      #     attr(terms(data$formula.mods), "term.labels")
      #   ),
      #   # this will require some troubleshooting/trial-and-error
      # alternatively
      term = rownames(data$b),
      b = round(data$b, 3),
      ci.lb = round(data$ci.lb, 3),
      ci.ub = round(data$ci.ub, 3),
      se = round(data$se, 3),
      tau = round(data$sigma2[1], 3),
      sigma = round(data$sigma2[2], 3),
      I2_all = round(I2[1], 1),
      I2_3 = round(I2[2], 1),
      I2_2 = round(I2[3], 1),
      QE = round(data$QE, 0),
      QEp = round(data$QEp, 4),
      QEdf = data$QEdf[1],
      QM = round(data$QM, 2),
      QMp = round(data$QMp, 2),
      QMdf = paste(data$QMdf[1], round(data$QMdf[2], 2), sep = ", "),
      k = k,
      j = j
    )
    
  }
  
  table
}

#' #' Title
#' #'
#' #' @param report_tab 
#' #' @param lab 
#' #' @param data 
#' #' @param form 
#' #' @param cat 
#' #' @param mod 
#' #' @param flabel 
#' #' @param slabel 
#' #' @param tab_j 
#' #' @param tab_k 
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' compile_model_table <- function(report_tab, 
#'                                 lab,
#'                                 data,
#'                                 form,
#'                                 cat,
#'                                 mod, 
#'                                 flabel,
#'                                 slabel,
#'                                 tab_j,
#'                                 tab_k) {
#'   I2 <- multilevel_I2(data)
#'   
#'   # print(lab)
#'   # print(report_tab)
#'   
#'   # # For debugging
#'   # print(as.list(sys.call())$report_tab)
#'   # print(as.list(sys.call())$lab)
#'   # print(as.list(sys.call())$data)
#'   # print(as.list(sys.call())$form)
#'   # print(as.list(sys.call())$cat)
#'   # print(as.list(sys.call())$mod)
#'   # print(as.list(sys.call())$flabel)
#'   # print(as.list(sys.call())$slabel)
#'   
#'   # # experimental
#'   # #--- small hack to bypass issue with multiple MR models ("mr_models")
#'   # mr_model <- if(!is.null(formula.rma(data))) {
#'   #   if(length(attr(terms(formula.rma(data)), "term.labels")) > 1)
#'   #   {TRUE} else {FALSE}
#'   # } else {FALSE} 
#'   # #---
#'   
#'   base_anly <- is.null(flabel)
#'   ctgr_modr <- cat
#'   cntn_modr <- !is.null(flabel) && !cat
#'   row_fill <- rep(NA, if(length(slabel) == 0) {0} else {length(slabel) - 1})
#'   
#'   # print(report_tab)
#'   
#'   report_tab <- add_row(
#'     report_tab,
#'     model = 
#'       if (base_anly) {
#'         "RE Base Model"
#'       } else {
#'         "Moderator Analyses"
#'       },
#'     factor = 
#'       if (base_anly) {
#'         "RE Base Model"
#'       } else {
#'         flabel
#'     },
#'     level = 
#'       if (base_anly) {
#'         "RE Base Model"
#'       } else if(cntn_modr) {
#'           c(paste(flabel, "(Intercept)", sep = ""), flabel)
#'         }
#'         else {
#'           c(paste(flabel, "(", slabel[1], ")"), slabel[2:length(slabel)])
#'     },
#'     b = round(data$b, 3),
#'     ci.lb = round(data$ci.lb, 3),
#'     ci.ub = round(data$ci.ub, 3),
#'     se = round(data$se, 3),
#'     tau = c(round(data$sigma2[1], 3), row_fill),
#'     sigma = c(round(data$sigma2[2], 3), row_fill),
#'     I2_all = c(round(I2[1], 1), row_fill),
#'     I2_3 = c(round(I2[2], 1), row_fill),
#'     I2_2 = c(round(I2[3], 1), row_fill),
#'     QM = if(base_anly) {NA} else {c(round(data$QM, 2), row_fill)},
#'     QMp = if(base_anly) {NA} else {c(round(data$QMp, 3), row_fill)},
#'     QMdf = 
#'       if(base_anly) {
#'         NA
#'       } else {
#'         c(paste(data$QMdf[1], round(data$QMdf[2], 2), sep = ", "), row_fill)
#'     },
#'     k = 
#'       if(base_anly || cntn_modr || mr_model) {
#'         c(data$s.nlevels[1], row_fill)
#'       } else {
#'         colSums(tab_k[, 2:(ncol(tab_k))])
#'     },
#'     j = 
#'       if(base_anly || cntn_modr || mr_model) {
#'         c(data$s.nlevels[2], row_fill)
#'       } else {
#'         colSums(tab_j[, 2:(ncol(tab_j))])
#'     }
#'   )
#'   
#'   assign(lab, value = report_tab, envir = .GlobalEnv)
#' }
