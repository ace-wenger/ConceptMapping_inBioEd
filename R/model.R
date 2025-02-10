library(metafor)
library(tidyverse)
library(formulaic)
library(clubSandwich)

### Constants
# inf_cases <- c(32, 34)

# Create string vectors of the different variables to include in models
var_ext <- c("Year_C", "Record_Type", "Country_C")
var_mth <- c("Comparison", "Setting")
var_sampint <- c("CM_Type", "Training", "Duration", "Interaction", "Level")

# Create model formulas using variable vectors
form_exmeth <- create.formula(
  outcome.name = "yi",
  input.names = c(var_ext, var_mth))

form_sampint <- create.formula(
  outcome.name = "yi",
  input.names = var_sampint)

form_sampint_list <- list()

for (i in seq_along(var_sampint)) {
  form_sampint_list[[i]] <- create.formula(
    outcome.name = "yi",
    input.names = c(var_ext, var_mth, var_sampint[[i]])
  )
}

form_univariate_list <- list()

for (i in seq_along(c(var_ext, var_mth, var_sampint))) {
  form_univariate_list[[i]] <- create.formula(
    outcome.name = "yi",
    input.names = c(var_ext, var_mth, var_sampint)[[i]]
  )
}

form_all <- create.formula(
  outcome.name = "yi",
  input.names = c(var_ext, var_mth, var_sampint))

### Models
res_basemodel <- function(data, vcv) {
  
  ### Random-effects: standard meta-analytic 2-level model.
  # Random "effect size" effects are nested within random "study" effects
  # but accounting for dependent effect sizes within study via a specified
  # variance-covariance matrix ("V").
  # rma.mv does not support the knapp-hartung method, however, the t
  # distribution is better than the z default.
  # The `control` argument was set in order to avoid local maximums in the
  # optimization algorithm.
  # Default method is restricted maximum liklelihood (REML) - can also set as
  # maximum likelihood (ML).
  
  res <- rma.mv(
    yi = data[["yi"]],
    V = vcv,
    data = data,
    random = ~ 1 | Study_ID / ES_ID,
    control = list(sigma2.init=0.5),
    test = "t",
    dfs = "contain",
    slab = Study_ID
  ) |> robust(Study_ID, clubSandwich = TRUE)
  
  return(res)
}

loop_basemodel <- function(data, vcv, name_list) {
  model_list <- list()
  
  for (i in seq_along(data)) {
    model_list[[i]] <- res_basemodel(
      data = data[[i]],
      vcv = vcv[[i]]
    )
  }
  
  names(model_list) <- name_list
  
  model_list
}

res_mrmodel <- function(data, vcv, form) {

  ### Random-effects: standard meta-analytic 2-level model.
  # Random "effect size" effects are nested within random "study" effects
  # but accounting for dependent effect sizes within study via a specified
  # variance-covariance matrix ("V").
  # rma.mv does not support the knapp-hartung method, however, the t
  # distribution is better than the z default.
  # The `control` argument was set in order to avoid local maximums in the
  # optimization algorithm.
  # Default method is restricted maximum liklelihood (REML) - can also set as
  # maximum likelihood (ML).

  res <- rma.mv(
    yi = data[["yi"]],
    V = vcv,
    data = data,
    mods = form,
    random = ~ 1 | Study_ID / ES_ID,
    control = list(sigma2.init=0.5),
    test = "t",
    dfs = "contain",
    slab = Study_ID
  ) |> robust(Study_ID, clubSandwich = TRUE)

  return(res)
}

# TESTING =====
# res_mrmodel(
#   form = form_exmeth$formula,
#   data = data_processed_out[[1]],
#   vcv = data_vcv_matrix_out[[1]]
# )
# loop_mrmodel(
#   form = form_exmeth,
#   data = data_processed_out, 
#   vcv = data_vcv_matrix_out, 
#   name_list = paste("base model w/o outliers,", r_list)
# )
# x <- loop_mrmodel_list(
#   form = form_sampint,
#   data = data_processed_out,
#   vcv = data_vcv_matrix_out,
#   name_list = paste(var_sampint, "(controlled, w/o outliers)"),
#   name_sublist = r_list
# )
# 
# x <- 1
# TESTING =====

loop_mrmodel <- function(form, data, vcv, name_list) {
  model_list <- list()

  for (i in seq_along(data)) {
    model_list[[i]] <- res_mrmodel(
      data = data[[i]],
      vcv = vcv[[i]],
      form = form$formula
    )
  }

  names(model_list) <- name_list

  model_list
}

loop_mrmodel_list <- function(form, data, vcv, name_list, name_sublist) {
  model_list <- list()
  
  for (i in seq_along(form)) {
    
    model_sublist <- list()
    
    for (j in seq_along(data)) {
      model_sublist[[j]] <- res_mrmodel(
        data = data[[j]],
        vcv = vcv[[j]],
        form = form[[i]]$formula
      )
    }
    names(model_sublist) <- name_sublist
    
    model_list[[i]] <- model_sublist
  }
  
  names(model_list) <- name_list
  
  model_list
}

### Table Extraction
# loop_basemodel <- function(data, vcv, name_list) {
#   model_list <- list()
#   
#   for (i in seq_along(data)) {
#     model_list[[i]] <- res_basemodel(
#       data = data[[i]],
#       vcv = vcv[[i]]
#     )
#   }
#   
#   names(model_list) <- name_list
#   
#   model_list
# }

loop_model_table <- function(model_list) {
  table <- list()
  
  for (i in seq_along(model_list)) {
    data <- model_list[[i]]
    data_name <- names(model_list)[[i]]
    
    table[[i]] <- compile_model_table(data, data_name)
  }
  
  names(table) <- names(model_list)
  
  table
}

loop_model_table_list <- function(model_list_list) {
  table_list <- list()
  
  for (i in seq_along(model_list_list)) {
    
    table <- list()
    
    for (j in seq_along(model_list_list[[i]])) {
      data <- model_list_list[[i]][[j]]
      data_name <- names(model_list_list[[i]])[[j]]
      
      table[[j]] <- compile_model_table(data, data_name)
    }
    
    names(table) <- names(model_list_list[[i]])
    
    table_list[[i]] <- table
  }
  
  names(table_list) <- names(model_list_list)
  
  table_list
}

# loop_basemodel_table <- function(model_list) {
#   table <- list(create_model_table())
#   
#   for (i in seq_along(model_list)) {
#     table[[i]] <- compile_model_table(model_list[[i]], table)
#   }
#   
#   names(table) <- names(model_list)
#   
#   table
# }
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

# loop_mrmodel <- function(form, data, vcv, name_list) {
#   model_list <- list()
#   
#   for (i in seq_along(data)) {
#     model_list[[i]] <- res_mrmodel(
#       data = data[[i]],
#       vcv = vcv[[i]],
#       form = form_exmeth$formula
#     )
#   }
#   
#   names(model_list) <- name_list
#   
#   model_list
# }

# loop_table_mrmodel_list

### Diagnostics
map_cooks_distance <- function(data) {
  cooks_distance <- list()
  
  # insert `r-list`
  for (i in 1:length(data)) {
    cooks_distance[[i]] <- cooks.distance.rma.mv(data[[i]], cluster = Study_ID)
  }
  
  # insert `r-list`
  names(cooks_distance) <- r_list
  cooks_distance
}

### Plots and Figures
plot_cooks <- function(data) {
  data_name <- deparse(substitute(data))
  file_name <- str_glue("figures/{data_name}.png")
  
  max_x <- max(as.numeric(names(data[[1]])))
  x_values <- as.numeric(names(data[[1]]))
  
  png(
    file_name, 
    width = 700,
    height = 325,
    type = "windows", 
    bg = "white")
 
  # Adjusting the margin to reduce whitespace
  par(mar = c(5, 5, 0, 2))  # bottom, left, top, right
   
  plot(
    NA, NA,
    xlim=c(1, max_x), ylim=c(0,0.50), xaxt="n", las=2,
    xlab="Study", ylab="Cook's Distance"
  )
  
  axis(1, 1:max_x, cex.axis=0.8, las=2)
  points(x_values, data$'0.6', type="o", cex = 1.2, pch=19, col="black")
  points(x_values, data$'0.4', type="o", pch=15, col="salmon")
  points(x_values, data$'0.8', type="o", pch=17, col="steelblue4")
  abline(h=mean(data$'0.6'), lty="dotted", lwd=2)
  # title("Cook's Distances")
  legend(
    "topright", 
    pch=c(15, 19, 17), 
    col=c("salmon", "black", "steelblue2"), 
    lty="solid", 
    legend=c("r = 0.4", "r = 0.6", "r = 0.8"), 
    bty="n"
  )
  
  dev.off()

}



# 
# ### FOREST PLOT: aggregated effect sizes by study
# # Cannot use "robust.rma" objects with `aggregate()`, thus need to run base model again in order to aggregate effect sizes by study
# data_basemodel_forest <- data_processed_out[[2]]
# data_vcv_matrix_forest <- data_vcv_matrix_out[[2]]
# 
# res <- rma.mv(
#   yi = data_processed_out[[2]][["yi"]],
#   V = data_vcv_matrix_out[[2]],
#   data = data_processed_out[[2]],
#   random = ~ 1 | Study_ID / ES_ID,
#   control = list(sigma2.init=0.5),
#   test = "t",
#   dfs = "contain",
#   slab = Study_ID
# )
# 
# # Aggregating effect sizes by study
# agg <- aggregate(
#   data_processed_out[[2]],
#   V = vcov(res, type = "obs"),
#   cluster = Study_ID,
#   addk = TRUE
# )
# 
# agg <- agg[c(1,4,5,13, 40,41,46)]
# 
# res <- rma(
#   yi,
#   vi,
#   data = agg,
#   method = "EE",
#   digits = 3,
#   slab = paste(Author, as.integer(Year), sep = ", ")
# )

# # Function to add model summary statistics on plot
# stat_model <- function(x) {
#   bquote(
#     paste(
#       "(Q = ", 
#       .(formatC(x$QE, digits=2, format="f")),
#       ", df = ", 
#       .(x$k - x$p),
#       ", p ", 
#       .(metafor:::.pval(x$QEp, digits=2, showeq=TRUE, sep=" ")), 
#       "; ",
#       # I^2, " = ", .(formatC(x$I2, digits=1, format="f")), "%, ",
#       tau[1]^2, 
#       " = ", 
#       .(formatC(x$sigma2[1], digits=2, format="f")), 
#       ", ",
#       tau[2]^2, 
#       " = ", 
#       .(formatC(x$sigma2[2], digits=2, format="f")), 
#       ")"
#     )
#   )
# }
# 
# Get the weights and format them as will be used in the plot
# weights <- paste0(formatC(weights(res), format="f", digits=2), "%")
# 
# png("figures/basemodel_forest.png", width = 750, height = 880, type = "windows", bg = "white")
# 
# par(mar = c(5, 3, 0, 1))  # bottom, left, top, right
# 
# # plot function
# sav <- forest(
#   res,
#   alim = c(-2.5,3.5),
#   at = c(-2, -1, 0, 1, 2, 3),
#   xlim=c(-9,6),
#   ylim = c(res$k + 3, 0),
#   # refline = c(coef(res), 0),
#   cex = 1,
#   mlab= "RE Model without influential studies (r = 0.6)",
#   header=TRUE,
#   ilab=cbind(ki, as.integer(N), weights),
#   ilab.xpos=c(-4.75, -4, -3),
#   ilab.pos = c(4, 4, 4),
#   textpos=c(-9,6),
#   showweights = FALSE,
#   top = 1
# )
# 
# # header labels
# text(-4.5, res$k+2, "j", font=2, cex = 1)
# text(-3.75, res$k+2, "N", font=2, cex = 1)
# text(-2.5, res$k+2, "Weight", font=2, cex = 1)
# 
# dev.off()
# 
# ### More complex forest plot with all r values and "out" combinations
# png("figures/basemodel_forest.png", width = 750, height = 940, type = "windows", bg = "white")
# 
# par(mar = c(5, 3, 0, 1))  # bottom, left, top, right
# 
# # plot function
# sav <- forest(
#   res, 
#   alim = c(-2.5,3.5), 
#   at = c(-2, -1, 0, 1, 2, 3), 
#   xlim=c(-9,6), 
#   ylim = c(res$k + 3, -4), 
#   refline = c(coef(res), 0),
#   cex = 1,
#   mlab= "RE Model without influential studies (r = 0.6)", 
#   header=TRUE, 
#   ilab=cbind(ki, as.integer(N), weights), 
#   ilab.xpos=c(-4.75, -4, -3), 
#   ilab.pos = c(4, 4, 4),
#   textpos=c(-9,6), 
#   showweights = FALSE
# )
# 
# # header labels
# text(-4.5, res$k+2, "j", font=2, cex = 1)
# text(-3.75, res$k+2, "N", font=2, cex = 1)
# text(-2.5, res$k+2, "Weight", font=2, cex = 1)
# 
# text(sav$xlim[1] + 0.25, -2.5, pos = 4, stat_model(model_base_out[[2]]))
# text(sav$xlim[1] + 0.25, -5.25, pos = 4, stat_model(model_base[[2]]))
# 
# addpoly(model_base_out[[3]], row = -2.25, annotate = FALSE, mlab = "", col = "steelblue2")
# addpoly(model_base_out[[1]], row = -2.25, annotate = FALSE, mlab = "", col = "salmon")
# 
# addpoly(model_base[[2]], row = -3.75, mlab = "RE Model with all studies (r = 0.6)")
# addpoly(model_base[[3]], row = -5, annotate = FALSE, mlab = "", col = "steelblue2")
# addpoly(model_base[[1]], row = -5, annotate = FALSE, mlab = "", col = "salmon")
# 
# # abline(v = c(res$beta,2), col = "red", lty = 2)
# 
# dev.off()
