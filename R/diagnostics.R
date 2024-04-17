# # res <- rma.mv(
# #   yi = yi,
# #   V = data_vcv_matrix_out[[2]],
# #   data = data_processed_out[[2]],
# #   random = ~ 1 | Study_ID / ES_ID,
# #   control = list(sigma2.init=0.5),
# #   test = "t",
# #   dfs = "contain",
# #   slab = Study_ID
# # )
# # 
# # funnel(res)
# # regtest(res)
# # 
# # resma <- rma.mv(
# #   yi = yi,
# #   V = data_vcv_matrix_out[[2]],
# #   data = data_processed_out[[2]],
# #   mods = form_all$formula,
# #   random = ~ 1 | Study_ID / ES_ID,
# #   control = list(sigma2.init=0.5),
# #   test = "t",
# #   dfs = "contain",
# #   slab = Study_ID
# # )
# # 
# # funnel(resma)
# # regtest(resma)
# # 
# # vif1 <- vif(resma)
# # vif2 <- vif(resma, reestimate = TRUE, table = TRUE)
# # vif3 <- vif(resma, reestimate = TRUE, sim = 100)
# # vif4 <- vif(resma, reestimate = TRUE, btt = "Level")
# # 
# 
# # Can a funnel plot and regression test be performed using a mixed-effects model?
# # In this case, with all variables I have?
# # Obviously no because some of the variables will differ within-study, so when effect sizes are aggregated by study that information will be lost
# 
# ### Aggregated
# resagg <- rma.mv(
#   yi = data_processed_out[[2]][["yi"]],
#   V = data_vcv_matrix_out[[2]],
#   data = data_processed_out[[2]],
#   mods = form_all$formula,
#   random = ~ 1 | Study_ID / ES_ID,
#   control = list(sigma2.init=0.5),
#   test = "t",
#   dfs = "contain",
#   slab = Study_ID
# )
# 
# par(mar = c(5, 5, 0, 2))  # bottom, left, top, right
# 
# png(
#   "figures/funnel_plot_all.png", 
#   width = 500,
#   height = 400,
#   type = "windows", 
#   bg = "white")
# 
# funnel(resagg)
# 
# dev.off()
# 
# # Aggregating effect sizes by study
# agg <- aggregate(
#   data_processed_out[[2]],
#   V = vcov(resagg, type = "obs"),
#   cluster = Study_ID,
#   addk = TRUE
# )
# 
# agg <- agg[c(1,4,5,13, 40,41,46)]
# 
# resagg <- rma(
#   yi,
#   vi,
#   data = agg,
#   method = "EE",
#   digits = 3,
#   test = "t",
#   slab = paste(Author, as.integer(Year), sep = ", ")
# )
# # resagg$ni <- log(agg$N)
# 
# # Adjusting the margin to reduce whitespace
# par(mar = c(5, 5, 0, 2))  # bottom, left, top, right
# 
# png(
#   "figures/funnel_plot_agg.png", 
#   width = 500,
#   height = 400,
#   type = "windows", 
#   bg = "white")
# 
# funnel(resagg)
# 
# dev.off()
# 
# regtest(resagg, ret.fit = TRUE)
# 
# # sel <- metafor::selmodel(resagg, type = "stepfun", steps = c(0.01, 0.05, 0.10, 0.50, 1))
# # plot(sel)
# # qqnorm(resagg)
# # 
# # ### Aggregated
# # resmaagg <- rma.mv(
# #   yi = data_processed_out[[2]][["yi"]],
# #   V = data_vcv_matrix_out[[2]],
# #   data = data_processed_out[[2]],
# #   mods = form_all$formula,
# #   random = ~ 1 | Study_ID / ES_ID,
# #   control = list(sigma2.init=0.5),
# #   test = "t",
# #   dfs = "contain",
# #   slab = Study_ID
# # )
# # 
# # # Aggregating effect sizes by study
# # agg <- aggregate(
# #   data_processed_out[[2]],
# #   V = vcov(resmaagg, type = "obs"),
# #   cluster = Study_ID,
# #   addk = TRUE
# # )
# # 
# # agg <- agg[c(1,4,5,13, 40,41,46)]
# # 
# # resmaagg <- rma(
# #   yi,
# #   vi,
# #   data = agg,
# #   method = "EE",
# #   digits = 3,
# #   slab = paste(Author, as.integer(Year), sep = ", ")
# # )
# # 
# # funnel(resmaagg)
# # regtest(resmaagg)
# # 
# # ### Curiosity 
# # res <- resmaagg
# # weights <- paste0(formatC(weights(res), format="f", digits=2), "%")
# # 
# # # png("figures/basemodel_forest.png", width = 750, height = 880, type = "windows", bg = "white")
# # 
# # par(mar = c(5, 3, 0, 1))  # bottom, left, top, right
# # 
# # # plot function
# # sav <- forest(
# #   res,
# #   alim = c(-2.5,3.5),
# #   at = c(-2, -1, 0, 1, 2, 3),
# #   xlim=c(-9,6),
# #   ylim = c(res$k + 3, 0),
# #   # refline = c(coef(res), 0),
# #   cex = 1,
# #   mlab= "RE Model without influential studies (r = 0.6)",
# #   header=TRUE,
# #   ilab=cbind(ki, as.integer(N), weights),
# #   ilab.xpos=c(-4.75, -4, -3),
# #   ilab.pos = c(4, 4, 4),
# #   textpos=c(-9,6),
# #   showweights = FALSE,
# #   top = 1
# # )
# # 
# # # header labels
# # text(-4.5, res$k+2, "j", font=2, cex = 1)
# # text(-3.75, res$k+2, "N", font=2, cex = 1)
# # text(-2.5, res$k+2, "Weight", font=2, cex = 1)
# # 
# # dev.off()
