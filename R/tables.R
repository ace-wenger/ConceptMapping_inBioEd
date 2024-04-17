# This file is meant to operate outside of the `targets` pipeline and only run
# directly by the user

### Setup and parameters ======================================================
library(tidyverse)
library(flextable)

# Set path(s) for table objects to be saved to
table_paths <- c(
  here::here("tables"), 
  "C:/Users/aw647/Documents/RProjects_Rp/my_dissertation/dissertation_proposal_manuscript/tables"
)

project_label <- "CMBioEd"

### Helpers ===================================================================
save_table <- function(
    flexobject, 
    label_name = project_label, 
    path_names = table_paths) {
  flex_name <- deparse(substitute(flexobject))
  files_to_write <- str_glue("{path_names}/flt_{label_name}_{flex_name}")
  
  walk(files_to_write, \(name) write_rds(flexobject, file = name))
}

round_neat <- function(x, digits = 2) {
  formatted_value <- sprintf(paste("%.", digits, "f", sep = ""), round(x, digits))
  return(formatted_value)
}

calc_r2 <- function(ref, Model2) {
  r1 <- (ref$sigma2[1] - Model2$sigma2[1]) / ref$sigma2[1]
  r2 <- (ref$sigma2[2] - Model2$sigma2[2]) / ref$sigma2[2]
  
  r1 <- if_else(r1 < 0, 0, r1)
  r2 <- if_else(r2 < 0, 0, r2)
  
  c(r1, r2)
}
  
### Prior Moderator Analysis ==================================================
prior_analysis <- read_csv(here::here("tables", "table1.csv")) |>
  mutate(
    k_constructed = as.integer(k_constructed),
    k_studied = as.integer(k_studied),
    g_constructed = if_else(
      !is.na(sig_constructed), 
      paste0(g_constructed, sig_constructed), 
      g_constructed
    ),
    g_studied = if_else(
      !is.na(sig_studied), 
      paste0(g_studied, sig_studied), 
      g_studied
    )
  ) |> 
  select(2:5, 7, 8) |> 
  flextable() |> 
  set_header_labels(
    mod_category = "Moderator",
    mod_variable = "Level",
    k_constructed = "k",
    g_constructed = "g",
    k_studied = "k",
    g_studied = "g",
    top = FALSE
  ) |> 
  merge_v(j = ~ mod_category) |> 
  hline(i = c(1, 7, 10, 14,18, 24, 27)) |> 
  add_header_row(
    colwidths = c(1, 1, 2, 2), 
    values = c("Moderator", "Level", "Constructed", "Studied")
  ) |> 
  merge_v(part = "header") |> 
  add_footer_lines(
    values = c(
      "Moderator analyses results from Schroeder et al. (2018)", 
      "* p < 0.05 for subgroup mean effect size"
    )
  ) |> 
  theme_apa() |>  
  align(align = "left", part = "body") |> 
  align(align = "left", part = "footer") |> 
  valign(valign = "top", j = ~ mod_category) |> 
  line_spacing(space = 0.4, part = "all") |> 
  fontsize(part = "all", size = 10) |>
  autofit() |> 
  paginate(group = "mod_category")

prior_analysis

save_table(prior_analysis)

### Frequency of Extrinsic and Methodological Factors by Level ================
descriptive1 <- read_csv(here::here("tables", "table2.1.csv")) |>
  mutate(
    k = paste(as.integer(k), paste0("(", kp, ")")),
    j = paste(as.integer(j), paste0("(", jp, ")"))
  ) |> 
  select(1:4, 6) |> 
  flextable() |>
  set_header_labels(
    mod_category = "Moderator",
    moderator = "Level",
    levels = "",
    k = "k",
    j = "j"
  ) |> 
  merge_v(j = c(1, 2)) |>
  hline(i = c(2, 7, 22, 28)) |> 
  theme_apa() |>  
  align(align = "left", part = "body") |> 
  align(align = "left", part = "footer") |> 
  valign(valign = "top", j = c(1, 2)) |> 
  line_spacing(space = 1, part = "all") |> 
  autofit(add_w = 0, add_h = 0) |>
  width(width = 1, j = "mod_category") |> 
  width(width = 2.5, j = "levels") |>
  padding(padding.top = 2) |> 
  paginate(group = "mod_category") |> 
  keep_with_next(i = 1:7)

descriptive1 |> flextable_dim()

descriptive1

save_table(descriptive1)

### Frequency of Sample and Intervention Factors by Level =====================
descriptive2 <- read_csv(here::here("tables", "table3.csv")) |>
  mutate(
    k = paste(as.integer(k), paste0("(", kp, ")")),
    j = paste(as.integer(j), paste0("(", jp, ")"))
  ) |>
  select(1:4, 6) |>
  flextable() |>
  set_header_labels(
    mod_category = "Moderator",
    moderator = "Level",
    levels = "",
    k = "k"
  ) |>
  merge_v(j = c(1, 2)) |>
  hline(i = c(2, 5, 7, 12)) |>
  theme_apa() |>
  align(align = "left", part = "body") |>
  # align(align = "left", i = 1, part = "header") |>
  # align(align = "left", part = "footer") |>
  valign(valign = "top", j = c(1, 2)) |>
  line_spacing(space = 1, part = "all") |>
  autofit() |>
  paginate(group = "mod_category") |> 
  keep_with_next(i = 1:2)

descriptive2

save_table(descriptive2)

### Base Model Stats ==========================================================
basemodel_stats <- read_rds(here::here("_targets", "objects", "table_model_base_out"))[[2]] |>
  bind_rows(read_rds(here::here("_targets", "objects", "table_model_base"))[[2]]) |> 
  mutate(
    Model = c("Without influential studies", "All studies"), 
    `Mean [CI]` = paste0(
      round_neat(b), 
      " [", 
      round_neat(ci.lb), 
      ", ", 
      round_neat(ci.ub), 
      "]"
    ),
    SE = se,
    tau = paste0(round_neat(tau), " (", round_neat(I2_3,1), "%", ")"),
    sigma = paste0(round_neat(sigma), " (", round_neat(I2_2,1), "%", ")"),
    # I2 = paste0(round_neat(I2_all, 1), "%"),
    `Q (df)` = paste0(QE, " (", QEdf, ")", "*"),
    `k (j)` = paste0(k, " (", j, ")"),
    .keep = "none",
  ) |>
  relocate(c(tau, sigma), .after = SE) |> 
  flextable() |> 
  mk_par(
    j = c("tau", "sigma"),
    value = c(
      as_paragraph(as_equation("\\sigma_3 ({I^2}_3)")), 
      as_paragraph(as_equation("\\sigma_2 ({I^2}_2)")) 
      # as_paragraph(as_equation("I^2"))
    ),
    part = "header"
  ) |>
  add_header_row(
    colwidths = c(1, 1, 1, 3, 1), 
    values = c("Model", "Mean [CI]", "SE", "Heterogeneity Statistics", "k (j)")
  ) |> 
  merge_v(part = "header") |> 
  theme_apa() |> 
  align(align = "left", part = "body", j = 1) |>
  line_spacing(space = 1, part = "all") |> 
  fontsize(part = "all", size = 10) |>
  width(width = c(1.2, 1.2, 0.5, 1 , 1, 1, 0.6))
# paginate(init = TRUE)

basemodel_stats

save_table(basemodel_stats)

### Compare Model Stats =======================================================
datm1 <- readRDS(here::here("_targets", "objects", "model_base_out"))[[2]]
datm2 <- readRDS(here::here("_targets", "objects", "model_exmeth_out"))[[2]]
datm3 <- readRDS(here::here("_targets", "objects", "model_sampint_out"))[[2]]
datm4 <- readRDS(here::here("_targets", "objects", "model_all_out"))[[2]]
datm5 <- readRDS(here::here("_targets", "objects", "model_base"))[[2]]
datm6 <- readRDS(here::here("_targets", "objects", "model_exmeth"))[[2]]
datm7 <- readRDS(here::here("_targets", "objects", "model_sampint"))[[2]]
datm8 <- readRDS(here::here("_targets", "objects", "model_all"))[[2]]

ref1 <- datm1
ref2 <- datm5

dat2r <- calc_r2(datm1, datm2)
dat3r <- calc_r2(datm1, datm3)
dat4r <- calc_r2(datm1, datm4)
dat6r <- calc_r2(datm5, datm6)
dat7r <- calc_r2(datm5, datm7)
dat8r <- calc_r2(datm5, datm8)

dat1 <- readRDS(here::here("_targets", "objects", "table_model_base_out"))[[2]]
dat2 <- readRDS(here::here("_targets", "objects", "table_model_exmeth_out"))[[2]]
dat3 <- readRDS(here::here("_targets", "objects", "table_model_sampint_out"))[[2]]
dat4 <- readRDS(here::here("_targets", "objects", "table_model_all_out"))[[2]]
dat5 <- readRDS(here::here("_targets", "objects", "table_model_base"))[[2]]
dat6 <- readRDS(here::here("_targets", "objects", "table_model_exmeth"))[[2]]
dat7 <- readRDS(here::here("_targets", "objects", "table_model_sampint"))[[2]]
dat8 <- readRDS(here::here("_targets", "objects", "table_model_all"))[[2]]

compare_model_stats <- dat1 |>
  bind_rows(list(dat2, dat3, dat4, dat5, dat6, dat7, dat8)) |> 
  filter(term == "intrcpt") |> 
  mutate(
    Model = c(rep(c("No moderators", "Ext-Met variables", "Sam-Int variables", "Combined"), times = 2)), 
    tau = paste0(round_neat(tau), " (", round_neat(I2_3,1), "%", ")"),
    sigma = paste0(round_neat(sigma), " (", round_neat(I2_2,1), "%", ")"),
    # I2 = paste0(round_neat(I2_all, 1), "%"),
    `Q (df)` = paste0(QE, " (", QEdf, ")", "*"),
    R2_3 = c(
      "-", 
      round_neat(dat2r[[1]]), 
      round_neat(dat3r[[1]]), 
      round_neat(dat4r[[1]]),
      "-",
      round_neat(dat6r[[1]]),
      round_neat(dat7r[[1]]),
      round_neat(dat8r[[1]])
    ),
    R2_2 = c(
      "-", 
      round_neat(dat2r[[2]]), 
      round_neat(dat3r[[2]]), 
      round_neat(dat4r[[2]]),
      "-",
      round_neat(dat6r[[2]]),
      round_neat(dat7r[[2]]),
      round_neat(dat8r[[2]])
    ),
    `k (j)` = paste0(k, " (", j, ")"),
  ) |>
  select(Model, tau, sigma, `Q (df)`, R2_3, R2_2, `k (j)`) |>
  add_row(.before = 1) |> 
  mutate(across(everything(), ~replace_na(., "Without Influential Studies"))) |> 
  add_row(.before = 6) |> 
  mutate(across(everything(), ~replace_na(., "All Studies"))) |> 
  flextable() |> 
  add_header_row(
    colwidths = c(1, 3, 1, 1, 1),
    values = c(
      "Model",
      "Heterogeneity Statistics",
      "R2_3",
      "R2_2",
      "k (j)"
    )
  ) |>
  merge_v(part = "header") |>
  merge_h(part = "body", i = c(1, 6)) |> 
  hline(i = 5) |> 
  mk_par(
    j = c("tau", "sigma"),
    i = 2,
    value = c(
      as_paragraph(as_equation("\\sigma_3 ({I^2}_3)")), 
      as_paragraph(as_equation("\\sigma_2 ({I^2}_2)"))
    ),
    part = "header"
  ) |>
  mk_par(
    j = c("R2_3", "R2_2"),
    i = 1,
    value = c(
      as_paragraph(as_equation("{R^2}_3")),
      as_paragraph(as_equation("{R^2}_2"))
    ),
    part = "header"
  ) |>
  theme_apa() |> 
  align(align = "left", part = "body", j = 1) |>
  align(align = "center", part = "body", j = 1, i = c(1, 6)) |> 
  line_spacing(space = 1, part = "all") |>
  # fontsize(part = "all", size = 10) |>
  width(width = c(1.5, 1, 1, 1 , 0.5, 0.5, 0.6))

compare_model_stats 

save_table(compare_model_stats )

### Combined Model, Moderator Stats ==========================================
dat1 <- readRDS(here::here("_targets", "objects", "table_model_all_out"))[[2]]
dat2 <- readRDS(here::here("_targets", "objects", "table_model_all"))[[2]]

# datm1 <- readRDS(here::here("_targets", "objects", "model_base_out"))[[2]]
# datm5 <- readRDS(here::here("_targets", "objects", "model_base"))[[2]]
# ref1 <- datm1
# ref2 <- datm5

combined_mod_stats <- dat1 |>
  mutate(
    Factor = term,
    Level = term,
    `g [CI]1` = paste0(
      round_neat(b), 
      " [", 
      round_neat(ci.lb),
      ", ",
      round_neat(ci.ub),
      "]",
      if_else((ci.lb >= 0 & ci.ub >= 0) | (ci.lb <= 0 & ci.ub <= 0), "**", "")
    ),
    `SE1` = se,
    `k (j)1` = paste0(k, " (", j, ")"),
    `g [CI]2` = paste0(
      round_neat(dat2$b), 
      " [", 
      round_neat(dat2$ci.lb),
      ", ",
      round_neat(dat2$ci.ub),
      "]"
    ),
    `SE2` = dat2$se,
    `k (j)2` = paste0(dat2$k, " (", dat2$j, ")"),
    .keep = "none",
  ) |> 
  flextable() |> 
  mk_par(
    i = 1:15, 
    j = 1, 
    value = c(
      as_paragraph("Intercept"), 
      as_paragraph("Year, from 1980 on"),
      as_paragraph("Record type*\n (dissertations/theses)"),
      as_paragraph("Country*\n (US)"),
      as_paragraph("Comparison condition*\n (BAU)"),
      as_paragraph("Setting*\n (Intact)"),
      as_paragraph(""),
      as_paragraph("CM Type*\n (constructed)"),
      as_paragraph("Student Training*\n (extensive)"),
      as_paragraph(""),
      as_paragraph("Duration"),
      as_paragraph("Student Interaction*\n (collaborative)"),
      as_paragraph("Grade Level*\n (post-baccalaureate)"),
      as_paragraph(""),
      as_paragraph("")
    ), 
    part = "body"
  ) |> 
  mk_par(
    i = 1:15, 
    j = 2, 
    value = c(
      as_paragraph("-"), 
      as_paragraph("-"),
      as_paragraph("Journal"),
      as_paragraph("non-US"),
      as_paragraph("Reform"),
      as_paragraph("Partial"),
      as_paragraph("Research"),
      as_paragraph("Studied"),
      as_paragraph("Minimal"),
      as_paragraph("None"),
      as_paragraph("-"),
      as_paragraph("Minimal"),
      as_paragraph("Primary"),
      as_paragraph("Secondary"),
      as_paragraph("Undergraduate")
    ), 
    part = "body"
  ) |> 
  set_header_labels(
    `g [CI]1` = "g [CI]", 
    `SE1` = "SE", 
    `k (j)1` = "k (j)",
    `g [CI]2` = "g [CI]", 
    `SE2` = "SE", 
    `k (j)2` = "k (j)"
  ) |> 
  add_header_row(
    colwidths = c(1, 1, 3, 3), 
    values = c("Factor", "Level", "Without Influential Studies", "All Studies")
  ) |> 
  merge_v(part = "header") |> 
  theme_apa() |> 
  align(align = "left", part = "body", j = 1) |>
  # valign(valign = "top", part = "body", j = c(5, 6, 7)) |> 
  line_spacing(space = 1, part = "all") |> 
  fontsize(part = "all", size = 8.5) |>
  autofit() |> 
  width(width = c(1.2, 0.9, 1.3, 0.4 , 0.6, 1.2, 0.4, 0.6))

combined_mod_stats

save_table(combined_mod_stats)

### Univariate Models, Moderator Stats ========================================
dat1 <- readRDS(here::here("_targets", "objects", "table_model_univariate_out"))
datm1 <- readRDS(here::here("_targets", "objects", "model_univariate_out"))
dat1l <- tibble()

ref1 <- readRDS(here::here("_targets", "objects", "model_base_out"))[[2]]
ref2 <- readRDS(here::here("_targets", "objects", "model_base"))[[2]]


z <- list()
for (i in seq(datm1)) {
  z[[i]] <- calc_r2(ref1, datm1[[i]][[2]])
}

for (i in seq(dat1)) {
  dat1l <- dat1l |>  
    bind_rows(
      mutate(
        dat1[[i]][[2]], 
        model = names(dat1)[[i]], 
        `R^2_3` = z[[i]][[1]],
        `R^2_2` = z[[i]][[2]]
      )
    )
}

dat2 <- readRDS(here::here("_targets", "objects", "table_model_univariate"))
datm2 <- readRDS(here::here("_targets", "objects", "model_univariate"))
dat2l <- tibble()

z <- list()
for (i in seq(datm2)) {
  z[[i]] <- calc_r2(ref2, datm2[[i]][[2]])
}

for (i in seq(dat2)) {
  dat2l <- dat2l |>  
    bind_rows(
      mutate(
        dat2[[i]][[2]], 
        model = names(dat2)[[i]], 
        `R^2_3` = z[[i]][[1]],
        `R^2_2` = z[[i]][[2]]
      )
    )
}

# It would be slick to figure out how to mutate values so that they are interpretable,
# right now statistics are by variable (dummy and otherwise) so that one needs to add the (dummy) variable value with the (reference) intercept value to obtain a mean for that subgroup.
# And calculating SE and variance is more complicated

univariate_mod_stats <- dat1l |>
  mutate(
    Factor = str_extract(model, pattern = boundary("word")),
    Level = term,
    `g [CI]1` = paste0(
      round_neat(b),
      " [",
      round_neat(ci.lb),
      ", ",
      round_neat(ci.ub),
      "]"
    ),
    `SE1` = se,
    tau1 = paste0(round_neat(tau), " (", round_neat(I2_3,1), "%", ")"),
    sigma1 = paste0(round_neat(sigma), " (", round_neat(I2_2,1), "%", ")"),
    `R2_31` = `R^2_3`,
    `R2_21`= `R^2_2`,
    `k (j)1` = paste0(k, " (", j, ")"),
    # `g [CI]2` = paste0(
    #   round_neat(dat2l$b),
    #   " [",
    #   round_neat(dat2l$ci.lb),
    #   ", ",
    #   round_neat(dat2l$ci.ub),
    #   "]"
    #   ),
    # `SE2` = dat2l$se,
    # tau2 = paste0(round_neat(dat2l$tau), " (", round_neat(dat2l$I2_3,1), "%", ")"),
    # sigma2 = paste0(round_neat(dat2l$sigma), " (", round_neat(dat2l$I2_2,1), "%", ")"),
    # `R2_32` = dat2l$`R^2_3`,
    # `R2_22`= dat2l$`R^2_2`,
    # `k (j)2` = paste0(dat2l$k, " (", dat2l$j, ")"),
    .keep = "none",
  ) |>
  flextable() |>
  mk_par(
    j = 2,
    value = c(
      as_paragraph("Intercept"),
      as_paragraph("Year, from 1980 on"),
      as_paragraph("Dissertations/Theses"),
      as_paragraph("Journal Article"),
      as_paragraph("nonUS"),
      as_paragraph("US"),
      as_paragraph("BAU"),
      as_paragraph("Reform"),
      as_paragraph("Intact"),
      as_paragraph("Partial"),
      as_paragraph("Research"),
      as_paragraph("Constructed"),
      as_paragraph("Studied"),
      as_paragraph("Extensive"),
      as_paragraph("Minimal"),
      as_paragraph("None"),
      as_paragraph("Intercept"),
      as_paragraph("Duration"),
      as_paragraph("Collaborative"),
      as_paragraph("Minimal"),
      as_paragraph("Post-baccalaureate"),
      as_paragraph("Primary"),
      as_paragraph("Secondary"),
      as_paragraph("Undergraduate")
    ),
    part = "body"
  ) |>
  set_header_labels(
    `g [CI]1` = "g [CI]",
    `SE1` = "SE",
    `k (j)1` = "k (j)"
    # `g [CI]2` = "g [CI]",
    # `SE2` = "SE",
    # `k (j)2` = "k (j)"
  ) |>
  mk_par(
    j = c("R2_31", "R2_21", "tau1", "sigma1"),
    i = 1,
    value = c(
      as_paragraph(as_equation("{R^2}_3")),
      as_paragraph(as_equation("{R^2}_2")),
      as_paragraph(as_equation("\\sigma_3 ({I^2}_3)")), 
      as_paragraph(as_equation("\\sigma_2 ({I^2}_2)"))
    ),
    part = "header"
  ) |>
  # mk_par(
  #   j = c("R2_31", "R2_21", "R2_32", "R2_22"),
  #   i = 1,
  #   value = c(
  #     as_paragraph(as_equation("{R^2}_3")),
  #     as_paragraph(as_equation("{R^2}_2")),
  #     as_paragraph(as_equation("{R^2}_3")),
  #     as_paragraph(as_equation("{R^2}_2"))
  #   ),
  # part = "header"
  # ) |>
# add_header_row(
#   colwidths = c(1, 1, 7, 7),
#   values = c("Factor", "Level", "Without Influential Studies", "All Studies")
# ) |>
  merge_v(j = 1, part = "body") |>
  # merge_v(part = "header") |>
  theme_apa() |> 
  align(align = "left", part = "body", j = c(1, 2)) |>
  valign(valign = "top", part = "body") |> 
  line_spacing(space = 1, part = "all") |>
  fontsize(part = "all", size = 10) |>
  autofit() |> 
  width(width = c(1.2, 1.2, 2, 0.4, 1.2, 1.2, 0.6, 0.6, 1)) |> 
  paginate(group = "Factor")

univariate_mod_stats

save_table(univariate_mod_stats)
