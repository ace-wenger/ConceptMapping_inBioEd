library(targets)

# load functions for target pipeline
sapply(list.files(here::here("R"), full.names = TRUE), source)

# target pipeline
list(
  # tar_target(
  #   file,
  #   here::here("data", "raw_es_dataframe_v6.csv")
  # ),
  tar_target(
    data_raw,
    prepare_flat_data(here::here("data", "raw_es_dataframe_v6.csv"))
  ),
  tar_target(
    data_raw_change,
    map_estimate_sd_change(
      data_raw, 
      r_values = r_list
    )
  ),
  tar_target(
    data_processed,
    map_calc_es_prepost(data_raw_change)
  ),
  tar_target(
    data_vcv_matrix,
    map_calc_vcv_matrix(
      data_processed,
      between_measure_r = 0.4,
      within_measure_r = 0.6
    )
  ),
### Meta-regresion basemodels
  tar_target(
    model_base,
    loop_basemodel(
      data = data_processed, 
      vcv = data_vcv_matrix, 
      name_list = paste("base model,", r_list)
    )
  ),
  tar_target(
    table_model_base,
    loop_model_table(model_list = model_base)
  ),
  tar_target(
    model_base_cooks,
    map_cooks_distance(model_base)
  ),
  # tar_target(
  #   plot_cooks_base,
  #   plot_cooks(model_base_cooks)
  # ),
  tar_target(
    data_processed_out,
    map(data_processed, ~ filter(.x, Study_ID != "32", Study_ID !="34"))
  ),
  tar_target(
    data_vcv_matrix_out,
    map_calc_vcv_matrix(
      data_processed_out,
      between_measure_r = 0.4,
      within_measure_r = 0.6
    )
  ),
  tar_target(
    model_base_out,
    loop_basemodel(
      data = data_processed_out, 
      vcv = data_vcv_matrix_out, 
      name_list = paste("base model w/o outliers,", r_list)
    )
  ),
  tar_target(
    table_model_base_out,
    loop_model_table(model_list = model_base_out)
  ),
  tar_target(
    model_base_cooks_out,
    map_cooks_distance(model_base_out)
  ),
  # tar_target(
  #   plot_cooks_base_out,
  #   plot_cooks(model_base_cooks_out)
  # ),

### Exporatory, univariate meta-regression models
  tar_target(
    model_univariate,
    loop_mrmodel_list(
      form = form_univariate_list,
      data = data_processed,
      vcv = data_vcv_matrix,
      name_list = paste(c(c(var_ext, var_mth, var_sampint)), "Exploratory, Univariate Analysis, All Studies"),
      name_sublist = r_list
    )
  ),
  tar_target(
    table_model_univariate,
    loop_model_table_list(model_list_list = model_univariate)
  ),
  tar_target(
    model_univariate_out,
    loop_mrmodel_list(
      form = form_univariate_list,
      data = data_processed_out,
      vcv = data_vcv_matrix_out,
      name_list = paste(c(c(var_ext, var_mth, var_sampint)), "Exploratory, Univariate Analysis, Without Influential Studies"),
      name_sublist = r_list
    )
  ),
  tar_target(
    table_model_univariate_out,
    loop_model_table_list(model_list_list = model_univariate_out)
  ),

### Multiple meta-regression models
  tar_target(
    model_exmeth,
    loop_mrmodel(
      form = form_exmeth,
      data = data_processed, 
      vcv = data_vcv_matrix, 
      name_list = paste("Extrinsic and Methodological Variables, ", r_list)
    )
  ),
  tar_target(
    table_model_exmeth,
    loop_model_table(model_list = model_exmeth)
  ),
  tar_target(
    model_exmeth_out,
    loop_mrmodel(
      form = form_exmeth,
      data = data_processed_out, 
      vcv = data_vcv_matrix_out, 
      name_list = paste("Extrinsic and Methodological Variables (w/o outliers), ", r_list)
    )
  ),
  tar_target(
    table_model_exmeth_out,
    loop_model_table(model_list = model_exmeth_out)
  ),
  tar_target(
    model_sampint,
    loop_mrmodel(
      form = form_sampint,
      data = data_processed, 
      vcv = data_vcv_matrix, 
      name_list = paste("Sample and Intervention Variables, ", r_list)
    )
  ),
  tar_target(
    table_model_sampint,
    loop_model_table(model_list = model_sampint)
  ),
  tar_target(
    model_sampint_out,
    loop_mrmodel(
      form = form_sampint,
      data = data_processed_out, 
      vcv = data_vcv_matrix_out, 
      name_list = paste("Sample and Intervention Variables (w/o outliers), ", r_list)
    )
  ),
  tar_target(
    table_model_sampint_out,
    loop_model_table(model_list = model_sampint_out)
  ),
  tar_target(
    model_allsampint,
    loop_mrmodel_list(
      form = form_sampint_list,
      data = data_processed,
      vcv = data_vcv_matrix,
      name_list = paste(var_sampint, "Sample and Intervention Variables (controlled)"),
      name_sublist = r_list
    )
  ),
  tar_target(
    table_model_allsampint,
    loop_model_table_list(model_list_list = model_allsampint)
  ),
  tar_target(
    model_allsampint_out,
    loop_mrmodel_list(
      form = form_sampint_list,
      data = data_processed_out,
      vcv = data_vcv_matrix_out,
      name_list = paste(var_sampint, "Sample and Intervention Variables (controlled, w/o outliers)"),
      name_sublist = r_list
    )
  ),
  tar_target(
    table_model_allsampint_out,
    loop_model_table_list(model_list_list = model_allsampint_out)
  ),
  tar_target(
    model_all,
    loop_mrmodel(
      form = form_all,
      data = data_processed,
      vcv = data_vcv_matrix,
      name_list = paste("All Variables, ", r_list)
    )
  ),
  tar_target(
    table_model_all,
    loop_model_table(model_list = model_all)
  ),
  tar_target(
    model_all_out,
    loop_mrmodel(
      form = form_all,
      data = data_processed_out,
      vcv = data_vcv_matrix_out,
      name_list = paste("All Variables (w/o outliers), ", r_list)
    )
  ),
  tar_target(
    table_model_all_out,
    loop_model_table(model_list = model_all_out)
  ),
### Bootstrapping R2 95% CIs
# Bootstrapping is computationally expensive so only the data of primary interest (e.g., 0.6 & out) is used
  tar_target(
    r2_table_univariate_out,
    boot_r2_mv(
      dat = data_processed_out[[2]],
      formula = ,
      reps = 2000,
      parallel = 4,
      plot = TRUE
    )
  ),
  tar_target(
    r2_table_exmeth_out,
  ),
  tar_target(
    r2_table_sampint_out,
  ),
  tar_target(
    r2_table_allsampint_out,
  ),
  tar_target(
    r2_table_all_out,
  )
)
