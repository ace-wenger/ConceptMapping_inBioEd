library(tidyverse)
library(metafor)

r_list <- c(0.4, 0.6, 0.8)

#' Prepare flat effect size dataframe
#'
#' @description
#' This function is a target in the `targets` workflow and accomplishes several tasks.
#'  1. It centers the numeric variables `Duration` and `Year`, and refactors `Record_Type`, `Comparison`, `Country` and `Interaction`.
#'  2. It creates dummy variables for the same categorical variables plus `Training`, `Setting`, and `Level`.
#'  3. It drops some variables and reorders the rest.
#'  4. It calculates missing statistics with those that are available which is important for the calculation of effect sizes.
#' 
#' @param file File path of coding data in csv format relative to the project root directory - e.g., "data/file.csv
#' 
#' @returns a dataframe
prepare_flat_data <- function(file) {
  flat_data <- read_csv(file) |> 
    arrange(Study_ID) |> 
    mutate(
      Duration     = Duration - 1,
      Year_C       = Year - min(Year),
      # should automate dummy coding or just use factoring methods rather than taking significant time and effort to generate this bespoke code
      Record_C     = if_else(Record_Type == "Journal", 1, 0),
      Comparison_D = if_else(Comparison == "BAU", 1, 0),
      Country_C    = if_else(Country == "United States", "US", "nonUS"),
      Country_D    = if_else(Country == "United States", 1, 0),
      CM_Type_D    = if_else(CM_Type == "Studied", 1, 0),
      Interaction_D= if_else(Interaction == "Collaborative", 1, 0),
      Training_Non = if_else(Training == "None", 1, 0),
      Training_Min = if_else(Training == "Minimal", 1, 0),
      Training_Ext = if_else(Training == "Extensive", 1, 0),
      Setting_Res  = if_else(Setting == "Research", 1, 0),
      Setting_Par  = if_else(Setting == "Partial", 1, 0),
      Setting_Int  = if_else(Setting == "Intact", 1, 0),
      Level_Pri    = if_else(Level == "Pri", 1, 0),
      Level_Sec    = if_else(Level == "Sec", 1, 0),
      Level_UG     = if_else(Level == "UG", 1, 0),
      Level_PB     = if_else(Level == "PB", 1, 0),
      .after       = Title
    ) |> 
    select(!c(Comparison_Author, Design_Exp:ES_stat)) |> 
    relocate(c(grpT:Occasion, Time, nT, nC, N), .before = Year_C) |> 
    expand_missing_stats()
}

#' Estimate change score standard deviation
#'
#' @description
#' This function is a target in the `targets` workflow.
#' Using a supplied list of pre/post correlation values, missing dataframe values are filled in and used to calculate change score standard deviations.
#' 
#' # Details
#' Often change score standard deviations are not reported and cannot be calculated based on reported statistics.
#' Their calculation requires paired t-test values, sample size, and mean change or pre/post means.
#' If a pre/post correlation value is assumed, then change score standard deviations can be calculated
#' 
#' @param data_raw 
#' @param r_values A list of pre/post correlation values to be 
#'
#' @returns one dataframe per pre/post correlation value supplied
#'
map_estimate_sd_change <- function(data_raw, r_values) {
  data_raw_list <- list()
  
  for (i in 1:length(r_values)) {
    data_raw_list[[i]] <- data.frame(data_raw)
  }
  
  map2(data_raw_list, r_values, assume_prepostr)
}

#' Title
#'
#' @param data_raw_change 
#'
#' @return
map_calc_es_prepost <- function(data_raw_change) {
  map(data_raw_change, calc_es_prepost)
}

map_calc_es_prepost_test <- function(data_raw_change) {
  map(data_raw_change, calc_es_prepost_test)
}

#' Title
#'
#' @param 
#'
#' @return
map_calc_vcv_matrix <- function(data_processed, between_measure_r, within_measure_r) {
  map(data_processed, calc_matrix, between_measure_r, within_measure_r)
}

# Helper Functions ============================================================
### Preparation for ES calculations

# calculate sdD from mean difference and paired t test value - Morris and 
# Deshon (2002)
calc_sdD_t <- function(data, mc, pairt, n1) {
  sqrt({{ n1 }} * ({{ mc }})^2) / {{ pairt }}
}

# calculate r from pretest, posttest, and change score standard deviations - 
# Morris and Deshon (2002)
calc_rpp <-  function(data, sd1, sd2, sdD) {
  ({{ sd1 }}^2 + {{ sd2 }}^2 - {{ sdD }}^2) / (2 * {{ sd1 }} * {{ sd2 }})
}

# calculating missing mean differences, sdD, and r
expand_missing_stats <- function(data) {
  mutate(data,
         mTc  = mTb - mTa, 
         mCc  = mCb - mCa,
         sdTc = if_else(is.na(pairtT), 
                        sdTc, 
                        calc_sdD_t(mc = mTc, pairt = pairtT, n1 = nT)
         ),
         sdCc = if_else(is.na(pairtC), 
                        sdCc, 
                        calc_sdD_t(mc = mCc, pairt = pairtC, n1 = nC)
         ),
         rppT = if_else(is.na(rppT),
                        calc_rpp(sd1 = sdTa, sd2 = sdTb, sdD = sdTc),
                        rppT
         ),
         rppC = if_else(is.na(rppC),
                        calc_rpp(sd1 = sdCa, sd2 = sdCb, sdD = sdCc),
                        rppC
         ),
  )
}

# calculate pooled standard deviation (sdP) - Borenstein et al. (2009)
calc_sdP <- function (data, sd1, sd2 = sd1, n1, n2 = n1, r) {
  sqrt(((({{ n1 }} - 1) * {{ sd1 }}^2) + (({{ n2 }}-1) * {{ sd2 }}^2))
       / ({{ n1 }} + {{ n2 }} - 2))
}

# calculate change score standard deviation (sdD) from sdP (pre- and posttest)
# and pre/post correlation (r) - Morris and Deshon (2002)
calc_sdD_r <- function (data, sd1, sd2 = sd1, n1, n2 = n1, r) {
  calc_sdP(data, sd1, sd2, n1, n2) * sqrt(2 * (1 - {{ r }}))
}

# calculate sdD from posttest standard deviation and r - Morris and Deshon (2002)
calc_sdD_pr <- function(sd1, r) {
  {{ sd1 }} * sqrt(2 * (1 - {{ r }}))
}

# calculate posttest standard deviation from sdD and r - inverse of above 
# calc_sdD_pr calculation which assumes homoscedasticity
calc_sdpost_pr <- function(sd1, r) {
  sqrt({{ sd1 }}^2 / (2 * (1- {{r}})))
}

# assume missing r values, estimate missing sdD values, calculate sdP
assume_prepostr <- function(data, r){
  data %>%
    replace_na(replace = list(rppT = r, rppC = r)) %>%
    mutate(. ,
           sdTc = case_when(
             !is.na(sdTc) ~ sdTc,
             !is.na(sdTa & sdTc) ~ calc_sdD_r(
               sd1 = sdTa, sd2 = sdTb, n1 = nT, r = rppT
             ),
             TRUE ~ calc_sdD_pr(sd1 = sdTb, r = rppT)
           ),
           sdCc = case_when(
             !is.na(sdCc) ~ sdCc,
             !is.na(sdCa & sdCb) ~ calc_sdD_r(
               sd1 = sdCa, sd2 = sdCb, n1 = nC, r = rppC
             ),
             TRUE ~ calc_sdD_pr(sd1 = sdCb, r = rppC)
           )
    )
}

# assume_postsd <- function(data, r = 0.6){
#   data %>%
#     replace_na(replace = list(rppT = r, rppC = r)) %>%
#     mutate(. ,
#            sdTb = if_else(!is.na(sdTb),
#                           sdTb,
#                           calc_sdpost_pr(sd1 = sdTc, r = rppT)
#            ),
#            sdCb = if_else(!is.na(sdCb),
#                           sdCb,
#                           calc_sdpost_pr(sd1 = sdCc, r = rppC)
#            )
#     )
# }

#==============================================================================
### ES calculations - combined in functions
# For prepost
calc_es_prepost <- function(data) {
  data %>%
    mutate(zdat = 0) %>% # vector of zeros for escalc functions
    escalc( # PPC designs, treatment group
      measure = "SMCC", 
      ni   = nT, 
      m1i  = mTc, 
      m2i  = zdat, 
      sd1i = sdTc, 
      sd2i = zdat, 
      ri   = zdat,
      var.names = c("yi_T", "vi_T"), 
      replace = FALSE, 
      data = .
    ) %>%
    escalc( # PPC designs, control
      measure = "SMCC", 
      ni   = nC, 
      m1i  = mCc, 
      m2i  = zdat,
      sd1i = sdCc, 
      sd2i = zdat,
      ri   = zdat,
      var.names = c("yi_C", "vi_C"), 
      replace = FALSE, 
      data = .
    ) %>%
    mutate( # PPC design, combining within group ES for study ES
      yi = yi_T - yi_C,
      vi = vi_T + vi_C
    ) %>%
    escalc( # POC design
      measure = "SMD", 
      n1i  = nT, 
      n2i  = nC, 
      m1i  = mTb, 
      m2i  = mCb,
      sd1i = sdTc, 
      sd2i = sdCc, 
      ti   = ti,
      replace = FALSE, 
      vtype = "LS", # "Large-sample" variance calculation (Hedges, 1982)
      data = .
    ) %>%
    select(Study_ID:Interaction, yi, vi, yi_T, vi_T, yi_C, vi_C)
}

# calc_es_prepost_test <- function(data) {
#   data %>%
#     mutate(zdat = 0) %>% # vector of zeros for escalc functions
#     escalc( # PPC designs, treatment group
#       measure = "SMCR", 
#       ni   = nT, 
#       m1i  = mTc, 
#       m2i  = zdat, 
#       sd1i = sdTb, 
#       ri   = zdat,
#       var.names = c("yi_T", "vi_T"), 
#       replace = FALSE, 
#       data = .
#     ) %>%
#     escalc( # PPC designs, control
#       measure = "SMCR", 
#       ni   = nC, 
#       m1i  = mCc, 
#       m2i  = zdat,
#       sd1i = sdCb, 
#       ri   = zdat,
#       var.names = c("yi_C", "vi_C"), 
#       replace = FALSE, 
#       data = .
#     ) %>%
#     mutate( # PPC design, combining within group ES for study ES
#       yi = yi_T - yi_C,
#       vi = vi_T + vi_C
#     ) %>%
#     escalc( # POC design
#       measure = "SMD", 
#       n1i  = nT, 
#       n2i  = nC, 
#       m1i  = mTb, 
#       m2i  = mCb,
#       sd1i = sdTb, 
#       sd2i = sdCb, 
#       ti   = ti,
#       replace = FALSE, 
#       vtype = "LS", # "Large-sample" variance calculation (Hedges, 1982)
#       data = .
#     ) %>%
#     select(Study_ID:Interaction, yi, vi, yi_T, vi_T, yi_C, vi_C)
# }
# 
# # For post only
# calc_es_postonly <- function(data) {
#   data %>%
#     escalc( # POC design
#       measure = "SMD", 
#       n1i  = nT, 
#       n2i  = nC, 
#       m1i  = mTb, 
#       m2i  = mCb,
#       sd1i = sdTb, 
#       sd2i = sdCb, 
#       ti   = ti,
#       replace = FALSE, 
#       vtype = "LS", # "Large-sample" variance calculation (Hedges, 1982)
#       data = .
#     ) %>%
#     select(Study_ID:Interaction, yi, vi)
# }

#==============================================================================
### Calculation of variance-covariance matrix
calc_matrix <- function(dat, between_measure_r, within_measure_r) {
  vcalc(data = dat, vi = vi,
        cluster = Study_ID,
        # multiple independent measures
        obs = Occasion, rho = between_measure_r,
        # multiple dependent measures
        time1 = Time, phi = within_measure_r,
        # dependent contrast measures
        grp1 = grpT, grp2 = grpC, w1 = nT, w2 = nC,
        checkpd = TRUE
  )
}

# This function accounts for three sources of dependency as I have used it
# (read -- as "same" and ++ as "different):
# 1. -- study, -- sample group pair, -- time, ++ measure
# 2. -- study, -- sample group pair, ++ time, -- measure
# 3. -- study, ++ sample group pair, -- time, -- measure
# Importantly, the different "group comparison pairs" can handle either 
# different treatment groups with the same comparison group or vice versa