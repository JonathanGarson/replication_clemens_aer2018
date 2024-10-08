# This code replicate the main results found by Clemens & al (2018)

rm(list = ls())
gc()

library(arrow)
library(data.table)
library(fixest)
library(modelsummary)

# Import Data -------------------------------------------------------------

data_final = as.data.table(fread("data/final/final_data_aer.csv"))

# Table 1 -----------------------------------------------------------------
tab1 = copy(data_final)
tab1[, `:=` (ln_realwage_hourly = log(realwage_hourly),
             ln_realwage_daily = log(realwage_daily))]

## Regressions -------------------------------------------------------------
tab1_models=list(
  "Hourly Composite" = feols(realwage_hourly ~ treatment_frac | State_FIPS + time_q, cluster = ~State_FIPS, data = tab1),
  "Daily w/o Board" = feols(realwage_daily ~ treatment_frac | State_FIPS + time_q, cluster = ~State_FIPS, data = tab1),
  "Hourly Composite" = feols(realwage_hourly ~ treatment_frac | State_FIPS + time_q, cluster = ~State_FIPS, data = tab1[Year %in% 1960:1970]),
  "Daily w/o Board" = feols(realwage_daily ~ treatment_frac | State_FIPS + time_q, cluster = ~State_FIPS, data = tab1[Year %in% 1960:1970]))

modelsummary(tab1_models,
             title = "TABLE 1: Differences-in-differences with continuous treatment, quarterly",
             stars = TRUE,
             coef_rename = c("treatment_frac" = "BraceroExclusion1965 * ExposureToExclusion"),
             gof_omit = 'R2 Within| R2 Within Adj.|AIC|BIC|RMSE|Std.Errors')


# esttab using tables_bracero.tex, se ar2 nostar compress append keep(treatment_frac) ///
#   booktabs alignment(D{.}{.}{-1}) title(TABLE 1: Differences-in-differences with continuous treatment, quarterly)  ///
#   scalars(N_clust) 
# 
# * Semielasticity
# mat pvals = (.)
# eststo clear
# eststo: qui xtreg ln_realwage_hourly treatment_frac i.time_q_plus, fe vce(cluster State_FIPS)
# test _b[treatment_frac]=.1
# mat new_p = (r(p))
# mat pvals = pvals\new_p
# eststo: qui xtreg ln_realwage_daily treatment_frac i.time_q_plus, fe vce(cluster State_FIPS)
# test _b[treatment_frac]=.1
# mat new_p = (r(p))
# mat pvals = pvals\new_p
# eststo: qui xtreg ln_realwage_hourly treatment_frac i.time_q_plus if Year>=1960 & Year<=1970, fe vce(cluster State_FIPS)
# test _b[treatment_frac]=.1
# mat new_p = (r(p))
# mat pvals = pvals\new_p
# eststo: qui xtreg ln_realwage_daily treatment_frac i.time_q_plus if Year>=1960 & Year<=1970, fe vce(cluster State_FIPS)
# test _b[treatment_frac]=.1
# mat new_p = (r(p))
# mat pvals = pvals\new_p
# 
# esttab using tables_bracero.tex, se ar2 nostar compress append keep(treatment_frac) ///
#   booktabs alignment(D{.}{.}{-1}) title(TABLE 1: Semielasticities, DD with continuous treatment, quarterly)  ///
#   scalars(N_clust) 
# 
# mat pvals = pvals[2...,1...]	// Drop leading blank
# mat pvals = pvals'			// transpose for LaTeX conversion
# outtable using "tables_bracero", mat(pvals) append norowlab nobox caption("TABLE 1: p vals of semielasticities") format(%5.4f %5.4f %5.4f %5.4f)
# 
