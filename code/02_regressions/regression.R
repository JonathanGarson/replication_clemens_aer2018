# This code replicate the main results found by Clemens & al (2018)

rm(list = ls())
gc()

source("./paths.R")
source("./code/functions/output_functions.R")
library(arrow)
library(data.table)
library(fixest)
library(modelsummary)
library(dplyr)
library(tinytex)
library(webshot2)
library(gt)
library(glue)
library(kableExtra)
library(ggplot2)

# Import Data -------------------------------------------------------------

data_final = as.data.table(fread(glue("{final_data}/final_data_aer.csv")))
# data_final = as.data.table(read_parquet(glue("{final_data}/bracero_final.parquet")))

# Table 1 -----------------------------------------------------------------
tab1 = copy(data_final)
tab1[, `:=` (ln_realwage_hourly = log(realwage_hourly),
             ln_realwage_daily = log(realwage_daily))]

## Regressions -------------------------------------------------------------

full_year = list(
  "Hourly Composite (Full)" = feols(realwage_hourly ~ treatment_frac | State_FIPS + time_q, cluster = ~State_FIPS, data = tab1),
  "Daily w/o Board (Full)" = feols(realwage_daily ~ treatment_frac | State_FIPS + time_q, cluster = ~State_FIPS, data = tab1),
  "Hourly Composite (1960-1970)" = feols(realwage_hourly ~ treatment_frac | State_FIPS + time_q, cluster = ~State_FIPS, data = tab1[Year %in% 1960:1970]),
  "Daily w/o Board (1960-1970)" = feols(realwage_daily ~ treatment_frac | State_FIPS + time_q, cluster = ~State_FIPS, data = tab1[Year %in% 1960:1970])
)

table_1 = modelsummary(
  full_year,
  output = glue("{output_tables}/table_1_reg.png"),  # Specify output as kableExtra
  title = "TABLE 1: Differences-in-differences with continuous treatment, quarterly",
  stars = TRUE,
  coef_rename = c("treatment_frac" = "BraceroExclusion1965 * ExposureToExclusion"),
  gof_omit = 'R2 Within|R2 Within Adj.|AIC|BIC|RMSE|Std.Errors'
)

## Semi Elasticities -------------------------------------------------------
semi_elasticities = list(
  "Log Hourly Composite (Full)" = feols(ln_realwage_hourly ~ treatment_frac | State_FIPS + time_q, cluster = ~State_FIPS, data = tab1),
  "Log Daily w/o Board (Full)" = feols(ln_realwage_daily ~ treatment_frac | State_FIPS + time_q, cluster = ~State_FIPS, data = tab1),
  "Log Hourly Composite (1960-1970)" = feols(ln_realwage_hourly ~ treatment_frac | State_FIPS + time_q, cluster = ~State_FIPS, data = tab1[Year %in% 1960:1970]),
  "Log Daily w/o Board (1960-1970)" = feols(ln_realwage_daily ~ treatment_frac | State_FIPS + time_q, cluster = ~State_FIPS, data = tab1[Year %in% 1960:1970])
)

modelsummary(
  semi_elasticities,
  output = glue("{output_tables}/table1_semi_elasticities.png"),
  title = "TABLE 1: Differences-in-differences with continuous treatment, quarterly (semi elasticities)",
  stars = TRUE,
  coef_rename = c("treatment_frac" = "BraceroExclusion1965 * ExposureToExclusion"),
  gof_omit = 'R2 Within|R2 Within Adj.|AIC|BIC|RMSE|Std.Errors'
)

# Table 2 -----------------------------------------------------------------
# Effect of the bracero exclusion on domestic seasonal agricultural employment
domestic_season_worker = list(
 "All States, all year" = feols(domestic_seasonal ~ treatment_frac | State_FIPS + time_q, cluster = ~State_FIPS, data = tab1),
 "All States, all year (ln)" = feols(ln_domestic_seasonal ~ treatment_frac | State_FIPS + time_q, cluster = ~State_FIPS, data = tab1),
 "All States, 1960-1970" = feols(domestic_seasonal ~ treatment_frac | State_FIPS + time_q, cluster = ~State_FIPS, data = tab1[Year %in% 1960:1970,]),
 "All States, 1960-1970 (ln)" = feols(ln_domestic_seasonal ~ treatment_frac | State_FIPS + time_q, cluster = ~State_FIPS, data = tab1[Year %in% 1960:1970,]),
 "Exposed States, all years" = feols(domestic_seasonal ~ treatment_frac | State_FIPS + time_q, cluster = ~State_FIPS, data = tab1[none == 0,]),
 "Exposed States, all years (ln)" = feols(ln_domestic_seasonal ~ treatment_frac | State_FIPS + time_q, cluster = ~State_FIPS, data = tab1[none == 0]),
 "Highly Exposed States, all years" = feols(domestic_seasonal ~ treatment_frac | State_FIPS + time_q, cluster = ~State_FIPS, data = tab1[exposure > 0.15,]),
 "Highly Exposed States, all years (ln)" = feols(ln_domestic_seasonal ~ treatment_frac | State_FIPS + time_q, cluster = ~State_FIPS, data = tab1[exposure > 0.15])
)

modelsummary(
  domestic_season_worker,
  output = glue("{output_tables}/table2_domestic_seasonal_worker_employment.png"),
  title = "TABLE 1: Differences-in-differences with continuous treatment, quarterly (semi elasticities)",
  stars = TRUE,
  coef_rename = c("treatment_frac" = "BraceroExclusion1965 * ExposureToExclusion"),
  gof_omit = 'R2 Within|R2 Within Adj.|AIC|BIC|RMSE|Std.Errors'
)

# Table 3 -----------------------------------------------------------------
# Effects of Bracero Exclusion on the three types of domestic seasonal agricultural employment

tab3 = copy(data_final)
tab3[is.na(Local_final), `:=`(Local_final = 0, 
                            Intrastate_final = 0, 
                            Interstate_final = 0)]
tab3[(Year < 1954 | Year > 1973 | (Year == 1973 & Month > 7)), `:=`(Local_final = NA_real_,
                                                                  Intrastate_final = NA_real_,
                                                                  Interstate_final = NA_real_)]

type_employment = list(
 "Local" = feols(Local_final ~ treatment_frac | State_FIPS + time_m, data = tab3), 
 "Instrate" = feols(Intrastate_final ~ treatment_frac | State_FIPS + time_m, data = tab3), 
 "Instrate" = feols(Interstate_final ~ treatment_frac | State_FIPS + time_m, data = tab3), 
 "Local (ln)" = feols(ln_local ~ treatment_frac | State_FIPS + time_m, data = tab3), 
 "Instrate (ln)" = feols(ln_intrastate ~ treatment_frac | State_FIPS + time_m, data = tab3), 
 "Instrate (ln)" = feols(ln_interstate ~ treatment_frac | State_FIPS + time_m, data = tab3) 
)

modelsummary(
  type_employment,
  output = glue("{output_tables}/table3_type_employment.png"),
  title = "TABLE 1: Differences-in-differences with continuous treatment, quarterly (semi elasticities)",
  stars = TRUE,
  coef_rename = c("treatment_frac" = "BraceroExclusion1965 * ExposureToExclusion"),
  gof_omit = 'R2 Within|R2 Within Adj.|AIC|BIC|RMSE|Std.Errors'
)

# Comment Tables ----------------------------------------------------------
## Pretrend analysis -------------------------------------------------------
# We set a date has being the reference treatment point, here 1965q1. We will test for 1961 and took it to yearly effect
tab1[, distance_treat_1965 := (Year - 1965)*4 + (quarter - 1)] # treatment in 1965 with full ban
tab1[, distance_treat_1961 := (Year - 1961)*4 + (quarter - 1)] # treatment in 1961 with first regulation

# full ban
hourly_full = feols(realwage_hourly ~ i(distance_treat_1965, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab1)
daily_full = feols(realwage_daily ~ i(distance_treat_1965, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab1)
hourly_ln = feols(ln_realwage_hourly ~ i(distance_treat_1965, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab1)
daily_ln = feols(ln_realwage_daily ~ i(distance_treat_1965, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab1)
# regulation
hourly_full_61 = feols(realwage_hourly ~ i(distance_treat_1961, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab1)
daily_full_61 = feols(realwage_daily ~ i(distance_treat_1961, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab1)
hourly_ln_61 = feols(ln_realwage_hourly ~ i(distance_treat_1961, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab1)
daily_ln_61 = feols(ln_realwage_daily ~ i(distance_treat_1961, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab1)

# full ban
result_hourly_full = retrieve_result(hourly_full)
result_daily_full = retrieve_result(daily_full)
result_hourly_ln = retrieve_result(hourly_ln)
result_daily_ln = retrieve_result(daily_ln)

# regulation
result_hourly_full_61 = retrieve_result(hourly_full_61)
result_daily_full_61 = retrieve_result(daily_full_61)
result_hourly_ln_61 = retrieve_result(hourly_ln_61)
result_daily_ln_61 = retrieve_result(daily_ln_61)

# ban
event_study_plot(result_hourly_full, title = "Effect of Bracero worker exclusion on real hourly wage of seasonal workers", 
                 x_label = "Distance to treatment (quarter)", y_label = "Real Hourly Wage ($)", save = T, output_path = "output/figures/regression/real_hourly_bracero55_et.pdf")
event_study_plot(result_daily_full, title = "Effect of Bracero worker exclusion on real daily wage of seasonal workers", 
                 x_label = "Distance to treatment (quarter)", y_label = "Real Daily Wage ($)", save = T, output_path = "output/figures/regression/real_daily_bracero55_et.pdf")
event_study_plot(result_hourly_ln, title = "Effect of Bracero worker exclusion on log real hourly wage of seasonal workers", 
                 x_label = "Distance to treatment (quarter)", y_label = "Real Hourly Wage (log)", save = T, output_path = "output/figures/regression/log_hourly_bracero55_et.pdf")
event_study_plot(result_daily_ln, title = "Effect of Bracero worker exclusion on log real daily wage of seasonal workers", 
                 x_label = "Distance to treatment (quarter)", y_label = "Real Daily Wage (log)", save = T, output_path = "output/figures/regression/log_daily_bracero55_et.pdf")

# regulation
event_study_plot(result_hourly_full_61, title = "Effect of Bracero worker exclusion on real hourly wage of seasonal workers", 
                 x_label = "Distance to treatment (quarter)", y_label = "Real Hourly Wage ($)", save = F, output_path = "output/figures/regression/real_hourly_bracero55_et.pdf")
event_study_plot(result_daily_full_61, title = "Effect of Bracero worker exclusion on real daily wage of seasonal workers", 
                 x_label = "Distance to treatment (quarter)", y_label = "Real Daily Wage ($)", save = F, output_path = "output/figures/regression/real_daily_bracero55_et.pdf")
event_study_plot(result_hourly_ln_61, title = "Effect of Bracero worker exclusion on log real hourly wage of seasonal workers", 
                 x_label = "Distance to treatment (quarter)", y_label = "Real Hourly Wage (log)", save = F, output_path = "output/figures/regression/log_hourly_bracero55_et.pdf")
event_study_plot(result_daily_ln_61, title = "Effect of Bracero worker exclusion on log real daily wage of seasonal workers", 
                 x_label = "Distance to treatment (quarter)", y_label = "Real Daily Wage (log)", save = F, output_path = "output/figures/regression/log_daily_bracero55_et.pdf")


## Look for distinction between states -------------------------------------

# look for distinction between states
# We trim our data to regress still on 1955 but distinguishing by treatment intensity.
# Highly treated are above 20% exposure, low treated are between 0% and 20%, and  the rest is control (following Clemens & al)
tab1[, group := fcase(
  mex_frac_55 >= 0.2, 2,
  mex_frac_55 > 0 & mex_frac_55 < 0.2, 1,
  default = 0
)]

tab_high = tab1[group %in% c(0,2),]

#ban
hourly_full_high = feols(realwage_hourly ~ i(distance_treat_1965, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab_high)
daily_full_high = feols(realwage_daily ~ i(distance_treat_1965, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab_high)
hourly_ln_high = feols(ln_realwage_hourly ~ i(distance_treat_1965, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab_high)
daily_ln_high = feols(ln_realwage_daily ~ i(distance_treat_1965, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab_high)

#regulation
hourly_full_high_61 = feols(realwage_hourly ~ i(distance_treat_1961, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab_high)
daily_full_high_61 = feols(realwage_daily ~ i(distance_treat_1961, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab_high)
hourly_ln_high_61 = feols(ln_realwage_hourly ~ i(distance_treat_1961, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab_high)
daily_ln_high_61 = feols(ln_realwage_daily ~ i(distance_treat_1961, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab_high)

#ban
result_hourly_full_high = retrieve_result(hourly_full_high)
result_daily_full_high = retrieve_result(daily_full_high)
result_hourly_ln_high = retrieve_result(hourly_ln_high)
result_daily_ln_high = retrieve_result(daily_ln_high)
#regulation
result_hourly_full_high_61 = retrieve_result(hourly_full_high_61)
result_daily_full_high_61 = retrieve_result(daily_full_high_61)
result_hourly_ln_high_61 = retrieve_result(hourly_ln_high_61)
result_daily_ln_high_61 = retrieve_result(daily_ln_high_61)


event_study_plot(result_hourly_full_high, title = "Effect of Bracero worker exclusion on real hourly wage of seasonal workers in most exposed States in 1955", 
                 x_label = "Distance to treatment (quarter)", y_label = "Real Hourly Wage ($)", save = T, output_path = "output/figures/regression/real_hourly_bracero55_high_et.pdf")
event_study_plot(result_daily_full_high, title = "Effect of Bracero worker exclusion on real daily wage of seasonal workers in most exposed States in 1955", 
                 x_label = "Distance to treatment (quarter)", y_label = "Real Daily Wage ($)", save = T, output_path = "output/figures/regression/real_daily_bracero55_high_et.pdf")
event_study_plot(result_hourly_ln_high, title = "Effect of Bracero worker exclusion on log real hourly wage of seasonal workers in most exposed States in 1955", 
                 x_label = "Distance to treatment (quarter)", y_label = "Real Hourly Wage (log)", save = T, output_path = "output/figures/regression/log_hourly_bracero55_high_et.pdf")
event_study_plot(result_daily_ln_high, title = "Effect of Bracero worker exclusion on log real daily wage of seasonal workers in most exposed States in 1955", 
                 x_label = "Distance to treatment (quarter)", y_label = "Real Daily Wage (log)", save = T, output_path = "output/figures/regression/log_daily_bracero55_high_et.pdf")

event_study_plot(result_hourly_full_high_61, title = "Effect of Bracero worker exclusion on real hourly wage of seasonal workers in most exposed States in 1955", 
                 x_label = "Distance to treatment (quarter)", y_label = "Real Hourly Wage ($)", save = F, output_path = "output/figures/regression/real_hourly_bracero55_high_et.pdf")
event_study_plot(result_daily_full_high_61, title = "Effect of Bracero worker exclusion on real daily wage of seasonal workers in most exposed States in 1955", 
                 x_label = "Distance to treatment (quarter)", y_label = "Real Daily Wage ($)", save = F, output_path = "output/figures/regression/real_daily_bracero55_high_et.pdf")
event_study_plot(result_hourly_ln_high_61, title = "Effect of Bracero worker exclusion on log real hourly wage of seasonal workers in most exposed States in 1955", 
                 x_label = "Distance to treatment (quarter)", y_label = "Real Hourly Wage (log)", save = F, output_path = "output/figures/regression/log_hourly_bracero55_high_et.pdf")
event_study_plot(result_daily_ln_high_61, title = "Effect of Bracero worker exclusion on log real daily wage of seasonal workers in most exposed States in 1955", 
                 x_label = "Distance to treatment (quarter)", y_label = "Real Daily Wage (log)", save = F, output_path = "output/figures/regression/log_daily_bracero55_high_et.pdf")

# Low treated
tab_low = tab1[group %in% c(0,1),]

hourly_full_low = feols(realwage_hourly ~ i(distance_treat_1965, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab_low)
daily_full_low = feols(realwage_daily ~ i(distance_treat_1965, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab_low)
hourly_ln_low = feols(ln_realwage_hourly ~ i(distance_treat_1965, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab_low)
daily_ln_low = feols(ln_realwage_daily ~ i(distance_treat_1965, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab_low)

result_hourly_full_low = retrieve_result(hourly_full_low)
result_daily_full_low = retrieve_result(daily_full_low)
result_hourly_ln_low = retrieve_result(hourly_ln_low)
result_daily_ln_low = retrieve_result(daily_ln_low)

event_study_plot(result_hourly_full_low, title = "Effect of Bracero worker exclusion on real hourly wage of seasonal workers in low exposed States in 1955", 
                 x_label = "Distance to treatment (quarter)", y_label = "Real Hourly Wage ($)", save = T, output_path = "output/figures/regression/real_hourly_bracero55_low_et.pdf")
event_study_plot(result_daily_full_low, title = "Effect of Bracero worker exclusion on real daily wage of seasonal workers in low exposed States in 1955", 
                 x_label = "Distance to treatment (quarter)", y_label = "Real Daily Wage ($)", save = T, output_path = "output/figures/regression/real_daily_bracero55_low_et.pdf")
event_study_plot(result_hourly_ln_low, title = "Effect of Bracero worker exclusion on log real hourly wage of seasonal workers in low exposed States in 1955", 
                 x_label = "Distance to treatment (quarter)", y_label = "Real Hourly Wage (log)", save = T, output_path = "output/figures/regression/log_hourly_bracero55_low_et.pdf")
event_study_plot(result_daily_ln_low, title = "Effect of Bracero worker exclusion on log real daily wage of seasonal workers in low exposed States in 1955", 
                 x_label = "Distance to treatment (quarter)", y_label = "Real Daily Wage (log)", save = T, output_path = "output/figures/regression/log_daily_bracero55_low_et.pdf")


## Year to year effect -----------------------------------------------------

# We average wages over the year to have yearly effects and limit seasonal variatons
tab1[, `:=` (realwage_hourly_year = mean(realwage_hourly, na.rm = T),
             realwage_daily_year = mean(realwage_daily, na.rm = T)), by = c("Year", "State")]
tab1[, `:=` (realwage_hourly_year_ln = log(realwage_hourly_year),
             realwage_daily_year_ln = log(realwage_daily_year))]

tab1[, distance_treat_1965_year := (Year - 1965)]
tab1[, distance_treat_1961_year := (Year - 1961)]

#ban
hourly_year = feols(realwage_hourly_year ~ i(distance_treat_1965_year, mex_frac_55, ref = -1) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab1[group %in% c(0,2)])
daily_year = feols(realwage_daily_year ~ i(distance_treat_1965_year, mex_frac_55, ref = -1) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab1[group %in% c(0,2)])
hourly_year_ln = feols(realwage_hourly_year_ln ~ i(distance_treat_1965_year, mex_frac_55, ref = -1) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab1[group %in% c(0,2)])
daily_year_ln = feols(realwage_daily_year_ln ~ i(distance_treat_1965_year, mex_frac_55, ref = -1) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab1[group %in% c(0,2)])

#regulation
hourly_year_61 = feols(realwage_hourly_year ~ i(distance_treat_1961_year, mex_frac_55, ref = -1) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab1[group %in% c(0,2)])
daily_year_61 = feols(realwage_daily_year ~ i(distance_treat_1961_year, mex_frac_55, ref = -1) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab1[group %in% c(0,2)])
hourly_year_ln_61 = feols(realwage_hourly_year_ln ~ i(distance_treat_1961_year, mex_frac_55, ref = -1) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab1[group %in% c(0,2)])
daily_year_ln_61 = feols(realwage_daily_year_ln ~ i(distance_treat_1961_year, mex_frac_55, ref = -1) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab1[group %in% c(0,2)])

#ban
result_hourly_year = retrieve_result(hourly_year)
result_daily_year = retrieve_result(daily_year)
result_hourly_year_ln = retrieve_result(hourly_year_ln)
result_daily_year_ln = retrieve_result(daily_year_ln)

#regulation
result_hourly_year_61 = retrieve_result(hourly_year_61)
result_daily_year_61 = retrieve_result(daily_year_61)
result_hourly_year_ln_61 = retrieve_result(hourly_year_ln_61)
result_daily_year_ln_61 = retrieve_result(daily_year_ln_61)

#ban
event_study_plot(result_hourly_year, title = "Effect of Bracero worker exclusion on real hourly wage of seasonal workers", 
                 x_label = "Distance to treatment (year)", y_label = "Real Hourly Wage ($)", save = F, output_path = "output/figures/regression/real_hourly_bracero55_year_et.pdf")
event_study_plot(result_daily_year, title = "Effect of Bracero worker exclusion on real daily wage of seasonal workers", 
                 x_label = "Distance to treatment (year)", y_label = "Real Daily Wage ($)", save = F, output_path = "output/figures/regression/real_daily_bracero55_year_et.pdf")
event_study_plot(result_hourly_year_ln, title = "Effect of Bracero worker exclusion on log real hourly wage of seasonal workers", 
                 x_label = "Distance to treatment (year)", y_label = "Real Hourly Wage (log)", save = F, output_path = "output/figures/regression/log_hourly_bracero55_year_et.pdf")
event_study_plot(result_daily_year_ln, title = "Effect of Bracero worker exclusion on log real daily wage of seasonal workers", 
                 x_label = "Distance to treatment (year)", y_label = "Real Daily Wage (log)", save = F, output_path = "output/figures/regression/log_daily_bracero55_year_et.pdf")

#regulation
event_study_plot(result_hourly_year_61, title = "Effect of Bracero worker exclusion on real hourly wage of seasonal workers", 
                 x_label = "Distance to treatment (year)", y_label = "Real Hourly Wage ($)", save = F, output_path = "output/figures/regression/real_hourly_bracero55_year_et.pdf")
event_study_plot(result_daily_year_61, title = "Effect of Bracero worker exclusion on real daily wage of seasonal workers", 
                 x_label = "Distance to treatment (year)", y_label = "Real Daily Wage ($)", save = F, output_path = "output/figures/regression/real_daily_bracero55_year_et.pdf")
event_study_plot(result_hourly_year_ln_61, title = "Effect of Bracero worker exclusion on log real hourly wage of seasonal workers", 
                 x_label = "Distance to treatment (year)", y_label = "Real Hourly Wage (log)", save = F, output_path = "output/figures/regression/log_hourly_bracero55_year_et.pdf")
event_study_plot(result_daily_year_ln_61, title = "Effect of Bracero worker exclusion on log real daily wage of seasonal workers", 
                 x_label = "Distance to treatment (year)", y_label = "Real Daily Wage (log)", save = F, output_path = "output/figures/regression/log_daily_bracero55_year_et.pdf")


## Ponderation -------------------------------------------------------------


## SDiD --------------------------------------------------------------------


## de Chaisemartin ------------------------------------------------------



