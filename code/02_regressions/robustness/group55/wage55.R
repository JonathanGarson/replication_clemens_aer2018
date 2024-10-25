# This code check the robustness findings for the 1955 original group of treated states on the wage component

rm(list = ls())
gc()

source("./paths.R")
source("./code/functions/output_functions.R")
library(arrow)
library(data.table)
library(dplyr)
library(fixest)
library(glue)
library(ggplot2)
library(MatchIt)
library(modelsummary)
library(tinytex)
library(tidyverse)
library(webshot2)

# Import Data -------------------------------------------------------------

data_final = as.data.table(fread(glue("{final_data}/final_data_aer.csv")))

# Settings -----------------------------------------------------------------
tab = copy(data_final)
# We take the log of the quartly data
tab[, `:=` (ln_realwage_hourly = log(realwage_hourly),
             ln_realwage_daily = log(realwage_daily))]

# We average wages over the year to have yearly effects and limit seasonal variations
tab[, `:=` (realwage_hourly_year = mean(realwage_hourly, na.rm = T),
             realwage_daily_year = mean(realwage_daily, na.rm = T)), by = c("Year", "State")]
tab[, `:=` (realwage_hourly_year_ln = log(realwage_hourly_year),
             realwage_daily_year_ln = log(realwage_daily_year))]

# We set a date has being the reference treatment point, here 1965q1. We will test for 1961 and took it to yearly effect
tab[, distance_treat_1965_q := (Year - 1965)*4 + (quarter - 1)] # treatment in 1965 with full ban
tab[, distance_treat_1961_q := (Year - 1961)*4 + (quarter - 1)] # treatment in 1961 with first regulation
# Similarly but for years
tab[, distance_treat_1965_year := (Year - 1965)]
tab[, distance_treat_1961_year := (Year - 1961)]

# Highly treated are above 20% exposure, low treated are between 0% and 20%, and  the rest is control (following Clemens & al)
tab[, group := fcase(
  mex_frac_55 >= 0.2, 2,
  mex_frac_55 > 0 & mex_frac_55 < 0.2, 1,
  default = 0
)]

tab_high = tab[group %in% c(0,2),]
tab_low = tab[group %in% c(0,1)]

# Pretrend analysis -------------------------------------------------------
# Full sample, quaterly rate

# full ban
hourly_full = feols(realwage_hourly ~ i(distance_treat_1965_q, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab)
daily_full = feols(realwage_daily ~ i(distance_treat_1965_q, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab)
hourly_ln = feols(ln_realwage_hourly ~ i(distance_treat_1965_q, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab)
daily_ln = feols(ln_realwage_daily ~ i(distance_treat_1965_q, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab)
# regulation
hourly_full_61 = feols(realwage_hourly ~ i(distance_treat_1961_q, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab)
daily_full_61 = feols(realwage_daily ~ i(distance_treat_1961_q, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab)
hourly_ln_61 = feols(ln_realwage_hourly ~ i(distance_treat_1961_q, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab)
daily_ln_61 = feols(ln_realwage_daily ~ i(distance_treat_1961_q, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab)

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


# Look for distinction between treatment exposure -------------------------------------

## High --------------------------------------------------------------------

#ban
hourly_full_high = feols(realwage_hourly ~ i(distance_treat_1965_q, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab_high)
daily_full_high = feols(realwage_daily ~ i(distance_treat_1965_q, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab_high)
hourly_ln_high = feols(ln_realwage_hourly ~ i(distance_treat_1965_q, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab_high)
daily_ln_high = feols(ln_realwage_daily ~ i(distance_treat_1965_q, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab_high)

#regulation
hourly_full_high_61 = feols(realwage_hourly ~ i(distance_treat_1961_q, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab_high)
daily_full_high_61 = feols(realwage_daily ~ i(distance_treat_1961_q, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab_high)
hourly_ln_high_61 = feols(ln_realwage_hourly ~ i(distance_treat_1961_q, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab_high)
daily_ln_high_61 = feols(ln_realwage_daily ~ i(distance_treat_1961_q, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab_high)

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

## Low ---------------------------------------------------------------------

hourly_full_low = feols(realwage_hourly ~ i(distance_treat_1965_q, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab_low)
daily_full_low = feols(realwage_daily ~ i(distance_treat_1965_q, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab_low)
hourly_ln_low = feols(ln_realwage_hourly ~ i(distance_treat_1965_q, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab_low)
daily_ln_low = feols(ln_realwage_daily ~ i(distance_treat_1965_q, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab_low)

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

# Year frequency -----------------------------------------------------

## High --------------------------------------------------------------------

# High exposed group
#ban
hourly_year = feols(realwage_hourly_year ~ i(distance_treat_1965_year, mex_frac_55, ref = -1) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab_high)
daily_year = feols(realwage_daily_year ~ i(distance_treat_1965_year, mex_frac_55, ref = -1) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab_high)
hourly_year_ln = feols(realwage_hourly_year_ln ~ i(distance_treat_1965_year, mex_frac_55, ref = -1) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab_high)
daily_year_ln = feols(realwage_daily_year_ln ~ i(distance_treat_1965_year, mex_frac_55, ref = -1) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab_high)

#regulation
hourly_year_61 = feols(realwage_hourly_year ~ i(distance_treat_1961_year, mex_frac_55, ref = -1) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab_high)
daily_year_61 = feols(realwage_daily_year ~ i(distance_treat_1961_year, mex_frac_55, ref = -1) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab_high)
hourly_year_ln_61 = feols(realwage_hourly_year_ln ~ i(distance_treat_1961_year, mex_frac_55, ref = -1) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab_high)
daily_year_ln_61 = feols(realwage_daily_year_ln ~ i(distance_treat_1961_year, mex_frac_55, ref = -1) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab_high)

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


## Low ---------------------------------------------------------------------

hourly_year = feols(realwage_hourly_year ~ i(distance_treat_1965_year, mex_frac_55, ref = -1) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab_low)
daily_year = feols(realwage_daily_year ~ i(distance_treat_1965_year, mex_frac_55, ref = -1) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab_low)
hourly_year_ln = feols(realwage_hourly_year_ln ~ i(distance_treat_1965_year, mex_frac_55, ref = -1) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab_low)
daily_year_ln = feols(realwage_daily_year_ln ~ i(distance_treat_1965_year, mex_frac_55, ref = -1) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab_low)

#ban
result_hourly_year = retrieve_result(hourly_year)
result_daily_year = retrieve_result(daily_year)
result_hourly_year_ln = retrieve_result(hourly_year_ln)
result_daily_year_ln = retrieve_result(daily_year_ln)

#ban
event_study_plot(result_hourly_year, title = "Effect of Bracero worker exclusion on real hourly wage of seasonal workers", 
                 x_label = "Distance to treatment (year)", y_label = "Real Hourly Wage ($)", save = F, output_path = "output/figures/regression/real_hourly_bracero55_year_et.pdf")
event_study_plot(result_daily_year, title = "Effect of Bracero worker exclusion on real daily wage of seasonal workers", 
                 x_label = "Distance to treatment (year)", y_label = "Real Daily Wage ($)", save = F, output_path = "output/figures/regression/real_daily_bracero55_year_et.pdf")
event_study_plot(result_hourly_year_ln, title = "Effect of Bracero worker exclusion on log real hourly wage of seasonal workers", 
                 x_label = "Distance to treatment (year)", y_label = "Real Hourly Wage (log)", save = F, output_path = "output/figures/regression/log_hourly_bracero55_year_et.pdf")
event_study_plot(result_daily_year_ln, title = "Effect of Bracero worker exclusion on log real daily wage of seasonal workers", 
                 x_label = "Distance to treatment (year)", y_label = "Real Daily Wage (log)", save = F, output_path = "output/figures/regression/log_daily_bracero55_year_et.pdf")

# Ponderation -------------------------------------------------------------

## de Chaisemartin ------------------------------------------------------



# SDiD --------------------------------------------------------------------



