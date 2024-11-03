# This code check the robustness findings for the 1955 original group of treated states on the employment component

rm(list = ls())
gc()

source("./paths.R")
source("./code/functions/output_functions.R")
library(arrow)
library(data.table)
library(DIDmultiplegtDYN)
library(did)
library(dplyr)
library(fixest)
library(glue)
library(ggplot2)
library(MatchIt)
library(modelsummary)
library(tidysynth)
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

# Ln of employment variables
tab[, `:=` (ln_local = log(Local_final),
            ln_interstate = log(Interstate_final),
            ln_intrastate = log(Intrastate_final))]

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

# We generate fraction of seasonal Mexican worker for each year 
tab = tab[, mex_frac_year := mean(mex_frac, na.rm = T),by = c("State", "Year")]
tab[, mex_frac_year := round(mex_frac_year, 3)]

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
domestic = feols(domestic_seasonal ~ i(distance_treat_1965_q, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab)
domestic_ln = feols(ln_domestic_seasonal ~ i(distance_treat_1965_q, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab)
local = feols(Local_final ~ i(distance_treat_1965_q, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab)
intrastate = feols(Intrastate_final ~ i(distance_treat_1965_q, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab)
interstate = feols(Interstate_final ~ i(distance_treat_1965_q, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab)
local_ln = feols(ln_local ~ i(distance_treat_1965_q, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab)
intrastate_ln = feols(ln_intrastate ~ i(distance_treat_1965_q, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab)
interstate_ln = feols(ln_interstate ~ i(distance_treat_1965_q, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab)

# regulation
hourly_full_61 = feols(realwage_hourly ~ i(distance_treat_1961_q, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab)
daily_full_61 = feols(realwage_daily ~ i(distance_treat_1961_q, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab)
hourly_ln_61 = feols(ln_realwage_hourly ~ i(distance_treat_1961_q, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab)
daily_ln_61 = feols(ln_realwage_daily ~ i(distance_treat_1961_q, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab)

# full ban
domestic_ban = retrieve_result(domestic)
domestic_ln_ban = retrieve_result(domestic_ln)
local_ban = retrieve_result(local)
intrastate_ban = retrieve_result(intrastate)
interstate_ban = retrieve_result(interstate)
local_ban_ln = retrieve_result(local_ln)
intrastate_ban_ln = retrieve_result(intrastate_ln)
interstate_ban_ln = retrieve_result(interstate_ln)

# regulation
result_hourly_full_61 = retrieve_result(hourly_full_61)
result_daily_full_61 = retrieve_result(daily_full_61)
result_hourly_ln_61 = retrieve_result(hourly_ln_61)
result_daily_ln_61 = retrieve_result(daily_ln_61)

# ban
event_study_plot(domestic_ban, title = "Effect of Bracero worker exclusion on domestic employment of seasonal workers", 
                 x_label = "Distance to treatment (quarter)", y_label = "Employment (thousand)", save = T, output_path = "output/figures/regression/domestic_employment_bracero55_et.pdf")
event_study_plot(domestic_ln_ban, title = "Effect of Bracero worker exclusion on domestic (log) employment of seasonal workers", 
                 x_label = "Distance to treatment (quarter)", y_label =  "Employment (log)", save = T, output_path = "output/figures/regression/domestic_employment_ln_bracero55_et.pdf")
event_study_plot(local_ban, title = "Effect of Bracero worker exclusion on local employment", 
                 x_label = "Distance to treatment (quarter)", y_label = "Employment (thousands)", save = T, output_path = "output/figures/regression/local_employment_bracero55_et.pdf")
event_study_plot(intrastate_ban, title = "Effect of Bracero worker exclusion on intrastate employment", 
                 x_label = "Distance to treatment (quarter)", y_label = "Employment (thousands)", save = T, output_path = "output/figures/regression/intrastate_employment_bracero55_et.pdf")
event_study_plot(interstate_ban, title = "Effect of Bracero worker exclusion on interstate employment", 
                 x_label = "Distance to treatment (quarter)", y_label = "Employment (thousands)", save = T, output_path = "output/figures/regression/interstate_employment_bracero55_et.pdf")
event_study_plot(local_ban_ln, title = "Effect of Bracero worker exclusion on local employment (log)", 
                 x_label = "Distance to treatment (quarter)", y_label = "Employment (log)", save = T, output_path = "output/figures/regression/local_employment_ln_bracero55_et.pdf")
event_study_plot(intrastate_ban_ln, title = "Effect of Bracero worker exclusion on intrastate employment (log)", 
                 x_label = "Distance to treatment (quarter)", y_label = "Employment (log)", save = T, output_path = "output/figures/regression/intrastate_employment_ln_bracero55_et.pdf")
event_study_plot(interstate_ban_ln, title = "Effect of Bracero worker exclusion on interstate employment (log)", 
                 x_label = "Distance to treatment (quarter)", y_label = "Employment (log)", save = T, output_path = "output/figures/regression/interstate_employment_ln_bracero55_et.pdf")

# regulation
event_study_plot(result_hourly_full_61, title = "Effect of Bracero worker exclusion on real hourly wage of seasonal workers", 
                 x_label = "Distance to treatment (quarter)", y_label = "Real Hourly Wage ($)", save = T, output_path = "output/figures/regression/real_hourly_bracero55_et.pdf")
event_study_plot(result_daily_full_61, title = "Effect of Bracero worker exclusion on real daily wage of seasonal workers", 
                 x_label = "Distance to treatment (quarter)", y_label = "Real Daily Wage ($)", save = T, output_path = "output/figures/regression/real_daily_bracero55_et.pdf")
event_study_plot(result_hourly_ln_61, title = "Effect of Bracero worker exclusion on log real hourly wage of seasonal workers", 
                 x_label = "Distance to treatment (quarter)", y_label = "Real Hourly Wage (log)", save = T, output_path = "output/figures/regression/log_hourly_bracero55_et.pdf")
event_study_plot(result_daily_ln_61, title = "Effect of Bracero worker exclusion on log real daily wage of seasonal workers", 
                 x_label = "Distance to treatment (quarter)", y_label = "Real Daily Wage (log)", save = T, output_path = "output/figures/regression/log_daily_bracero55_et.pdf")


# Look for distinction between treatment exposure -------------------------------------

## High --------------------------------------------------------------------

#ban
domestic = feols(domestic_seasonal ~ i(distance_treat_1965_q, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab_high)
domestic_ln = feols(ln_domestic_seasonal ~ i(distance_treat_1965_q, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab_high)
local = feols(Local_final ~ i(distance_treat_1965_q, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab_high)
intrastate = feols(Intrastate_final ~ i(distance_treat_1965_q, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab_high)
interstate = feols(Interstate_final ~ i(distance_treat_1965_q, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab_high)
local_ln = feols(ln_local ~ i(distance_treat_1965_q, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab_high)
intrastate_ln = feols(ln_intrastate ~ i(distance_treat_1965_q, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab_high)
interstate_ln = feols(ln_interstate ~ i(distance_treat_1965_q, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab_high)

#regulation
domestic = feols(domestic_seasonal ~ i(distance_treat_1961_q, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab_high)
domestic_ln = feols(ln_domestic_seasonal ~ i(distance_treat_1961_q, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab_high)
local = feols(Local_final ~ i(distance_treat_1961_q, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab_high)
intrastate = feols(Intrastate_final ~ i(distance_treat_1961_q, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab_high)
interstate = feols(Interstate_final ~ i(distance_treat_1961_q, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab_high)
local_ln = feols(ln_local ~ i(distance_treat_1961_q, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab_high)
intrastate_ln = feols(ln_intrastate ~ i(distance_treat_1961_q, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab_high)
interstate_ln = feols(ln_interstate ~ i(distance_treat_1961_q, mex_frac_55, ref = -4) | State_FIPS + time_q, cluster = ~State_FIPS, data = tab_high)

#ban
domestic_ban = retrieve_result(domestic)
domestic_ln_ban = retrieve_result(domestic_ln)
local_ban = retrieve_result(local)
intrastate_ban = retrieve_result(intrastate)
interstate_ban = retrieve_result(interstate)
local_ban_ln = retrieve_result(local_ln)
intrastate_ban_ln = retrieve_result(intrastate_ln)
interstate_ban_ln = retrieve_result(interstate_ln)

#regulation
result_hourly_full_high_61 = retrieve_result(hourly_full_high_61)
result_daily_full_high_61 = retrieve_result(daily_full_high_61)
result_hourly_ln_high_61 = retrieve_result(hourly_ln_high_61)
result_daily_ln_high_61 = retrieve_result(daily_ln_high_61)

event_study_plot(domestic_ban, title = "Effect of Bracero worker exclusion on domestic employment of seasonal workers \n in most exposed states", 
                 x_label = "Distance to treatment (quarter)", y_label = "Employment (thousand)", save = T, output_path = "output/figures/regression/domestic_employment_bracero55_high_et.pdf")
event_study_plot(domestic_ln_ban, title = "Effect of Bracero worker exclusion on domestic (log) employment of seasonal workers \n in most exposed states", 
                 x_label = "Distance to treatment (quarter)", y_label =  "Employment (log)", save = T, output_path = "output/figures/regression/domestic_employment_ln_bracero55_high_et.pdf")
event_study_plot(local_ban, title = "Effect of Bracero worker exclusion on local employment \n in most exposed states", 
                 x_label = "Distance to treatment (quarter)", y_label = "Employment (thousands)", save = T, output_path = "output/figures/regression/local_employment_bracero55_high_et.pdf")
event_study_plot(intrastate_ban, title = "Effect of Bracero worker exclusion on intrastate employment \n in most exposed states", 
                 x_label = "Distance to treatment (quarter)", y_label = "Employment (thousands)", save = T, output_path = "output/figures/regression/intrastate_employment_bracero55_high_et.pdf")
event_study_plot(interstate_ban, title = "Effect of Bracero worker exclusion on interstate employment \n in most exposed states", 
                 x_label = "Distance to treatment (quarter)", y_label = "Employment (thousands)", save = T, output_path = "output/figures/regression/interstate_employment_bracero55_high_et.pdf")
event_study_plot(local_ban_ln, title = "Effect of Bracero worker exclusion on local employment (log) \n in most exposed states", 
                 x_label = "Distance to treatment (quarter)", y_label = "Employment (log)", save = T, output_path = "output/figures/regression/local_employment_ln_bracero55_high_et.pdf")
event_study_plot(intrastate_ban_ln, title = "Effect of Bracero worker exclusion on intrastate employment (log) \n in most exposed states", 
                 x_label = "Distance to treatment (quarter)", y_label = "Employment (log)", save = T, output_path = "output/figures/regression/intrastate_employment_ln_bracero55_high_et.pdf")
event_study_plot(interstate_ban_ln, title = "Effect of Bracero worker exclusion on interstate employment (log) \n in most exposed states", 
                 x_label = "Distance to treatment (quarter)", y_label = "Employment (log)", save = T, output_path = "output/figures/regression/interstate_employment_ln_bracero55_high_et.pdf")

event_study_plot(result_hourly_full_high_61, title = "Effect of Bracero worker exclusion on real hourly wage of seasonal workers in most exposed States in 1955", 
                 x_label = "Distance to treatment (quarter)", y_label = "Real Hourly Wage ($)", save = T, output_path = "output/figures/regression/real_hourly_bracero55_high_et.pdf")
event_study_plot(result_daily_full_high_61, title = "Effect of Bracero worker exclusion on real daily wage of seasonal workers in most exposed States in 1955", 
                 x_label = "Distance to treatment (quarter)", y_label = "Real Daily Wage ($)", save = T, output_path = "output/figures/regression/real_daily_bracero55_high_et.pdf")
event_study_plot(result_hourly_ln_high_61, title = "Effect of Bracero worker exclusion on log real hourly wage of seasonal workers in most exposed States in 1955", 
                 x_label = "Distance to treatment (quarter)", y_label = "Real Hourly Wage (log)", save = T, output_path = "output/figures/regression/log_hourly_bracero55_high_et.pdf")
event_study_plot(result_daily_ln_high_61, title = "Effect of Bracero worker exclusion on log real daily wage of seasonal workers in most exposed States in 1955", 
                 x_label = "Distance to treatment (quarter)", y_label = "Real Daily Wage (log)", save = T, output_path = "output/figures/regression/log_daily_bracero55_high_et.pdf")

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

## All ---------------------------------------------------------------------
domestic = feols(domestic_seasonal ~ i(distance_treat_1965_year, mex_frac_55, ref = -1) | State_FIPS + Year, cluster = ~State_FIPS, data = tab)
domestic_ln = feols(ln_domestic_seasonal ~ i(distance_treat_1965_year, mex_frac_55, ref = -1) | State_FIPS + Year, cluster = ~State_FIPS, data = tab)

# 1961
domestic_61 = feols(domestic_seasonal ~ i(distance_treat_1961_year, mex_frac_55, ref = -1) | State_FIPS + Year, cluster = ~State_FIPS, data = tab)
domestic_ln_61 = feols(ln_domestic_seasonal ~ i(distance_treat_1961_year, mex_frac_55, ref = -1) | State_FIPS + Year, cluster = ~State_FIPS, data = tab)

#ban
domestic_ban_year = retrieve_result(domestic)
domestic_ban_ln_year = retrieve_result(domestic_ln)

# regulation
domestic_ban_year_61 = retrieve_result(domestic_61)
domestic_ban_ln_year_61 = retrieve_result(domestic_ln_61)

#ban
event_study_plot(domestic_ban_year, title = "Effect of Bracero worker exclusion on domestic employment of seasonal workers", 
                 x_label = "Distance to treatment (year)", y_label = "Employment (thousands)", save = T, output_path = "output/figures/regression/domestic_employment_bracero55_year_et.pdf")
event_study_plot(domestic_ban_ln_year, title = "Effect of Bracero worker exclusion on domestic employment of seasonal workers", 
                 x_label = "Distance to treatment (year)", y_label = "Employment (log)", save = T, output_path = "output/figures/regression/domestic_employment_ln_bracero55_year_et.pdf")

#regulation
event_study_plot(domestic_ban_year_61, title = "Effect of Bracero worker regulation on domestic employment of seasonal workers for exclusion in 1961", 
                 x_label = "Distance to treatment (year)", y_label = "Employment (thousands)", save = T, output_path = "output/figures/regression/domestic_employment_bracero55_year_1961_et.pdf")
event_study_plot(domestic_ban_ln_year_61, title = "Effect of Bracero worker regulation on domestic employment of seasonal workers for exclusion in 1961", 
                 x_label = "Distance to treatment (year)", y_label = "Employment (log)", save = T, output_path = "output/figures/regression/domestic_employment_ln_bracero55_year_1961_et.pdf")

## High --------------------------------------------------------------------

# High exposed group
#ban
domestic = feols(domestic_seasonal ~ i(distance_treat_1965_year, mex_frac_55, ref = -1) | State_FIPS + Year, cluster = ~State_FIPS, data = tab_high)
domestic_ln = feols(ln_domestic_seasonal ~ i(distance_treat_1965_year, mex_frac_55, ref = -1) | State_FIPS + Year, cluster = ~State_FIPS, data = tab_high)

#regulation
domestic_61 = feols(domestic_seasonal ~ i(distance_treat_1961_year, mex_frac_55, ref = -1) | State_FIPS + Year, cluster = ~State_FIPS, data = tab_high)
domestic_ln_61 = feols(ln_domestic_seasonal ~ i(distance_treat_1961_year, mex_frac_55, ref = -1) | State_FIPS + Year, cluster = ~State_FIPS, data = tab_high)

#ban
domestic_ban_year = retrieve_result(domestic)
domestic_ban_ln_year = retrieve_result(domestic_ln)

#regulation
domestic_reg_year_61 = retrieve_result(domestic_61)
domestic_reg_ln_year_61 = retrieve_result(domestic_ln_61)

#ban
event_study_plot(domestic_ban_year, title = "Effect of Bracero worker exclusion on domestic employment of seasonal workers \nin the most exposed states", 
                 x_label = "Distance to treatment (year)", y_label = "Employment (thousands)", save = T, output_path = "output/figures/regression/domestic_employment_bracero55_year_high_et.pdf")
event_study_plot(domestic_ban_ln_year, title = "Effect of Bracero worker exclusion on domestic employment of seasonal workers \nin the most exposed states", 
                 x_label = "Distance to treatment (year)", y_label = "Employment (log)", save = T, output_path = "output/figures/regression/domestic_employment_ln_bracero55_year_high_et.pdf")

#regulation
event_study_plot(domestic_reg_year_61, title = "Effect of Bracero worker regulation (1962) on domestic employment of seasonal workers \nin the most exposed states", 
                 x_label = "Distance to treatment (year)", y_label = "Employment (thousands)", save = T, output_path = "output/figures/regression/domestic_employment_bracero55_year_high_et_1961.pdf")
event_study_plot(domestic_reg_ln_year_61, title = "Effect of Bracero worker regulation (1962) on domestic employment of seasonal workers \nin the most exposed states", 
                 x_label = "Distance to treatment (year)", y_label = "Employment (log)", save = T, output_path = "output/figures/regression/domestic_employment_ln_bracero55_year_high_et_1961.pdf")

## Low ---------------------------------------------------------------------

#ban
domestic = feols(domestic_seasonal ~ i(distance_treat_1965_year, mex_frac_55, ref = -1) | State_FIPS + Year, cluster = ~State_FIPS, data = tab_low)
domestic_ln = feols(ln_domestic_seasonal ~ i(distance_treat_1965_year, mex_frac_55, ref = -1) | State_FIPS + Year, cluster = ~State_FIPS, data = tab_low)

#regulation
domestic_61 = feols(domestic_seasonal ~ i(distance_treat_1961_year, mex_frac_55, ref = -1) | State_FIPS + Year, cluster = ~State_FIPS, data = tab_low)
domestic_ln_61 = feols(ln_domestic_seasonal ~ i(distance_treat_1961_year, mex_frac_55, ref = -1) | State_FIPS + Year, cluster = ~State_FIPS, data = tab_low)

#ban
domestic_ban_year = retrieve_result(domestic)
domestic_ban_ln_year = retrieve_result(domestic_ln)

#regulation
domestic_reg_year_61 = retrieve_result(domestic_61)
domestic_reg_ln_year_61 = retrieve_result(domestic_ln_61)

#ban
event_study_plot(domestic_ban_year, title = "Effect of Bracero worker exclusion on domestic employment of seasonal workers \nin the low exposed states", 
                 x_label = "Distance to treatment (year)", y_label = "Employment (thousands)", save = T, output_path = "output/figures/regression/domestic_employment_bracero55_year_low_et.pdf")
event_study_plot(domestic_ban_ln_year, title = "Effect of Bracero worker exclusion on domestic employment of seasonal workers \nin the low exposed states", 
                 x_label = "Distance to treatment (year)", y_label = "Employment (log)", save = T, output_path = "output/figures/regression/domestic_employment_ln_bracero55_year_low_et.pdf")

#regulation
event_study_plot(domestic_reg_year_61, title = "Effect of Bracero worker regulation (1962) on domestic employment of seasonal workers \nin the low exposed states", 
                 x_label = "Distance to treatment (year)", y_label = "Employment (thousands)", save = T, output_path = "output/figures/regression/domestic_employment_bracero55_year_low_et_1961.pdf")
event_study_plot(domestic_reg_ln_year_61, title = "Effect of Bracero worker regulation (1962) on domestic employment of seasonal workers \nin the low exposed states", 
                 x_label = "Distance to treatment (year)", y_label = "Employment (log)", save = T, output_path = "output/figures/regression/domestic_employment_ln_bracero55_year_low_et_1961.pdf")

# Alternative method of estimations -------------------------------------------------------------

## Synthetic Control --------------------------------------------------------------------



