# This code replicate the main results found by Clemens & al (2018)

clear_rm()
gc()

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

# Import Data -------------------------------------------------------------

data_final = as.data.table(fread(glue("{final_data}/final_data_aer.csv")))

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
