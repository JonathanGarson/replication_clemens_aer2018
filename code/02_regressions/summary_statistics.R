# This code is mainly exploratory. It produces descriptive statistics about the different states.

rm(list = ls())
gc()

library(arrow)
library(data.table)
library(ggplot2)
library(gt)

# Import Data -------------------------------------------------------------

bracero = read_parquet("data/final/bracero_final.parquet")


# Appendix ----------------------------------------------------------------

## Table A2 ----------------------------------------------------------------------

# We want to reproduce the fraction of total seasonal farm workers, average across months
bracero55 = bracero[Year == 1955, .(State, Year, mex_frac, Mexican, TotalHiredSeasonal, HiredWorkersonFarms_final)] # I do not use mex_frac_tot since no data are reported for this year
bracero55 = na.omit(unique(bracero55[, .(mex_frac_year = mean(mex_frac, na.rm = T),
                                         mexican_mean = mean(Mexican, na.rm =T),
                                         TotalHiredSeasonal_mean = mean(TotalHiredSeasonal, na.rm = T), 
                                         HiredWorkersonFarms_mean = mean(HiredWorkersonFarms_final, na.rm =T)),
                                     by = c("State")]))
bracero55[, mex_frac_year := round(mex_frac_year, 3)]
bracero55[, mexican_mean := round(mexican_mean, )]
bracero55[, HiredWorkersonFarms_mean := round(HiredWorkersonFarms_mean, )]
bracero55[, TotalHiredSeasonal_mean := round(TotalHiredSeasonal_mean, )]
setorderv(bracero55, cols = "mex_frac_year", order = -1)

# Interesting points :
# - some of the most affected states count a very low total number of Mexican workers in their workforce. 
# In fact, TX an CA represents 80% of the concerned work force.
# - considering know more qualitative aspect : the states seems to have a high number of total hired workers, mexican are mostly seasonal
# and then probably entitled with more manual task. More in depth analysis of farm size, arable land, type of culture is necessary

## Work force composition in farms -----------------------------------------
farmsworkers = bracero[Year == 1955, .(State, Year, Mexican,TotalHiredSeasonal, HiredWorkersonFarms_final)]
farmsworkers = na.omit(unique(farmsworkers[, .(mean_seasonal = mean(TotalHiredSeasonal, na.rm = T),
                                               mexican_mean = mean(Mexican, na.rm =T),
                                               mean_workersonfarm = mean(HiredWorkersonFarms_final, na.rm = T)), by = "State"]))
farmsworkers[, `:=`(seasonal_frac = mean_seasonal/mean_workersonfarm,
                    seasonal_frac_mex = mexican_mean/mean_seasonal), by = "State"]
farmsworkers[, mean_workersonfarm := round(mean_workersonfarm, )]
farmsworkers[, mean_seasonal := round(mean_seasonal, )]
farmsworkers[, mexican_mean := round(mexican_mean, )]
setorderv(farmsworkers, cols = "mexican_mean", order = -1)
