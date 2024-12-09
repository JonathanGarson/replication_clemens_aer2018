# This code generates the data used by Clemens & al (2018)
# It reproduces the data tables producede by the code : "bracero_aer_code.do" from line 54 to 138

clear_rm()
invisible(gc)

library(arrow)
library(data.table)
library(glue)
library(ggplot2)
library(lubridate)
library(haven)
library(tictoc)
library(tinytable)
library(zoo)

tic()
# Data Path ---------------------------------------------------------------

raw_path = "data/raw"
final_path = "data/final"

# Data Cleaning -----------------------------------------------------------

bracero = as.data.table(read_parquet(glue("{raw_path}/bracero_aer_dataset.parquet")))
cpi_data = as.data.table(read_parquet(glue("{raw_path}/cpi_data.parquet")))

## Cotton machine ----------------------------------------------------------

bracero[, Cotton_machine := ifelse(Cotton_machine == 0 & State == "FL" & Year > 1969, NA, Cotton_machine)]
bracero[, Cotton_machine := ifelse(Cotton_machine == 0 & State == "VA" & Year > 1965, NA, Cotton_machine)]

## Generate Flag -----------------------------------------------------------
bracero[, Month := as.numeric(as_factor(Month))]
bracero[, `:=` (january = ifelse(Month == 1, 1, 0),
                april = ifelse(Month == 4, 1, 0),
                july = ifelse(Month == 7, 1, 0),
                october = ifelse(Month == 10, 1, 0))]
bracero[, quarterly_flag := fcase(
  Month %in% 1:3, 1,
  Month %in% 4:6, 2,
  Month %in% 7:9, 3,
  Month %in% 10:12, 4,
  default = NA
)]

## Generate time variables -------------------------------------------------

bracero[, `:=`(
  time_m = make_date(year = Year, month = Month, day = 1),
  time_m_formatted = format(make_date(year = Year, month = Month, day = 1), "%Ym%m")
)]

bracero[, `:=`(
  time_q_formatted = factor(paste0(Year,"q",quarterly_flag))
)]

## Merge different Mexican Series ----------------------------------------------------

bracero[, `:=` (Mexican = Mexican_final,
                ln_Mexican = log(Mexican_final))]


## Set panel ---------------------------------------------------------------

bracero = na.omit(bracero,cols = c("time_m", "State_FIPS"))
# CHECK FOR THE YEAR BEFORE
bracero[, fulldata := ((Year >= 1954 & Month >= 7)|(Year >= 1955)) & (Year <= 1972)]
# // Few states are covered in the employment data outside of this window
bracero[is.na(Mexican) & fulldata != 0, Mexican := 0]

## Non-Mexican workers -----------------------------------------------------

bracero[, TotalHiredSeasonal := TotalHiredSeasonal_final,]
bracero[, `:=` (NonMexican = TotalHiredSeasonal - Mexican),]
bracero[, `:=` (ln_NonMexican = log(NonMexican),
                ln_HiredWorkersonFarms = log(HiredWorkersonFarms_final),
                mex_frac = Mexican/TotalHiredSeasonal)]                
bracero[, mex_frac_tot := Mexican/(Farmworkers_Hired*1000)]         

## Merging datasets --------------------------------------------------------

setorderv(bracero, cols = c("State_FIPS","time_m"))
bracero = merge(bracero, cpi_data, by = c("State_FIPS", "time_m"), all.x = TRUE)

## Setting real wages ------------------------------------------------------

bracero[, priceadjust := cpi/0.1966401] #Divide by value of index in January 1965 TRY TO NORMALIZE BY OTHER YEARS
bracero[, `:=` (realwage_daily = DailywoBoard_final/priceadjust,
                realwage_hourly = HourlyComposite_final/priceadjust)]
bracero[, cpi := NULL]

## Generate employment data ------------------------------------------------

setorderv(bracero, cols = c("State_FIPS", "time_m"))
bracero[, domestic_seasonal := rowSums(.SD, na.rm = TRUE), .SDcols = c("Local_final", "Intrastate_final", "Interstate_final")]
bracero[, `:=` (ln_domestic_seasonal = log(domestic_seasonal),
                ln_foreign = log(TotalForeign_final),
                dom_frac = domestic_seasonal/TotalHiredSeasonal_final,
                for_frac = TotalForeign_final/TotalHiredSeasonal_final,
                ln_local = log(Local_final),
                ln_instrastate = log(Intrastate_final),
                ln_interstate = log(Interstate_final))]

bracero[Year<1954 | Year>1973 | (Year == 1973 & Month>7), domestic_seasonal := NA] # No coverage in original sources outside Jan 1954 to Jul 1973
bracero[Year<1954 | Year>1973 | (Year == 1973 & Month>7), ln_domestic_seasonal := NA] # No coverage in original sources outside Jan 1954 to Jul 1973

# Normalize by, respectively, data from the latest Census of Agriculture before 1955 and latest Census of Population before 1955:
bracero[, `:=` (mex_area = Mexican/(cropland_1954/1000), # Mexican seasonal workers per 1000 acres of (predetermined 1954) harvested cropland
                dom_area = domestic_seasonal/(cropland_1954/1000), # Domestic hired seasonal workers per 1000 acres of (predetermined 1954) harvested cropland
                mex_pop = Mexican/(pop1950/1000), # Mexican seasonal workers per 1000 population
                dom_pop = domestic_seasonal/(pop1950/1000), # Domestic hired seasonal workers per 1000 population
                Farmworkers_Hired_pop = (Farmworkers_Hired*1000)/(pop1950/1000),
                Farmworkers_Hired_area = (Farmworkers_Hired*1000)/(cropland_1954/1000))] 

# For Appendix graph comparing Mexican to non-Mexican foreign
bracero[, Mexican_zeros := Mexican]
bracero[Year >= 1967 & is.na(Mexican_zeros), Mexican_zeros := 0]
bracero[, ForNonMexican := rowSums(.SD, na.rm = TRUE), .SDcols = c("Jamaican_final", "Bahamian_final", "BWIOthers_final", 
                                                                  "Canadian_final", "PuertoRican_final", "OtherForeign_final")]
bracero[, mextot := sum(Mexican_zeros, na.rm = TRUE), by = time_m]
bracero[, fornonmextot := sum(ForNonMexican, na.rm = TRUE), by = time_m]

# Creating the treatment and control groups -------------------------------
# Create the treatment exposure var
bracero_exposure_treatment = bracero[, .(mex_frac_year = mean(mex_frac, na.rm = TRUE),
                                 mexican_mean = mean(Mexican, na.rm = TRUE),
                                 TotalHiredSeasonal_mean = mean(TotalHiredSeasonal, na.rm = TRUE), 
                                 HiredWorkersonFarms_mean = mean(HiredWorkersonFarms_final, na.rm = TRUE)),
                             by = .(State, Year)]
bracero_exposure_treatment = na.omit(unique(bracero_exposure_treatment))
# Round the relevant columns
bracero_exposure_treatment[, mex_frac_year := round(mex_frac_year, 3)]
bracero_exposure_treatment[, mexican_mean := round(mexican_mean, 0)]
bracero_exposure_treatment[, HiredWorkersonFarms_mean := round(HiredWorkersonFarms_mean, 0)]
bracero_exposure_treatment[, TotalHiredSeasonal_mean := round(TotalHiredSeasonal_mean, 0)]
mex_frac_1955_value = bracero_exposure_treatment[Year == 1955, .(State,mex_frac_year)]
setnames(mex_frac_1955_value, "mex_frac_year", "mex_frac_1955")
bracero_exposure_treatment = merge(bracero_exposure_treatment, mex_frac_1955_value, by = c("State"), all.x = T)
# Create the three groups based on the year 1955
# Check that all states are treated
bracero_exposure_treatment[Year == 1955, group := fcase(
  mex_frac_year >= 0.20, 2,
  mex_frac_year < 0.20 & mex_frac_year > 0.2, 1,
  mex_frac_year == 0, 0
)]
bracero_exposure_treatment[, post := (Year >= 1965)*1] #the authors propose to period of treatment, the final exclusion
bracero_exposure_treatment[, post_2 := (Year >= 1962)*1] # the alternative is 1962 when the wages were mandatory increased
bracero_exposure_treatment[, post_3 := (Year >= 1960)*1] # I add this one to account for eventual anticipatory effect
bracero_exposure_treatment[, treatment_frac := post * mex_frac_1955]
bracero_exposure_treatment[, treatment_frac_2 := post_2 * mex_frac_1955]

# We merge the dataset
bracero = merge(bracero, bracero_exposure_treatment, by = c("State", "Year"), all.x = T) 

write_parquet(bracero, glue("{final_path}/bracero_final.parquet"))
toc()

