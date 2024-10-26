# This code is mainly exploratory. It produces descriptive statistics about the different states.

rm(list = ls())
gc()

library(arrow)
library(data.table)
library(ggplot2)
library(flextable)
library(magick)
source("./paths.R")

# Import Data -------------------------------------------------------------

# bracero = read_parquet("data/final/bracero_final.parquet")
bracero = as.data.table(fread(glue("{final_data}/final_data_aer.csv")))

# Tables and figures setting ----------------------------------------------

use_df_printer()
set_flextable_defaults(
  theme_fun = theme_booktabs,
  big.mark = " ", 
  font.color = "black",
  border.color = "black",
  padding = 3,
)

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
bracero55 = rbind(bracero55, bracero55[, lapply(.SD, function(x) if(is.numeric(x)) sum(x, na.rm = TRUE) else NA)], use.names = FALSE)
bracero55[.N, State := "Sum" ]

# We isolate the most exposed states
bracero55_highexposed = list(bracero55[mex_frac_year >= 0.2 & mex_frac_year <= 1,.(State)])

bracero55 |> 
  flextable() |> 
  autofit() %>% 
  add_header_lines("Summary table of Mexican workers present in the U.S.A farms in 1955") %>% 
  set_header_labels(mex_frac_year = "Prop. Seasonal Mexican Workers",
                    mexican_mean = "Total Mexican Seasonal mexican workers",
                    TotalHiredSeasonal_mean = "Total Seasonal workers",
                    HiredWorkersonFarms_mean = "Total Workers on farm") %>% 
  save_as_image("output/tables/appendix/mexican_usfarms_1955.png")

bracero55 %>% 
  select(State, mex_frac_year) %>% 
  filter(mex_frac_year >0) %>%
  flextable() %>% 
  autofit() %>% 
  add_header_lines("Proportion of Mexican seasonal workers among seasonal workers in U.S.A. farms in 1955") %>% 
  set_header_labels(mex_frac_year = "Prop. Seasonal Mexican Workers") %>% 
  save_as_image("output/tables/main/mexicain_usfarms_1955_short.png")

# Interesting points :
# - some of the most affected states count a very low total number of Mexican workers in their workforce. 
# In fact, TX an CA represents 80% of the concerned work force.
# - considering know more qualitative aspect : the states seems to have a high number of total hired workers, mexican are mostly seasonal
# and then probably entitled with more manual task. More in depth analysis of farm size, arable land, type of culture is necessary

# Robustness --------------------------------------------------------------
bracero60 = bracero[Year == 1960, .(State, Year, mex_frac, Mexican, TotalHiredSeasonal, HiredWorkersonFarms_final)] # I do not use mex_frac_tot since no data are reported for this year
bracero60 = na.omit(unique(bracero60[, .(mex_frac_year = mean(mex_frac, na.rm = T),
                                         mexican_mean = mean(Mexican, na.rm =T),
                                         TotalHiredSeasonal_mean = mean(TotalHiredSeasonal, na.rm = T), 
                                         HiredWorkersonFarms_mean = mean(HiredWorkersonFarms_final, na.rm =T)),
                                     by = c("State")]))
bracero60[, mex_frac_year := round(mex_frac_year, 3)]
bracero60[, mexican_mean := round(mexican_mean, )]
bracero60[, HiredWorkersonFarms_mean := round(HiredWorkersonFarms_mean, )]
bracero60[, TotalHiredSeasonal_mean := round(TotalHiredSeasonal_mean, )]
setorderv(bracero60, cols = "mex_frac_year", order = -1)
bracero60 = rbind(bracero60, bracero60[, lapply(.SD, function(x) if(is.numeric(x)) sum(x, na.rm = TRUE) else NA)], use.names = FALSE)
bracero60[.N, State := "Sum" ]

# We select the most affected state in 1960
bracero60_highexposed = (list(bracero60[mex_frac_year >= 0.2 & mex_frac_year <= 1,.(State)]))
bracero_fallen = setdiff(unlist(bracero55_highexposed), unlist(bracero60_highexposed)) # collecting the state who are less exposed in 1960
bracero55_highexposed = setdiff(unlist(bracero55_highexposed), bracero_fallen)

bracero60 |> 
  flextable() |> 
  autofit() %>% 
  add_header_lines("Summary table of Mexican workers present in the U.S.A farms in 1960") %>% 
  set_header_labels(mex_frac_year = "Prop. Seasonal Mexican Workers",
                    mexican_mean = "Total Mexican Seasonal mexican workers",
                    TotalHiredSeasonal_mean = "Total Seasonal workers",
                    HiredWorkersonFarms_mean = "Total Workers on farm") %>% 
  style(i = ~State %in%  unlist(bracero55_highexposed), 
        pr_t = fp_text_default(
          italic = TRUE,
          color = "red")) %>%
  style(i = ~State %in%  unlist(bracero_fallen), 
        pr_t = fp_text_default(
          italic = TRUE,
          color = "blue")) %>%
  save_as_image("output/tables/appendix/mexican_usfarms_1960.png")

bracero60 %>% 
  select(State, mex_frac_year) %>% 
  filter(mex_frac_year >0) %>%
  flextable() %>% 
  autofit() %>% 
  add_header_lines("Proportion of Mexican seasonal workers among seasonal workers in U.S.A. farms in 1960") %>% 
  set_header_labels(mex_frac_year = "Prop. Seasonal Mexican Workers") %>% 
  save_as_image("output/tables/main/mexicain_usfarms_1960_short.png")


## Evolution of the mexican force (exposure treatment) ---------------------
bracero_all_years = bracero[,.(State, Year, mex_frac, Mexican, TotalHiredSeasonal, HiredWorkersonFarms_final)]
bracero_all_years = na.omit(unique(bracero_all_years[, .(mex_frac_year = mean(mex_frac, na.rm = T),
                             mexican_mean = mean(Mexican, na.rm =T),
                             TotalHiredSeasonal_mean = mean(TotalHiredSeasonal, na.rm = T), 
                             HiredWorkersonFarms_mean = mean(HiredWorkersonFarms_final, na.rm =T)),
                         by = c("State", "Year")]))
bracero_all_years[, mex_frac_year := round(mex_frac_year, 3)]
bracero_all_years[, mexican_mean := round(mexican_mean, )]
bracero_all_years[, HiredWorkersonFarms_mean := round(HiredWorkersonFarms_mean, )]
bracero_all_years[, TotalHiredSeasonal_mean := round(TotalHiredSeasonal_mean, )]

# A table to know them all 
# Generate the table with unique state counts by Year and treatment_status
bracero_all_years[Year %in% 1950:1960, treatment_status := fcase(
  mex_frac_year >= 0.2, "Treated",
  mex_frac_year > 0 & mex_frac_year < 0.2, "Low_Treated",
  mex_frac_year == 0, "Non_Treated"
)]
# Check stability: flag as switcher if group_cm changes over years 1955-1960
bracero_all_years[Year %in% 1950:1960, stability_check := uniqueN(treatment_status, na.rm = TRUE), by = State]
bracero_all_years[stability_check > 1, switcher_status := "Switcher"]
bracero_all_years[stability_check == 1, switcher_status := "Stayer"]
bracero_summary = bracero_all_years[Year %in% 1950:1960, .(unique_states = uniqueN(State)), by = .(Year, treatment_status)]
bracero_summary = dcast(bracero_summary, Year ~ treatment_status, value.var = "unique_states", fill = 0)
setnames(bracero_summary, c("Non_Treated", "Low_Treated", "Treated"), c("Not Treated", "Low Treated", "Treated"))
bracero_summary[, Total := rowSums(.SD), .SDcols = c("Not Treated", "Low Treated", "Treated")]

bracero_summary %>% 
  flextable(col_keys = c("Year","Not Treated", "Low Treated", "Treated", "Total")) %>%
  autofit() %>% 
  add_header_lines("Evolution of Treated, Low treated, and Control group") %>% 
  colformat_num(big.mark = "") %>% 
  save_as_image(path = glue("{output_tables}/main/evolution_group_bracero.png"))
  
# We set the groups to distinguish them
bracero_all_years[, group_treatment := fcase(
  Year == 1955 & mex_frac_year >= 0.20, 2,
  Year == 1955 & mex_frac_year < 0.20 & mex_frac_year >= 0.05, 1,
  default = 0 
)]
group_1955 = bracero_all_years[Year == 1955, .(State, group_treatment)]
bracero_all_years = merge(bracero_all_years, group_1955, by = "State", all.x = TRUE, suffixes = c("", "_1955"))

ggplot(bracero_all_years[Year %in% 1954:1970], aes(x = Year, y = mex_frac_year, color = factor(group_treatment_1955), group = State)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 1961, linetype = "dashed", color = "red")+
  geom_vline(xintercept = 1964, linetype = "dashed", color = "red")+
  theme_minimal() +
  labs(title = "Evolution of the Ratio of Seasonal Mexican Worker State (1954 - 1970)",
       x = "Year",
       y = "Mexican Worker % of Seasonal Worker") +
  theme(panel.grid.major = element_line(size = 0.5),
        panel.grid.minor = element_blank(),
        legend.position = "right") +
  scale_color_discrete(name = "State")
ggsave("output/figures/ratio_mexican_all_states.pdf")

ggplot(bracero_all_years[Year %in% 1954:1970 & group_treatment_1955 %in% 1:2], aes(x = Year, y = mex_frac_year, color = State, group = State)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 1961, linetype = "dashed", color = "red")+
  geom_vline(xintercept = 1964, linetype = "dashed", color = "red")+
  theme_minimal() +
  labs(title = "Evolution of the Ratio of Seasonal Mexican Worker State \nin the moderatly and highly exposed states (1954 - 1970)",
       x = "Year",
       y = "Mexican Worker % of Seasonal Worker") +
  theme(panel.grid.major = element_line(size = 0.5),
        panel.grid.minor = element_blank(),
        legend.position = "right") +
  scale_color_discrete(name = "State")
ggsave("output/figures/ratio_mexican_moderatly_exposed_states.pdf")

ggplot(bracero_all_years[Year %in% 1954:1970 & group_treatment_1955 == 0], aes(x = Year, y = mex_frac_year, color = State, group = State)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 1961, linetype = "dashed", color = "red")+
  geom_vline(xintercept = 1964, linetype = "dashed", color = "red")+
  theme_minimal() +
  labs(title = "Evolution of the Ratio of Seasonal Mexican Worker State in the non Exposed States (1954 - 1970)",
       x = "Year",
       y = "Mexican Worker % of Seasonal Worker") +
  theme(panel.grid.major = element_line(size = 0.5),
        panel.grid.minor = element_blank(),
        legend.position = "right") +
  scale_color_discrete(name = "State")
ggsave("output/figures/ratio_mexican_non_exposed_states.pdf")

# Show the proportion of Mexican worker out of all Mexican per states in a stacked line figures

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
