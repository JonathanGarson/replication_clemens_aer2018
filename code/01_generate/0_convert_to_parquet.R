# This code converts to parquet formats all .dta dataset

clear_rm()
gc()

library(arrow)
library(haven)
library(glue)

for (file in list.files("original_data/", pattern = ".dta")){
  data = read_dta(glue("original_data/{file}"))
  data_name = gsub(".dta", "", x = file)
  write_parquet(data, glue("data/raw/{data_name}.parquet"))
}
