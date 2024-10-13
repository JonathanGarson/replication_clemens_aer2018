# Main Settings ---------------------------------------------------------------
source("renv/activate.R")
setwd("C:/Users/jonat/sciencespo/replication/labor/clemens_immigration/")

# Set paths ---------------------------------------------------------------
# Data --------------------------------------------------------------------
raw_data = "data/raw"
final_data = "data/final"

# Output ------------------------------------------------------------------
output_tables = "output/tables"
output_fig = "output/figures"

# Utils Function ----------------------------------------------------------
clear_rm = function(keep = NULL) {
  if (is.null(keep)) {
    keep <- c("raw_data", "final_data", "output_tables", "output_fig", "clear_rm()")
  }
  # Ensure 'keep' variable itself is also not removed
  keep = c(keep, "keep")
  
  # Remove all objects except those in 'keep' from the global environment
  rm(list = setdiff(ls(envir = .GlobalEnv), keep), envir = .GlobalEnv)
}

