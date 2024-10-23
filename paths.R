if (Sys.getenv("USERNAME") == "jonat"){
  setwd(dir = "C:/Users/jonat/sciencespo/replication/labor/clemens_immigration")
} else if(Sys.getenv("USER" == "nuvolos")){
  setwd(dir = "/files/")
}

# Data --------------------------------------------------------------------
raw_data = "data/raw"
final_data = "data/final"

# Output ------------------------------------------------------------------
output_tables = "output/tables"
output_fig = "output/figures"
