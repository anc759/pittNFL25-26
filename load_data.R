# Functions for loading data

library(dotenv)

load_dot_env()
meta_url <- Sys.getenv("META_URL")
output_url <- Sys.getenv("OUTPUT_URL")
input_url <- Sys.getenv("INPUT_URL")
week1_output_url <- Sys.getenv("WEEK1OUTPUT_URL")
week1_input_url <- Sys.getenv("WEEK1INPUT_URL")

meta <- read.csv(meta_url, header = TRUE)
week1O <- read.csv(week1_output_url, header = TRUE)
week1I <- read.csv(week1_input_url, header = TRUE)
