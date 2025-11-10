# Functions for loading data

library(dotenv)
library(readr)

load_dot_env()
meta_url <- Sys.getenv("META_URL")
output_url <- Sys.getenv("OUTPUT_URL")
input_url <- Sys.getenv("INPUT_URL")
week1_output_url <- Sys.getenv("WEEK1OUTPUT_URL")
week1_input_url <- Sys.getenv("WEEK1INPUT_URL")

meta <- read_csv(meta_url)
week1O <- read_csv(week1_output_url)
week1I <- read_csv(week1_input_url)
