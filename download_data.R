# Code to download and save data to local directory

library(dotenv)

load_dot_env()
meta_url <- Sys.getenv("META_URL")
output_url <- Sys.getenv("OUTPUT_URL")
input_url <- Sys.getenv("INPUT_URL")
week1_output_url <- Sys.getenv("WEEK1OUTPUT_URL")
week1_input_url <- Sys.getenv("WEEK1INPUT_URL")
data_dir <- Sys.getenv("DATA_DIR")

meta <- read_csv(meta_url)
input <- read_csv(input_url)
output <- read_csv(output_url)

# Write CSVs
write.csv(meta, file.path(data_dir, 'meta.csv'))
write.csv(input, file.path(data_dir, 'input.csv'))
write.csv(output, file.path(data_dir, 'output.csv'))

input <- read_csv(week1_input_url)
output <- read_csv(week1_output_url)
write.csv(input, file.path(data_dir, 'week1_input.csv'))
write.csv(output, file.path(data_dir, 'week1_output.csv'))

# Test loading CSVs
input <- read_csv(file.path(data_dir, 'input.csv'))
