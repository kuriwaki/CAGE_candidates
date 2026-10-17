library(tidyverse)
library(haven)

source("00e_check-functions.R")

# Check the saved release ----
check_candidates(read_dta("release/candidates_1990-2024.dta"))
