library(tidyverse)
library(haven)

source("00e_check-functions.R")

# Read the saved release ----
cand_release <- read_dta("release/candidates_1990-2024.dta")

# Congressional and gubernatorial checks ----
check_candidates(cand_release)

# Presidential checks ----
check_presidents(cand_release)
