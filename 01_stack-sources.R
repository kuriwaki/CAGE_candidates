library(tidyverse)
library(haven)
stopifnot(packageVersion("readr") >= "2.0.0")

# read data ----
jsdat_HSG <- read_dta("data/snyder/2026-01-07 tmp_S_H_G_1990_2024.dta")
G_2024 <- read_csv("data/2024/2024-governor-raw.csv", show_col_types = FALSE)

# president
# 00d_download-MEDSL-president.R
medsl_P <- read_csv("data/intermediate/1992-2024_pres.csv", show_col_types = FALSE) |>
  mutate(office = "P", type = "G", nextup = year + 4)

# variable recodings ---
cands_stacked <- bind_rows(jsdat_HSG, G_2024) |>
  mutate(party_formal = party) |>
  # ONLY keep three offices
  filter(office %in% c("S", "H", "G", "P")) |>
  bind_rows(medsl_P) |> # President
  # VARIABLE RENAME
  mutate(
    candidatevotes = coalesce(candidatevotes, vote_g),
    won = coalesce(w_g)) |>
  select(-vote_g, -w_g) |>
  # TOTAL VOTE
  mutate(totalvotes = sum(candidatevotes), .by = c(year, office, state, dist, type)) |>
  # ARRANGE
  arrange(year, state, desc(office), dist, type, party)


# Save -----
write_rds(cands_stacked, "data/intermediate/prelim/candidates_stacked.rds")
