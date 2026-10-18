library(haven)
library(fs)
library(knitr)
library(glue)
library(kableExtra)
library(tidyverse)

source("00b_xtabs-functions.R")

# data ---
jsdat <- read_dta("release/candidates_1990-2024.dta") |>
  mutate(won_by_type = str_c(type, won, sep = "-"),
         wonparty_by_type = str_c(type, won, sep = "-"))


# version where labelled class is a factor
js_fct <- jsdat |>
  mutate(party = recode_factor(party, D = "D", R = "R", Lbt = "Lbt", Grn = "Grn", .default = "Oth"))

# Counts ----
js_fct |> filter(party %in% c("D", "R"), office != "P") |>  write_numbers("all-DR")
js_fct |> filter(office == "G", party %in% c("D", "R")) |> write_numbers("gov")
js_fct |> filter(office == "H", party %in% c("D", "R")) |> write_numbers("house")
js_fct |> filter(office == "S", party %in% c("D", "R")) |> write_numbers("sen")

jsdat |>
  filter(office != "P") |>
  distinct(year, state, office, dist, type) |>
  nrow() |>
  scales::comma() |>
  write_lines("guide/Ns/all_races.tex")

# summaries
write_lines(nrow(jsdat), "guide/Ns/all_rows.tex")

js_fct |>
  group_by(office) |>
  summarize(
    dists = n_distinct(dist),
    maxdist = max(dist, na.rm = TRUE)
  )

js_fct |>
  mutate(office = fct_relevel(office, "H", "S", "G", "P")) |>
  group_by(office) |>
  summarize(
    min = min(candidatevotes, na.rm = TRUE),
    q10 = quantile(candidatevotes, 0.10, na.rm = TRUE),
    q25 = quantile(candidatevotes, 0.25, na.rm = TRUE),
    median = median(candidatevotes, na.rm = TRUE),
    q75 = quantile(candidatevotes, 0.75, na.rm = TRUE),
    q90 = quantile(candidatevotes, 0.90, na.rm = TRUE),
    max = max(candidatevotes, na.rm = TRUE)
  ) |>
  kbl(col.names = c("Office", "Minimum", "10th Quantile", "25th", "50th", "75th", "90th", "Maximum"),
      digits = 0,
      booktabs = TRUE,
      format.args = list(big.mark = ","),
      format = "latex") |>
  column_spec(2:8, monospace = TRUE) |>
  write_lines("guide/Tables/cands/candidatevotes_sum.tex")

options(knitr.kable.NA = '')

jsdat |>
  mutate(office = fct_relevel(office, "H", "S", "G", "P")) |>
  group_by(office) |>
  summarize(
    min = min(dist, na.rm = TRUE),
    max = max(dist, na.rm = TRUE),
    n_distinct = n_distinct(dist)
  ) |>
  mutate(across(everything(), ~ replace(.x, is.infinite(.x),  NA))) |>
  kbl(col.names = c("Office", "Minimum", "Maximum", "Distinct Values"),
      digits = 0,
      booktabs = TRUE,
      format.args = list(big.mark = ","),
      format = "latex") |>
  write_lines("guide/Tables/cands/dist_sum.tex")

# xtabs -------

# party short
xtabs(~ year + party, js_fct, subset = (office == "G"), drop.unused.levels = TRUE) |>
  fmt_xtab("party") |> wri_xtab("party_G")
xtabs(~ year + party, js_fct, subset = (office == "H"), drop.unused.levels = TRUE) |>
  fmt_xtab("party") |> wri_xtab("party_H")
xtabs(~ year + party, js_fct, subset = (office == "S"), drop.unused.levels = TRUE) |>
  fmt_xtab("party") |> wri_xtab("party_S")



# party formal
js_fct  |>
  filter(office != "P") |>
  mutate(party_formal = fct_relevel(party_formal, "D", "R", "Lbt", "Grn", "I")) %>%
  xtabs(~ party_formal + office, .) |>
  fmt_xtab("Office", long = TRUE, rowname = "party-formal") |>
  wri_xtab("party-formal_by-office")


# office
jsdat  |>
  mutate(office = fct_relevel(factor(office), "P", "S", "H", "G")) %>%
  xtabs(~ year + office, .) |>
  fmt_xtab("office") |>
  wri_xtab("office")


# incumbent
xtabs(~ year + inc, jsdat, subset = office == "G") |>
  fmt_xtab("inc") |> wri_xtab("inc_G")
xtabs(~ year + inc, jsdat, subset = office == "H") |>
  fmt_xtab("inc") |> wri_xtab("inc_H")
xtabs(~ year + inc, jsdat, subset = office == "S") |>
  fmt_xtab("inc") |> wri_xtab("inc_S")


# special vs. general
xtabs(~ year + type, jsdat, subset = office == "G") |>
  fmt_xtab("type") |> wri_xtab("type_G")
xtabs(~ year + type, jsdat, subset = office == "H") |>
  fmt_xtab("type") |> wri_xtab("type_H")
xtabs(~ year + type, jsdat, subset = office == "S") |>
  fmt_xtab("type") |> wri_xtab("type_S")


# runoff
xtabs(~ year + runoff, jsdat, subset = office == "G") |>
  fmt_xtab("runoff") |> wri_xtab("runoff_G")
xtabs(~ year + runoff, jsdat, subset = office == "H") |>
  fmt_xtab("runoff") |> wri_xtab("runoff_H")
xtabs(~ year + runoff, jsdat, subset = office == "S") |>
  fmt_xtab("runoff") |> wri_xtab("runoff_S")

# Next election years ----
xtabs(~ year + nextup, jsdat, subset = office == "G") |>
  fmt_nextup() |> wri_xtab("nextup_G")
xtabs(~ year + nextup, jsdat, subset = office == "H") |>
  fmt_nextup() |> wri_xtab("nextup_H")
xtabs(~ year + nextup, jsdat, subset = office == "S") |>
  fmt_nextup() |> wri_xtab("nextup_S")

# won
xtabs(~ year + won, jsdat, subset = office == "G") |>
    fmt_xtab("won") |> wri_xtab("won_G")
xtabs(~ year + won, jsdat, subset = office == "H") |>
  fmt_xtab("won") |> wri_xtab("won_H")
xtabs(~ year + won, jsdat, subset = office == "S") |>
  fmt_xtab("won") |> wri_xtab("won_S")

# Generals -- won
xtabs(~ year + won, jsdat, subset = (office == "G" & type == "G")) |>
  fmt_xtab("\\\\shortstack{won\\\\\\\\ (generals)}") |> wri_xtab("won_G_gen")
xtabs(~ year + won, jsdat, subset = (office == "H" & type == "G")) |>
  fmt_xtab("\\\\shortstack{won\\\\\\\\ (generals)}") |> wri_xtab("won_H_gen")
xtabs(~ year + won, jsdat, subset = (office == "S" & type == "G")) |>
  fmt_xtab("\\\\shortstack{won\\\\\\\\ (generals)}") |> wri_xtab("won_S_gen")

# winning party
xtabs(~ year + party, js_fct, subset = (office == "G" & won == 1), drop.unused.levels = TRUE) |>
  fmt_xtab("\\\\shortstack{party\\\\\\\\ (winners)}") |> wri_xtab("party-won_G")
xtabs(~ year + party, js_fct, subset = (office == "H" & won == 1), drop.unused.levels = TRUE) |>
  fmt_xtab("\\\\shortstack{party\\\\\\\\ (winners)}") |> wri_xtab("party-won_H")
xtabs(~ year + party, js_fct, subset = (office == "S" & won == 1), drop.unused.levels = TRUE) |>
  fmt_xtab("\\\\shortstack{party\\\\\\\\ (winners)}") |> wri_xtab("party-won_S")

# jsdat |>
#   filter(name_snyder %in% c("DELAURO, ROSA L.", "DEMINGS, VALDEZ B. (VAL)"), year == 2018) |>
#   select(year, office, state, dist, party, party_formal, name_snyder, candidatevotes) |>
#   kbl(booktabs = TRUE, format = "latex")
# jsdat |>
#   filter(name_snyder %in% c("BRIDENSTINE, JAMES FREDERICK (JIM)"), year == 2016) |>
#   select(year, office, state, dist, party, party_formal, name_snyder, candidatevotes) |>
#   kbl(booktabs = TRUE, format = "latex")

