library(dataverse)
library(tidyverse)
library(tidylog)

# Dataverse ----
pres_raw <- get_dataframe_by_name(
  filename = "1976-2024-president.csv",
  dataset = "doi:10.7910/DVN/42MVDX",
  version = "10.1",
  server = "dataverse.harvard.edu",
  original = TRUE,
  .f = readr::read_csv
)

# Fill missing 2024 parties ----
pres_24_init <- pres_raw |>
  filter(year == 2024)

pres_24_ptys <- pres_24_init |>
  filter(!writein, !is.na(party_detailed)) |>
  count(candidate, party_detailed) |>
  slice_max(n, by = c(candidate), with_ties = FALSE) # modal party

pres_24 <- pres_24_init |>
  left_join(pres_24_ptys, by = "candidate", relationship = "many-to-one") |>
  mutate(party_detailed = coalesce(party_detailed.x, party_detailed.y))

# DC 2020 certified results ----
# Dataverse V10.1 flags five ballot candidates as write-ins. Replace the race
# with the six named candidates in the DC Board of Elections certified results,
# p. 1; the separate 3,137-vote write-in total is omitted, as for other races.
# https://electionresults.dcboe.org/Downloads/Reports/November_3_2020_General_Election_Certified_Results.pdf
pres_dc_2020 <- tribble(
  ~candidate, ~party_detailed, ~party_simplified, ~candidatevotes,
  "BIDEN, JOSEPH R. JR", "DEMOCRAT", "DEMOCRAT", 317323,
  "TRUMP, DONALD J.", "REPUBLICAN", "REPUBLICAN", 18586,
  "JORGENSEN, JO", "LIBERTARIAN", "LIBERTARIAN", 2036,
  "HAWKINS, HOWIE", "DC STATEHOOD GREEN", "OTHER", 1726,
  "LA RIVA, GLORIA ESTELLA", "INDEPENDENT", "OTHER", 855,
  "PIERCE, BROCK", "INDEPENDENT", "OTHER", 693
) |>
  mutate(year = 2020, state_po = "DC", writein = FALSE)

# Recode party, variable names, etc..
pres_fmt <- pres_raw |>
  filter(year < 2024, !(year == 2020 & state_po == "DC")) |>
  bind_rows(pres_24, pres_dc_2020) |>
  dplyr::filter(year >= 1990) |>
  # Virginia lists Phillips/Frazier as Constitution; MEDSL labels it Conservative.
  # https://historical.elections.virginia.gov/contest/39517
  dplyr::mutate(
    party_detailed = replace(
      party_detailed,
      year == 2000 & state_po == "VA" & candidate == "PHILLIPS, HOWARD",
      "CONSTITUTION"
    )
  ) |>
  dplyr::transmute(
    year,
    state = state_po,
    candidate,
    writein,
    party = recode(party_simplified, REPUBLICAN = "R", DEMOCRAT = "D", LIBERTARIAN = "Lbt", GREEN = "Grn", OTHER = "Other"),
    party_formal = recode(str_to_title(party_detailed), Republican = "R", Democrat = "D", Libertarian = "Lbt", Green = "Grn"),
    candidatevotes
  ) |>
  tidylog::mutate(
    party = replace(party, party_formal == "Working Families", "D"),
    party = replace(party, party_formal == "Women's Equality" & state == "NY", "D"),
    party = replace(party, party_formal == "Independence" & state == "NY" & year %in% c(2008), "R"),
    party = replace(party, party_formal == "Independence" & state == "NY" & year %in% c(2016), "Lbt"),
    party = replace(party, party_formal == "Conservative" & state == "NY", "R")) |>
  tidylog::mutate(
    # writins
    party = replace(party, (writein), "W-I"),
    candidate = replace(candidate, writein, "W-I"),
    party_formal = replace(party_formal, writein, NA_character_),
    party_formal = replace(party_formal, party_formal == "Democratic", "D"),
    party_formal = replace(party_formal, party_formal == "Dfl", "DFL"),
    name = recode(
      candidate,
      `OBAMA, BARACK H.` = "OBAMA, BARACK",
      `CLINTON, HILLARY` = "CLINTON, HILLARY RODHAM",
      `BIDEN, JOSEPH R. JR` = "BIDEN, JOSEPH R., JR.",
      `MCCAIN, JOHN` = "MCCAIN, JOHN S.",
      `PAUL, RONALD \"\"RON\"\"` = "PAUL, RONALD E. (RON)",
    ),
    name = str_replace(name, "\"{1,}$", ")"),
    name = str_replace(name, "\\s\"{1,}", " ("),
    inc = case_when(
      str_detect(name, "OBAMA") & year == 2012 ~ 1,
      str_detect(name, "TRUMP") & year == 2020 ~ 1,
      TRUE ~ 0
    )
  ) |>
  select(-writein, -candidate) |>
  relocate(year, state,
           party, party_formal,
           name,
           inc,
           candidatevotes)

# collapse fusion
pres_fmt2 <- pres_fmt |>
  summarize(
    party_formal = str_c(as.character(party_formal), collapse = ", "),
    candidatevotes = sum(candidatevotes, na.rm = TRUE),
    .by = c(year, state, party, name, inc)
    )

# drop
# - candidates with less than 10 votes
# - blank votes, overvotes, scatterings, etc..
pres_sel <- pres_fmt2 |>
  filter(candidatevotes >= 10) |>
  filter(!is.na(party_formal))

xtabs(~ year + party, pres_sel)

# Write to intermediate ----
# Use the historical state-FIPS order for every year, then descending votes.
state_order <- pres_raw |>
  distinct(state_po, state_fips) |>
  arrange(state_fips) |>
  pull(state_po)

pres_sorted <- pres_sel |>
  arrange(year, match(state, state_order), desc(candidatevotes), name, party)

write_csv(pres_sorted, "data/intermediate/1992-2024_pres.csv")
