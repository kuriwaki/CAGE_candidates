library(tidyverse)
library(tidylog)

# Helper: add rows from `additions` that don't already exist in `data`
add_rows_if_new <- function(data, additions, by = c("state", "year", "office", "dist", "name_snyder")) {
  bind_rows(data, anti_join(additions, data, by = by))
}

jsdat_raw <- read_rds("data/intermediate/candidates_1990-2024-v1.rds")

# GitHub issues ----


## Issues #9-20: House append ----

house_append_raw <- read_csv(
  "data/intermediate/cand_house_append.csv",
  col_types = cols(dist = col_character(), runoff = col_character()),
  na = "NA",
  trim_ws = FALSE,
  show_col_types = FALSE
)

# Keep blank state codes for Issue #26; numeric/logical blanks are missing values.
house_append_typed <- house_append_raw |>
  mutate(dist = parse_double(dist), runoff = parse_logical(runoff))

# Rename 'name' to 'name_snyder' if needed
house_append_named <- if ("name" %in% names(house_append_typed) && !"name_snyder" %in% names(house_append_typed)) {
  house_append_typed |> rename(name_snyder = name)
} else {
  house_append_typed
}

# Remove u_g and vote_g_share columns if they exist
house_append <- house_append_named |>
  select(-any_of(c("u_g", "vote_g_share")))

jsdat_house <- add_rows_if_new(jsdat_raw, house_append)


## Issue #21: Adding Bridenstine ----
# https://github.com/kuriwaki/cces_candidates/issues/21

jsdat <- add_rows_if_new(jsdat_house, tibble(
  state = "OK", year = 2016, office = "H", dist = 1, type = "G", nextup = 2018,
  party = "R", name_snyder = "BRIDENSTINE, JAMES FREDERICK (JIM)", inc = 1,
  w_g = 1, vote_g = NA_real_
))


## Issue #26: Missing gubernatorial candidate states ----

cand_state <- jsdat |>
  tidylog::mutate(
    state = case_when(
      state == "" & year == 2011 & office == "G" ~ "LA",
      .default = state
    )
  )


## Issue #27: Three last name misspellings ----

cand_spelling <- cand_state |>
  tidylog::mutate(
    name_snyder = case_when(
      str_detect(name_snyder, "YEVANCY") ~ "YEVANCEY, MANNY",
      str_detect(name_snyder, "STANCZACK") ~ "STANCZAK, JAMES",
      str_detect(name_snyder, "WEIDER") ~ "WIEDER, JOHN",
      .default = name_snyder
    )
  )


## Issue #29: Fix Ron Caesar Spelling ----

cand_caesar <- cand_spelling |>
  tidylog::mutate(
    name_snyder = case_when(
      name_snyder == "CEASAR, RON" & state == "LA" ~ "CAESAR, RON",
      .default = name_snyder
    )
  )


## Issue #30: Corrections on incumbency ----

cand_incumbency <- cand_caesar |>
  tidylog::mutate(
    inc = case_when(
      name_snyder == "BREWER, JANICE (JAN)" & state == "AZ" & office == "G" & year == 2010 ~ 1,
      name_snyder == "MURRAY, JULIANNE E." & state == "DE" & office == "G" & year == 2020 ~ 0,
      .default = inc
    )
  )


## Issue #35: Standardized names ----
# Issue #43

cand_names <- cand_incumbency |>
  tidylog::mutate(name_snyder = recode_values(
    name_snyder,
    "BOYDA, NANCY\xcaE." ~ "BOYDA, NANCY E.",
    "KENNEDY, JOSEPH P., III" ~ "KENNEDY, JOSEPH P. (JOE), III",
    "BEUTLER, JAIME HERRERA" ~ "HERRERA BEUTLER, JAIME",
    "WAKELY, TOMMY" ~ "WAKELY, THOMAS J. (TOM)",
    "CUELLAR, HENRY" ~ "CUELLAR, ENRIQUE ROBERTO (HENRY)",
    "SHERRILL, MIKIE" ~ "SHERRILL, REBECCA MICHELLE (MIKIE)",
    "KELLY, TRENT" ~ "KELLY, JOHN TRENT",
    "SCOTT, JAMES AUSTIN" ~ "SCOTT, AUSTIN",
    "FERGUSON, DREW" ~ "FERGUSON, ANDREW DREW, IV",
    "STEUBE, GREG" ~ "STEUBE, W. GREGORY (GREG)",
    default = name_snyder
  ))


## Issue #41: Missing inc values ----

cand_missing_inc <- cand_names |>
  left_join(
    tibble::tribble(
      ~state, ~year, ~office,                               ~name_snyder, ~inc,
      "IN",  2012,     "G",                   "BONEHAM, RUPERT",  0,
      "IN",  2012,     "G",                    "GREGG, JOHN R.",  0,
      "KY",  2015,     "G",                       "BEVIN, MATT",  0,
      "KY",  2015,     "G",                      "CURTIS, DREW",  0,
      "LA",  2015,     "G",                 "EDWARDS, JOHN BEL",  0,
      "MO",  2016,     "G",                         "FITZ, DON",  0,
      "MO",  2016,     "G", "TURILLI, LESTER BENTON (LES), JR.",  0,
      "VA",  2013,     "G",               "MCAULIFFE, TERRY R.",  0,
      "VA",  2013,     "G",                 "SARVIS, ROBERT C.",  0
    ),
    by = c("state", "year", "office", "name_snyder"),
    relationship = "one-to-one"
  ) |>
  mutate(inc = coalesce(inc.y, inc.x),
         inc.x = NULL,
         inc.y = NULL)



# 2024 additions ----

# Define 2024 additions
additions_2024 <- tibble::tribble(
  ~state, ~year, ~office, ~dist, ~type, ~nextup, ~party, ~party_formal, ~name_snyder, ~inc, ~vote_g, ~w_g,
  # NY-15
  "NY", 2024, "H", 15, "G", 2026, "D", "D", "TORRES, RITCHIE", 1, 130392, 1,
  "NY", 2024, "H", 15, "G", 2026, "R", "R, Conservative", "DURAN, GONZALO", 0, 36010, 0,
  "NY", 2024, "H", 15, "G", 2026, "Other", "LaRouche", "VEGA, JOSE", 0, 0, 0,
  # Maine Senate
  "ME", 2024, "S", 2, "G", 2030, "R", "R", "KOUZOUNAS, DEMI", 0, 284338, 0,
  "ME", 2024, "S", 2, "G", 2030, "D", "D", "COSTELLO, DAVID ALLEN", 0, 88891, 0,
  "ME", 2024, "S", 2, "G", 2030, "I", "I", "KING, ANGUS S., JR.", 1, 427331, 1,
  "ME", 2024, "S", 2, "G", 2030, "I", "I", "CHERRY, JASON S.", 0, 20222, 0,
  # Vermont Senate
  "VT", 2024, "S", 2, "G", 2030, "R", "R", "MALLOY, GERALD", 0, 116512, 0,
  "VT", 2024, "S", 2, "G", 2030, "I", "I", "SANDERS, BERNARD (BERNIE)", 1, 229429, 1,
  "VT", 2024, "S", 2, "G", 2030, "I", "I", "BERRY, STEVE", 0, 7941, 0,
  "VT", 2024, "S", 2, "G", 2030, "Lbt", "Lbt", "HILL, MATT", 0, 4530, 0,
  "VT", 2024, "S", 2, "G", 2030, "Other", "Green Mountain Peace and Justice", "SCHOVILLE, JUSTIN", 0, 3339, 0,
  "VT", 2024, "S", 2, "G", 2030, "Other", "Epic", "STEWART GREENSTEIN, MARK", 0, 1104, 0
)



cand_2024 <- add_rows_if_new(cand_missing_inc, additions_2024)


# Post-processing ----

## Fix NA runoffs for GA/LA ----
# Rows added after the runoff logic don't have runoff set; default them to 0
cand_runoff <- cand_2024 |>
  mutate(runoff = if_else(state %in% c("GA", "LA") & is.na(runoff), 0, runoff))

## Adding won variable for 2024 Governor races ----

cand_winners <- cand_runoff |>
  mutate(
    w_g = case_when(
      year == 2024 & office == "G" & vote_g == max(vote_g, na.rm = TRUE) ~ 1,
      year == 2024 & office == "G" ~ 0,
      .default = w_g
    ),
    .by = c(state, year, office)
  )

# Calculate totalvotes
cand_totals <- cand_winners |>
  mutate(
    totalvotes = sum(vote_g, na.rm = TRUE),
    .by = c(state, year, office, dist, type)
  )


## Reorder the dataset ----
cand <- cand_totals |>
  arrange(year, office, state, dist)

## Write the dataset ----

write_rds(cand, "data/intermediate/candidates_1990-2024-v2.rds")
