library(tidyverse)
library(haven)

cand <- read_dta("release/candidates_1990-2024.dta") |>
  zap_labels() |>
  filter(office != "P")

races <- cand |>
  summarize(
    winners = sum(won, na.rm = TRUE),
    total = sum(candidatevotes, na.rm = TRUE),
    top = max(candidatevotes, na.rm = TRUE),
    winner_votes = max(candidatevotes[won %in% 1], -Inf, na.rm = TRUE),
    .by = c(year, state, office, dist, type)
  )

# 1. Winner counts ----
# expect 435 House winners, 33 or 34 regular Senate winners, and 36 governors in midterms (11 or 12 otherwise)
counts <- races |>
  filter(year %% 2 == 0, type == "G") |>
  count(year, office, wt = winners) |>
  pivot_wider(names_from = office, values_from = n)
print(counts, n = Inf)
counts |>
  filter(
    H != 435 | !S %in% 33:34 |
      case_when(
        year %% 4 == 2 ~ G != 36,
        year %% 4 == 0 ~ !G %in% 11:12,
        .default = FALSE
      )
  ) |>
  print()

races |> filter(winners != 1) |> print()

# 2. Votes ----
# winner without the most votes (runoff and ranked-choice rows carry the deciding round, so none should show)
races |> filter(is.finite(winner_votes), winner_votes < top) |> print()

cand |> filter(candidatevotes < 0 | candidatevotes > totalvotes) |> print()
cand |> filter(totalvotes != sum(candidatevotes, na.rm = TRUE), .by = c(year, state, office, dist, type)) |> print()

# an extra digit shows up as a race far larger than the state's other races of that office or statewide turnout
statewide <- races |>
  filter(office %in% c("S", "G"), type == "G") |>
  summarize(statewide = max(total), .by = c(year, state))
races |>
  mutate(
    state_median = median(total[type == "G" & total > 0]),
    .by = c(year, state, office)
  ) |>
  left_join(statewide, by = c("year", "state"), relationship = "many-to-one") |>
  filter(
    case_when(
      office == "H" ~ total > 2.5 * state_median,
      office %in% c("S", "G") ~ total > 1.5 * statewide,
      .default = FALSE
    )
  ) |>
  print()

# 3. Party and names ----
cand |> filter(!party %in% c("D", "R", "I", "Lbt", "Grn", "Other", "NPA")) |> count(party) |> print()
cand |>
  mutate(first_formal = str_trim(str_split_i(party_formal, ",", 1))) |>
  filter(
    case_when(
      party == "D" ~ first_formal == "R",
      party == "R" ~ first_formal %in% c("D", "DFL"),
      party == "Other" ~ first_formal %in% c("D", "R", "Grn", "Lbt", "I"),
      .default = FALSE
    )
  ) |>
  select(year, state, office, name_snyder, party, party_formal) |>
  print(n = Inf)

snyder_pat <- "^[A-Z][A-Z'. -]*[A-Z.], [A-Z][A-Z'. -]*?( \\([A-Z0-9'. -]+\\))?(, (JR\\.|SR\\.|II|III|IV|V|VI))?$"
cand |> filter(!str_detect(name_snyder, snyder_pat), name_snyder != "[NONE OF THESE]") |> distinct(state, name_snyder) |> print()
cand |> filter(n() > 1, .by = c(year, state, office, dist, type, name_snyder)) |> select(year, state, office, dist, name_snyder) |> print()
