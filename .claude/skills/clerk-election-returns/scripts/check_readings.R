# Check the readers' output for a Clerk volume and list what is coded by hand.
#
# Run from the repo root after the reader agents have finished:
#
#   Rscript .claude/skills/clerk-election-returns/scripts/check_readings.R 2026
#
# Merges the reader outputs into data/clerk/claude_rows_<year>.tsv and writes, in
# data/<year>/, candidates_<year>_discrepancies.csv (rows that are internally
# inconsistent or that a reader flagged) and candidates_<year>_hand-entry.csv
# (special elections, runoffs and other races coded by hand).

library(tidyverse)
library(fs)

YEAR <- as.integer(commandArgs(trailingOnly = TRUE)[[1]])

work_dir <- path("data", "clerk", "claude_read", YEAR)
out_dir <- path("data", YEAR)
dir_create(out_dir)

COLS <- c("year", "page", "state", "office", "dist", "special", "kind", "name_printed", "party_lines",
          "line_votes", "candidatevotes", "footnote", "flags")
STATES <- c(state.abb, "DC", "AS", "GU", "PR", "VI", "MP")

first_part <- \(x) str_trim(str_split_i(coalesce(x, ""), ";", 1))

# Merge the readers' rows ----

batches <- read_csv(path(work_dir, "batches.csv"), show_col_types = FALSE)
missing <- filter(batches, !file_exists(path(work_dir, output)))
if (nrow(missing) > 0) stop("no reader output for pages ", str_c(missing$start, "-", missing$end, collapse = ", "))

# rows written without their empty trailing cells are padded
read_batch <- function(f) {
  x <- read_tsv(f, col_names = COLS, skip = 1, col_types = cols(.default = "c"), quote = "", na = "")
  if (!identical(str_split_1(read_lines(f, n_max = 1), "\t"), COLS)) stop(f, " does not have the columns reader.md specifies")
  x
}
claude_raw <- map_dfr(path(work_dir, batches$output), read_batch) |>
  arrange(as.integer(page))
write_tsv(claude_raw, path("data", "clerk", str_glue("claude_rows_{YEAR}.tsv")), na = "")

claude <- claude_raw |>
  mutate(dist = if_else(office == "S", "", as.character(as.integer(dist))),
         special = coalesce(special, "0"),
         race = str_c(state, office, dist, special, sep = "|"),
         label = if_else(kind %in% c("unnamed", "nonvote"), first_part(party_lines), name_printed),
         is_cand = kind %in% c("candidate", "writein", "unnamed"),
         total_votes = as.numeric(candidatevotes),
         line_sum = map_dbl(str_split(coalesce(line_votes, ""), ";"), \(v) sum(as.numeric(str_trim(v)))),
         n_party_lines = str_count(coalesce(party_lines, ""), ";") + 1,
         n_vote_lines = str_count(coalesce(line_votes, ""), ";") + 1)

# Discrepancies ----

issue <- \(df, what, detail) df |>
  transmute(type = what, state, office, dist, special, page, kind, name = label, detail = {{ detail }})

discrepancies <- bind_rows(
  claude |>
    filter(!state %in% STATES | !office %in% c("H", "S") | !special %in% c("0", "1") |
             !kind %in% c("candidate", "writein", "unnamed", "nonvote")) |>
    issue("unexpected code in state, office, special or kind", str_c(state, office, special, kind, sep = " / ")),
  claude |>
    filter(n() > 1, .by = c(race, label, line_votes)) |>
    issue("line appears more than once", line_votes),
  claude |>
    filter(!is.na(line_votes), n_party_lines != n_vote_lines) |>
    issue("party lines and figures do not line up", str_c(party_lines, " | ", line_votes)),
  claude |>
    filter(!is.na(total_votes), total_votes != line_sum) |>
    issue("candidatevotes is not the sum of line_votes", str_c(candidatevotes, " vs ", line_votes)),
  claude |>
    filter(is.na(line_votes), is.na(flags), is.na(footnote)) |>
    issue("figure missing with no flag or footnote", NA_character_),
  claude |>
    filter(!is.na(flags)) |>
    issue("reader flag", flags)
) |>
  arrange(type, state, office, as.integer(dist), as.integer(page))

write_csv(discrepancies, path(out_dir, str_glue("candidates_{YEAR}_discrepancies.csv")), na = "")

# Hand-entry list ----

hand_races <- claude |>
  summarise(
    special_election = any(special == "1"),
    no_majority = state[1] %in% c("GA", "LA") && any(is_cand) && all(!is.na(total_votes[is_cand])) &&
      2 * max(total_votes[is_cand]) <= sum(total_votes[is_cand]),
    other_round = any(str_detect(coalesce(flags, ""), "not the November first round") |
                        str_detect(coalesce(footnote, ""), "(?i)runoff|ranked|round")),
    # the volume does not always mark ranked-choice races; CAGE keeps the final round
    ranked_choice = ((state[1] == "ME" & YEAR >= 2018) | (state[1] == "AK" & YEAR >= 2022)) &&
      any(is_cand) && 2 * max(total_votes[is_cand], na.rm = TRUE) <= sum(total_votes[is_cand], na.rm = TRUE),
    .by = race
  ) |>
  pivot_longer(-race, names_to = "check", values_to = "hit") |>
  filter(hit) |>
  mutate(reason = recode(check,
                         special_election = "special election",
                         no_majority = "runoff state with no majority: the deciding round is not in this volume",
                         other_round = "figure may not be the November first round (footnote or ranked-choice lines)",
                         ranked_choice = "ranked-choice race with no first-round majority: CAGE keeps the final tabulation round")) |>
  summarise(reason = str_c(reason, collapse = "; "), .by = race)

hand_entry <- bind_rows(
  claude |>
    inner_join(hand_races, by = "race") |>
    mutate(scope = "race"),
  claude |>
    filter(!race %in% hand_races$race, kind == "unnamed") |>
    mutate(scope = "line", reason = "ballot line with no candidate name printed")
) |>
  select(scope, reason, state, office, dist, special, page, kind, name_printed, party_lines, line_votes,
         candidatevotes, footnote, flags) |>
  arrange(scope, state, office, as.integer(dist))

write_csv(hand_entry, path(out_dir, str_glue("candidates_{YEAR}_hand-entry.csv")), na = "")

message(str_glue(
  "{YEAR}: {nrow(claude_raw)} rows read, {sum(discrepancies$type != 'reader flag')} discrepancies ",
  "+ {sum(discrepancies$type == 'reader flag')} reader flags, ",
  "{nrow(hand_races)} hand-entry races and {sum(hand_entry$scope == 'line')} unnamed lines"
))
