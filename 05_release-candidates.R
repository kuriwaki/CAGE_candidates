library(tidyverse)
library(haven)
library(fs)

source("00e_check-functions.R")

#' function to add labels
paste_labels <- function(tbl, lab_df = var_labels) {
  stopifnot(all(!duplicated(var_labels$alias)))
  stopifnot(all(!duplicated(var_labels$label)))

  for (c in colnames(tbl)) {
    attr(tbl[[c]], "label") <- lab_df$label[which(lab_df$alias == c)]
  }
  tbl
}


# Read from Output -----
cand_raw <- read_rds("data/intermediate/candidates_1990-2024.rds")


# remove variables from candidate -----
order_candvars <- c("year", "state",
                    "office", "dist", "type", "nextup",
                    "party", "party_formal",
                    "name_snyder", "inc",
                    "candidatevotes", "totalvotes", "won",
                    "runoff")

cand <- cand_raw |>
  rename(candidatevotes = vote_g,
         won = w_g) |>
  select(!!!order_candvars)

# Validate before saving the release ----
check_candidates(cand)
check_presidents(cand)

# Contest-level data ----
cand_by_contest <- cand |>
  filter(party %in% c("D", "R")) |>
  filter(
    sum(party == "D") == 1 & sum(party == "R") == 1,
    .by = c(year, state, office, dist, type)
  ) |>
  pivot_wider(
    id_cols = c(year, state, office, dist, type),
    names_from = party,
    values_from = c(name_snyder, inc, candidatevotes)
  ) |>
  mutate(share_D_twoparty = candidatevotes_D / (candidatevotes_D + candidatevotes_R)) |>
  arrange(desc(year), desc(office))

# Variable labels ----

# Define labels
var_labels <- tribble(
  ~alias, ~label,
  "year",  "Year of the general election or CCES",
  "state",     "State two-letter abbreviation",
  "office", "Office (H = House, S = Senate, G = Governor, P = President)",
  "dist", "Congressional district number, current",
  "runoff", "Whether the election is a runoff (if applicable)",
  "nextup", "The next year the winner will be up",
  "party", "Candidate party affiliation (short)",
  "party_formal", "Candidate party affiliation (formal)",
  "name_snyder", "Candidate name",
  "inc", "Candidate is incumbent",
  "candidatevotes", "Votes for the candidate",
  "totalvotes", "Total votes cast in the race",
  "won", "Whether the candidate won the general election",
  "type", "Type of election. (G = general, S = special)",
  "name_snyder_D", "Democratic candidate name",
  "name_snyder_R", "Republican candidate name",
  "inc_D", "Democratic candidate is incumbent",
  "inc_R", "Republican candidate is incumbent",
  "candidatevotes_D", "Votes for the Democratic candidate",
  "candidatevotes_R", "Votes for the Republican candidate",
  "share_D_twoparty", "Democratic share of the Democratic and Republican vote"
)

# add labels ------
cand_fmt <- paste_labels(cand)
cand_by_contest_fmt <- paste_labels(cand_by_contest)

# Save to Release ------
release_dir <- "release"

# Clear
# file_delete(dir_ls(release_dir, regexp = "(csv|dta|rds)$"))

# Save
write_dta(cand_fmt, path(release_dir, "candidates_1990-2024.dta"))
write_dta(cand_by_contest_fmt, path(release_dir, "candidates_1990-2024-by-contest.dta"))



