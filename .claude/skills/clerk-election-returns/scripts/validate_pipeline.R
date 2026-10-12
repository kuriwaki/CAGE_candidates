# Validate the clerk-election-returns pipeline on a year CAGE already covers.
#
# Run from the repo root after TASK.md has been executed with a test roster:
#
#   Rscript .claude/skills/clerk-election-returns/scripts/validate_pipeline.R 2024
#
# Compares data/<year>/candidates_<year>_skeleton.csv with the audited CAGE rows
# for that year (data/intermediate/candidates_1990-2024.rds), prints a summary and
# every difference, and writes data/clerk/validation/<year>/candidates.csv.

library(tidyverse)
library(fs)

YEAR <- as.integer(commandArgs(trailingOnly = TRUE)[[1]])
options(width = 220)

val_dir <- path("data", "clerk", "validation", YEAR)
dir_create(val_dir)

key <- c("state", "office", "dist", "type", "name_snyder")

skel <- read_csv(path("data", YEAR, str_glue("candidates_{YEAR}_skeleton.csv")), show_col_types = FALSE) |>
  filter(office %in% c("H", "S"))
cage <- read_rds("data/intermediate/candidates_1990-2024.rds") |>
  filter(year == YEAR, office %in% c("H", "S")) |>
  select(all_of(key), c_party = party, c_votes = vote_g, c_total = totalvotes, c_won = w_g, c_runoff = runoff)

hand <- str_detect(coalesce(skel$flags, ""), "hand entry")
roster_rows <- filter(skel, !match %in% "not in roster", !hand)
added <- filter(skel, match %in% "not in roster")

same <- \(a, b) coalesce(a == b, is.na(a) & is.na(b))
cmp <- roster_rows |>
  left_join(cage, by = key) |>
  mutate(votes_equal = same(candidatevotes, c_votes), won_equal = same(won, c_won), total_equal = same(totalvotes, c_total))

# added rows: does CAGE have the same person (exact name) or the same race and votes?
added_cmp <- added |>
  left_join(cage |> select(all_of(key), c_votes), by = key) |>
  left_join(cage |> select(state, office, dist, type, c_name = name_snyder, c_votes2 = c_votes),
            by = c("state", "office", "dist", "type", "candidatevotes" = "c_votes2"), relationship = "many-to-many") |>
  mutate(in_cage = case_when(!is.na(c_votes) ~ "same name", !is.na(c_name) ~ "same race and votes, other name", .default = "not in CAGE"))

metric <- \(name, n, d = NA) tibble(metric = name, value = if (is.na(d)) as.character(n) else str_glue("{n} / {d} ({round(100 * n / d, 1)}%)"))
summary <- bind_rows(
  metric("House/Senate skeleton rows", nrow(skel)),
  metric("rows in hand-entry races (left blank)", sum(hand)),
  metric("roster rows matched to a Clerk line", sum(!is.na(roster_rows$match)), nrow(roster_rows)),
  metric("roster rows not printed by the Clerk", sum(str_detect(coalesce(roster_rows$flags, ""), "not printed"))),
  metric("candidatevotes identical to CAGE", sum(cmp$votes_equal), nrow(cmp)),
  metric("totalvotes identical to CAGE", sum(cmp$total_equal), nrow(cmp)),
  metric("won identical to CAGE", sum(cmp$won_equal), nrow(cmp)),
  metric("rows added from the Clerk (not in roster)", nrow(added)),
  metric("  added rows CAGE has under the same name", sum(added_cmp$in_cage == "same name")),
  metric("  added rows CAGE has under another name", sum(added_cmp$in_cage == "same race and votes, other name")),
  metric("  added rows not in CAGE", sum(added_cmp$in_cage == "not in CAGE"))
)

show <- \(df) if (nrow(df) == 0) message("none") else print(as.data.frame(df), row.names = FALSE, right = FALSE)
message(str_glue("\n# {YEAR}: skeleton vs audited CAGE"))
show(summary)
message("\n# candidatevotes, totalvotes or won differ")
cmp |>
  filter(!votes_equal | !won_equal | !total_equal) |>
  select(state, office, dist, type, name_snyder, name_printed, match, candidatevotes, c_votes, totalvotes, c_total, won, c_won, flags) |>
  show()
message("\n# roster rows not printed by the Clerk")
roster_rows |> filter(str_detect(coalesce(flags, ""), "not printed")) |> select(state, office, dist, type, name_snyder, party) |> show()
message("\n# added rows CAGE has under another name, or not at all")
added_cmp |> filter(in_cage != "same name") |> select(state, office, dist, name_printed, name_snyder, c_name, candidatevotes, in_cage) |> show()
message("\n# hand-entry rows")
skel |> filter(hand) |> count(state, office, dist, type, flags) |> show()

write_csv(bind_rows(mutate(cmp, part = "roster"), mutate(added_cmp, part = "added")), path(val_dir, "candidates.csv"), na = "")
write_csv(summary, path(val_dir, "summary.csv"))
message(str_glue("\nwrote summary.csv and candidates.csv to {val_dir}"))
