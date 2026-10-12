# Prepare the reader folder for Claude's reading of a Clerk volume.
#
# Run from the repo root:
#
#   Rscript .claude/skills/clerk-election-returns/scripts/prepare_reading.R 2026
#
# Writes to data/clerk/claude_read/<year>/: page images (top and bottom halves
# at 160 dpi), history.tsv (candidate rows from years before <year>), party_labels.tsv, a copy of
# reader.md, and batches.csv (the page batches to hand to reader agents).

library(tidyverse)
library(fs)

YEAR <- as.integer(commandArgs(trailingOnly = TRUE)[[1]])
BATCH_SIZE <- 10L
DPI <- 160

skill_dir <- path(".claude", "skills", "clerk-election-returns")
pdf_path <- path("data", "clerk", str_glue("{YEAR}election.pdf"))
# the most recent release build, e.g. data/intermediate/candidates_1990-2024.rds
history_path <- dir_ls(path("data", "intermediate"), regexp = "candidates_\\d{4}-\\d{4}\\.rds$") |>
  sort() |>
  tail(1)
work_dir <- path("data", "clerk", "claude_read", YEAR)

stopifnot(file_exists(pdf_path), length(history_path) == 1)
dir_create(path(work_dir, c("pages", "out", "work")))

# Page images ----

# halves overlap by 40 px so no printed line is cut in two
pages <- pdftools::pdf_pagesize(pdf_path) |>
  as_tibble() |>
  mutate(page = row_number(),
         w = round(width / 72 * DPI),
         h = round(height / 72 * DPI),
         stem = path(work_dir, "pages", sprintf("p%03d", page)))

walk(seq_len(nrow(pages)), \(i) {
  p <- pages[i, ]
  system2("pdftoppm", c("-f", p$page, "-l", p$page, "-r", DPI, "-x", 0, "-y", 0,
                        "-W", p$w, "-H", p$h %/% 2 + 20, "-png", "-singlefile", pdf_path, str_c(p$stem, "_top")))
  system2("pdftoppm", c("-f", p$page, "-l", p$page, "-r", DPI, "-x", 0, "-y", p$h %/% 2 - 20,
                        "-W", p$w, "-H", p$h - p$h %/% 2 + 20, "-png", "-singlefile", pdf_path, str_c(p$stem, "_bottom")))
})

# Coding files ----

# earlier years only, so a run on a year already in the dataset cannot copy its answers
hist <- read_rds(history_path) |>
  filter(year < YEAR)

hist |>
  select(year, state, office, dist, type, party, party_formal, name_snyder) |>
  write_tsv(path(work_dir, "history.tsv"), na = "")

hist |>
  filter(!is.na(party_formal)) |>
  count(state, party_formal, party, sort = TRUE) |>
  write_tsv(path(work_dir, "party_labels.tsv"), na = "")

file_copy(path(skill_dir, "reference", "reader.md"), path(work_dir, "reader.md"), overwrite = TRUE)

# Batches ----

# page 1 is the cover; pages without candidate listings simply produce no rows
batches <- tibble(start = seq(2L, nrow(pages), by = BATCH_SIZE)) |>
  mutate(end = pmin(start + BATCH_SIZE - 1L, nrow(pages)),
         context_before = start - 1L,
         context_after = if_else(end < nrow(pages), end + 1L, NA_integer_),
         output = sprintf("out/pages_%03d-%03d.tsv", start, end))

write_csv(batches, path(work_dir, "batches.csv"), na = "")

message(str_glue("{YEAR}: {nrow(pages)} pages rendered, {nrow(batches)} batches, history through {max(hist$year)} in {work_dir}"))
