# Prepare the reader folder for Claude's reading of a Clerk volume.
#
# Run from the repo root:
#
#   Rscript .claude/skills/clerk-election-returns/scripts/prepare_reading.R 2026
#   Rscript .claude/skills/clerk-election-returns/scripts/prepare_reading.R 2024 --roster data/2024/roster_test.csv
#
# Writes to data/clerk/claude_read/<year>/: page images (top and bottom halves at
# 160 dpi), roster.csv (the CAGE roster for <year>, from the Google Sheet tab
# Results_<year> unless --roster gives a CSV), a copy of reader.md, and
# batches.csv (the page batches to hand to reader agents).

library(tidyverse)
library(fs)

args <- commandArgs(trailingOnly = TRUE)
YEAR <- as.integer(args[[1]])
ROSTER_CSV <- if ("--roster" %in% args) args[[which(args == "--roster") + 1]] else NA
SHEET_ID <- "1otML47xPsa9Wf1A_XEISLJFHEm6wpv5D3FzRe2-xZXY"
BATCH_SIZE <- 10L
DPI <- 160

skill_dir <- path(".claude", "skills", "clerk-election-returns")
pdf_path <- path("data", "clerk", str_glue("{YEAR}election.pdf"))
work_dir <- path("data", "clerk", "claude_read", YEAR)

stopifnot(file_exists(pdf_path))
dir_create(path(work_dir, c("pages", "out", "work")))

# Roster ----

roster <- if (is.na(ROSTER_CSV)) {
  googlesheets4::gs4_auth(cache = TRUE, email = TRUE)
  googlesheets4::read_sheet(SHEET_ID, sheet = str_glue("Results_{YEAR}"), col_types = "c")
} else {
  read_csv(ROSTER_CSV, col_types = cols(.default = "c"))
}
write_csv(roster, path(work_dir, "roster.csv"), na = "")

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

file_copy(path(skill_dir, "reference", "reader.md"), path(work_dir, "reader.md"), overwrite = TRUE)

# Batches ----

# page 1 is the cover; pages without candidate listings simply produce no rows
batches <- tibble(start = seq(2L, nrow(pages), by = BATCH_SIZE)) |>
  mutate(end = pmin(start + BATCH_SIZE - 1L, nrow(pages)),
         context_before = start - 1L,
         context_after = if_else(end < nrow(pages), end + 1L, NA_integer_),
         output = sprintf("out/pages_%03d-%03d.tsv", start, end))

write_csv(batches, path(work_dir, "batches.csv"), na = "")

message(str_glue("{YEAR}: {nrow(pages)} pages rendered, {nrow(batches)} batches, {nrow(roster)} roster rows in {work_dir}"))
