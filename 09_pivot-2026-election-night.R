# Written by Codex
# Create the 2026 election-night contest sheet with live candidate lookups and Democratic two-party vote-share formulas.

library(tidyverse)
library(googlesheets4)

# Read the candidate sheet ----
spreadsheet_id <- "1otML47xPsa9Wf1A_XEISLJFHEm6wpv5D3FzRe2-xZXY"
source_sheet <- "Results_2026"
output_sheet <- "2026_by-contest"
gs4_auth(email = TRUE)
source_data <- read_sheet(spreadsheet_id, sheet = source_sheet, col_types = "c")
stopifnot(
  identical(names(source_data)[c(7, 8, 17, 23)], c("inc", "votes_live", "name_snyder", "cage_row_id")),
  !anyNA(source_data$cage_row_id), !anyDuplicated(source_data$cage_row_id)
)

# Pivot the same one-D/one-R contests as the historical release ----
contest_pairs <- source_data |>
  filter(party %in% c("D", "R")) |>
  filter(sum(party == "D") == 1 & sum(party == "R") == 1,
         .by = c(year, state, office, dist, type)) |>
  pivot_wider(
    id_cols = c(year, state, office, dist, type), names_from = party,
    values_from = c(name_snyder, inc, votes_live, cage_row_id)
  ) |>
  arrange(desc(year), desc(office))

# Link values by candidate ID, preserving missing cells and explicit zeros ----
lookup_formula <- function(candidate_id, column) {
  quoted_id <- str_replace_all(candidate_id, '"', '""')
  ids <- glue::glue("'{source_sheet}'!$W$2:$W")
  values <- glue::glue("'{source_sheet}'!${column}$2:${column}")
  gs4_formula(glue::glue('=IF(AND(COUNTIF({ids},"{quoted_id}")=1,COUNTIFS({ids},"{quoted_id}",{values},"<>")=1),XLOOKUP("{quoted_id}",{ids},{values},""),"")'))
}
share_formula <- function(row) {
  gs4_formula(glue::glue('=IF(AND(COUNT(J{row}:K{row})=2,MIN(J{row}:K{row})>=0,SUM(J{row}:K{row})>0),J{row}/SUM(J{row}:K{row}),"")'))
}
contest_formulas <- contest_pairs |>
  transmute(
    year = as.integer(year), state, office, dist = as.integer(dist), type,
    name_snyder_D = lookup_formula(cage_row_id_D, "Q"),
    name_snyder_R = lookup_formula(cage_row_id_R, "Q"),
    inc_D = lookup_formula(cage_row_id_D, "G"),
    inc_R = lookup_formula(cage_row_id_R, "G"),
    candidatevotes_D = lookup_formula(cage_row_id_D, "H"),
    candidatevotes_R = lookup_formula(cage_row_id_R, "H"),
    share_D_twoparty = share_formula(row_number() + 1L)
  )

# Replace the generated tab and display shares as percentages ----
sheet_write(contest_formulas, ss = spreadsheet_id, sheet = output_sheet)
output_id <- sheet_properties(spreadsheet_id) |>
  filter(name == output_sheet) |>
  pull(id)
format_request <- request_generate(
  "sheets.spreadsheets.batchUpdate",
  params = list(spreadsheetId = spreadsheet_id, requests = list(list(repeatCell = list(
    range = list(sheetId = output_id, startRowIndex = 1, startColumnIndex = 11, endColumnIndex = 12),
    cell = list(userEnteredFormat = list(numberFormat = list(type = "PERCENT", pattern = "0.00%"))),
    fields = "userEnteredFormat.numberFormat"
  ))))
)
format_response <- request_make(format_request)
invisible(gargle::response_process(format_response))
cli::cli_alert_success(glue::glue("Uploaded {scales::comma(nrow(contest_formulas))} contests to {output_sheet}."))
