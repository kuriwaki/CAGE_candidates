# Written by Codex
# Temporary upload for the 2026 election-night spreadsheet.

library(tidyverse)
library(haven)
library(googlesheets4)

# Read the release files ----
cand_sorted <- read_dta("release/candidates_1990-2024.dta") |>
  mutate(across(everything(), as.vector)) |>
  arrange(desc(year), desc(office))

contest_sorted <- read_dta("release/candidates_1990-2024-by-contest.dta") |>
  mutate(across(everything(), as.vector)) |>
  arrange(desc(year), desc(office))

# Google Sheets connection ----
spreadsheet_id <- "1otML47xPsa9Wf1A_XEISLJFHEm6wpv5D3FzRe2-xZXY"
gs4_auth(email = TRUE)

# Sheet formatting ----
format_release_sheet <- function(data, sheet, ss) {
  sheet_info <- sheet_properties(ss) |>
    filter(name == sheet)

  # Leave room for both the longest value and the header's filter button.
  column_widths <- imap_dbl(data, \(column, name) {
    max(80, 8 * max(nchar(c(name, as.character(column))), na.rm = TRUE) + 36)
  })
  width_requests <- map(seq_along(data), \(column) {
    list(updateDimensionProperties = list(
      range = list(
        sheetId = sheet_info$id,
        dimension = "COLUMNS",
        startIndex = column - 1,
        endIndex = column
      ),
      properties = list(pixelSize = column_widths[[column]]),
      fields = "pixelSize"
    ))
  })

  request <- request_generate(
    "sheets.spreadsheets.batchUpdate",
    params = list(
      spreadsheetId = ss,
      requests = c(width_requests, list(
        list(setBasicFilter = list(filter = list(range = list(
          sheetId = sheet_info$id,
          startRowIndex = 0,
          endRowIndex = nrow(data) + 1,
          startColumnIndex = 0,
          endColumnIndex = ncol(data)
        ))))
      ))
    )
  )
  response <- request_make(request)
  gargle::response_process(response)
}

# Upload and verify each sheet ----
# Re-running this script replaces these two tabs with the current release.
upload_release_sheet <- function(data, sheet, ss) {
  sheet_write(data, ss = ss, sheet = sheet)
  format_release_sheet(data, sheet, ss)

  column_types <- map_chr(data, \(column) if (is.numeric(column)) "d" else "c") |>
    str_c(collapse = "")

  # Stata represents missing text as empty strings; Sheets reads blank cells as NA.
  uploaded <- read_sheet(ss, sheet = sheet, col_types = column_types, trim_ws = FALSE) |>
    mutate(across(where(is.character), \(column) replace_na(column, "")))
  if (!identical(names(uploaded), names(data)) ||
      !isTRUE(all.equal(uploaded, data, check.attributes = FALSE))) {
    cli::cli_abort(glue::glue("Upload verification failed for {sheet}."))
  }

  cli::cli_alert_success(glue::glue(
    "{sheet}: uploaded and verified {scales::comma(nrow(data))} rows."
  ))
}

# Upload the two release versions ----
upload_release_sheet(cand_sorted, "1990-2024-CAGE_by-cand", spreadsheet_id)
upload_release_sheet(contest_sorted, "1990-2024-CAGE_by-contest", spreadsheet_id)
