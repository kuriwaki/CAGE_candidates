# Written by Codex

library(tidyverse)
source("../../00e_check-functions.R")

# Complete presidential fixtures ----
president_rows <- function(years = 2024) {
  crossing(year = years, state = c(state.abb, "DC"), party = c("D", "R")) |>
    mutate(
      office = "P", dist = NA_real_, type = "G", party_formal = party,
      name_snyder = if_else(party == "D", "SMITH, JANE", "JONES, JOHN"),
      candidatevotes = 50, totalvotes = 100, won = NA_real_
    )
}

test_that("presidential coverage includes both nominees in all states and DC", {
  expect_no_warning(check_presidents(president_rows(), years = 2024))
  expect_no_warning(check_presidents(president_rows(seq(1992, 2024, by = 4))))
})

test_that("presidential nominees may differ across election years", {
  different_nominees <- president_rows(c(2020, 2024)) |>
    mutate(name_snyder = case_when(
      year == 2024 & party == "D" ~ "LEE, ALICE",
      year == 2024 & party == "R" ~ "BROWN, JAMES",
      .default = name_snyder
    ))
  expect_no_warning(check_presidents(different_nominees, years = c(2020, 2024)))
})

test_that("missing and duplicate D/R state rows fail presidential coverage", {
  complete <- president_rows()
  bad_data <- list(
    "DC missing" = complete |> filter(state != "DC"),
    "one nominee missing" = complete |> filter(!(state == "TX" & party == "D")),
    "duplicate nominee" = bind_rows(complete, slice(complete, 1)),
    "unexpected state" = complete |> mutate(state = if_else(state == "DC", "PR", state)),
    "missing state" = complete |> mutate(state = if_else(state == "DC", NA_character_, state))
  )
  iwalk(bad_data, \(data, case) {
    error <- tryCatch(check_presidents(data, years = 2024), cage_presidential_validation_error = identity)
    expect_s3_class(error, "cage_presidential_validation_error")
    expect_true("D/R presidential coverage in every state and DC" %in% names(error$checks), info = case)
  })
})

test_that("unexpected D/R election years fail presidential coverage", {
  extra_year <- president_rows() |>
    slice(1) |>
    mutate(year = 2020)
  error <- tryCatch(
    check_presidents(bind_rows(president_rows(), extra_year), years = 2024),
    cage_presidential_validation_error = identity
  )
  expect_s3_class(error, "cage_presidential_validation_error")
  expect_true("D/R presidential coverage in every state and DC" %in% names(error$checks))
})

test_that("empty presidential data and entirely missing years are errors", {
  no_presidents <- president_rows() |>
    slice_head(n = 0)
  expect_error(check_presidents(no_presidents, years = 2024),
               class = "cage_presidential_validation_error")
  error <- tryCatch(
    check_presidents(president_rows(), years = c(2020, 2024)),
    cage_presidential_validation_error = identity
  )
  expect_s3_class(error, "cage_presidential_validation_error")
  expect_true("D/R presidential coverage in every state and DC" %in% names(error$checks))
})

test_that("nominee discrepancies and missing names fail without changing coverage", {
  walk(c("DIFFERENT, NAME", "", NA_character_), \(wrong_name) {
    inconsistent <- president_rows() |>
      mutate(name_snyder = if_else(state == "DC" & party == "D", wrong_name, name_snyder))
    error <- tryCatch(check_presidents(inconsistent, years = 2024), cage_presidential_validation_error = identity)
    expect_s3_class(error, "cage_presidential_validation_error")
    expect_named(error$checks, "D/R presidential nominees consistent across states")
  })
  unnamed_nominees <- president_rows() |>
    mutate(name_snyder = NA_character_)
  error <- tryCatch(check_presidents(unnamed_nominees, years = 2024), cage_presidential_validation_error = identity)
  expect_s3_class(error, "cage_presidential_validation_error")
  expect_named(error$checks, "D/R presidential nominees consistent across states")
})

test_that("presidential winner flags must be missing for major and minor parties", {
  walk(c(0, 1), \(winner_flag) {
    major_flag <- president_rows() |>
      mutate(won = if_else(state == "DC" & party == "D", winner_flag, won))
    minor_flag <- president_rows() |>
      slice(1) |>
      mutate(party = "Other", party_formal = "Other", name_snyder = "THIRD, CANDIDATE", won = winner_flag)
    walk(list(major_flag, bind_rows(president_rows(), minor_flag)), \(data) {
      error <- tryCatch(check_presidents(data, years = 2024), cage_presidential_validation_error = identity)
      expect_s3_class(error, "cage_presidential_validation_error")
      expect_named(error$checks, "Presidential winner flags all missing")
    })
  })
})

test_that("non-presidential records are excluded from presidential checks", {
  house_winner <- president_rows() |>
    slice(1) |>
    mutate(office = "H", state = "ZZ", name_snyder = "OTHER, WINNER", won = 1)
  expect_no_warning(check_presidents(bind_rows(president_rows(), house_winner), years = 2024))
  expect_error(check_presidents(house_winner, years = 2024),
               class = "cage_presidential_validation_error")
})

test_that("each presidential check reports its pass or fail symbol", {
  withr::local_options(list(cli.unicode = TRUE, cli.num_colors = 1, cli.width = 200))
  passed <- capture_messages(check_presidents(president_rows(), years = 2024))
  walk(c("D/R presidential coverage in every state and DC",
         "D/R presidential nominees consistent across states",
         "Presidential winner flags all missing"), \(check) {
    expect_true(any(str_detect(passed, fixed(as.character(glue::glue("✔ {check}: passed"))))))
  })
  flagged <- president_rows() |>
    mutate(won = 0)
  failed <- capture_messages(
    expect_error(check_presidents(flagged, years = 2024),
                 class = "cage_presidential_validation_error")
  )
  expect_true(any(str_detect(failed, "✖.*Presidential winner flags all missing: failed")))
})
