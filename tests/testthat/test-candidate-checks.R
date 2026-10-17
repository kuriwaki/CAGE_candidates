# Written by Codex

library(tidyverse)
source("../../00e_check-functions.R")

# An odd-year race keeps national election totals out of these unit tests.
valid_race <- tibble(
  year = 2023, state = "AZ", office = "H", dist = 1, type = "G",
  name_snyder = c("SMITH, JANE", "JONES, JOHN"),
  party = c("D", "R"), party_formal = c("D", "R"),
  candidatevotes = c(60, 40), totalvotes = 100, won = c(1, 0)
)

# Synthetic national totals keep these tests independent of the released data.
regular_elections <- function(year, house = 435, senate = if (year %% 6 == 0) 34 else 33) {
  senate_class <- case_when(
    year %% 6 == 2 ~ 1,
    year %% 6 == 4 ~ 2,
    year %% 6 == 0 ~ 3
  )
  bind_rows(
    tibble(state = "AZ", office = "H", dist = seq_len(house)),
    tibble(state = state.abb[seq_len(senate)], office = "S", dist = senate_class)
  ) |>
    mutate(
      year = year, type = "G", name_snyder = "SMITH, JANE",
      party = "D", party_formal = "D", candidatevotes = 100, totalvotes = 100, won = 1
    )
}

test_that("valid races pass, including unreported uncontested votes", {
  expect_no_error(check_candidates(valid_race))
  unreported <- valid_race |>
    slice(1) |>
    mutate(candidatevotes = NA_real_, totalvotes = NA_real_)
  expect_no_warning(check_candidates(unreported))
})

test_that("presidential rows remain excluded", {
  presidential <- valid_race |>
    mutate(office = "P", party = "INVALID", candidatevotes = -1)
  expect_no_error(check_candidates(bind_rows(valid_race, presidential)))
  expect_error(check_candidates(slice_head(valid_race, n = 0)),
               class = "cage_validation_error")
})

test_that("each enforced check rejects the relevant bad data", {
  bad_data <- list(
    "Missing or invalid candidate identifiers" = valid_race |>
      mutate(name_snyder = NA_character_),
    "Duplicate candidates within a race" = bind_rows(valid_race, slice(valid_race, 2)),
    "Invalid or missing winner indicators" = valid_race |>
      mutate(won = NA_real_),
    "Races with an unexpected number of winners" = valid_race |>
      mutate(won = 1),
    "Winners with fewer votes than another candidate" = valid_race |>
      mutate(won = 1 - won),
    "Invalid vote counts" = valid_race |>
      mutate(candidatevotes = -candidatevotes),
    "Race totals inconsistent with candidate votes" = valid_race |>
      mutate(totalvotes = 101),
    "Invalid party codes" = valid_race |>
      mutate(party = NA_character_),
    "Inconsistent short and formal parties" = valid_race |>
      mutate(party_formal = "R")
  )

  iwalk(bad_data, \(data, check) {
    error <- tryCatch(check_candidates(data), cage_validation_error = identity)
    expect_s3_class(error, "cage_validation_error")
    expect_true(check %in% names(error$checks), info = check)
  })
})

test_that("multiple failures are reported together", {
  bad_race <- valid_race |>
    mutate(party = "INVALID", totalvotes = 101)
  error <- tryCatch(check_candidates(bad_race), cage_validation_error = identity)
  expect_setequal(names(error$checks), c(
    "Invalid party codes", "Race totals inconsistent with candidate votes"
  ))
})

test_that("missing vote totals and non-finite vote counts are errors", {
  expect_error(
    check_candidates(mutate(valid_race, totalvotes = NA_real_)),
    class = "cage_validation_error"
  )
  expect_error(
    check_candidates(mutate(valid_race, candidatevotes = Inf, totalvotes = Inf)),
    class = "cage_validation_error"
  )
})

test_that("the NC-09 exception is limited to the uncertified 2018 election", {
  nc_2018 <- valid_race |>
    mutate(year = 2018, state = "NC", dist = 9, won = NA_real_)
  error <- tryCatch(check_candidates(nc_2018), cage_validation_error = identity)
  expect_s3_class(error, "cage_validation_error")
  expect_false("Invalid or missing winner indicators" %in% names(error$checks))
  expect_false("Races with an unexpected number of winners" %in% names(error$checks))
  expect_true("House general election winners (435; 434 in 2018)" %in% names(error$checks))
  expect_error(
    check_candidates(mutate(nc_2018, year = 2023)),
    class = "cage_validation_error"
  )
  expect_error(
    suppressWarnings(check_candidates(mutate(nc_2018, won = c(1, 0)))),
    class = "cage_validation_error"
  )
})

test_that("governor counts, unusual names and turnout remain advisory", {
  governor <- valid_race |>
    mutate(year = 2024, office = "G", dist = NA_real_)
  expect_warning(
    check_candidates(bind_rows(regular_elections(2024), governor)),
    class = "cage_validation_warning"
  )
  expect_warning(
    check_candidates(mutate(valid_race, name_snyder = str_to_lower(name_snyder))),
    class = "cage_validation_warning"
  )
  turnout <- map_dfr(1:3, \(district) {
    valid_race |>
      mutate(
        dist = district,
        candidatevotes = candidatevotes * if_else(district == 3, 10, 1),
        totalvotes = totalvotes * if_else(district == 3, 10, 1)
      )
  })
  expect_warning(check_candidates(turnout), class = "cage_validation_warning")
})

test_that("House general elections require exactly 435 winners", {
  expect_no_warning(check_candidates(regular_elections(2024)))
  walk(c(434, 436), \(n_winners) {
    error <- tryCatch(
      check_candidates(regular_elections(2024, house = n_winners)),
      cage_validation_error = identity
    )
    expect_s3_class(error, "cage_validation_error")
    expect_named(error$checks, "House general election winners (435; 434 in 2018)")
    expect_equal(error$checks[[1]]$winners, n_winners)
    expect_equal(error$checks[[1]]$expected, 435)
  })
})

test_that("the 434-winner House exception applies only to 2018", {
  expect_no_warning(check_candidates(regular_elections(2018, house = 434)))
  error <- tryCatch(
    check_candidates(regular_elections(2018, house = 435)),
    cage_validation_error = identity
  )
  expect_named(error$checks, "House general election winners (435; 434 in 2018)")
  expect_equal(error$checks[[1]]$expected, 434)
  expect_error(
    check_candidates(regular_elections(2016, house = 434)),
    class = "cage_validation_error"
  )
})

test_that("Senate winner totals depend on the scheduled class", {
  walk(c(2020, 2022, 2024), \(election_year) {
    expect_no_warning(check_candidates(regular_elections(election_year)))
    wrong_total <- if (election_year == 2022) 33 else 34
    error <- tryCatch(
      check_candidates(regular_elections(election_year, senate = wrong_total)),
      cage_validation_error = identity
    )
    expect_s3_class(error, "cage_validation_error")
    expect_named(error$checks, "Senate general election winners (33 or 34 by class)")
    expect_equal(error$checks[[1]]$senate_class,
                 c(`2020` = 2, `2022` = 3, `2024` = 1)[[as.character(election_year)]])
    expect_equal(error$checks[[1]]$expected, if (election_year == 2022) 34 else 33)
  })
})

test_that("Senate general election candidates require the scheduled race class", {
  general <- regular_elections(2024)
  walk(c(2, 999, NA_real_), \(wrong_class) {
    wrong_class_data <- general |>
      mutate(dist = if_else(office == "S" & state == "AZ", wrong_class, dist))
    error <- tryCatch(check_candidates(wrong_class_data), cage_validation_error = identity)
    expect_s3_class(error, "cage_validation_error")
    expect_true("Senate general election race classes" %in% names(error$checks))
    expect_false("Senate general election winners (33 or 34 by class)" %in% names(error$checks))
    expect_equal(error$checks[["Senate general election race classes"]]$state, "AZ")
  })
})

test_that("Senate race classes are checked for losing candidates too", {
  general <- regular_elections(2024)
  wrong_class_loser <- general |>
    filter(office == "S", state == "AZ") |>
    mutate(dist = 2, name_snyder = "JONES, JOHN", candidatevotes = 40, totalvotes = 40, won = 0)
  error <- tryCatch(
    check_candidates(bind_rows(general, wrong_class_loser)),
    cage_validation_error = identity
  )
  expect_s3_class(error, "cage_validation_error")
  expect_true("Senate general election race classes" %in% names(error$checks))
  expect_false("Senate general election winners (33 or 34 by class)" %in% names(error$checks))
  expect_equal(error$checks[["Senate general election race classes"]]$state, "AZ")
  expect_equal(error$checks[["Senate general election race classes"]]$dist, 2)
  expect_equal(error$checks[["Senate general election race classes"]]$winners, 0)
})

test_that("the Alaska 2010 and Louisiana 2020 class errors are rejected", {
  cases <- tribble(
    ~election_year, ~race_state, ~wrong_class,
    2010, "AK", 2,
    2020, "LA", 1
  )
  pwalk(cases, \(election_year, race_state, wrong_class) {
    wrong_class_data <- regular_elections(election_year) |>
      mutate(dist = if_else(office == "S" & state == race_state, wrong_class, dist))
    error <- tryCatch(check_candidates(wrong_class_data), cage_validation_error = identity)
    expect_s3_class(error, "cage_validation_error")
    expect_named(error$checks, "Senate general election race classes")
    expect_equal(error$checks[[1]]$state, race_state)
    expect_equal(error$checks[[1]]$year, election_year)
    expect_no_warning(check_candidates(regular_elections(election_year)))
  })
})

test_that("off-cycle Senate classes are allowed only for special elections", {
  general <- regular_elections(2024)
  special <- general |>
    filter(office == "S", state == "AZ") |>
    mutate(type = "S", dist = 2)
  expect_no_warning(check_candidates(bind_rows(general, special)))

  odd_year_special <- special |>
    mutate(year = 2023)
  expect_no_warning(check_candidates(bind_rows(valid_race, odd_year_special)))

  odd_year_general <- odd_year_special |>
    mutate(type = "G")
  error <- tryCatch(check_candidates(odd_year_general), cage_validation_error = identity)
  expect_s3_class(error, "cage_validation_error")
  expect_named(error$checks, "Senate general election race classes")
})

test_that("missing offices and all-special elections do not bypass national counts", {
  general <- regular_elections(2024)
  missing_senate <- general |>
    filter(office != "S")
  error <- tryCatch(check_candidates(missing_senate), cage_validation_error = identity)
  expect_named(error$checks, "Senate general election winners (33 or 34 by class)")
  expect_equal(error$checks[[1]]$winners, 0)
  expect_error(
    check_candidates(mutate(general, type = "S")),
    class = "cage_validation_error"
  )
})

test_that("special election winners do not increase general election totals", {
  general <- regular_elections(2024)
  special <- general |>
    slice(1, n()) |>
    mutate(type = "S")
  expect_no_warning(check_candidates(bind_rows(general, special)))
})

test_that("every check is listed with its pass or fail symbol", {
  withr::local_options(list(cli.unicode = TRUE, cli.num_colors = 1, cli.width = 200))
  passed <- capture_messages(check_candidates(valid_race))
  expect_true(any(str_detect(passed, "✔.*Invalid party codes: passed")))
  expect_true(any(str_detect(passed, "✔.*House general election winners.*passed")))
  expect_true(any(str_detect(passed, "✔.*Unusually high turnout: passed")))

  failed <- capture_messages(
    expect_error(check_candidates(mutate(valid_race, party = "INVALID")),
                 class = "cage_validation_error")
  )
  expect_true(any(str_detect(failed, "✖.*Invalid party codes: failed")))
  expect_true(any(str_detect(failed, "✔.*Invalid vote counts: passed")))
})

test_that("05 validates before replacing the release", {
  project_dir <- normalizePath("../..")
  workspace <- withr::local_tempdir()
  dir.create(file.path(workspace, "data", "intermediate"), recursive = TRUE)
  dir.create(file.path(workspace, "release"))
  file.copy(file.path(project_dir, "00e_check-functions.R"), workspace)

  raw_house <- valid_race |>
    mutate(nextup = 2025, inc = if_else(party == "D", 1, 0), runoff = 0) |>
    rename(vote_g = candidatevotes, w_g = won)
  raw_presidents <- crossing(
    year = seq(1992, 2024, by = 4), state = c(state.abb, "DC"), party = c("D", "R")
  ) |>
    mutate(
      office = "P", dist = NA_real_, type = "G", nextup = year + 4,
      party_formal = party, name_snyder = if_else(party == "D", "SMITH, JANE", "JONES, JOHN"),
      inc = 0, runoff = NA_real_, vote_g = 50, totalvotes = 100, w_g = NA_real_
    )
  raw <- bind_rows(raw_house, raw_presidents)
  bad_raw <- raw |>
    mutate(party = if_else(office == "H", "INVALID", party))
  input <- file.path(workspace, "data", "intermediate", "candidates_1990-2024.rds")
  output <- file.path(workspace, "release", "candidates_1990-2024.dta")
  contest_output <- file.path(workspace, "release", "candidates_1990-2024-by-contest.dta")
  write_rds(bad_raw, input)
  write_lines("Existing release must survive failed validation", output)
  write_lines("Existing contest release must survive failed validation", contest_output)

  withr::with_dir(workspace, {
    expect_error(
      source(file.path(project_dir, "05_release-candidates.R"), local = new.env()),
      class = "cage_validation_error"
    )
  })
  expect_equal(read_lines(output), "Existing release must survive failed validation")
  expect_equal(read_lines(contest_output), "Existing contest release must survive failed validation")

  bad_presidents <- raw |>
    filter(!(office == "P" & year == 2024 & state == "DC" & party == "R"))
  write_rds(bad_presidents, input)
  withr::with_dir(workspace, {
    expect_error(
      source(file.path(project_dir, "05_release-candidates.R"), local = new.env()),
      class = "cage_presidential_validation_error"
    )
  })
  expect_equal(read_lines(output), "Existing release must survive failed validation")
  expect_equal(read_lines(contest_output), "Existing contest release must survive failed validation")

  write_rds(raw, input)
  withr::with_dir(workspace, {
    expect_no_error(
      source(file.path(project_dir, "05_release-candidates.R"), local = new.env())
    )
    expect_no_error(
      source(file.path(project_dir, "07_checks.R"), local = new.env())
    )
  })
  saved <- haven::read_dta(output) |>
    haven::zap_labels() |>
    filter(office == "H") |>
    select(all_of(names(valid_race))) |>
    mutate(across(everything(), as.vector))
  expect_equal(saved, valid_race)

  contests_labelled <- haven::read_dta(contest_output)
  expect_true(all(map_lgl(contests_labelled, \(column) !is.null(attr(column, "label")))))
  contests <- contests_labelled |>
    haven::zap_labels() |>
    mutate(across(everything(), as.vector))
  expect_named(contests, c(
    "year", "state", "office", "dist", "type", "name_snyder_D", "name_snyder_R",
    "inc_D", "inc_R", "candidatevotes_D", "candidatevotes_R", "share_D_twoparty"
  ))
  expect_equal(nrow(contests), 460)
  expect_equal(nrow(distinct(contests, year, state, office, dist, type)), 460)
  expect_equal(
    contests |> select(year, office),
    contests |> arrange(desc(year), desc(office)) |> select(year, office)
  )
  house_contest <- contests |>
    filter(office == "H")
  expect_equal(house_contest$year, 2023)
  expect_equal(house_contest$state, "AZ")
  expect_equal(house_contest$dist, 1)
  expect_equal(house_contest$type, "G")
  expect_equal(house_contest$name_snyder_D, "SMITH, JANE")
  expect_equal(house_contest$name_snyder_R, "JONES, JOHN")
  expect_equal(house_contest$inc_D, 1)
  expect_equal(house_contest$inc_R, 0)
  expect_equal(house_contest$candidatevotes_D, 60)
  expect_equal(house_contest$candidatevotes_R, 40)
  expect_equal(house_contest$share_D_twoparty, 0.6)
  presidential_contests <- contests |>
    filter(office == "P")
  expect_equal(nrow(presidential_contests), 459)
  expect_true(all(is.na(presidential_contests$dist)))
  expect_equal(presidential_contests$share_D_twoparty, rep(0.5, 459))

  bad_saved_presidents <- haven::read_dta(output) |>
    filter(!(office == "P" & year == 2024 & state == "DC" & party == "R"))
  haven::write_dta(bad_saved_presidents, output)
  withr::with_dir(workspace, {
    expect_error(
      source(file.path(project_dir, "07_checks.R"), local = new.env()),
      class = "cage_presidential_validation_error"
    )
  })
})
