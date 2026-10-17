# Written by Codex

# Senate election calendar ----
scheduled_senate_class <- function(year) {
  case_when(
    year %% 6 == 2 ~ 1,
    year %% 6 == 4 ~ 2,
    year %% 6 == 0 ~ 3
  )
}

# Check reports ----
report_validation_check <- function(rows, label) {
  if (nrow(rows) == 0) {
    cli::cli_alert_success("{label}: passed")
  } else {
    cli::cli_alert_danger("{label}: failed")
    print(rows, n = Inf, width = Inf)
  }
}

# Candidate validation ----
# Used by 05 before saving and by 07 to check the saved release.
check_candidates <- function(cand_raw) {
  cand <- cand_raw |>
    haven::zap_labels() |>
    filter(is.na(office) | office != "P") |>
    mutate(
      # NC-09's 2018 election was not certified; 03 deliberately leaves won missing.
      uncertified = coalesce(
        year == 2018 & state == "NC" & office == "H" & dist == 9 & type == "G",
        FALSE
      )
    )

  if (nrow(cand) == 0) {
    cli::cli_abort(
      "No non-presidential candidate rows to validate.",
      class = "cage_validation_error"
    )
  }

  races <- cand |>
    summarize(
      winners = sum(won, na.rm = TRUE),
      total = sum(candidatevotes, na.rm = TRUE),
      top = max(candidatevotes, -Inf, na.rm = TRUE),
      winner_votes = max(candidatevotes[won %in% 1], -Inf, na.rm = TRUE),
      expected_winners = if_else(all(uncertified), 0, 1),
      .by = c(year, state, office, dist, type)
    )

  # Errors that stop the pipeline ----
  missing_keys <- cand |>
    filter(
      is.na(year) | is.na(state) | state == "" |
        !office %in% c("H", "S", "G") | !type %in% c("G", "S") |
        is.na(name_snyder) | str_trim(name_snyder) == "" |
        (office %in% c("H", "S") & is.na(dist))
    )

  duplicates <- cand |>
    filter(n() > 1, .by = c(year, state, office, dist, type, name_snyder))

  invalid_outcomes <- cand |>
    filter(case_when(
      uncertified ~ !is.na(won),
      .default = !won %in% c(0, 1)
    ))

  winner_counts <- races |>
    filter(winners != expected_winners)

  winner_votes <- races |>
    filter(is.finite(winner_votes), winner_votes < top)

  senate_classes <- races |>
    filter(office == "S", type == "G") |>
    mutate(expected_class = scheduled_senate_class(year)) |>
    filter(is.na(expected_class) | is.na(dist) | dist != expected_class) |>
    select(year, state, dist, expected_class, winners)

  invalid_votes <- cand |>
    filter(
      candidatevotes < 0 | totalvotes < 0 | candidatevotes > totalvotes |
        (!is.na(candidatevotes) & !is.finite(candidatevotes)) |
        (!is.na(totalvotes) & !is.finite(totalvotes))
    )

  inconsistent_totals <- cand |>
    filter(
      (!is.na(candidatevotes) & is.na(totalvotes)) |
        totalvotes != sum(candidatevotes, na.rm = TRUE),
      .by = c(year, state, office, dist, type)
    )

  invalid_parties <- cand |>
    filter(!party %in% c("D", "R", "I", "Lbt", "Grn", "Other", "NPA"))

  inconsistent_parties <- cand |>
    mutate(first_formal = str_trim(str_split_i(party_formal, ",", 1))) |>
    filter(case_when(
      party == "D" ~ first_formal == "R",
      party == "R" ~ first_formal %in% c("D", "DFL"),
      party == "Other" ~ first_formal %in% c("D", "R", "Grn", "Lbt", "I"),
      .default = FALSE
    )) |>
    select(year, state, office, name_snyder, party, party_formal)

  # National general election winners ----
  # Class 1: 2000, 2006, ...; class 2: 2002, 2008, ...; class 3: 1998, 2004, ...
  # Classes 1 and 2 have 33 seats; class 3 has 34. See senate.gov/senators/Class_I.htm
  # and senate.gov/about/origins-foundations/senate-and-constitution/senators-receive-class-assignments.htm.
  expected_counts <- cand |>
    distinct(year) |>
    filter(year %% 2 == 0) |>
    crossing(office = c("H", "S")) |>
    mutate(
      senate_class = if_else(office == "S", scheduled_senate_class(year), NA_real_),
      expected = case_when(
        # Handcoded exception: NC-09's 2018 election had no certified winner.
        office == "H" & year == 2018 ~ 434,
        office == "H" ~ 435,
        senate_class == 3 ~ 34,
        .default = 33
      )
    )

  observed_counts <- races |>
    filter(year %% 2 == 0, type == "G", office %in% c("H", "S")) |>
    summarize(winners = sum(winners), .by = c(year, office))

  national_counts <- expected_counts |>
    left_join(observed_counts, by = c("year", "office"), relationship = "one-to-one") |>
    mutate(winners = coalesce(winners, 0))

  house_counts <- national_counts |>
    filter(office == "H", winners != expected) |>
    select(year, office, winners, expected)

  senate_counts <- national_counts |>
    filter(office == "S", winners != expected)

  checks <- list(
    "Missing or invalid candidate identifiers" = missing_keys,
    "Duplicate candidates within a race" = duplicates,
    "Invalid or missing winner indicators" = invalid_outcomes,
    "Races with an unexpected number of winners" = winner_counts,
    "Winners with fewer votes than another candidate" = winner_votes,
    "Invalid vote counts" = invalid_votes,
    "Race totals inconsistent with candidate votes" = inconsistent_totals,
    "Invalid party codes" = invalid_parties,
    "Inconsistent short and formal parties" = inconsistent_parties,
    "House general election winners (435; 434 in 2018)" = house_counts,
    "Senate general election race classes" = senate_classes,
    "Senate general election winners (33 or 34 by class)" = senate_counts
  )

  # Advisory checks ----
  governor_counts <- races |>
    filter(year %% 2 == 0, type == "G", office == "G") |>
    summarize(winners = sum(winners), .by = year) |>
    mutate(
      minimum = if_else(year %% 4 == 2, 36, 11),
      maximum = if_else(year %% 4 == 2, 36, 12)
    ) |>
    filter(winners < minimum | winners > maximum)

  statewide <- races |>
    filter(office %in% c("S", "G"), type == "G") |>
    summarize(statewide = max(total, -Inf), .by = c(year, state))

  unusual_turnout <- races |>
    mutate(
      state_median = median(total[type == "G" & total > 0]),
      .by = c(year, state, office)
    ) |>
    left_join(statewide, by = c("year", "state"), relationship = "many-to-one") |>
    filter(case_when(
      office == "H" ~ total > 2.5 * state_median,
      office %in% c("S", "G") ~ total > 1.5 * statewide,
      .default = FALSE
    ))

  snyder_pat <- "^[A-Z][A-Z'. -]*[A-Z.], [A-Z][A-Z'. -]*?( \\([A-Z0-9'. -]+\\))?(, (JR\\.|SR\\.|II|III|IV|V|VI))?$"
  unusual_names <- cand |>
    filter(!str_detect(name_snyder, snyder_pat), name_snyder != "[NONE OF THESE]") |>
    distinct(state, name_snyder)

  advisory_checks <- list(
    "Governor winner counts outside the usual range" = governor_counts,
    "Unusually high turnout" = unusual_turnout,
    "Names outside the usual Snyder format" = unusual_names
  )

  # Report all findings before stopping ----
  cli::cli_h2("Enforced checks (presidents excluded)")
  iwalk(checks, report_validation_check)
  cli::cli_h2("Advisory checks")
  iwalk(advisory_checks, report_validation_check)

  advisories <- advisory_checks |>
    keep(\(rows) nrow(rows) > 0)
  if (length(advisories) > 0) {
    cli::cli_warn(
      "Review the {length(advisories)} advisory check{?s} printed above.",
      class = "cage_validation_warning"
    )
  }

  errors <- checks |>
    keep(\(rows) nrow(rows) > 0)
  if (length(errors) > 0) {
    cli::cli_abort(
      c(
        "Candidate validation failed.",
        "x" = "{length(errors)} enforced check{?s} failed; see the rows printed above."
      ),
      class = "cage_validation_error",
      checks = errors
    )
  }

  cli::cli_alert_success("All enforced candidate checks passed (presidents excluded).")
  invisible(list(checks = checks, advisories = advisories))
}

# Presidential validation ----
check_presidents <- function(cand_raw, years = seq(1992, 2024, by = 4)) {
  pres <- cand_raw |>
    haven::zap_labels() |>
    filter(office == "P")

  major_party <- pres |>
    filter(party %in% c("D", "R"))

  expected_counts <- crossing(
    year = years,
    state = c(state.abb, "DC"),
    party = c("D", "R")
  ) |>
    mutate(expected = 1L)

  observed_counts <- major_party |>
    count(year, state, party, name = "observed")

  coverage <- expected_counts |>
    full_join(
      observed_counts,
      by = c("year", "state", "party"),
      relationship = "one-to-one"
    ) |>
    filter(is.na(expected) | coalesce(observed, 0L) != expected)

  nominees <- major_party |>
    summarize(
      nominees = n_distinct(name_snyder),
      missing_names = any(is.na(name_snyder) | str_trim(name_snyder) == ""),
      names = str_c(unique(name_snyder), collapse = "; "),
      .by = c(year, party)
    ) |>
    filter(nominees != 1 | missing_names)

  winner_flags <- pres |>
    filter(!is.na(won)) |>
    select(year, state, party, name_snyder, won)

  checks <- list(
    "D/R presidential coverage in every state and DC" = coverage,
    "D/R presidential nominees consistent across states" = nominees,
    "Presidential winner flags all missing" = winner_flags
  )

  cli::cli_h2("Presidential checks")
  iwalk(checks, report_validation_check)

  errors <- checks |>
    keep(\(rows) nrow(rows) > 0)
  if (length(errors) > 0) {
    cli::cli_abort(
      c(
        "Presidential validation failed.",
        "x" = "{length(errors)} enforced check{?s} failed; see the rows printed above."
      ),
      class = "cage_presidential_validation_error",
      checks = errors
    )
  }

  invisible(list(checks = checks))
}
