# Formatting functions ----

#' write with comma and percentage point


fmt_xtab <- function(xtbl, name, long = FALSE, rowname = "year") {
  tab <- kable(xtbl,
               format = "latex",
               booktabs = TRUE,
               longtable = long,
               linesep = "",
               format.args = list(big.mark = ',')) |>
    add_header_above(header = c(rowname = 1, XXXXX = ncol(xtbl))) |>
    kable_styling(position = "center")

  tab |>
  str_replace("XXXXX", name) |>
    str_replace("rowname", rowname)
}

# Next election years ----
fmt_nextup <- function(xtbl) {
  as.data.frame(xtbl, responseName = "n") |>
    as_tibble() |>
    arrange(year, nextup) |>
    mutate(entry = glue::glue("{nextup} ({scales::comma(n)})")) |>
    summarize(nextup = str_c(entry[n > 0], collapse = ", "), .by = year) |>
    kable(
      format = "latex",
      booktabs = TRUE,
      col.names = c("year", "nextup (candidate count)"),
      align = c("l", "l"),
      linesep = ""
    ) |>
    kable_styling(position = "center")
}

wri_xtab <- function(tbl, name, dir = "guide/Tables/cands") {
  tbl |>
    str_remove("\\\\begin\\{table\\}.*\\n") |>
    str_remove("\\\\centering\\n") |>
    str_remove("\\\\caption\\{NA\\}\\n") |>
    str_remove("\\n\\\\end\\{table\\}") |>
    str_replace("\\\\begin\\{tabular\\}", "\\\\footnotesize\\\\begin{tabular}[t]") |>
    str_replace_all("longtable", "supertabular") |>
    write_lines(glue("{dir}/{name}.tex"))
}

write_numbers <- function(tbl, office_lbl, ndir = "guide/Ns") {
  tbl |>
    distinct(office, state, year, dist) |>
    nrow() |>
    format(big.mark = ",") |>
    write_lines(path(ndir, glue("{office_lbl}_dists.tex")))

  tbl |>
    distinct(office, state, year, name_snyder) |>
    nrow() |>
    format(big.mark = ",") |>
    write_lines(path(ndir, glue("{office_lbl}_cands.tex")))
}

# Import data ----
release_dir <- "release/"





# Counts -- respondents
count_rdat <- function(tbl, name) {
  tbl |>
  mutate(year = factor(as.character(year), levels = as.character(seq(2006, 2018, 2)))) |>
  group_by(year, .drop = FALSE) |>
  summarize(n_respondents = n_distinct(case_id),
            n_nonmissing = sum(name_snyder != ""),
            n_rows = n(),
            .groups = "drop") |>
  kable(format = "latex", booktabs = TRUE,
        format.args = list(big.mark = ","),
        col.names = c("Year", "Respondents", "Non-Missing", "Total"),
        linesep = "") |>
    add_header_above(c(" " = 2, "\\\\shortstack{Respondent $\\\\times$\\\\\\\\ Office $\\\\times$ Candidate Pairs}" = 2),
                     escape = FALSE) |>
    wri_xtab(name)
}

