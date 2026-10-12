library(tidyverse)
library(tidylog)

# add rows from `additions` that don't already exist in `data` (claude)
add_rows_if_new <- function(data, additions, by = c("state", "year", "office", "dist", "name_snyder")) {
  bind_rows(data, anti_join(additions, data, by = by))
}

# Data --
jsdat_raw <- readRDS("data/intermediate/prelim/candidates_party-recoded.rds")

# Rename variables to match expected names (if needed)
if ("candidatevotes" %in% names(jsdat_raw)) {
  jsdat_raw <- jsdat_raw |> rename(vote_g = candidatevotes)
}
if ("won" %in% names(jsdat_raw)) {
  jsdat_raw <- jsdat_raw |> rename(w_g = won)
}
if ("name" %in% names(jsdat_raw)) {
  jsdat_raw <- jsdat_raw |> rename(name_snyder = name)
}

# Remove u_g and vote_g_share columns if they exist
jsdat_raw <- jsdat_raw |> select(-any_of(c("u_g", "vote_g_share")))

jsdat <- jsdat_raw


# MANUAL FIXES =====

## Adding Georgia 2020/2021 elections ----

jsdat <- jsdat |>
  filter(!(state == "GA" & year %in% 2020:2022 & office == "S"))

ga_additions <- tibble::tribble(
  ~state, ~year, ~office, ~dist, ~type, ~nextup, ~party, ~party_formal, ~name_snyder, ~vote_g, ~w_g, ~inc,
  "GA", 2020, "S", 3, "S", 2022, "D", "D", "WARNOCK, RAPHAEL GAMALIEL", 2289113, 1, 0,
  "GA", 2020, "S", 3, "S", 2022, "R", "R", "LOEFFLER, KELLY", 2195841, 0, 3,
  "GA", 2020, "S", 2, "G", 2026, "D", "D", "OSSOFF, THOMAS JONATHAN (JON)", 2269923, 1, 0,
  "GA", 2020, "S", 2, "G", 2026, "R", "R", "PERDUE, DAVID A.", 2214979, 0, 1,
  "GA", 2022, "S", 3, "G", 2028, "R", "R", "WALKER, HERSCHEL JUNIOR", 1721244, 0, 0,
  "GA", 2022, "S", 3, "G", 2028, "D", "D", "WARNOCK, RAPHAEL GAMALIEL", 1820633, 1, 2,
)

jsdat <- add_rows_if_new(jsdat, ga_additions)

## Runoff elections ----

# Removing extraneous candidates
## Georgia data comes from https://sos.ga.gov/index.php/Elections/current_and_past_elections_results
## Louisiana data comes from https://voterportal.sos.la.gov/graphical
jsdat <- jsdat |>
  mutate(
    temp = 0, # Temporary var that ids non-runoff candidates
    temp = replace(temp, state == "LA" & year == 2020 & dist == 5, 1),
    temp = replace(temp, name_snyder == "LETLOW, LUKE J." & year == 2020 |
                     name_snyder == "HARRIS, LANCE" & year == 2020, 0)
  ) |>
  filter(temp == 0,
         name_snyder != "BUCKLEY, ALLEN" | year != 2008) |>
  select(-temp) |>
  tidylog::mutate(runoff = case_when(
    state == "GA" & year == 2020 & office == "S" ~ 1, # 2022 Georgia Runoff
    state == "GA" & year == 2022 & office == "S" ~ 1, # 2022 Georgia Runoff
    state == "GA" & year == 2007 & office == "H" & dist == 10 ~ 1,
    state == "GA" & year == 2008 & office == "S" ~ 1,
    state == "GA" & year == 2010 & dist == 9 & type == "S" ~ 1,
    state == "GA" & year == 2017 & dist == 6 & type == "S" ~ 1,
    state == "LA" & year == 2020 & dist == 5 & type == "G" ~ 1,
    state == "LA" & year == 2016 & office == "S" ~ 1,
    state == "LA" & year == 2016 & dist == 3 & type == "G" ~ 1,
    state == "LA" & year == 2014 & office == "S" ~ 1,
    state == "LA" & year == 2014 & office == "H" & dist == 5 ~ 1,
    state == "LA" & year == 2014 & office == "H" & dist == 6 ~ 1,
    state == "LA" & year == 2013 & office == "H" & dist == 5 ~ 1,
    state == "LA" & year == 2012 & office == "H" & dist == 3 ~ 1,
    state == "LA" & year == 2006 & office == "H" & dist == 2 ~ 1,
    state == "LA" & year == 2016 & dist == 4 & type == "G" ~ 1,
    TRUE ~ 0
  )) |>
  mutate(runoff = case_when(
    state == "GA" | state == "LA" ~ runoff,
    TRUE ~ NA_real_
  ))

# Other runoffs
jsdat <- jsdat |>
  tidylog::mutate(vote_g = case_when(
    year == 2008 & state == "GA" & name_snyder == "MARTIN, JAMES FRANCIS (JIM)" ~ 909923,
    year == 2008 & state == "GA" & name_snyder == "CHAMBLISS, C. SAXBY" ~ 1228033,
    year == 2020 & state == "LA" & name_snyder == "LETLOW, LUKE J." ~ 49183,
    year == 2020 & state == "LA" & name_snyder == "HARRIS, LANCE" ~ 30124,
    TRUE ~ vote_g
  ))


# VARIABLE CORRECTIONS =====

# Other vote total additions
# Note: LA House vote_g fixes for 2006 (dists 1,3,4,5,6,7), 2012 (dists 1,2,4,5,6),
# 2014 (dists 1,2,3,4), 2016 (dists 1,2,6) are omitted here because those rows
# are deleted and re-added with correct values in the LA House section below.
jsdat <- jsdat |>
  tidylog::mutate(vote_g = replace(vote_g, name_snyder == "JINDAL, BOBBY" & office == "G" & year == 2007 & state == "LA", 699275),
                  vote_g = replace(vote_g, name_snyder == "GAIERO, THEODORE J., JR." & office == "H" & year == 2008 & state == "MA", 114),
                  vote_g = replace(vote_g, name_snyder == "SPEIER, KAREN (JACKIE)" & type == "S" & year == 2008 & state == "CA", 66279),
                  vote_g = replace(vote_g, name_snyder == "DJOU, CHARLES KONG" & type == "S" & year == 2010 & state == "HI", 67610),
                  vote_g = replace(vote_g, name_snyder == "PAYNE, DONALD M. (DON), JR." & type == "S" & year == 2012 & state == "NJ", 166413),
                  vote_g = replace(vote_g, name_snyder == "ABRAHAM, RALPH LEE" & type == "G" & year == 2016 & state == "LA", 208345),
                  vote_g = replace(vote_g, name_snyder == "MURPHY, GREGORY F. (GREG)" & type == "S" & year == 2019 & state == "NC", 70407),
                  vote_g = replace(vote_g, name_snyder == "BISHOP, DAN" & type == "S" & year == 2019 & state == "NC", 96573),
                  vote_g = replace(vote_g, name_snyder == "KELLER, FREDERICK B. (FRED)" & type == "S" & year == 2019 & state == "PA", 90000),
                  vote_g = replace(vote_g, name_snyder == "LETLOW, JULIA" & type == "S" & year == 2021 & state == "LA", 67203),
                  vote_g = replace(vote_g, name_snyder == "ROSSANO, TIMOTHY" & year == 2014 & state == "FL", 12),
                  vote_g = replace(vote_g, name_snyder == "JINDAL, BOBBY" & office == "G" & year == 2011 & state == "LA", 673239),
                  vote_g = replace(vote_g, name_snyder == "FLORES, MAYRA" & year == 2022 & type == "S", 14799)
  )


## Winner (w_g) corrections ----

# Fix Oregon 2018 Gov winner
jsdat <- jsdat |>
  tidylog::mutate(w_g = replace(w_g, year == 2018 & state == "OR" & office == "G" & name_snyder == "BUEHLER, KNUTE", 0))

# Fix Rhode Island 2006 Gov results
jsdat <- jsdat |>
  tidylog::mutate(w_g = replace(w_g, year == 2006 & state == "RI" & office == "G" & name_snyder == "CARCIERI, DONALD L.", 1),
                  w_g = replace(w_g, year == 2006 & state == "RI" & office == "G" & name_snyder == "FOGARTY, CHARLES J.", 0),
                  vote_g = replace(vote_g, year == 2006 & state == "RI" & office == "G" & name_snyder == "CARCIERI, DONALD L.", 197306),
                  vote_g = replace(vote_g, year == 2006 & state == "RI" & office == "G" & name_snyder == "FOGARTY, CHARLES J.", 189503)
  )

# Karin Housley w_g fix
jsdat <- jsdat |>
  tidylog::mutate(w_g = replace(w_g, year == 2018 & state == "MN" & name_snyder == "HOUSLEY, KARIN", 0))

# NC-09 election fraud case
jsdat <- jsdat |>
  tidylog::mutate(w_g = replace(w_g, office == "H" & year == 2018 & state == "NC" & dist == 9 & type == "G", NA))

# Fix IN-02 Walorski result
jsdat <- jsdat |>
  tidylog::mutate(w_g = replace(w_g, name_snyder == "STEURY, PAUL D." & office == "H" & year == 2022, 0))

# TX-23 special election runoff
jsdat <- jsdat |>
  tidylog::filter(
    !(year == 2006 & state == "TX" & office == "H" & dist == 23 & vote_g < 24594)
  ) |>
  tidylog::mutate(type = replace(type, year == 2006 & state == "TX" & dist == 23, "G"),
                  vote_g = replace(vote_g, year == 2006 & state == "TX" & dist == 23 & party == "R", 32217),
                  vote_g = replace(vote_g, year == 2006 & state == "TX" & dist == 23 & party == "D", 38256),
                  w_g = replace(w_g, year == 2006 & state == "TX" & dist == 23 & party == "R", 0),
                  w_g = replace(w_g, year == 2006 & state == "TX" & dist == 23 & party == "D", 1))


## Removing candidates ----

# Removing duplicates or not-rans
jsdat <- jsdat |>
  # Removing Ellen Brickley
  tidylog::filter(name_snyder != "BRICKLEY, ELLEN") |>
  # duplicate entry with VAN DUYNE
  tidylog::filter(!(office == "H" & state == "TX" & dist == 24 & year == 2022 & name_snyder == "VANDUYNE, BETH")) |>
  # rest were primary losses, not in general
  tidylog::filter(!(office == "H" & state == "FL" & dist == 5 & year == 2022 & name_snyder != "RUTHERFORD, JOHN H.")) |>
  # no votes
  tidylog::filter(!(office == "H" & state == "CT" & dist == 4 & year == 2022 & name_snyder == "GOLDSTEIN, MICHAEL TED"))

# Removing entries without names
jsdat <- jsdat |>
  filter(name_snyder != "" | !is.na(vote_g))


## Party corrections ----

jsdat <- jsdat |>
  # writein
  tidylog::mutate(party = replace(party, name_snyder == "SMITH, DELLA JEAN (DJ)" & year == 2016, "Other")) |>
  tidylog::mutate(party = replace(party, name_snyder == "RAMSBURG, KAREN LYNN" & year == 2012, "D")) |>
  tidylog::mutate(party = replace(party, name_snyder != "VAN HOLLEN, CHRISTOPHER (CHRIS), JR." & year == 2016 & office == "S" & party == "D" & state == "MD", "Other"))


## Incumbency fixes ----

jsdat <- jsdat |>
  tidylog::mutate(
    inc = replace(inc, office == "S" & state == "MO" & year == 2016 & party %in% c("Grn", "I"), 0),
    inc = replace(inc, office == "S" & state == "NV" & year == 2016 & party %in% c(""), 0),
    inc = replace(inc, office == "S" & state == "SD" & year == 2014, 0),
    inc = replace(inc, office == "S" & state == "NJ" & year == 2013, 0),
    inc = replace(inc, office == "S" & state == "MA" & year == 2013 & party == "12 Visions Pty", 0),
    inc = replace(inc, office == "H" & state == "LA" & year == 2021 & name_snyder == "LETLOW, JULIA", 0),
    inc = replace(inc, office == "H" & state == "KS" & year == 2017 & name_snyder == "ROCKHOLD, CHRIS", 0),
    inc = replace(inc, office %in% c("H", "S") & year %% 2 == 0 & party %in% c("Grn", "Lbt", "W-I", "", "US Taxpayers"), 0)
  )


## Name recodings ----

jsdat <- jsdat |>
  tidylog::mutate(name_snyder = str_replace(name_snyder, "Ê", " "))


## Manually fixing TX-22 2006 ----

jsdat <- jsdat |>
  tidylog::filter(
    !(state == "TX" & year == 2006 & office == "H" & dist == 22)
  )

# TX-22 2006 general election
tx22_additions <- tibble::tribble(
  ~state, ~year, ~office, ~dist, ~type, ~nextup, ~party, ~party_formal, ~name_snyder, ~w_g, ~inc, ~vote_g,
  "TX", 2006, "H", 22, "G", 2008, "D", "D", "LAMPSON, NICHOLAS V. (NICK)", 1, 0, 76775,
  "TX", 2006, "H", 22, "G", 2008, "Lbt", "Lbt", "SMITHER, M. BOB", 0, 0, 9009,
  "TX", 2006, "H", 22, "G", 2008, "Other", "W-I", "SEKULA GIBBS, SHELLEY A.", 0, 0, 61938,
  "TX", 2006, "H", 22, "G", 2008, "Other", "W-I", "RICHARDSON, DONALD LUTHER (DON)", 0, 0, 428,
  "TX", 2006, "H", 22, "G", 2008, "Other", "W-I", "REASBECK, JOE", 0, 0, 89
)


jsdat <- add_rows_if_new(jsdat, tx22_additions, by = c("state", "year", "office", "dist", "name_snyder", "type"))

## McMasters, Thomas (Tom) Manual Edit ----

jsdat <- jsdat |>
  tidylog::mutate(
    party = replace(party, name_snyder == "MCMASTERS, THOMAS (TOM)", "I"),
    party_formal = replace(party_formal, name_snyder == "MCMASTERS, THOMAS (TOM)", "W-I")
  )

## Fixing nextup for Arizona races ----

jsdat <- jsdat |>
  tidylog::mutate(
    nextup = replace(nextup, state == "AZ" & year == 2020 & office == "S" & type == "S", 2022)
  )

## Dist fixes found by Claude
jsdat <- jsdat |>
  mutate(
    dist = replace(dist, state == "MT" & year == 2017 & type == "S", 1),
    party = replace(party, year == 2018 & name_snyder == "CROWLEY, JOSEPH (JOE)", "Other"),
    party_formal = replace(party_formal, year == 2018 & name_snyder == "CROWLEY, JOSEPH (JOE)", "Wk Fam, Women's Equality"),
    vote_g = replace(vote_g, year == 2018 & name_snyder == "CROWLEY, JOSEPH (JOE)", 9348),
  ) |>
  add_row(
    state = "NY", year = 2022, office = "H", dist = 19, type = "S", nextup = 2022, party = "D",
    party_formal = "D, Wk Fam", name_snyder = "RYAN, PATRICK (PAT)", inc = 0, vote_g = 66088, w_g = 1,
  ) |>
  filter(!(name_snyder == "CROWLEY, JOSEPH (JOE)" & year == 2018))

# INDEPENDENT SENATORS =====

## Adding Angus King races ----

king_additions <- tibble::tribble(
  ~state, ~year, ~office, ~dist, ~type, ~nextup, ~party, ~party_formal, ~name_snyder, ~inc, ~vote_g, ~w_g,
  # 2018 Senate election
  "ME", 2018, "S", 1, "G", 2024, "I", "I", "KING, ANGUS S., JR.", 0, 370580, 1,
  "ME", 2018, "S", 1, "G", 2024, "R", "R", "SUMNERS, CHARLES E., JR.", 0, 215399, 0,
  "ME", 2018, "S", 1, "G", 2024, "D", "D", "DILL, CYNTHIA ANN", 0, 92900, 0,
  "ME", 2018, "S", 1, "G", 2024, "I", "I", "WOODS, STEPHEN M.", 0, 10289, 0,
  "ME", 2018, "S", 1, "G", 2024, "Other", "Nonparty", "DALTON, DANNY FRANCIS", 0, 5624, 0,
  "ME", 2018, "S", 1, "G", 2024, "Other", "Independent for Liberty", "DODGE, ANDREW IAN", 0, 5624, 0,
  # 2012 Senate election
  "ME", 2012, "S", 1, "G", 2018, "I", "I", "KING, ANGUS S., JR.", 1, 344575, 1,
  "ME", 2012, "S", 1, "G", 2018, "R", "R", "BRAKEY, ERIC L.", 0, 223502, 0,
  "ME", 2012, "S", 1, "G", 2018, "D", "D", "RINGELSTEIN, ZAK", 0, 66268, 0
)

jsdat <- add_rows_if_new(jsdat, king_additions)

## Adding Joe Lieberman races ----

lieberman_additions <- tibble::tribble(
  ~state, ~year, ~office, ~dist, ~type, ~nextup, ~party, ~party_formal, ~name_snyder, ~inc, ~vote_g, ~w_g,
  # 2006 Senate election
  "CT", 2006, "S", 1, "G", 2012, "I", "Connecticut for Lieberman", "LIEBERMAN, JOSEPH I.", 1, 564095, 1,
  "CT", 2006, "S", 1, "G", 2012, "D", "D", "LAMONT, NED", 0, 450844, 0,
  "CT", 2006, "S", 1, "G", 2012, "R", "R", "SCHLESINGER, ALAN", 0, 109198, 0,
  "CT", 2006, "S", 1, "G", 2012, "Grn", "Green", "FERRUCCI, RALPH A.", 0, 5922, 0,
  "CT", 2006, "S", 1, "G", 2012, "Other", "Concerned Citizens", "KNIBBS, TIMOTHY A.", 0, 4638, 0,
  "CT", 2006, "S", 1, "G", 2012, "Other", "Write-in", "VASSAR, CARL E.", 0, 80, 0,
)

jsdat <- add_rows_if_new(jsdat, lieberman_additions)


## Adding Bernard Sanders Senate elections ----

# Vermont Senate races for Bernard Sanders (2018, 2012, 2006)
sanders_additions <- tibble::tribble(
  ~state, ~year, ~office, ~dist, ~type, ~nextup, ~party, ~party_formal, ~name_snyder, ~inc, ~vote_g, ~w_g,
  # 2018 Senate election
  "VT", 2018, "S", 1, "G", 2024, "I", "I", "SANDERS, BERNARD (BERNIE)", 1, 183649, 1,
  "VT", 2018, "S", 1, "G", 2024, "R", "R", "ZUPAN, LAWRENCE", 0, 74815, 0,
  "VT", 2018, "S", 1, "G", 2024, "I", "I", "PEACOCK, BRAD J.", 0, 3655, 0,
  "VT", 2018, "S", 1, "G", 2024, "I", "I", "BESTE, RUSSELL", 0, 2763, 0,
  "VT", 2018, "S", 1, "G", 2024, "I", "I", "GILBERT, EDWARD S., JR.", 0, 2244, 0,
  "VT", 2018, "S", 1, "G", 2024, "I", "I", "ADELUOLA, FOLASADE", 0, 1979, 0,
  "VT", 2018, "S", 1, "G", 2024, "Other", "Liberty Union", "KANE, REID", 0, 1171, 0,
  "VT", 2018, "S", 1, "G", 2024, "I", "I", "SVITAVSKY, JON", 0, 1130, 0,
  "VT", 2018, "S", 1, "G", 2024, "I", "I", "BUSA, BRUCE", 0, 914, 0,
  # 2012 Senate election
  "VT", 2012, "S", 1, "G", 2018, "I", "I", "SANDERS, BERNARD (BERNIE)", 1, 207848, 1,
  "VT", 2012, "S", 1, "G", 2018, "R", "R", "MACGOVERN, JOHN", 0, 72898, 0,
  "VT", 2012, "S", 1, "G", 2018, "Other", "Liberty Union", "DIAMONDSTONE, PETER", 0, 2511, 0,
  "VT", 2012, "S", 1, "G", 2018, "Other", "Peace and Prosperity", "MOSS, PETER", 0, 2452, 0,
  "VT", 2012, "S", 1, "G", 2018, "Other", "United States Marijuana", "ERICSON, CHRIS", 0, 5924, 0,
  "VT", 2012, "S", 1, "G", 2018, "Other", "VoteKISS", "Laframboise", 0, 877, 0,
  # 2006 Senate election
  "VT", 2006, "S", 1, "G", 2012, "I", "I", "SANDERS, BERNARD (BERNIE)", 0, 171638, 1,
  "VT", 2006, "S", 1, "G", 2012, "R", "R", "TARRANT, RICHARD", 0, 84924, 0,
  "VT", 2006, "S", 1, "G", 2012, "I", "I", "ERICSON, CRIS", 0, 1735, 0,
  "VT", 2006, "S", 1, "G", 2012, "Grn", "Vermont Green", "HILL, CRAIG", 0, 1536, 0,
  "VT", 2006, "S", 1, "G", 2012, "Other", "Liberty Union", "DIAMONDSTONE, PETER", 0, 801, 0,
  "VT" , 2006, "S", 1, "G", 2012, "Other", "Anti-Bush", "MOSS, PETER", 0, 1518, 0
)

jsdat <- add_rows_if_new(jsdat, sanders_additions)


# STATE-SPECIFIC FIXES =====

## Adding Bill Cassidy 2020 race ----

jsdat <- jsdat |>
  filter(!(state == "LA" & year == 2020 & office == "S"))

louisiana_senate_2020 <- tibble::tribble(
  ~state, ~year, ~office, ~dist, ~type, ~nextup, ~party, ~party_formal, ~name_snyder, ~inc, ~vote_g, ~w_g,
  # 2020 Senate election
  "LA", 2020, "S", 1, "G", 2026, "R", "R", "CASSIDY, WILLIAM (BILL)", 1, 1228908, 1,
  "LA", 2020, "S", 1, "G", 2026, "D", "D", "PERKINS, ADRIAN", 0, 394049, 0,
  "LA", 2020, "S", 1, "G", 2026, "D", "D", "EDWARDS, DERRICK (CHAMP)", 0, 229814, 0,
  "LA", 2020, "S", 1, "G", 2026, "D", "D", "PIERCE, ANTOINE", 0, 55710, 0,
  "LA", 2020, "S", 1, "G", 2026, "R", "R", "MURPHY, DUSTIN", 0, 38383, 0,
  "LA", 2020, "S", 1, "G", 2026, "D", "D", "KNIGHT, DAVID DREW", 0, 36962, 0,
  "LA", 2020, "S", 1, "G", 2026, "I", "I", "BILLIOT, BERYL", 0, 17362, 0,
  "LA", 2020, "S", 1, "G", 2026, "I", "I", "BOURGEOIS, JOHN PAUL", 0, 16518, 0,
  "LA", 2020, "S", 1, "G", 2026, "D", "D", "WENSTRUP, PETER", 0, 14454, 0,
  "LA", 2020, "S", 1, "G", 2026, "L", "L", "SIGLER, AARON C.", 0, 11321, 0,
  "LA", 2020, "S", 1, "G", 2026, "I", "I", "MENDOZA, MANILLO V. (VINNY)", 0, 7811, 0,
  "LA", 2020, "S", 1, "G", 2026, "Other", "Other", "PRICE, MELINDA MARY", 0, 7680, 0,
  "LA", 2020, "S", 1, "G", 2026, "I", "I", "MONTGOMERY, JAMAR", 0, 5804, 0,
  "LA", 2020, "S", 1, "G", 2026, "I", "I", "DARET, RENO JEAN, III", 0, 3954, 0,
  "LA", 2020, "S", 1, "G", 2026, "Other", "Other", "JOHN, XAN", 0, 2813, 0
)


jsdat <- add_rows_if_new(jsdat, louisiana_senate_2020)

## Fixing battery of Louisiana issues ----

# Remove existing LA House entries for the years/districts we're adding
jsdat <- jsdat |>
  filter(!(state == "LA" & office == "H" &
             ((year == 2006 & dist %in% c(1, 3, 4, 5, 6, 7)) |
                (year == 2008 & dist == 7) |
                (year == 2012 & dist %in% c(1, 2, 4, 5, 6)) |
                (year == 2014 & dist %in% c(1, 2, 3, 4)) |
                (year == 2016 & dist %in% c(1, 2, 6)) |
                (year == 2020 & dist %in% c(1, 2, 3, 4, 6)))))

la_additions <- tibble::tribble(
  ~state, ~year, ~office, ~dist, ~type, ~nextup, ~party, ~party_formal, ~name_snyder, ~inc, ~vote_g, ~w_g,
  # LA-01 House 2006
  "LA", 2006, "H", 1, "G", 2008, "D", "D", "GEREIGHTY, DAVID", 0, 10919, 0,
  "LA", 2006, "H", 1, "G", 2008, "D", "D", "TALLITSCH, STACEY", 0, 5025, 0,
  "LA", 2006, "H", 1, "G", 2008, "R", "R", "JINDAL, BOBBY", 0, 130508, 1,
  "LA", 2006, "H", 1, "G", 2008, "Lbt", "Lbt", "BEARY, PETER", 0, 1676, 0,
  # LA-03 House 2006
  "LA", 2006, "H", 3, "G", 2008, "D", "D", "BREECH, OLANGEE (OJ)", 0, 4190, 0,
  "LA", 2006, "H", 3, "G", 2008, "D", "D", "MELANCON, CHARLES J. (CHARLIE), JR.", 0, 75023, 1,
  "LA", 2006, "H", 3, "G", 2008, "R", "R", "ROMERO, CRAIG", 0, 54950, 0,
  "LA", 2006, "H", 3, "G", 2008, "Lbt", "Lbt", "BLAKE, JAMES LEE, JR.", 0, 2168, 0,
  # LA-04 House 2006
  "LA", 2006, "H", 4, "G", 2008, "D", "D", "CASH, ARTIS R., SR.", 0, 22757, 0,
  "LA", 2006, "H", 4, "G", 2008, "D", "D", "COX, PATTI", 0, 17788, 0,
  "LA", 2006, "H", 4, "G", 2008, "R", "R", "KELLEY, CHESTER T.", 0, 16649, 0,
  "LA", 2006, "H", 4, "G", 2008, "R", "R", "MCCRERY, JAMES O. (JIM)", 0, 77078, 1,
  # LA-05 House 2006
  "LA", 2006, "H", 5, "G", 2008, "D", "D", "HEARN, GLORIA WILLIAMS", 0, 33233, 0,
  "LA", 2006, "H", 5, "G", 2008, "R", "R", "ALEXANDER, RODNEY M.", 0, 78211, 1,
  "LA", 2006, "H", 5, "G", 2008, "Lbt", "Lbt", "SANDERS, BRENT", 0, 1876, 0,
  "LA", 2006, "H", 5, "G", 2008, "Other", "Other", "WATTS, JOHN", 0, 1262, 0,
  # LA-06 House 2006
  "LA", 2006, "H", 6, "G", 2008, "R", "R", "BAKER, RICHARD HUGH", 0, 94658, 1,
  "LA", 2006, "H", 6, "G", 2008, "Lbt", "Lbt", "FONTANESI, RICHARD M.", 0, 19648, 0,
  # LA-07 House 2006
  "LA", 2006, "H", 7, "G", 2008, "D", "D", "STAGG, MIKE", 0, 47133, 0,
  "LA", 2006, "H", 7, "G", 2008, "R", "R", "BOUSTANY, CHARLES W., JR.", 0, 113720, 1,
  # LA-07 House 2008
  "LA", 2008, "H", 7, "G", 2010, "D", "D", "CRAVINS, DONALD (DON), JR.", 0, 98280, 0,
  "LA", 2008, "H", 7, "G", 2010, "R", "R", "BOUSTANY, CHARLES W., JR.", 1, 177173, 1,
  "LA", 2008, "H", 7, "G", 2010, "Other", "Other", "VIDRINE, PETER", 0, 10846, 0,
  # LA-01 House 2012
  "LA", 2012, "H", 1, "G", 2014, "R", "R", "KING, GARY", 0, 24844, 0,
  "LA", 2012, "H", 1, "G", 2014, "R", "R", "SCALISE, STEPHEN J. (STEVE)", 1, 193496, 1,
  "LA", 2012, "H", 1, "G", 2014, "D", "D", "MENDOZA, MANILLO V. (VINNY)", 0, 61703, 0,
  "LA", 2012, "H", 1, "G", 2014, "I", "No Party", "TURKNETT, DAVID (TURK)", 0, 6079, 0,
  "LA", 2012, "H", 1, "G", 2014, "I", "No Party", "WELLS, ARDEN", 0, 4288, 0,
  # LA-02 House 2012
  "LA", 2012, "H", 2, "G", 2014, "R", "R", "BAILEY, DWAYNE", 0, 38801, 0,
  "LA", 2012, "H", 2, "G", 2014, "R", "R", "LAROSE, JOSUE", 0, 11345, 0,
  "LA", 2012, "H", 2, "G", 2014, "D", "D", "LANDRIEU, GARY", 0, 71916, 0,
  "LA", 2012, "H", 2, "G", 2014, "D", "D", "RICHMOND, CEDRIC L.", 1, 158501, 1,
  "LA", 2012, "H", 2, "G", 2014, "Lbt", "Lbt", "TROTTER, CALEB", 0, 6791, 0,
  # LA-04 House 2012
  "LA", 2012, "H", 4, "G", 2014, "R", "R", "FLEMING, JOHN C., JR.", 1, 187894, 1,
  "LA", 2012, "H", 4, "G", 2014, "Lbt", "Lbt", "LORD, RANDALL", 0, 61637, 0,
  # LA-05 House 2012
  "LA", 2012, "H", 5, "G", 2014, "R", "R", "ALEXANDER, RODNEY M.", 1, 202536, 1,
  "LA", 2012, "H", 5, "G", 2014, "Lbt", "Lbt", "GRANT, CLAY STEVEN", 0, 20194, 0,
  "LA", 2012, "H", 5, "G", 2014, "I", "No Party", "CEASAR, RON", 0, 37486, 0,
  # LA-06 House 2012
  "LA", 2012, "H", 6, "G", 2014, "R", "R", "CASSIDY, WILLIAM (BILL)", 1, 243553, 1,
  "LA", 2012, "H", 6, "G", 2014, "Lbt", "Lbt", "CRAIG, RUFUS HOLT, JR.", 0, 32185, 0,
  "LA", 2012, "H", 6, "G", 2014, "I", "No Party", "TORREGANO, RICHARD (RPT)", 0, 30975, 0,
  # LA-01 House 2014
  "LA", 2014, "H", 1, "G", 2016, "R", "R", "SCALISE, STEPHEN J. (STEVE)", 1, 189250, 1,
  "LA", 2014, "H", 1, "G", 2016, "D", "D", "MENDOZA, MANILLO V. (VINNY)", 0, 24761, 0,
  "LA", 2014, "H", 1, "G", 2016, "D", "D", "DUGAS, LEE ANN", 0, 21286, 0,
  "LA", 2014, "H", 1, "G", 2016, "Lbt", "Lbt", "SANFORD, JEFFRY (JEFF)", 0, 8707, 0,
  # LA-02 House 2014
  "LA", 2014, "H", 2, "G", 2016, "D", "D", "RICHMOND, CEDRIC L.", 1, 152201, 1,
  "LA", 2014, "H", 2, "G", 2016, "D", "D", "LANDRIEU, GARY", 0, 37805, 0,
  "LA", 2014, "H", 2, "G", 2016, "I", "No Party", "BROOKS, DAVID", 0, 16327, 0,
  "LA", 2014, "H", 2, "G", 2016, "Lbt", "Lbt", "DAVENPORT, SAMUEL", 0, 15237, 0,
  # LA-03 House 2014
  "LA", 2014, "H", 3, "G", 2016, "R", "R", "BOUSTANY, CHARLES W., JR.", 1, 185867, 1,
  "LA", 2014, "H", 3, "G", 2016, "R", "R", "BARRILLEAUX, BRYAN", 0, 22059, 0,
  "LA", 2014, "H", 3, "G", 2016, "I", "No Party", "RICHARD, RUSSELL", 0, 28342, 0,
  # LA-04 House 2014
  "LA", 2014, "H", 4, "G", 2016, "R", "R", "FLEMING, JOHN C., JR.", 1, 152683, 1,
  "LA", 2014, "H", 4, "G", 2016, "Lbt", "Lbt", "LORD, RANDALL", 0, 55236, 0,
  # LA-01 House 2016
  "LA", 2016, "H", 1, "G", 2018, "R", "R", "SCALISE, STEPHEN J. (STEVE)", 1, 243645, 1,
  "LA", 2016, "H", 1, "G", 2018, "D", "D", "DUGAS, LEE ANN", 0, 41840, 0,
  "LA", 2016, "H", 1, "G", 2018, "D", "D", "FAUST, DANIL EZEKIEL", 0, 12708, 0,
  "LA", 2016, "H", 1, "G", 2018, "D", "D", "SWIDER, JOSEPH (JOE)", 0, 9237, 0,
  "LA", 2016, "H", 1, "G", 2018, "Lbt", "Lbt", "KEARNEY, HOWARD", 0, 9405, 0,
  "LA", 2016, "H", 1, "G", 2018, "Grn", "Green", "BARRON, ELIOT", 0, 6717, 0,
  "LA", 2016, "H", 1, "G", 2018, "I", "No Party", "YANG, CHUEMAI", 0, 3236, 0,
  # LA-02 House 2016
  "LA", 2016, "H", 2, "G", 2018, "D", "D", "RICHMOND, CEDRIC L.", 1, 198289, 1,
  "LA", 2016, "H", 2, "G", 2018, "D", "D", "HOLDEN, MELVIN L. (KIP)", 0, 57125, 0,
  "LA", 2016, "H", 2, "G", 2018, "D", "D", "CUTNO, KENNETH", 0, 28855, 0,
  # LA-06 House 2016
  "LA", 2016, "H", 6, "G", 2018, "R", "R", "GRAVES, GARRET", 1, 207483, 1,
  "LA", 2016, "H", 6, "G", 2018, "R", "R", "BELL, ROBERT LAMAR (BOB)", 0, 33592, 0,
  "LA", 2016, "H", 6, "G", 2018, "D", "D", "LIEBERMAN, RICHARD", 0, 49380, 0,
  "LA", 2016, "H", 6, "G", 2018, "D", "D", "SAMPSON, JERMAINE", 0, 29822, 0,
  "LA", 2016, "H", 6, "G", 2018, "Lbt", "Lbt", "FONTANESI, RICHARD M.", 0, 7603, 0,
  "LA", 2016, "H", 6, "G", 2018, "Other", "Other", "GRAHAM, DEVIN LANCE", 0, 3218, 0,
  # LA-01 House 2020
  "LA", 2020, "H", 1, "G", 2022, "D", "D", "DUGAS, LEE ANN", 0, 94730, 0,
  "LA", 2020, "H", 1, "G", 2022, "R", "R", "SCALISE, STEPHEN J. (STEVE)", 1, 270330, 1,
  "LA", 2020, "H", 1, "G", 2022, "Lbt", "Lbt", "KEARNEY, HOWARD", 0, 9309, 0,
  # LA-02 House 2020
  "LA", 2020, "H", 2, "G", 2022, "D", "D", "RICHMOND, CEDRIC L.", 1, 201636, 1,
  "LA", 2020, "H", 2, "G", 2022, "D", "D", "HARRIS, GLENN ADRAIN", 0, 33684, 0,
  "LA", 2020, "H", 2, "G", 2022, "R", "R", "SCHILLING, DAVID M.", 0, 47575, 0,
  "LA", 2020, "H", 2, "G", 2022, "R", "R", "VINCENT, SHELDON C., SR.", 0, 15565, 0,
  "LA", 2020, "H", 2, "G", 2022, "I", "Independent", "BATISTE, BELDEN (NOONIE MAN)", 0, 12268, 0,
  "LA", 2020, "H", 2, "G", 2022, "I", "Independent", "JAMES, COLBY", 0, 6254, 0,
  # LA-03 House 2020
  "LA", 2020, "H", 3, "G", 2022, "D", "D", "HARRIS, BRAYLON", 0, 60852, 0,
  "LA", 2020, "H", 3, "G", 2022, "D", "D", "ANDERSON, ROB", 0, 39423, 0,
  "LA", 2020, "H", 3, "G", 2022, "R", "R", "HIGGINS, CLAY", 1, 230480, 1,
  "LA", 2020, "H", 3, "G", 2022, "Lbt", "Lbt", "LELEUX, BRANDON", 0, 9365, 0,
  # LA-04 House 2020
  "LA", 2020, "H", 4, "G", 2022, "D", "D", "HOUSTON, KENNY", 0, 78157, 0,
  "LA", 2020, "H", 4, "G", 2022, "D", "D", "TRUNDLE, RYAN", 0, 23813, 0,
  "LA", 2020, "H", 4, "G", 2022, "R", "R", "JOHNSON, MIKE", 1, 185265, 1,
  "LA", 2020, "H", 4, "G", 2022, "R", "R", "GIBSON, BEN", 0, 19343, 0,
  # LA-06 House 2020
  "LA", 2020, "H", 6, "G", 2022, "D", "D", "WILLIAMS, DARTANYON (DAW)", 0, 95541, 0,
  "LA", 2020, "H", 6, "G", 2022, "R", "R", "GRAVES, GARRET", 1, 265706, 1,
  "LA", 2020, "H", 6, "G", 2022, "Lbt", "Lbt", "SLOAN, SHANNON", 0, 9732, 0,
  "LA", 2020, "H", 6, "G", 2022, "I", "No Party", "TORREGANO, RICHARD (RPT)", 0, 3017, 0,
  # LA-05 House 2020
  "LA", 2020, "H", 5, "G", 2022, "R", "R", "HARRIS, LANCE", 0, 30124, 0,
)

jsdat <- add_rows_if_new(jsdat, la_additions)

## Adding Lisa Murkowski 2010 ----

alaska_senate_2010 <- tibble::tribble(
  ~state, ~year, ~office, ~dist, ~type, ~nextup, ~party, ~party_formal, ~name_snyder, ~inc, ~vote_g, ~w_g,
  "AK", 2010, "S", 2, "G", 2016, "Other", "W-I", "MURKOWSKI, LISA ANN", 1, 101091, 1,
  "AK", 2010, "S", 2, "G", 2016, "R", "R", "MILLER, JOE", 0, 90860, 0,
  "AK", 2010, "S", 2, "G", 2016, "D", "D", "MCADAMS, SCOTT T.", 0, 60045, 0,
  "AK", 2010, "S", 2, "G", 2016, "Lbt", "Lbt", "HAASE, FREDRICK (DAVID)", 0, 1459, 0,
  "AK", 2010, "S", 2, "G", 2016, "I", "Nonaffiliated", "CARTER, TIM", 0, 927, 0,
  "AK", 2010, "S", 2, "G", 2016, "I", "Nonaffiliated", "GIANOUTSOS, TED", 0, 458, 0
)

jsdat <- add_rows_if_new(jsdat, alaska_senate_2010)

## NY, CT, SC fusion voting aggregation ----
# In these states candidates can run on multiple party lines (e.g., Democratic and Working Families).
# Some years have these as separate rows, some as aggregated. This code aggregates all to a single
# row per candidate-race with summed votes and comma-separated party_formal.
# missing in v4 source data

# manual additions
fusion_candidates <- read_csv(
  "data/intermediate/fusion_candidates.csv",
  col_types = "cdcdcdcccddd")

# Data is sorted by desc(vote_g), so higher candidate first
jsdat_fusion <- jsdat |>
  filter(state %in% c("NY", "CT", "SC")) |>
  bind_rows(fusion_candidates) |>
  arrange(state, year, office, dist, desc(vote_g)) |>
  tidylog::distinct(state, year, office, dist, name_snyder, type, .keep_all = TRUE)

jsdat <- jsdat |>
  filter(!state %in% c("NY", "CT", "SC")) |>
  bind_rows(jsdat_fusion)

# Verify no duplicate winners remain after fusion dedup
stopifnot(
  "Duplicate winners found" =
    jsdat |>
      filter(!is.na(w_g), w_g == 1) |>
      duplicated(x = _, subset = c("state", "year", "office", "dist", "type")) |>
      sum() == 0
)


write_rds(jsdat, "data/intermediate/candidates_1990-2024-v1.rds")


