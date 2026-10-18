# Note: Script 04b_audit-corrections.R was written using Claude Opus 5.5. The script mainly makes manual adjustments to the candidate dataset based on audits and corrections from the Clerk of the House and other validated sources. 

library(tidyverse)
library(tidylog)
source("00c_party-recode-functions.R")

cand <- read_rds("data/intermediate/candidates_1990-2024-v2.rds")

# 1. Pre-2006 structure ----

## Fusion party ----
# pre-2006 Snyder rows join fusion lines into one label ("D,L", "R,C,RTL"), which 02 codes as Other
cand <- cand |>
  mutate(
    party_first = party_formal |>
      str_split_i(",", 1) |>
      str_trim() |>
      party_fringeparties() |>
      party_oneletter()
  ) |>
  tidylog::mutate(
    party = if_else(
      year < 2006 & str_detect(party_formal, ",") & party_first %in% c("D", "R", "I", "Lbt", "Grn"),
      party_first,
      party
    )
  ) |>
  select(-party_first)

## Sanders ----
# Snyder codes Sanders' House rows D; he was elected as an independent
cand <- cand |>
  tidylog::mutate(
    party = replace(party, state == "VT" & office == "H" & name_snyder == "SANDERS, BERNARD (BERNIE)", "I"),
    party_formal = replace(party_formal, state == "VT" & office == "H" & name_snyder == "SANDERS, BERNARD (BERNIE)", "I")
  )

## Fusion duplicates outside NY, CT, SC ----
# Infante has one row per line plus the combined row
cand <- cand |>
  tidylog::filter(!(year == 2004 & state == "DE" & office == "G" & name_snyder == "INFANTE, FRANK" & !str_detect(party_formal, ",")))

# 2. Clerk audit, 1990 ----

## Votes ----
# Snyder's 1990 NJ figures differ from the Clerk in every district; the rest are typos
votes_1990 <- tribble(
  ~year, ~state, ~office, ~dist, ~type, ~name_snyder, ~vote_g,
  1990, "CO", "H", 3, "G", "FIELDS, HOWARD E.", 2859,  # p. 7
  1990, "HI", "S", 1, "S", "SCHOOLLAND, KEN", 4787,  # p. 11
  1990, "MO", "H", 2, "G", "HORN, JOAN KELLY", 94308,  # p. 22
  1990, "MO", "H", 2, "G", "BUECHNER, JOHN (JACK)", 94260,  # p. 22
  1990, "NH", "S", 2, "G", "DURKIN, JOHN A.", 91299,  # p. 24
  1990, "NH", "S", 2, "G", "SMITH, ROBERT C. (BOB)", 189792,  # p. 24
  1990, "NH", "H", 2, "G", "SWETT, DICK", 74866,  # p. 24
  1990, "NH", "H", 2, "G", "DOUGLAS, CHUCK", 67225,  # p. 24
  1990, "NJ", "H", 1, "G", "ZELDIN, JERRY", 1592,  # p. 25
  1990, "NJ", "H", 1, "G", "HARRIS, WILLIAM HENRY", 1066,  # p. 25
  1990, "NJ", "H", 1, "G", "KONSTANTY, WALTER E.", 1422,  # p. 25
  1990, "NJ", "H", 2, "G", "HUGHES, WILLIAM J.", 97698,  # p. 25
  1990, "NJ", "H", 3, "G", "PALLONE, FRANK J., JR.", 77866,  # p. 25
  1990, "NJ", "H", 3, "G", "KAPALKO, PAUL A.", 73696,  # p. 25
  1990, "NJ", "H", 3, "G", "MCKEAN, RICHARD D.", 4377,  # p. 25
  1990, "NJ", "H", 3, "G", "STEWART, WILLIAM", 1833,  # p. 25
  1990, "NJ", "H", 3, "G", "PLONSKI, JOSEPH A.", 871,  # p. 25
  1990, "NJ", "H", 4, "G", "SETARO, MARK S.", 54961,  # p. 25
  1990, "NJ", "H", 4, "G", "SMITH, CHRISTOPHER H. (CHRIS)", 99920,  # p. 25
  1990, "NJ", "H", 4, "G", "PETERS, CARL", 2178,  # p. 25
  1990, "NJ", "H", 4, "G", "NOTARANGELO, JOSEPH J.", 1206,  # p. 25
  1990, "NJ", "H", 4, "G", "CARTER, J.M.", 1034,  # p. 25
  1990, "NJ", "H", 6, "G", "DWYER, BERNARD J.", 63745,  # p. 25
  1990, "NJ", "H", 6, "G", "DANIELCZYK, PAUL", 58147,  # p. 25
  1990, "NJ", "H", 6, "G", "SCHOEN, HOWARD F.", 1782,  # p. 25
  1990, "NJ", "H", 6, "G", "WALLER, RANDOLPH", 2348,  # p. 25
  1990, "NJ", "H", 7, "G", "BERGEN, BRUCE H.", 31099,  # p. 25
  1990, "NJ", "H", 7, "G", "RINALDO, MATTHEW J.", 100066,  # p. 25
  1990, "NJ", "H", 7, "G", "SARNOWSKI, THOMAS V.", 2907,  # p. 25
  1990, "NJ", "H", 8, "G", "ROE, ROBERT A.", 55797,  # p. 25
  1990, "NJ", "H", 8, "G", "EDEN, BRUCE", 3563,  # p. 25
  1990, "NJ", "H", 9, "G", "TORRICELLI, ROBERT G.", 82535,  # p. 25
  1990, "NJ", "H", 9, "G", "RUSSO, PETER J.", 69658,  # p. 25
  1990, "NJ", "H", 10, "G", "PAYNE, DONALD M.", 42106,  # p. 25
  1990, "NJ", "H", 10, "G", "BERKELEY, HOWARD E.", 8954,  # p. 25
  1990, "NJ", "H", 10, "G", "MEHRABIAN, GEORGE", 643,  # p. 25
  1990, "NJ", "H", 11, "G", "GORDON, MICHAEL", 47414,  # p. 25
  1990, "NJ", "H", 11, "G", "GALLO, DEAN A.", 92681,  # p. 25
  1990, "NJ", "H", 11, "G", "GOULD, JASPER (JACK)", 3591,  # p. 25
  1990, "NJ", "H", 12, "G", "CHANDLER, MARGUERITE", 52256,  # p. 25
  1990, "NJ", "H", 12, "G", "ZIMMER, RICHARD A. (DICK)", 107851,  # p. 25
  1990, "NJ", "H", 12, "G", "NOTARANGELO, MICHAEL A.", 1411,  # p. 25
  1990, "NJ", "H", 12, "G", "KORTEPETER, C. MAX", 2431,  # p. 25
  1990, "NJ", "H", 13, "G", "ADLER, JOHN H.", 67587,  # p. 26
  1990, "NJ", "H", 13, "G", "SAXTON, H. JAMES (JIM)", 99688,  # p. 26
  1990, "NJ", "H", 13, "G", "PEARLMAN, HOWARD SCOTT", 4131,  # p. 26
  1990, "NJ", "H", 14, "G", "GUARINI, FRANK JOSEPH", 56455,  # p. 26
  1990, "NJ", "H", 14, "G", "THEEMLING, FRED J.", 24870,  # p. 26
  1990, "NJ", "H", 14, "G", "STOVEKEN, DONALD K.", 502,  # p. 26
  1990, "NJ", "H", 14, "G", "HARRIS, JANE E.", 1318,  # p. 26
  1990, "NJ", "H", 14, "G", "ZIRUOLO, MICHAEL", 1822,  # p. 26
  1990, "NJ", "H", 14, "G", "VERNOTICO, LOUIS", 309,  # p. 26
  1990, "NC", "S", 2, "G", "HELMS, JESSE", 1087331,  # p. 33
  1990, "TN", "S", 2, "G", "GORE, ALBERT, JR.", 530898,  # p. 39
  1990, "TN", "S", 2, "G", "HAWKINS, WILLIAM R.", 233703,  # p. 39
  1990, "TN", "S", 2, "G", "JACOX, WILLIAM (BILL)", 11191,  # p. 39
  1990, "NJ", "H", 1, "G", "ANDREWS, ROBERT E. (ROB)", 72415,  # p. 25
  1990, "NJ", "H", 1, "G", "MANGINI, DANIEL J.", 57299,  # p. 25
  1990, "NJ", "H", 12, "G", "BOTTCHER, JOAN I.", 4441   # p. 25
)

cand <- cand |>
  rows_update(votes_1990, by = c("year", "state", "office", "dist", "type", "name_snyder"), unmatched = "error")

## Candidates missing from Snyder ----
# VT at large is Sanders' seat, missing like the independent senators 03 adds back
adds_1990 <- tribble(
  ~state, ~year, ~office, ~dist, ~type, ~nextup, ~party, ~party_formal, ~name_snyder, ~inc, ~vote_g, ~w_g,
  "VT", 1990, "H", 1, "G", 1992, "I", "I", "SANDERS, BERNARD (BERNIE)", 0, 117522, 1,
  "VT", 1990, "H", 1, "G", 1992, "R", "R", "SMITH, PETER", 1, 82938, 0,
  "VT", 1990, "H", 1, "G", 1992, "D", "D", "SANDOVAL, DOLORES", 0, 6315, 0,
  "VT", 1990, "H", 1, "G", 1992, "Other", "Liberty Union", "DIAMONDSTONE, PETER ISAAC (PETE)", 0, 1965, 0,
  "AL", 1990, "H", 6, "G", 1992, "I", "I", "ALVAREZ, DAVID A.", 0, 8640, 0,
  "CA", 1990, "H", 43, "G", 1992, "Other", "P&F", "HANSEN, DOUG", 0, 40212, 0,
  "CA", 1990, "H", 45, "G", 1992, "Lbt", "Lbt", "SHEA, JOE", 0, 46068, 0,
  "IL", 1990, "H", 8, "G", 1992, "Lbt", "Lbt", "MARSHALL, ROBERT", 0, 18529, 0,
  "IL", 1990, "H", 12, "G", 1992, "Other", "Solidarity", "PEDERSEN, STEVE", 0, 24450, 0,
  "KY", 1990, "H", 1, "G", 1992, "Other", "Populist", "SEAT, MARVIN", 0, 12879, 0,
  "MA", 1990, "H", 9, "G", 1992, "I", "I", "HORAN, ROBERT W.", 0, 52660, 0,
  "NJ", 1990, "H", 2, "G", 1992, "Other", "Populist", "KANENGISER, WILLIAM A.", 0, 13120, 0,
  "NJ", 1990, "H", 8, "G", 1992, "Other", "Independent Conservative", "SIBILIA, STEPHEN", 0, 13180, 0,
  "NY", 1990, "H", 25, "G", 1992, "Lbt", "Lbt", "GRIFFEN, WILLIAM L.", 0, 17481, 0,
  "OR", 1990, "H", 4, "G", 1992, "Lbt", "Lbt", "NATHAN, TONIE", 0, 26432, 0,
  "SC", 1990, "H", 2, "G", 1992, "Lbt", "Lbt", "SOMMER, GEB", 0, 11101, 0,
  "TN", 1990, "H", 2, "G", 1992, "I", "I", "HEBERT, PETER", 0, 15127, 0,
  "TN", 1990, "H", 5, "G", 1992, "I", "I", "STONE, TOM", 0, 13577, 0,
  "VA", 1990, "H", 2, "G", 1992, "I", "I", "BROSKIE, HARRY G.", 0, 15915, 0,
  "VA", 1990, "H", 4, "G", 1992, "I", "I", "MCREYNOLDS, DON L.", 0, 12295, 0,
  "VA", 1990, "H", 6, "G", 1992, "I", "I", "BERG, GERALD E. (GERRY)", 0, 18148, 0,
  # named write-ins
  "AZ", 1990, "H", 1, "G", 1992, "Other", "W-I", "ROSE, TIM", 0, 621, 0,
  "FL", 1990, "H", 15, "G", 1992, "Other", "W-I", "GOODMON, CHARLES", 0, 2374, 0,
  "IL", 1990, "H", 18, "G", 1992, "Other", "W-I", "GILLIN, WALTER", 0, 1524, 0,
  "TX", 1990, "H", 3, "G", 1992, "Other", "W-I", "KOPALA, NOEL", 0, 617, 0,
  "TX", 1990, "H", 4, "G", 1992, "Other", "W-I", "MCCORD, TIM J.", 0, 394, 0,
  "TX", 1990, "H", 16, "G", 1992, "Other", "W-I", "BURGETT, WILLIAM", 0, 2854, 0
)

cand <- cand |>
  rows_insert(adds_1990, by = c("year", "state", "office", "dist", "type", "name_snyder"), conflict = "error")

## Louisiana open primary ----
# contested seats were decided in the October 6 open primary (LA Secretary of State); Baker (6) was unopposed
la_1990 <- tribble(
  ~state, ~year, ~office, ~dist, ~type, ~nextup, ~party, ~party_formal, ~name_snyder, ~inc, ~vote_g, ~w_g,
  "LA", 1990, "S", 2, "G", 1996, "D", "D", "JOHNSTON, J. BENNETT, JR.", 1, 753198, 1,
  "LA", 1990, "S", 2, "G", 1996, "R", "R", "DUKE, DAVID E.", 0, 607091, 0,
  "LA", 1990, "S", 2, "G", 1996, "D", "D", "ACCARDO, NICK JOSEPH", 0, 21578, 0,
  "LA", 1990, "S", 2, "G", 1996, "D", "D", "CROWE, LARRY", 0, 14345, 0,
  "LA", 1990, "H", 1, "G", 1992, "R", "R", "LIVINGSTON, ROBERT L. (BOB)", 1, 132855, 1,
  "LA", 1990, "H", 1, "G", 1992, "R", "R", "BRUNO, VINCENT", 0, 25494, 0,
  "LA", 1990, "H", 3, "G", 1992, "D", "D", "TAUZIN, WILBERT J. (BILLY)", 1, 155251, 1,
  "LA", 1990, "H", 3, "G", 1992, "Other", "Other", "DUPLANTIS, RONALD P. (RON)", 0, 14904, 0,
  "LA", 1990, "H", 3, "G", 1992, "Other", "Other", "CLEMENT, MILLARD F.", 0, 6562, 0,
  "LA", 1990, "H", 4, "G", 1992, "R", "R", "MCCRERY, JAMES O. (JIM)", 1, 89859, 1,
  "LA", 1990, "H", 4, "G", 1992, "D", "D", "CAMPBELL, FOSTER L., JR.", 0, 74388, 0,
  "LA", 1990, "H", 5, "G", 1992, "D", "D", "HUCKABY, THOMAS JERALD (JERRY)", 1, 128117, 1,
  "LA", 1990, "H", 5, "G", 1992, "D", "D", "BATEY, CARL", 0, 24050, 0,
  "LA", 1990, "H", 5, "G", 1992, "R", "R", "ROARK, BRADLEY THOMASON", 0, 16331, 0,
  "LA", 1990, "H", 5, "G", 1992, "Other", "Other", "KNOX, L.D.", 0, 5376, 0,
  "LA", 1990, "H", 7, "G", 1992, "D", "D", "HAYES, JAMES A. (JIMMY)", 1, 103308, 1,
  "LA", 1990, "H", 7, "G", 1992, "R", "R", "THIBODAUX, DAVID", 0, 68430, 0,
  "LA", 1990, "H", 7, "G", 1992, "D", "D", "MYERS, JOHNNY", 0, 7369, 0,
  "LA", 1990, "H", 8, "G", 1992, "R", "R", "HOLLOWAY, CLYDE C.", 1, 113607, 1,
  "LA", 1990, "H", 8, "G", 1992, "D", "D", "FIELDS, CLEO", 0, 59511, 0,
  "LA", 1990, "H", 8, "G", 1992, "D", "D", "MCPHERSON, JOE", 0, 28170, 0
) |>
  mutate(runoff = 0)

cand <- cand |>
  rows_upsert(la_1990, by = c("year", "state", "office", "dist", "type", "name_snyder")) |>
  # the November contest in LA-2 was the second round; Jefferson per SOS (Clerk 55,621)
  tidylog::mutate(
    runoff = replace(runoff, year == 1990 & state == "LA" & office == "H" & dist == 2, 1),
    vote_g = replace(vote_g, year == 1990 & state == "LA" & name_snyder == "JEFFERSON, WILLIAM J. (BILL)", 55239)
  )

## Names ----
# WA-6 carried OH-11's Margaret Mueller; Hammersmith is printed Donalda in 1990 and 1992
names_1990 <- tribble(
  ~state, ~name_snyder, ~name_new,
  "WA", "MUELLER, MARGARET R.", "MUELLER, NORBERT (BERT)",
  "WI", "HAMMERSMITH, DONALD A.", "HAMMERSMITH, DONALDA A.",
  "IL", "CASSIDY, TERENCE P.", "CASSIDY, ROBERT J.",
  "IL", "WESTPHAL, DONAL", "WESTPHAL, DONALD J.",
  "NY", "KOEPPEL, WILLIAN W.", "KOEPPEL, WILLIAM W.",
  "NY", "BELLITTO, GLEN D.", "BELLITTO, GLENN D."
)

cand <- cand |>
  left_join(names_1990, by = c("state", "name_snyder"), relationship = "many-to-one") |>
  tidylog::mutate(name_snyder = coalesce(name_new, name_snyder)) |>
  select(-name_new)

# 3. Clerk audit, 1992 ----

## Votes ----
# Snyder's 1992 FL figures are slightly off the Clerk in every race
votes_1992 <- tribble(
  ~year, ~state, ~office, ~dist, ~type, ~name_snyder, ~vote_g,
  1992, "CA", "S", 1, "S", "BODDIE, RICHARD B.", 247799,  # p. 6
  1992, "CA", "S", 3, "G", "MCCREADY, JEROME (JERRY)", 373051,  # p. 7
  1992, "FL", "S", 3, "G", "GRAHAM, D. ROBERT (BOB)", 3245565,  # p. 17
  1992, "FL", "S", 3, "G", "GRANT, JAMES WILLIAM (BILL)", 1716505,  # p. 17
  1992, "FL", "H", 1, "G", "HUTTO, EARL DEWITT", 118941,  # p. 17
  1992, "FL", "H", 1, "G", "KETCHEL, TERRANCE R. (TERRY)", 100349,  # p. 17
  1992, "FL", "H", 2, "G", "PETERSON, DOUGLAS BRIAN (PETE)", 167215,  # p. 17
  1992, "FL", "H", 2, "G", "WAGNER, RAYMOND WALTER", 60425,  # p. 17
  1992, "FL", "H", 3, "G", "BROWN, CORRINE", 91915,  # p. 17
  1992, "FL", "H", 3, "G", "WEIDNER, DONALD WARREN", 63114,  # p. 17
  1992, "FL", "H", 4, "G", "HAIR, MATTOX S.", 103531,  # p. 17
  1992, "FL", "H", 4, "G", "FOWLER, TILLIE KIDD", 135883,  # p. 17
  1992, "FL", "H", 5, "G", "THURMAN, KAREN L.", 129698,  # p. 17
  1992, "FL", "H", 5, "G", "HOGAN, THOMAS S., JR.", 114356,  # p. 17
  1992, "FL", "H", 6, "G", "DENTON, PHILIP A.", 76419,  # p. 17
  1992, "FL", "H", 6, "G", "STEARNS, CLIFFORD B. (CLIFF)", 144195,  # p. 17
  1992, "FL", "H", 7, "G", "WEBSTER, DANIEL J.", 96945,  # p. 17
  1992, "FL", "H", 7, "G", "MICA, JOHN L.", 125823,  # p. 17
  1992, "FL", "H", 8, "G", "KOVALESKI, CHARLES J.", 65145,  # p. 18
  1992, "FL", "H", 8, "G", "MCCOLLUM, BILL", 141977,  # p. 18
  1992, "FL", "H", 9, "G", "KNAPP, CHERYL DAVIS", 110135,  # p. 18
  1992, "FL", "H", 9, "G", "BILIRAKIS, MICHAEL", 158028,  # p. 18
  1992, "FL", "H", 10, "G", "MOFFITT, KAREN A.", 114809,  # p. 18
  1992, "FL", "H", 10, "G", "YOUNG, C.W. (BILL)", 149606,  # p. 18
  1992, "FL", "H", 11, "G", "GIBBONS, SAM M.", 100984,  # p. 18
  1992, "FL", "H", 11, "G", "SHARPE, MARK STEPHEN", 77640,  # p. 18
  1992, "FL", "H", 12, "G", "MIMS, WILLIAM THOMAS (TOM)", 92346,  # p. 18
  1992, "FL", "H", 12, "G", "CANADY, CHARLES T.", 100484,  # p. 18
  1992, "FL", "H", 13, "G", "SNELL, RANDOLPH R.", 115767,  # p. 18
  1992, "FL", "H", 13, "G", "MILLER, DAN", 158881,  # p. 18
  1992, "FL", "H", 14, "G", "GOSS, PORTER JOHNSTON", 220351,  # p. 18
  1992, "FL", "H", 15, "G", "BACCHUS, JAMES L. (JIM)", 132412,  # p. 18
  1992, "FL", "H", 15, "G", "TOLLEY, WILLIAM RUSSELL (BILL), JR.", 128873,  # p. 18
  1992, "FL", "H", 16, "G", "COMERFORD, JOHN P.", 101237,  # p. 18
  1992, "FL", "H", 16, "G", "LEWIS, THOMAS F. (TOM)", 157322,  # p. 18
  1992, "FL", "H", 17, "G", "MEEK, CARRIE P.", 102784,  # p. 18
  1992, "FL", "H", 18, "G", "DAVIS, MAGDA MONTIEL", 52142,  # p. 18
  1992, "FL", "H", 18, "G", "ROS-LEHTINEN, ILEANA", 104755,  # p. 18
  1992, "FL", "H", 19, "G", "JOHNSTON, HARRY ALLISON, II", 177423,  # p. 18
  1992, "FL", "H", 19, "G", "METZ, LARRY", 103867,  # p. 18
  1992, "FL", "H", 20, "G", "DEUTSCH, PETER RUSSELL", 130959,  # p. 18
  1992, "FL", "H", 20, "G", "KENNEDY, BEVERLY B. (BEV)", 91589,  # p. 18
  1992, "FL", "H", 22, "G", "MARGOLIS, GWEN", 91625,  # p. 18
  1992, "FL", "H", 22, "G", "SHAW, EUGENE CLAY, JR.", 128400,  # p. 18
  1992, "FL", "H", 23, "G", "HASTINGS, ALCEE LAMAR", 84249,  # p. 18
  1992, "FL", "H", 23, "G", "FIELDING, EDWARD V.", 44807,  # p. 18
  1992, "GA", "H", 8, "G", "CUNNINGHAM, ROBERT FREDERICK (ROB), JR.", 86220,  # p. 19
  1992, "KS", "H", 3, "G", "LOVE, TOM", 110071,  # p. 28
  1992, "MN", "H", 3, "G", "FELLMAN, DWIGHT", 9164,  # p. 39
  1992, "MN", "H", 8, "G", "HERWIG, PHILIP", 83823,  # p. 39
  1992, "NV", "S", 3, "G", "GARCIA, JOE S.", 11240,  # p. 43
  1992, "NJ", "H", 6, "G", "SPALLETTA, JOSEPH", 2153,  # p. 46
  1992, "TX", "H", 30, "G", "JOHNSON, EDDIE BERNICE", 107831   # p. 71
)

cand <- cand |>
  rows_update(votes_1992, by = c("year", "state", "office", "dist", "type", "name_snyder"), unmatched = "error")

## Misplaced and misattributed rows ----
# Rothacker ran in TX-4 and Knapp in MA-10; Lonsdale's figure is OR's Miscellaneous plus Write-in lines
cand <- cand |>
  tidylog::mutate(
    dist = replace(dist, year == 1992 & state == "TX" & name_snyder == "ROTHACKER, STEVEN M.", 4),
    dist = replace(dist, year == 1992 & state == "MA" & name_snyder == "KNAPP, ROBERT W.", 10)
  ) |>
  tidylog::filter(!(year == 1992 & state == "OR" & name_snyder == "LONSDALE, HAROLD K. (HARRY)"))

## Candidates missing from Snyder ----
adds_1992 <- tribble(
  ~state, ~year, ~office, ~dist, ~type, ~nextup, ~party, ~party_formal, ~name_snyder, ~inc, ~vote_g, ~w_g,
  "VT", 1992, "H", 1, "G", 1994, "I", "I", "SANDERS, BERNARD (BERNIE)", 1, 162724, 1,
  "VT", 1992, "H", 1, "G", 1994, "Other", "Liberty Union", "DIAMONDSTONE, PETER ISAAC (PETE)", 0, 3660, 0,
  "NH", 1992, "S", 3, "G", 1998, "I", "I", "BLEVENS, KEN", 0, 4752, 0,
  "CA", 1992, "H", 32, "G", 1994, "Lbt", "Lbt", "WEBER, BOB", 0, 12384, 0,
  "IA", 1992, "S", 3, "G", 1998, "Other", "Nominated by Petition", "ATKINSON, SUE A.", 0, 6277, 0,
  "NV", 1992, "S", 3, "G", 1998, "Other", "Natural Law", "AVERY, LOIS JEANNE", 0, 7279, 0,
  "CA", 1992, "S", 3, "G", 1998, "Lbt", "Lbt", "GENIS, JUNE R.", 0, 235919, 0,
  "HI", 1992, "S", 3, "G", 1998, "Lbt", "Lbt", "ROWLAND, RICHARD O. (DICK)", 0, 7547, 0,
  "IL", 1992, "S", 3, "G", 1998, "Other", "Natural Law", "WINTER, CHARLES A.", 0, 15118, 0,
  "IL", 1992, "S", 3, "G", 1998, "Other", "New Alliance", "PORT, ALAN J.", 0, 12689, 0,
  "IA", 1992, "S", 3, "G", 1998, "Other", "Grass Roots", "OLSEN, CARL ERIC", 0, 3404, 0,
  "IA", 1992, "S", 3, "G", 1998, "Other", "Nominated by Petition", "HUGHES, RICHARD O.", 0, 2918, 0,
  "OK", 1992, "S", 3, "G", 1998, "I", "I", "LEDGERWOOD, THOMAS D., II", 0, 20972, 0,
  "CA", 1992, "S", 3, "G", 1998, "Other", "P&F", "TORRES, GENEVIEVE", 0, 372817, 0,
  "AZ", 1992, "S", 3, "G", 1998, "Lbt", "Lbt", "DELAMARE, KIANA", 0, 22613, 0,
  "AZ", 1992, "S", 3, "G", 1998, "Other", "New Alliance", "FINKELSTEIN, ED", 0, 6335, 0,
  "CO", 1992, "S", 3, "G", 1998, "I", "I", "WINTERS, DAN", 0, 20347, 0,
  "CO", 1992, "S", 3, "G", 1998, "Lbt", "Lbt", "FUTCH, HUE", 0, 23, 0,
  "CO", 1992, "S", 3, "G", 1998, "Other", "Christian Pro-life", "NOAH, MATT", 0, 22846, 0,
  "CT", 1992, "S", 3, "G", 1998, "Lbt", "Lbt", "GRAYSON, HOWARD A., JR.", 0, 10741, 0,
  "FL", 1992, "H", 14, "G", 1994, "I", "I", "KING, JAMES H.", 0, 48160, 0,
  "IL", 1992, "S", 3, "G", 1998, "Lbt", "Lbt", "SPIEGEL, ANDREW B.", 0, 34527, 0,
  "IL", 1992, "S", 3, "G", 1998, "Other", "Populist", "JUSTICE, JOHN", 0, 8656, 0,
  "IL", 1992, "S", 3, "G", 1998, "Other", "SWP", "KAKU, KATHLEEN", 0, 10056, 0,
  "IA", 1992, "S", 3, "G", 1998, "Other", "SWP", "PULLEY, CLEVE ANDREW", 0, 1370, 0,
  "IA", 1992, "S", 3, "G", 1998, "Other", "Nominated by Petition", "BORING, MEL", 0, 5508, 0,
  "IA", 1992, "S", 3, "G", 1998, "Other", "Nominated by Petition", "FREEBURG, ROSANNE", 0, 4999, 0,
  "KS", 1992, "S", 3, "G", 1998, "Lbt", "Lbt", "KIRK, MARK B.", 0, 25253, 0,
  "MD", 1992, "H", 5, "G", 1994, "I", "I", "JOHNSTON, WILLIAM D., III", 0, 6990, 0,
  "NV", 1992, "S", 3, "G", 1998, "Lbt", "Lbt", "CROMWELL, H. KENT", 0, 7222, 0,
  "NV", 1992, "S", 3, "G", 1998, "Other", "Populist", "TOOTLE, HARRY", 0, 4429, 0,
  "NH", 1992, "S", 3, "G", 1998, "I", "I", "BRADY, LARRY", 0, 9340, 0,
  "NH", 1992, "S", 3, "G", 1998, "Other", "Natural Law", "HAIGHT, DAVID", 0, 1291, 0,
  "NY", 1992, "S", 3, "G", 1998, "Other", "New Alliance", "MEHDI, MOHAMMAD T.", 0, 56631, 0,
  "NY", 1992, "S", 3, "G", 1998, "Other", "SWP", "WARREN, ED", 0, 16724, 0,
  "NY", 1992, "S", 3, "G", 1998, "Other", "Natural Law", "NELSON, STANLEY", 0, 23747, 0,
  "NC", 1992, "S", 3, "G", 1998, "Other", "Natural Law", "ZAKUTNEY, MARY ANN", 0, 13, 0,
  "NC", 1992, "S", 3, "G", 1998, "Other", "SWP", "KIMBALL, BRUCE", 0, 23, 0,
  "SC", 1992, "S", 3, "G", 1998, "Other", "American", "CLARKSON, ROBERT BARNWELL, II", 0, 11568, 0,
  "SD", 1992, "S", 3, "G", 1998, "I", "I", "HYDE, KENT", 0, 4314, 0,
  "UT", 1992, "S", 3, "G", 1998, "Lbt", "Lbt", "MODINE, MAURY", 0, 14341, 0,
  "UT", 1992, "S", 3, "G", 1998, "Other", "SWP", "GROGAN, PATRICIA", 0, 5292, 0,
  "VT", 1992, "S", 3, "G", 1998, "Other", "Freedom for LaRouche", "GODECK, MICHAEL B.", 0, 1780, 0,
  "VT", 1992, "H", 1, "G", 1994, "D", "D", "YOUNG, LEWIS E.", 0, 22279, 0,
  "VT", 1992, "H", 1, "G", 1994, "R", "R", "PHILBIN, TIM", 0, 86901, 0,
  "VT", 1992, "H", 1, "G", 1994, "Other", "Freedom for LaRouche", "MILLER, DOUGLAS M.", 0, 2049, 0,
  "VT", 1992, "H", 1, "G", 1994, "Other", "Natural Law/New Alliance", "DEWEY, JOHN", 0, 3549, 0,
  "WI", 1992, "S", 3, "G", 1998, "I", "I", "HANSON, MERVIN A., SR.", 0, 3264, 0,
  "WI", 1992, "S", 3, "G", 1998, "I", "I", "KUNDERT, ROBERT L.", 0, 2747, 0,
  "WI", 1992, "S", 3, "G", 1998, "I", "I", "SELLIKEN, JOSEPH", 0, 2733, 0,
  "WI", 1992, "S", 3, "G", 1998, "Lbt", "Lbt", "BITTNER, WILLIAM", 0, 9147, 0,
  "TX", 1992, "H", 5, "G", 1994, "Lbt", "Lbt", "WALKER, WILLIAM H.", 0, 6344, 0
)

cand <- cand |>
  rows_insert(adds_1992, by = c("year", "state", "office", "dist", "type", "name_snyder"), conflict = "error")

## Georgia Senate runoff ----
# Coverdell won the November 24 runoff; figures as printed by the Clerk
cand <- cand |>
  tidylog::filter(!(year == 1992 & state == "GA" & office == "S")) |>
  bind_rows(tribble(
    ~state, ~year, ~office, ~dist, ~type, ~nextup, ~party, ~party_formal, ~name_snyder, ~inc, ~vote_g, ~w_g, ~runoff,
    "GA", 1992, "S", 3, "G", 1998, "R", "R", "COVERDELL, PAUL DOUGLAS", 0, 635114, 1, 1,
    "GA", 1992, "S", 3, "G", 1998, "D", "D", "FOWLER, W. WYCHE, JR.", 1, 618877, 0, 1
  ))

## Louisiana open primary ----
# October 3 primary decided the Senate seat and districts 1, 2, 3 and 7; 4, 5 and 6 went to the November runoff
la_1992 <- tribble(
  ~state, ~year, ~office, ~dist, ~type, ~nextup, ~party, ~party_formal, ~name_snyder, ~inc, ~vote_g, ~w_g,
  "LA", 1992, "S", 3, "G", 1998, "D", "D", "BREAUX, JOHN B.", 1, 616021, 1,
  "LA", 1992, "S", 3, "G", 1998, "Other", "Other", "KHACHATURIAN, JON", 0, 74785, 0,
  "LA", 1992, "S", 3, "G", 1998, "R", "R", "STOCKSTILL, LYLE", 0, 69986, 0,
  "LA", 1992, "S", 3, "G", 1998, "D", "D", "ACCARDO, NICK JOSEPH", 0, 45839, 0,
  "LA", 1992, "S", 3, "G", 1998, "R", "R", "STRONG, FRED CLEGG", 0, 36406, 0,
  "LA", 1992, "H", 1, "G", 1994, "R", "R", "LIVINGSTON, ROBERT L. (BOB)", 1, 83685, 1,
  "LA", 1992, "H", 1, "G", 1994, "R", "R", "THOMPSON, ANNE", 0, 11620, 0,
  "LA", 1992, "H", 1, "G", 1994, "R", "R", "BRUNO, VINCENT", 0, 7874, 0,
  "LA", 1992, "H", 1, "G", 1994, "R", "R", "MARTIN, RICHIE", 0, 4789, 0,
  "LA", 1992, "H", 1, "G", 1994, "Other", "Other", "HILLERY, JULES W.", 0, 4442, 0,
  "LA", 1992, "H", 1, "G", 1994, "Other", "Other", "REINHARD, GREG V.", 0, 2641, 0,
  "LA", 1992, "H", 2, "G", 1994, "D", "D", "JEFFERSON, WILLIAM J. (BILL)", 1, 67030, 1,
  "LA", 1992, "H", 2, "G", 1994, "D", "D", "IRVIN, WILMA KNOX", 0, 14121, 0,
  "LA", 1992, "H", 2, "G", 1994, "Other", "Other", "JOHNSON, ROGER C.", 0, 10090, 0,
  "LA", 1992, "H", 3, "G", 1994, "D", "D", "TAUZIN, WILBERT J. (BILLY)", 1, 82047, 1,
  "LA", 1992, "H", 3, "G", 1994, "R", "R", "BOYNTON, PAUL I.", 0, 18402, 0,
  "LA", 1992, "H", 7, "G", 1994, "D", "D", "HAYES, JAMES A. (JIMMY)", 1, 84149, 1,
  "LA", 1992, "H", 7, "G", 1994, "R", "R", "HAYES, FREDRIC", 0, 23870, 0,
  "LA", 1992, "H", 7, "G", 1994, "R", "R", "NAIN, ROBERT J. (BOB)", 0, 7184, 0
) |>
  mutate(runoff = 0)

cand <- cand |>
  rows_upsert(la_1992, by = c("year", "state", "office", "dist", "type", "name_snyder")) |>
  tidylog::mutate(runoff = replace(runoff, year == 1992 & state == "LA" & office == "H" & dist %in% 4:6, 1))

# 4. Clerk audit, 1994 ----

## Votes ----
votes_1994 <- tribble(
  ~year, ~state, ~office, ~dist, ~type, ~name_snyder, ~vote_g,
  1994, "AZ", "H", 1, "G", "BLANCHARD, CHUCK", 70627,  # p. 3
  1994, "DE", "S", 1, "G", "ROTH, WILLIAM V., JR.", 111074,  # p. 9
  1994, "DE", "S", 1, "G", "OBERLY, CHARLES M., III", 84540,  # p. 9
  1994, "DE", "S", 1, "G", "DIERICKX, JOHN C.", 3386,  # p. 9
  1994, "DE", "H", 1, "G", "CASTLE, MICHAEL N.", 137945,  # p. 9
  1994, "DE", "H", 1, "G", "DESANTIS, CAROL ANN", 51793,  # p. 9
  1994, "DE", "H", 1, "G", "BEAVER, DANNY RAY", 3866,  # p. 9
  1994, "DE", "H", 1, "G", "HOCKMUTH, DONALD M.", 1404,  # p. 9
  1994, "FL", "S", 1, "G", "MACK, CONNIE, III", 2895200,  # p. 9
  1994, "FL", "S", 1, "G", "RODHAM, HUGH E.", 1210577,  # p. 9
  1994, "FL", "H", 1, "G", "SCARBOROUGH, CHARLES JOSEPH (JOE)", 112974,  # p. 9
  1994, "FL", "H", 1, "G", "WHIBBS, VINCE J.", 70416,  # p. 9
  1994, "FL", "H", 2, "G", "GRIFFIN, CAROLE ANN", 74040,  # p. 9
  1994, "FL", "H", 2, "G", "PETERSON, DOUGLAS BRIAN (PETE)", 117420,  # p. 9
  1994, "FL", "H", 3, "G", "LITTLE, MARC", 46907,  # p. 9
  1994, "FL", "H", 3, "G", "BROWN, CORRINE", 63855,  # p. 9
  1994, "FL", "H", 5, "G", "GARLITS, DONALD GLENN", 94097,  # p. 9
  1994, "FL", "H", 5, "G", "THURMAN, KAREN L.", 125792,  # p. 9
  1994, "FL", "H", 6, "G", "STEARNS, CLIFFORD B. (CLIFF)", 148741,  # p. 9
  1994, "FL", "H", 7, "G", "MICA, JOHN L.", 131731,  # p. 9
  1994, "FL", "H", 7, "G", "GODDARD, EDWARD DEAN", 47758,  # p. 9
  1994, "FL", "H", 8, "G", "MCCOLLUM, BILL", 131393,  # p. 10
  1994, "FL", "H", 9, "G", "BILIRAKIS, MICHAEL", 177265,  # p. 10
  1994, "FL", "H", 11, "G", "SHARPE, MARK STEPHEN", 72129,  # p. 10
  1994, "FL", "H", 11, "G", "GIBBONS, SAM M.", 76821,  # p. 10
  1994, "FL", "H", 12, "G", "CANADY, CHARLES T.", 106140,  # p. 10
  1994, "FL", "H", 12, "G", "CONNORS, ROBERT", 57210,  # p. 10
  1994, "FL", "H", 15, "G", "WELDON, DAVID JOSEPH (DAVE)", 117044,  # p. 10
  1994, "FL", "H", 15, "G", "MUNSEY, SUE", 100526,  # p. 10
  1994, "FL", "H", 16, "G", "FOLEY, MARK ADAM", 122760,  # p. 10
  1994, "FL", "H", 16, "G", "COMERFORD, JOHN P.", 88653,  # p. 10
  1994, "FL", "H", 17, "G", "MEEK, CARRIE P.", 75756,  # p. 10
  1994, "FL", "H", 19, "G", "TSAKANIKAS, PETER JAMES", 75788,  # p. 10
  1994, "FL", "H", 19, "G", "JOHNSTON, HARRY ALLISON, II", 147595,  # p. 10
  1994, "FL", "H", 20, "G", "KENNEDY, BEVERLY B. (BEV)", 72525,  # p. 10
  1994, "FL", "H", 20, "G", "DEUTSCH, PETER RUSSELL", 114623,  # p. 10
  1994, "FL", "H", 21, "G", "DIAZ-BALART, LINCOLN", 90951,  # p. 10
  1994, "FL", "H", 22, "G", "SHAW, EUGENE CLAY, JR.", 119696,  # p. 10
  1994, "FL", "H", 22, "G", "WIENER, HERMINE", 69221,  # p. 10
  1994, "GA", "H", 4, "G", "YATES, JAMES COMER", 65566,  # p. 11
  1994, "IN", "H", 4, "G", "SOUDER, MARK EDWARD", 88584,  # p. 13
  1994, "MI", "H", 12, "G", "PAPPAGEORGE, JOHN G.", 92762,  # p. 19
  1994, "OH", "H", 6, "G", "CREMEANS, FRANK ARNOLD", 91263,  # p. 33
  1994, "OR", "H", 1, "G", "WITT, WILLIAM DAVID (BILL)", 120846,  # p. 35
  1994, "OR", "H", 1, "G", "FURSE, ELIZABETH", 121147,  # p. 35
  1994, "VT", "H", 1, "G", "SANDERS, BERNARD (BERNIE)", 105502   # p. 42
)

cand <- cand |>
  rows_update(votes_1994, by = c("year", "state", "office", "dist", "type", "name_snyder"), unmatched = "error")

## NY fusion totals ----
# Popkin lacked his combined row, Solomon's R line has a typo that beat his combined row, Hinchey's lines are mis-added
cand <- cand |>
  rows_update(tribble(
    ~year, ~state, ~office, ~dist, ~type, ~name_snyder, ~party_formal, ~vote_g,
    1994, "NY", "H", 11, "G", "POPKIN, GARY S.", "R,Lbt", 6605,
    1994, "NY", "H", 22, "G", "SOLOMON, GERALD B.H. (JERRY)", "R,C,RTL", 157717,
    1994, "NY", "H", 26, "G", "HINCHEY, MAURICE D.", "D,L", 95492
  ), by = c("year", "state", "office", "dist", "type", "name_snyder"), unmatched = "error")

## Candidates missing from Snyder ----
adds_1994 <- tribble(
  ~state, ~year, ~office, ~dist, ~type, ~nextup, ~party, ~party_formal, ~name_snyder, ~inc, ~vote_g, ~w_g,
  "NJ", 1994, "S", 1, "G", 2000, "Other", "Conservative", "PEZZULLO, RICHARD J.", 0, 9387, 0,
  "CA", 1994, "S", 1, "G", 2000, "Lbt", "Lbt", "BODDIE, RICHARD B.", 0, 179100, 0,
  "MI", 1994, "S", 1, "G", 2000, "Other", "Workers World", "ROUNDTREE, WILLIAM", 0, 20010, 0,
  "NJ", 1994, "S", 1, "G", 2000, "Other", "Jobs, Property Rights", "LIPPI, ANDREA", 0, 6303, 0,
  "NJ", 1994, "S", 1, "G", 2000, "Other", "SWP", "KUNIANSKY, JOANNE", 0, 3606, 0,
  "UT", 1994, "S", 1, "G", 2000, "Other", "American", "VAN HORN, GARY R.", 0, 2543, 0,
  "VT", 1994, "S", 1, "G", 2000, "Other", "Liberty Union", "LEVY, JERRY E.", 0, 1376, 0,
  "CA", 1994, "S", 1, "G", 2000, "Other", "American Independent", "MEEUWENBERG, PAUL", 0, 142771, 0,
  "CT", 1994, "H", 1, "G", 1996, "Other", "Concerned Citizens", "FORRY, JOHN FRANCIS, III", 0, 3405, 0,
  "MN", 1994, "S", 1, "G", 2000, "Other", "Natural Law", "JOHNSON, STEPHEN R. (STEVE)", 0, 5054, 0,
  "NJ", 1994, "S", 1, "G", 2000, "Other", "Damn Drug Dealers", "PREDHAM, GEORGE P.", 0, 4226, 0,
  "NY", 1994, "S", 1, "G", 2000, "Lbt", "Lbt", "SEGAL, NORMA", 0, 17991, 0,
  "TN", 1994, "S", 1, "G", 2000, "I", "I", "JOHNSON, CHARLES FREELON", 0, 6631, 0,
  "TN", 1994, "S", 1, "G", 2000, "I", "I", "KIENLEN, PHILIP LEE", 0, 3087, 0,
  "UT", 1994, "S", 1, "G", 2000, "Other", "Independent American", "TOPHAM, LAWRENCE", 0, 1462, 0,
  "VT", 1994, "S", 1, "G", 2000, "I", "I", "MULLIGAN, MATTHEW", 0, 3141, 0,
  "TN", 1994, "S", 2, "S", 1996, "I", "I", "MARTIN, KERRY", 0, 1719, 0,
  "NJ", 1994, "S", 1, "G", 2000, "Lbt", "Lbt", "GRINDLINGER, BEN", 0, 14042, 0,
  "TN", 1994, "S", 2, "S", 1996, "I", "I", "MOORE, CHARLES M.", 0, 2219, 0,
  "VT", 1994, "S", 1, "G", 2000, "Other", "Grassroots", "MELAMEDE, BOB", 0, 1416, 0,
  "CA", 1994, "S", 1, "G", 2000, "Grn", "Grn", "BLONG, BARBARA", 0, 140567, 0,
  "IN", 1994, "S", 1, "G", 2000, "Other", "New Alliance", "BARTON, MARY CATHERINE", 0, 15801, 0,
  "MD", 1994, "H", 2, "G", 1996, "I", "I", "GRACE, CHARLES G.", 0, 20, 0,
  "MA", 1994, "S", 1, "G", 2000, "Other", "LaRouche Was Right", "FERGUSON, WILLIAM A., JR.", 0, 4776, 0,
  "MI", 1994, "S", 1, "G", 2000, "Other", "Natural Law", "WEGE, CHRIS", 0, 14746, 0,
  "MN", 1994, "S", 1, "G", 2000, "Other", "Grassroots", "SJOSTROM, CANDICE E.", 0, 15920, 0,
  "MN", 1994, "S", 1, "G", 2000, "Other", "SWP", "HIMELGRIN, MAREA", 0, 2428, 0,
  "NV", 1994, "S", 1, "G", 2000, "Lbt", "Lbt", "DAYS, BOB", 0, 5964, 0,
  "NV", 1994, "S", 1, "G", 2000, "Other", "Independent American", "GRASTEIT, NEAL A.", 0, 5450, 0,
  "NJ", 1994, "S", 1, "G", 2000, "Other", "Natural Law", "GOLD, ARLENE", 0, 3249, 0,
  "NY", 1994, "S", 1, "G", 2000, "Other", "Independence Fusion", "BETANCOURT, ISMAEL, JR.", 0, 26650, 0,
  "NY", 1994, "S", 1, "G", 2000, "Other", "SWP", "CRAINE, NAOMI L.", 0, 14892, 0,
  "PA", 1994, "S", 1, "G", 2000, "Lbt", "Lbt", "ERNSBERGER, DONALD C.", 0, 59115, 0,
  "TN", 1994, "S", 2, "S", 1996, "I", "I", "LYTLE, TERRY L.", 0, 1934, 0,
  "TN", 1994, "S", 2, "S", 1996, "I", "I", "WALLS, JON", 0, 1532, 0,
  "TN", 1994, "S", 2, "S", 1996, "I", "I", "LUMPKIN, HOBART R.", 0, 1184, 0,
  "TN", 1994, "S", 2, "S", 1996, "I", "I", "SCHNELLER, DON", 0, 1150, 0,
  "UT", 1994, "S", 1, "G", 2000, "Other", "SWP", "GONZALEZ, NELSON", 0, 1514, 0,
  "VT", 1994, "S", 1, "G", 2000, "Other", "Natural Law", "PARDO, JOSEPH VICTOR", 0, 709, 0,
  "VA", 1994, "S", 1, "G", 2000, "I", "I", "WILDER, L. DOUGLAS", 0, 113, 0
)

cand <- cand |>
  rows_insert(adds_1994, by = c("year", "state", "office", "dist", "type", "name_snyder"), conflict = "error")

## Louisiana open primary ----
# all seven seats were decided in the October 1 primary
la_1994 <- tribble(
  ~state, ~year, ~office, ~dist, ~type, ~nextup, ~party, ~party_formal, ~name_snyder, ~inc, ~vote_g, ~w_g,
  "LA", 1994, "H", 1, "G", 1996, "R", "R", "LIVINGSTON, ROBERT L. (BOB)", 1, 83928, 1,
  "LA", 1994, "H", 1, "G", 1996, "D", "D", "MCNEIR, FOREST", 0, 12336, 0,
  "LA", 1994, "H", 1, "G", 1996, "Other", "Other", "SIMMONS, CLARK", 0, 7139, 0,
  "LA", 1994, "H", 2, "G", 1996, "D", "D", "JEFFERSON, WILLIAM J. (BILL)", 1, 60906, 1,
  "LA", 1994, "H", 2, "G", 1996, "R", "R", "NAMER, ROBERT (BOB)", 0, 15113, 0,
  "LA", 1994, "H", 2, "G", 1996, "Other", "Other", "LAWRENCE, JOHN C.", 0, 3036, 0,
  "LA", 1994, "H", 2, "G", 1996, "D", "D", "LEAHMAN, JULIUS (UNCLE CHIP)", 0, 2513, 0,
  "LA", 1994, "H", 3, "G", 1996, "D", "D", "TAUZIN, WILBERT J. (BILLY)", 1, 90536, 1,
  "LA", 1994, "H", 3, "G", 1996, "Other", "Other", "ACCARDO, NICK JOSEPH", 0, 28250, 0,
  "LA", 1994, "H", 4, "G", 1996, "D", "D", "FIELDS, CLEO", 1, 88288, 1,
  "LA", 1994, "H", 4, "G", 1996, "R", "R", "SLOCUM, PATRICIA", 0, 37935, 0,
  "LA", 1994, "H", 5, "G", 1996, "R", "R", "MCCRERY, JAMES O. (JIM)", 1, 106204, 1,
  "LA", 1994, "H", 5, "G", 1996, "D", "D", "KIDD, PAUL HENRY", 0, 21467, 0,
  "LA", 1994, "H", 5, "G", 1996, "Other", "Other", "SIMMONS, E. AUSTIN", 0, 5365, 0,
  "LA", 1994, "H", 6, "G", 1996, "R", "R", "BAKER, RICHARD HUGH", 1, 103174, 1,
  "LA", 1994, "H", 6, "G", 1996, "D", "D", "WARD, DARRYL PAUL", 0, 24033, 0,
  "LA", 1994, "H", 7, "G", 1996, "D", "D", "HAYES, JAMES A. (JIMMY)", 1, 72426, 1,
  "LA", 1994, "H", 7, "G", 1996, "R", "R", "HOLLOWAY, CLYDE C.", 0, 54256, 0,
  "LA", 1994, "H", 7, "G", 1996, "Other", "Other", "CAESAR, RON", 0, 9938, 0
) |>
  mutate(runoff = 0)

cand <- cand |>
  rows_upsert(la_1994, by = c("year", "state", "office", "dist", "type", "name_snyder"))

# 5. Clerk audit, 1996 ----

## Votes ----
votes_1996 <- tribble(
  ~year, ~state, ~office, ~dist, ~type, ~name_snyder, ~vote_g,
  1996, "AL", "H", 1, "G", "WOMACK, DON", 69470,  # p. 2
  1996, "AL", "H", 6, "G", "VOGEL, DIANE SUSAN", 2113,  # p. 2
  1996, "CA", "H", 1, "G", "RIGGS, FRANK DUNCAN", 110243,  # p. 7
  1996, "CA", "H", 7, "G", "THOMPSON, WILLIAM C.", 6866,  # p. 7
  1996, "CA", "H", 14, "G", "BRINK, BENJAMIN MCALESTER (BEN)", 71571,  # p. 8
  1996, "CA", "H", 14, "G", "ESHOO, ANNA G.", 148773,  # p. 8
  1996, "CO", "S", 2, "G", "ALLARD, A. WAYNE", 750315,  # p. 13
  1996, "CO", "S", 2, "G", "STRICKLAND, THOMAS LEE", 667600,  # p. 13
  1996, "FL", "H", 1, "G", "SCARBOROUGH, CHARLES JOSEPH (JOE)", 175946,  # p. 17
  1996, "FL", "H", 1, "G", "BECK, KEVIN TIMOTHY", 66495,  # p. 17
  1996, "FL", "H", 2, "G", "SUTTON, BILL", 94122,  # p. 17
  1996, "FL", "H", 2, "G", "BOYD, F. ALLEN, JR.", 138151,  # p. 17
  1996, "FL", "H", 3, "G", "FIELDS, PRESTON JAMES", 62196,  # p. 17
  1996, "FL", "H", 3, "G", "BROWN, CORRINE", 98085,  # p. 17
  1996, "FL", "H", 5, "G", "GENTRY, RAYMOND DAVIS, JR.", 100051,  # p. 17
  1996, "FL", "H", 5, "G", "THURMAN, KAREN L.", 161050,  # p. 17
  1996, "FL", "H", 6, "G", "STEARNS, CLIFFORD B. (CLIFF)", 161527,  # p. 17
  1996, "FL", "H", 6, "G", "O'BRIEN, NEWELL WILLIAM", 78908,  # p. 17
  1996, "FL", "H", 7, "G", "MICA, JOHN L.", 143667,  # p. 17
  1996, "FL", "H", 7, "G", "STUART, GEORGE LEWIS, JR.", 87832,  # p. 17
  1996, "FL", "H", 8, "G", "MCCOLLUM, BILL", 136515,  # p. 18
  1996, "FL", "H", 8, "G", "KRULICK, ALAN D. (AL)", 65794,  # p. 18
  1996, "FL", "H", 9, "G", "BILIRAKIS, MICHAEL", 161708,  # p. 18
  1996, "FL", "H", 9, "G", "PROVENZANO, JEROME THOMAS", 73809,  # p. 18
  1996, "FL", "H", 10, "G", "YOUNG, C.W. (BILL)", 114443,  # p. 18
  1996, "FL", "H", 10, "G", "GREEN, HENRY", 57375,  # p. 18
  1996, "FL", "H", 11, "G", "SHARPE, MARK STEPHEN", 78881,  # p. 18
  1996, "FL", "H", 11, "G", "DAVIS, JAMES OSCAR (JIM), III", 108522,  # p. 18
  1996, "FL", "H", 13, "G", "MILLER, DAN", 173671,  # p. 18
  1996, "FL", "H", 13, "G", "GORDON, SANFORD D.", 96098,  # p. 18
  1996, "FL", "H", 14, "G", "GOSS, PORTER JOHNSTON", 176992,  # p. 18
  1996, "FL", "H", 14, "G", "NOLAN, JAMES H. (JIM)", 63842,  # p. 18
  1996, "FL", "H", 15, "G", "WELDON, DAVID JOSEPH (DAVE)", 139014,  # p. 18
  1996, "FL", "H", 15, "G", "BYRON, JOHN L.", 115981,  # p. 18
  1996, "FL", "H", 16, "G", "FOLEY, MARK ADAM", 175714,  # p. 18
  1996, "FL", "H", 16, "G", "STUBER, JAMES A. (JIM)", 98827,  # p. 18
  1996, "FL", "H", 17, "G", "ROLLE, WELLINGTON", 14525,  # p. 18
  1996, "FL", "H", 17, "G", "MEEK, CARRIE P.", 114638,  # p. 18
  1996, "FL", "H", 18, "G", "ROS-LEHTINEN, ILEANA", 123659,  # p. 18
  1996, "FL", "H", 19, "G", "KENNEDY, BEVERLY B. (BEV)", 99101,  # p. 18
  1996, "FL", "H", 19, "G", "WEXLER, ROBERT", 188766,  # p. 18
  1996, "FL", "H", 20, "G", "JACOBS, JAMES MADISON", 85777,  # p. 18
  1996, "FL", "H", 20, "G", "DEUTSCH, PETER RUSSELL", 159256,  # p. 18
  1996, "FL", "H", 21, "G", "DIAZ-BALART, LINCOLN", 125469,  # p. 18
  1996, "FL", "H", 22, "G", "SHAW, EUGENE CLAY, JR.", 137098,  # p. 18
  1996, "FL", "H", 22, "G", "COOPER, KENNETH DONALD", 84517,  # p. 18
  1996, "FL", "H", 23, "G", "BROWN, ROBERT PAUL (BOB)", 36907,  # p. 18
  1996, "FL", "H", 23, "G", "HASTINGS, ALCEE LAMAR", 102161,  # p. 18
  1996, "GA", "H", 3, "G", "CHAFIN, JAMES TROY, III", 76538,  # p. 19
  1996, "MA", "S", 2, "G", "STOWE, ROBERT C.", 7169,  # p. 30
  1996, "DE", "H", 1, "G", "CASTLE, MICHAEL N.", 185577,  # p. 17
  1996, "DE", "H", 1, "G", "WILLIAMS, DENNIS EDWARD", 73258,  # p. 17
  1996, "MN", "S", 2, "G", "BOSCHWITZ, RUDOLF ELY (RUDY)", 901194,  # p. 35
  1996, "MN", "S", 2, "G", "WELLSTONE, PAUL D.", 1098430,  # p. 35
  1996, "MN", "S", 2, "G", "BARKLEY, DEAN M.", 152328,  # p. 35
  1996, "MN", "S", 2, "G", "HANSON, HOWARD B.", 4381,  # p. 35
  1996, "MN", "H", 1, "G", "GUTKNECHT, GILBERT W. (GIL)", 137486,  # p. 35
  1996, "MN", "H", 1, "G", "RIEDER, MARY ELIZABETH", 123147,  # p. 35
  1996, "MN", "H", 3, "G", "RAMSTAD, JAMES M. (JIM)", 205816,  # p. 35
  1996, "MN", "H", 3, "G", "LEINO, STANLEY JAMES", 87350,  # p. 35
  1996, "MN", "H", 5, "G", "ULDRICH, JOHN M. (JACK)", 70109,  # p. 35
  1996, "MN", "H", 5, "G", "SABO, MARTIN OLAV", 158261,  # p. 35
  1996, "MN", "H", 6, "G", "JUDE, TAD", 129988,  # p. 35
  1996, "NJ", "S", 2, "G", "ZIMMER, RICHARD A. (DICK)", 1227351,  # p. 41
  1996, "NJ", "S", 2, "G", "TORRICELLI, ROBERT G.", 1519154,  # p. 41
  1996, "NJ", "H", 13, "G", "MUNOZ, CARLOS E.", 25427,  # p. 42
  1996, "NY", "H", 20, "G", "GOODMAN, IRA W.", 5016,  # p. 46
  1996, "NY", "H", 22, "G", "JAMES, STEVE", 94192,  # p. 46
  1996, "OH", "H", 5, "G", "GILLMOR, PAUL E.", 145892,  # p. 51
  1996, "OH", "H", 13, "G", "BROWN, SHERROD C.", 148690,  # p. 51
  1996, "PA", "H", 3, "G", "BORSKI, ROBERT A. (BOB), JR.", 121120,  # p. 57
  1996, "PA", "H", 15, "G", "MCHALE, PAUL F., JR.", 109812,  # p. 58
  1996, "RI", "S", 2, "G", "LOVEJOY, DONALD W.", 6327,  # p. 60
  1996, "SC", "S", 2, "G", "THURMOND, JAMES STROM, SR.", 619859,  # p. 60
  1996, "SC", "S", 2, "G", "CLOSE, ELLIOTT SPRINGS", 510951,  # p. 60
  1996, "SC", "H", 3, "G", "GRAHAM, LINDSEY O.", 114273,  # p. 60
  1996, "SC", "H", 5, "G", "BIGHAM, LARRY L.", 81455,  # p. 61
  1996, "SC", "H", 5, "G", "SPRATT, JOHN M., JR.", 97335,  # p. 61
  1996, "TN", "H", 7, "G", "BRYANT, EDWARD G.", 126737,  # p. 62
  1996, "TN", "H", 7, "G", "TROTTER, DONALD W. (DON)", 64512,  # p. 62
  1996, "WA", "H", 1, "G", "WHITE, RICHARD", 141948   # p. 73
)

cand <- cand |>
  rows_update(votes_1996, by = c("year", "state", "office", "dist", "type", "name_snyder"), unmatched = "error") |>
  rows_update(tribble(
    ~year, ~state, ~office, ~dist, ~type, ~name_snyder, ~vote_g,
    1996, "FL", "H", 12, "G", "CANADY, CHARLES T.", 122584,
    1996, "FL", "H", 12, "G", "CANADY, MICHAEL L. (MIKE)", 76513
  ), by = c("year", "state", "office", "dist", "type", "name_snyder"), unmatched = "error")

## Misplaced rows ----
cand <- cand |>
  tidylog::mutate(
    dist = replace(dist, year == 1996 & state == "OR" & name_snyder == "GUILLEBEAU, VICTORIA P.", 3),
    name_snyder = replace(name_snyder, state == "MO" & name_snyder == "TIAPEK, GREG", "TLAPEK, GREG J.")
  )

## Candidates missing from Snyder ----
# MO-8 had a special and a regular race on Nov 5; Snyder has only the special, where Emerson ran as a Republican
adds_1996 <- tribble(
  ~state, ~year, ~office, ~dist, ~type, ~nextup, ~party, ~party_formal, ~name_snyder, ~inc, ~vote_g, ~w_g,
  "VT", 1996, "H", 1, "G", 1998, "I", "I", "SANDERS, BERNARD (BERNIE)", 1, 140678, 1,
  "VT", 1996, "H", 1, "G", 1998, "Other", "Liberty Union", "DIAMONDSTONE, PETER ISAAC (PETE)", 0, 1965, 0,
  "DE", 1996, "H", 1, "G", 1998, "Other", "Natural Law", "MATTSON, ROBERT E.", 0, 987, 0,
  "NY", 1996, "H", 4, "G", 1998, "Other", "Right To Life", "GARBITELLI, VINCENT P.", 0, 3252, 0,
  "AZ", 1996, "H", 5, "G", 1998, "Other", "Reform", "FINKELSTEIN, ED", 0, 6630, 0,
  "VT", 1996, "H", 1, "G", 1998, "Lbt", "Lbt", "MORSE, THOMAS J.", 0, 2693, 0,
  "VT", 1996, "H", 1, "G", 1998, "Other", "Vermont Grass Roots", "MELAMEDE, ROBERT (DOC BOB)", 0, 1350, 0,
  "AR", 1996, "H", 3, "G", 1998, "Other", "W-I", "IVY, DANNY CHRIS (DAN)", 0, 71, 0,
  "IL", 1996, "H", 2, "G", 1998, "Lbt", "Lbt", "STRATMAN, FRANK H.", 0, 10880, 0,
  "NY", 1996, "H", 8, "G", 1998, "Other", "Conservative", "GALIP, GEORGE A., JR.", 0, 2381, 0,
  "VT", 1996, "H", 1, "G", 1998, "R", "R", "SWEETSER, SUSAN W.", 0, 83021, 0,
  "VT", 1996, "H", 1, "G", 1998, "D", "D", "LONG, JACK", 0, 23830, 0,
  "VT", 1996, "H", 1, "G", 1998, "Other", "Natural Law", "KUSHI, NORIO", 0, 812, 0,
  "MO", 1996, "H", 8, "G", 1998, "I", "I", "EMERSON, JO ANN", 0, 112472, 1,
  "MO", 1996, "H", 8, "G", 1998, "D", "D", "FIREBAUGH, EMILY R.", 0, 83084, 0,
  "MO", 1996, "H", 8, "G", 1998, "R", "R", "KLINE, RICHARD ALLEN", 0, 23477, 0,
  "MO", 1996, "H", 8, "G", 1998, "Lbt", "Lbt", "TLAPEK, GREG J.", 0, 2503, 0,
  "MO", 1996, "H", 8, "G", 1998, "Other", "Natural Law", "ZIMMER, DAVID R.", 0, 1318, 0
)

cand <- cand |>
  rows_insert(adds_1996, by = c("year", "state", "office", "dist", "type", "name_snyder"), conflict = "error")

## Texas general elections and runoffs ----
# TX's 13 court-ordered 1996 elections filled full terms: code G, retaining December runoff flags.
# Clerk figures for the Dec 10 runoffs in TX-8, 9 and 25; the SOS tabulation is unofficial.
cand <- cand |>
  mutate(type = case_when(
    year == 1996 & state == "TX" & office == "H" &
      dist %in% c(3, 5, 6, 7, 8, 9, 18, 22, 24, 25, 26, 29, 30) ~ "G",
    .default = type
  )) |>
  tidylog::filter(!(year == 1996 & state == "TX" & office == "H" & dist %in% c(8, 9, 25))) |>
  bind_rows(tribble(
    ~state, ~year, ~office, ~dist, ~type, ~nextup, ~party, ~party_formal, ~name_snyder, ~inc, ~vote_g, ~w_g, ~runoff,
    "TX", 1996, "H", 8, "G", 1998, "R", "R", "BRADY, KEVIN P.", 0, 30366, 1, 1,
    "TX", 1996, "H", 8, "G", 1998, "R", "R", "FONTENOT, EUGENE J. (GENE)", 0, 21004, 0, 1,
    "TX", 1996, "H", 9, "G", 1998, "D", "D", "LAMPSON, NICHOLAS V. (NICK)", 0, 59225, 1, 1,
    "TX", 1996, "H", 9, "G", 1998, "R", "R", "STOCKMAN, STEPHEN E. (STEVE)", 1, 52870, 0, 1,
    "TX", 1996, "H", 25, "G", 1998, "D", "D", "BENTSEN, KENNETH E. (KEN)", 1, 29396, 1, 1,
    "TX", 1996, "H", 25, "G", 1998, "R", "R", "MCKENNA, DOLLY MADISON", 0, 21892, 0, 1
  ))

## Louisiana ----
# September 21 primary decided districts 4 and 6; the Senate seat, 5 and 7 went to the November round (SOS)
la_1996 <- tribble(
  ~state, ~year, ~office, ~dist, ~type, ~nextup, ~party, ~party_formal, ~name_snyder, ~inc, ~vote_g, ~w_g, ~runoff,
  "LA", 1996, "H", 4, "G", 1998, "R", "R", "MCCRERY, JAMES O. (JIM)", 1, 94822, 1, 0,
  "LA", 1996, "H", 4, "G", 1998, "D", "D", "CHACHERE, PAUL M.", 0, 38015, 0, 0,
  "LA", 1996, "H", 6, "G", 1998, "R", "R", "BAKER, RICHARD HUGH", 1, 117598, 1, 0,
  "LA", 1996, "H", 6, "G", 1998, "D", "D", "MYERS, STEVE", 0, 52092, 0, 0
)

cand <- cand |>
  rows_upsert(la_1996, by = c("year", "state", "office", "dist", "type", "name_snyder")) |>
  tidylog::mutate(runoff = replace(runoff, year == 1996 & state == "LA" & (office == "S" | dist %in% c(5, 7)), 1))

# 6. Clerk audit, 1998 ----

## Votes ----
# Louisiana held its open primary on the November date from 1998; NY-6 and NY-25 lacked their minor-party lines
votes_1998 <- tribble(
  ~year, ~state, ~office, ~dist, ~type, ~name_snyder, ~vote_g,
  1998, "AL", "H", 4, "G", "BEVILL, DONALD H. (DON)", 82065,  # p. 2
  1998, "CA", "S", 3, "G", "FONG, MATTHEW K. (MATT)", 3575078,  # p. 4
  1998, "CA", "S", 3, "G", "BOXER, BARBARA", 4410056,  # p. 4
  1998, "CA", "S", 3, "G", "BROWN, T.E. (TED), JR.", 93926,  # p. 4
  1998, "CA", "S", 3, "G", "REES, BRIAN M.", 46543,  # p. 4
  1998, "CA", "S", 3, "G", "ERICH, TIMOTHY R.", 82918,  # p. 4
  1998, "CA", "S", 3, "G", "PERRIN, H. JOSEPH, SR.", 54699,  # p. 4
  1998, "CA", "S", 3, "G", "BELTRAN, OPHIE C.", 48685,  # p. 4
  1998, "CA", "H", 1, "G", "LUCE, MARK CHRISTOPHER", 64692,  # p. 4
  1998, "CA", "H", 22, "G", "PORTER, RICHARD D. (DICK)", 1241,  # p. 5
  1998, "CA", "H", 23, "G", "GALLEGLY, ELTON WILLIAM", 96322,  # p. 5
  1998, "CA", "H", 23, "G", "GONZALEZ, DANIEL RICARDO (DAN)", 64032,  # p. 5
  1998, "GA", "H", 2, "G", "MCCORMICK, JOSEPH F.", 59305,  # p. 11
  1998, "GA", "H", 2, "G", "BISHOP, SANFORD DIXON, JR.", 77953,  # p. 11
  1998, "GA", "H", 8, "G", "CHAMBLISS, C. SAXBY", 87993,  # p. 11
  1998, "IA", "H", 4, "G", "GANSKE, JOHN G. (GREG)", 129942,  # p. 15
  1998, "IA", "H", 4, "G", "DVORAK, JOHN E.", 67550,  # p. 15
  1998, "LA", "S", 3, "G", "BREAUX, JOHN B.", 620502,  # p. 17
  1998, "LA", "H", 2, "G", "JEFFERSON, WILLIAM J. (BILL)", 102247,  # p. 17
  1998, "LA", "H", 6, "G", "BAKER, RICHARD HUGH", 97044,  # p. 17
  1998, "MD", "H", 5, "G", "HOYER, STENY HAMILTON", 126792,  # p. 18
  1998, "MS", "H", 2, "G", "THOMPSON, BENNIE G.", 80284,  # p. 22
  1998, "MS", "H", 5, "G", "MCDONNELL, RANDY", 19341,  # p. 22
  1998, "NV", "S", 3, "G", "ENSIGN, JOHN ERIC", 208220,  # p. 24
  1998, "NV", "S", 3, "G", "REID, HARRY M.", 208621,  # p. 24
  1998, "NV", "S", 3, "G", "CLOUD, MICHAEL", 8129,  # p. 24
  1998, "NV", "S", 3, "G", "WILLIAMS, MICHAEL E.", 2781,  # p. 24
  1998, "NY", "H", 15, "G", "MCMANUS, PATRICK A.", 22052,  # p. 28
  1998, "NY", "H", 6, "G", "MEEKS, GREGORY W.", 76122,  # p. 27
  1998, "NY", "H", 24, "G", "TALLON, CORNELIUS P. (NEIL)", 31011,  # p. 29
  1998, "NY", "H", 25, "G", "TASKER-ROTHENBERG, YVONNE", 53461,  # p. 29
  1998, "NY", "H", 27, "G", "COOK, WILLIAM R. (BILL)", 75978,  # p. 29
  1998, "OH", "H", 14, "G", "WATKINS, THOMAS (TOM)", 63027,  # p. 34
  1998, "SC", "S", 3, "G", "INGLIS, ROBERT D. (BOB)", 488238,  # p. 39
  1998, "SC", "S", 3, "G", "HOLLINGS, ERNEST FRITZ", 563377,  # p. 39
  1998, "SC", "S", 3, "G", "QUILLIAN, RICHARD T.", 16991,  # p. 39
  1998, "SC", "H", 2, "G", "SPENCE, FLOYD DAVIDSON", 119538,  # p. 39
  1998, "SC", "H", 5, "G", "BURKHOLD, MICHAEL B., JR.", 66367,  # p. 39
  1998, "SC", "H", 5, "G", "SPRATT, JOHN M., JR.", 95696,  # p. 39
  1998, "SC", "H", 5, "G", "NEVINS, DIANNE L.", 2765,  # p. 39
  1998, "TX", "H", 7, "G", "ARCHER, WILLIAM R. (BILL)", 111010,  # p. 41
  1998, "TX", "H", 14, "G", "SNEARY, LOY E.", 68404,  # p. 42
  1998, "TX", "H", 22, "G", "DELAY, THOMAS D. (TOM)", 87840,  # p. 42
  1998, "UT", "H", 1, "G", "HANSEN, JAMES V.", 109708,  # p. 43
  1998, "VT", "S", 3, "G", "MELAMEDE, ROBERT (DOC BOB)", 2459,  # p. 44
  1998, "VT", "H", 1, "G", "SANDERS, BERNARD (BERNIE)", 136403,  # p. 44
  1998, "CA", "H", 22, "G", "BORDONARO, TOM J., JR.", 85927,  # p. 5
  1998, "CA", "H", 22, "G", "CAPPS, LOIS G.", 109517,  # p. 5
  1998, "CA", "H", 22, "G", "BAKHAUS, ROBERT O.", 2579   # p. 5
)

cand <- cand |>
  rows_update(votes_1998, by = c("year", "state", "office", "dist", "type", "name_snyder"), unmatched = "error")

## Candidates missing from Snyder ----
adds_1998 <- tribble(
  ~state, ~year, ~office, ~dist, ~type, ~nextup, ~party, ~party_formal, ~name_snyder, ~inc, ~vote_g, ~w_g,
  "FL", 1998, "H", 1, "G", 2000, "Other", "W-I", "WELLS, TOM", 0, 663, 0,
  "AK", 1998, "S", 3, "G", 2004, "Lbt", "Lbt", "KOHLHAAS, SCOTT A.", 0, 5046, 0,
  "LA", 1998, "S", 3, "G", 2004, "R", "R", "WARD, DARRYL PAUL", 0, 7964, 0,
  "LA", 1998, "S", 3, "G", 2004, "D", "D", "MELTON, SAM HOUSTON, JR.", 0, 9893, 0,
  "LA", 1998, "S", 3, "G", 2004, "Other", "Other", "KNOX, L.D.", 0, 6366, 0,
  "LA", 1998, "S", 3, "G", 2004, "Other", "Other", "BROWN, RAYMOND", 0, 12203, 0,
  "LA", 1998, "H", 2, "G", 2000, "D", "D", "REED, DAVID", 0, 10803, 0,
  "IL", 1998, "S", 3, "G", 2004, "Other", "U.S. Taxpayers", "STALKER, RAYMOND W.", 0, 280, 0,
  "LA", 1998, "S", 3, "G", 2004, "R", "R", "DONELON, JIM", 0, 306616, 0,
  "LA", 1998, "S", 3, "G", 2004, "Other", "Other", "DIKET, JEFFREY H.", 0, 3227, 0,
  "LA", 1998, "S", 3, "G", 2004, "Other", "Other", "ROSENTHAL, MARTIN A.", 0, 2394, 0,
  "LA", 1998, "H", 2, "G", 2000, "D", "D", "VEAL, DON-TERRY", 0, 5899, 0,
  "LA", 1998, "H", 6, "G", 2000, "D", "D", "MCKEITHEN, MARJORIE", 0, 94201, 0,
  "OH", 1998, "H", 11, "G", 2000, "I", "I", "CAPERS, JEAN MURRELL", 0, 9477, 0,
  "OH", 1998, "H", 15, "G", 2000, "I", "I", "NESTOR, KEVIN", 0, 9996, 0,
  "OR", 1998, "H", 5, "G", 2000, "Other", "Natural Law", "BURNS, JIM", 0, 2284, 0
)

cand <- cand |>
  rows_insert(adds_1998, by = c("year", "state", "office", "dist", "type", "name_snyder"), conflict = "error")

# 7. Clerk audit, 2000 ----

## Votes ----
# Louisiana's November open primary; NY Democrats lacked their Green and Working Families lines
votes_2000 <- tribble(
  ~year, ~state, ~office, ~dist, ~type, ~name_snyder, ~vote_g,
  2000, "AL", "H", 2, "G", "MCGAHAN, WALLACE BYRON, JR.", 4111,  # p. 2
  2000, "CA", "H", 39, "G", "KANEL, GIL G.", 64398,  # p. 8
  2000, "GA", "S", 3, "S", "MILLER, ZELL BRYAN", 1413224,  # p. 17
  2000, "IL", "H", 2, "G", "GORDON, ROBERT, III", 19906,  # p. 19
  2000, "KS", "H", 1, "G", "MORAN, JERRY W.", 214328,  # p. 24
  2000, "KS", "H", 1, "G", "WARNER, JACK W.", 25581,  # p. 24
  2000, "LA", "H", 1, "G", "VITTER, DAVID B.", 191379,  # p. 25
  2000, "LA", "H", 3, "G", "TAUZIN, WILBERT J. (BILLY)", 143446,  # p. 25
  2000, "LA", "H", 4, "G", "MCCRERY, JAMES O. (JIM)", 122678,  # p. 25
  2000, "LA", "H", 5, "G", "COOKSEY, JOHN CHARLES", 123975,  # p. 25
  2000, "LA", "H", 6, "G", "BAKER, RICHARD HUGH", 165637,  # p. 25
  2000, "LA", "H", 7, "G", "JOHN, CHRISTOPHER (CHRIS)", 152796,  # p. 25
  2000, "MI", "H", 8, "G", "ROGERS, MICHAEL J. (MIKE)", 145179,  # p. 31
  2000, "MI", "H", 8, "G", "BYRUM, DIANNE", 145019,  # p. 31
  2000, "MI", "H", 8, "G", "BUCQUEROUX, BONNIE L.", 3484,  # p. 31
  2000, "MI", "H", 8, "G", "EYSTER, JAMES PARRY", 2443,  # p. 31
  2000, "MI", "H", 8, "G", "GUALDONI, FRANCISCO R. (RICK)", 695,  # p. 31
  2000, "MI", "H", 8, "G", "ALLEN, PATRICIA RAYFIELD", 713,  # p. 31
  2000, "MO", "H", 4, "G", "NOLAND, JAMES A. (JIM), JR.", 84406,  # p. 36
  2000, "NJ", "H", 1, "G", "PARRISH, CATHERINE L.", 3090,  # p. 41
  2000, "NJ", "H", 10, "G", "WILLIAMS, MAURICE", 536,  # p. 42
  2000, "NY", "H", 3, "G", "LAMAGNA, DAL ANTHONY", 95787,  # p. 44
  2000, "NY", "H", 8, "G", "NADLER, JERROLD L. (JERRY)", 150291,  # p. 45
  2000, "NY", "H", 16, "G", "JUSTICE, AARON", 3943,  # p. 46
  2000, "NY", "H", 20, "G", "FEINER, PAUL J.", 94646,  # p. 46
  2000, "NY", "H", 22, "G", "MCCALLION, KENNETH F.", 79111,  # p. 46
  2000, "NY", "H", 30, "G", "FEE, JOHN", 67819,  # p. 47
  2000, "OH", "H", 10, "G", "SMITH, BILL", 48930,  # p. 51
  2000, "OH", "H", 10, "G", "KUCINICH, DENNIS J.", 167063,  # p. 51
  2000, "OH", "H", 10, "G", "PETRIE, RON", 6762,  # p. 51
  2000, "OH", "H", 11, "G", "JONES, STEPHANIE TUBBS", 164134,  # p. 51
  2000, "OH", "H", 11, "G", "TURNER, JOEL C.", 4230,  # p. 51
  2000, "OH", "H", 11, "G", "GLAVINA, SONJA K.", 3525,  # p. 51
  2000, "SC", "H", 2, "G", "SPENCE, FLOYD DAVIDSON", 154338,  # p. 58
  2000, "SC", "H", 2, "G", "FREDERICK, LINNA JANE Y.", 110672,  # p. 58
  2000, "SC", "H", 2, "G", "MOULTRIE, TIMOTHY NATHAN", 3622,  # p. 58
  2000, "SC", "H", 2, "G", "TAYLOR, GEORGE C.", 2273,  # p. 58
  2000, "SC", "H", 3, "G", "GRAHAM, LINDSEY O.", 150180,  # p. 58
  2000, "SC", "H", 3, "G", "BRIGHTHARP, GEORGE L.", 67170,  # p. 58
  2000, "TX", "S", 1, "G", "HUTCHISON, KAY BAILEY", 4078954,  # p. 60
  2000, "TX", "S", 1, "G", "KELLY, EUGENE EDWARD (GENE)", 2025024,  # p. 60
  2000, "TX", "S", 1, "G", "RUWART, MARY J.", 72657,  # p. 60
  2000, "TX", "S", 1, "G", "SANDAGE, DOUGLAS S.", 91329   # p. 60
)

cand <- cand |>
  rows_update(votes_2000, by = c("year", "state", "office", "dist", "type", "name_snyder"), unmatched = "error")

## Candidates missing from Snyder ----
# VA-5 is Goode's first race as an independent; the GA special was nonpartisan, minor candidates coded I
adds_2000 <- tribble(
  ~state, ~year, ~office, ~dist, ~type, ~nextup, ~party, ~party_formal, ~name_snyder, ~inc, ~vote_g, ~w_g,
  "VT", 2000, "H", 1, "G", 2002, "I", "I", "SANDERS, BERNARD (BERNIE)", 1, 196118, 1,
  "NJ", 2000, "S", 1, "G", 2006, "I", "I", "PASON, GREGORY", 0, 3365, 0,
  "VA", 2000, "H", 5, "G", 2002, "I", "I", "GOODE, VIRGIL H., JR.", 1, 143312, 1,
  "MD", 2000, "H", 8, "G", 2002, "Other", "W-I (D)", "YOUNG, LIH Y.", 0, 77, 0,
  "VT", 2000, "S", 1, "G", 2006, "Other", "Liberty Union", "LEVY, JERRY E.", 0, 1477, 0,
  "MD", 2000, "S", 1, "G", 2006, "Other", "W-I", "VAUGHN, CORROGAN R.", 0, 113, 0,
  "LA", 2000, "H", 5, "G", 2002, "D", "D", "MELTON, SAM HOUSTON, JR.", 0, 7186, 0,
  "LA", 2000, "H", 1, "G", 2002, "D", "D", "DEATON, CARY J.", 0, 10982, 0,
  "LA", 2000, "H", 1, "G", 2002, "Other", "Other", "ROSENTHAL, MARTIN A.", 0, 3129, 0,
  "NJ", 2000, "S", 1, "G", 2006, "I", "I", "LANEVE, LORRAINE L.", 0, 3836, 0,
  "VT", 2000, "H", 1, "G", 2002, "Other", "Vermont Grassroots", "ROGERS, JACK ALBERT (BUCK)", 0, 4799, 0,
  "VT", 2000, "H", 1, "G", 2002, "Lbt", "Lbt", "KRYMKOWSKI, DANIEL H.", 0, 2978, 0,
  "LA", 2000, "H", 3, "G", 2002, "Other", "Other", "ROSENTHAL, ANITA W.", 0, 13488, 0,
  "LA", 2000, "H", 7, "G", 2002, "Other", "Other", "HARRIS, MICHAEL P.", 0, 30687, 0,
  "MD", 2000, "H", 6, "G", 2002, "Grn", "Grn", "MCCOWN, TIM", 0, 82, 0,
  "MD", 2000, "H", 8, "G", 2002, "Other", "W-I", "WALKER, SCOTT", 0, 19, 0,
  "VA", 2000, "H", 5, "G", 2002, "D", "D", "BOYD, JOHN W., JR.", 0, 65387, 0,
  "LA", 2000, "H", 1, "G", 2002, "D", "D", "ARMATO, MICHAEL A.", 0, 29935, 0,
  "LA", 2000, "H", 1, "G", 2002, "Other", "Other", "SIMANONOK, JOHN PAUL (JACK)", 0, 2385, 0,
  "LA", 2000, "H", 3, "G", 2002, "Other", "Other", "ALBARES, EDWIN J. (EDDIE)", 0, 16908, 0,
  "LA", 2000, "H", 3, "G", 2002, "Other", "Other", "BOURQUE, DION", 0, 10118, 0,
  "LA", 2000, "H", 4, "G", 2002, "D", "D", "GREEN, PHILLIP R.", 0, 43600, 0,
  "LA", 2000, "H", 4, "G", 2002, "Other", "Other", "SKAINS, JAMES RONALD", 0, 3630, 0,
  "LA", 2000, "H", 4, "G", 2002, "Other", "Other", "TAYLOR, MICHAEL (MIKE)", 0, 4059, 0,
  "LA", 2000, "H", 5, "G", 2002, "D", "D", "BEALL, ROGER", 0, 42977, 0,
  "LA", 2000, "H", 5, "G", 2002, "Other", "Other", "DUMAS, RAYMOND A. (CHUCK)", 0, 5335, 0,
  "LA", 2000, "H", 6, "G", 2002, "D", "D", "ROGILLIO, KATHY J.", 0, 72192, 0,
  "LA", 2000, "H", 6, "G", 2002, "Other", "Other", "WOLF, MICHAEL S.", 0, 5649, 0,
  "MD", 2000, "H", 1, "G", 2002, "Grn", "Grn", "GROSS, DAVID M.", 0, 73, 0,
  "NV", 2000, "S", 1, "G", 2006, "Other", "Independent American", "BERGHOF, ERNIE", 0, 2540, 0,
  "NV", 2000, "S", 1, "G", 2006, "Other", "Citizens First", "GRUTZMACHER, BILL", 0, 1579, 0,
  "NJ", 2000, "S", 1, "G", 2006, "I", "I", "ROSENSTOCK, NANCY", 0, 3309, 0,
  "NJ", 2000, "S", 1, "G", 2006, "I", "I", "GOSTIGIAN, GEORGE", 0, 2536, 0,
  "VT", 2000, "H", 1, "G", 2002, "R", "R", "KERIN, KAREN ANN", 0, 51977, 0,
  "VT", 2000, "H", 1, "G", 2002, "I", "I", "SKRILL, STEWART", 0, 11816, 0,
  "VT", 2000, "H", 1, "G", 2002, "D", "D,Liberty Union", "DIAMONDSTONE, PETER ISAAC (PETE)", 0, 14918, 0,
  "VA", 2000, "H", 5, "G", 2002, "I", "I", "SPENCE, JOSEPH S.", 0, 3936, 0,
  "TX", 2000, "H", 17, "G", 2002, "Other", "W-I", "JULIA, PETE", 0, 45, 0,
  "GA", 2000, "S", 3, "S", 2004, "R", "Nonpartisan", "MATTINGLY, MACK F.", 0, 920478, 0,
  "GA", 2000, "S", 3, "S", 2004, "I", "Nonpartisan", "MACGREGOR, PAUL ROBERT", 0, 25942, 0,
  "GA", 2000, "S", 3, "S", 2004, "I", "Nonpartisan", "BALLENGER, BEN", 0, 22975, 0,
  "GA", 2000, "S", 3, "S", 2004, "I", "Nonpartisan", "GATES, JEFF", 0, 21249, 0,
  "GA", 2000, "S", 3, "S", 2004, "I", "Nonpartisan", "WALSH, WINNIE", 0, 11875, 0
)

cand <- cand |>
  rows_insert(adds_2000, by = c("year", "state", "office", "dist", "type", "name_snyder"), conflict = "error") |>
  tidylog::mutate(party_formal = replace(party_formal, year == 2000 & state == "GA" & office == "S" & name_snyder == "MILLER, ZELL BRYAN", "Nonpartisan"))

## Names ----
cand <- cand |>
  tidylog::mutate(
    name_snyder = replace(name_snyder, state == "TX" & name_snyder == "RAYMOND R. (RAY)", "CARR, RAYMOND R. (RAY)"),
    name_snyder = replace(name_snyder, state == "NJ" & name_snyder == "SARGIS, VIJI, L.", "SARGIS, VIJI L.")
  )

# 8. Clerk audit, 2002 ----

## Votes ----
votes_2002 <- tribble(
  ~year, ~state, ~office, ~dist, ~type, ~name_snyder, ~vote_g,
  2002, "AR", "S", 2, "G", "HUTCHINSON, YOUNG TIMOTHY (TIM)", 370735,  # p. 4
  2002, "AR", "S", 2, "G", "PRYOR, MARK LUNSFORD", 433386,  # p. 4
  2002, "LA", "H", 1, "G", "VITTER, DAVID B.", 147117,  # p. 18
  2002, "LA", "H", 2, "G", "JEFFERSON, WILLIAM J. (BILL)", 90310,  # p. 18
  2002, "LA", "H", 3, "G", "TAUZIN, WILBERT J. (BILLY)", 130323,  # p. 18
  2002, "LA", "H", 4, "G", "MCCRERY, JAMES O. (JIM)", 114649,  # p. 18
  2002, "LA", "H", 6, "G", "BAKER, RICHARD HUGH", 146932,  # p. 19
  2002, "LA", "H", 7, "G", "JOHN, CHRISTOPHER (CHRIS)", 138659,  # p. 19
  2002, "NM", "S", 2, "G", "DOMENICI, PETE V.", 314193,  # p. 31
  2002, "NM", "S", 2, "G", "TRISTANI, GLORIA", 168863,  # p. 31
  2002, "NM", "H", 3, "G", "UDALL, TOM S.", 122921,  # p. 31
  2002, "NY", "H", 5, "G", "ACKERMAN, GARY L.", 75810,  # p. 32
  2002, "NY", "H", 28, "G", "WOJTASZEK, HENRY F.", 58547,  # p. 34
  2002, "RI", "S", 2, "G", "TINGLE, ROBERT G. (BOB)", 69808,  # p. 41
  2002, "RI", "S", 2, "G", "REED, JOHN F. (JACK)", 253774,  # p. 41
  2002, "SC", "S", 2, "G", "KOCHER, VICTOR L.", 6648,  # p. 42
  2002, "SC", "H", 6, "G", "MCLEOD, GARY M.", 55490,  # p. 42
  2002, "SC", "H", 6, "G", "CLYBURN, JAMES E. (JIM)", 115855,  # p. 42
  2002, "SC", "H", 6, "G", "AUGENSTEIN, R. CRAIG", 1662,  # p. 42
  2002, "SD", "S", 2, "G", "THUNE, JOHN R.", 166949,  # p. 42
  2002, "SD", "S", 2, "G", "EVANS, KURT", 3071,  # p. 42
  2002, "SD", "H", 1, "G", "BEGAY, TERRY L.", 3117,  # p. 42
  2002, "TN", "S", 2, "G", "ALEXANDER, LAMAR", 891498,  # p. 43
  2002, "TN", "S", 2, "G", "CLEMENT, ROBERT N. (BOB)", 728232,  # p. 43
  2002, "TN", "S", 2, "G", "HOOKER, JOHN JAY", 6401,  # p. 43
  2002, "TN", "S", 2, "G", "BAKER, WESLEY M.", 6106,  # p. 43
  2002, "TN", "S", 2, "G", "GAMMON, CONNIE", 5349,  # p. 43
  2002, "TN", "S", 2, "G", "DAVIDSON, KARL STANLEY", 2217,  # p. 43
  2002, "TN", "S", 2, "G", "MARCEAUX, BASIL J., SR.", 1170,  # p. 43
  2002, "TN", "H", 6, "G", "GARRISON, ROBERT L.", 57397,  # p. 43
  2002, "TN", "H", 6, "G", "GORDON, BART J.", 117119,  # p. 43
  2002, "VT", "H", 1, "G", "SANDERS, BERNARD (BERNIE)", 144880,  # p. 47
  2002, "MD", "H", 5, "G", "AUERBACH, BOB SHIPLEY", 158,  # p. 20
  2002, "SD", "H", 1, "G", "SANDLIN, STEPHANIE HERSETH", 153551   # p. 42
)

cand <- cand |>
  rows_update(votes_2002, by = c("year", "state", "office", "dist", "type", "name_snyder"), unmatched = "error")

## Candidates missing from Snyder ----
# Wellstone died before the election; his absentee votes are certified. FL-5's write-in "Werder" (53) has no given name
adds_2002 <- tribble(
  ~state, ~year, ~office, ~dist, ~type, ~nextup, ~party, ~party_formal, ~name_snyder, ~inc, ~vote_g, ~w_g,
  "OH", 2002, "H", 2, "G", 2004, "Other", "W-I", "CONDIT, JAMES J. (JIM), JR.", 0, 13, 0,
  "FL", 2002, "H", 1, "G", 2004, "Other", "W-I", "WELLS, TOM", 0, 19, 0,
  "AZ", 2002, "H", 5, "G", 2004, "Lbt", "Lbt", "SEVERIN, WARREN D.", 0, 4383, 0,
  "MN", 2002, "S", 2, "G", 2008, "D", "DFL", "WELLSTONE, PAUL D.", 1, 11381, 0,
  "CA", 2002, "H", 46, "G", 2004, "Other", "W-I", "LASH, THOMAS WESLEY (TOM)", 0, 80, 0,
  "LA", 2002, "H", 1, "G", 2004, "R", "R", "NAMER, ROBERT (BOB)", 0, 7229, 0,
  "OH", 2002, "H", 14, "G", 2004, "I", "I", "STONE, SIDNEY J. (SID)", 0, 113, 0,
  "FL", 2002, "H", 22, "G", 2004, "Other", "W-I", "SMILAN, STANLEY (STAN)", 0, 18, 0,
  "LA", 2002, "H", 2, "G", 2004, "Other", "Other", "CLEMENT, WAYNE E.", 0, 3789, 0,
  "CA", 2002, "H", 47, "G", 2004, "Other", "W-I", "FISHER, KENNETH M. VALENZUELA", 0, 382, 0,
  "AZ", 2002, "H", 4, "G", 2004, "Lbt", "Lbt", "GIBBONS, AMY", 0, 3167, 0,
  "AZ", 2002, "H", 6, "G", 2004, "Lbt", "Lbt", "WAGNER, ANDY", 0, 3888, 0,
  "LA", 2002, "H", 1, "G", 2004, "R", "R", "MONICA, MONICA L.", 0, 20268, 0,
  "LA", 2002, "H", 1, "G", 2004, "Other", "Other", "HAWXHURST, IAN P.", 0, 5956, 0,
  "LA", 2002, "H", 2, "G", 2004, "R", "R", "SULLIVAN, SILKY", 0, 15440, 0,
  "LA", 2002, "H", 2, "G", 2004, "D", "D", "DIXON, IRMA MUSE", 0, 28480, 0,
  "LA", 2002, "H", 2, "G", 2004, "D", "D", "HUNT, CLARENCE (BUDDY)", 0, 4137, 0,
  "LA", 2002, "H", 3, "G", 2004, "Other", "Other", "BEIER, WILLIAM", 0, 12964, 0,
  "LA", 2002, "H", 3, "G", 2004, "Other", "Other", "IWANCIO, DAVID", 0, 7055, 0,
  "LA", 2002, "H", 4, "G", 2004, "D", "D", "MILKOVICH, JOHN", 0, 42340, 0,
  "LA", 2002, "H", 4, "G", 2004, "Other", "Other", "JACOBS, BILL", 0, 3104, 0,
  "LA", 2002, "H", 6, "G", 2004, "Other", "Other", "MOSCATELLO, RICK", 0, 27898, 0,
  "LA", 2002, "H", 7, "G", 2004, "Other", "Other", "VALLETTA, ROBERTO", 0, 21051, 0,
  "NM", 2002, "H", 2, "G", 2004, "Other", "W-I (Grn)", "DEWEY, GEORGE L.", 0, 43, 0,
  "NM", 2002, "H", 2, "G", 2004, "Other", "W-I (R)", "LYNCH, PADRAIG M.", 0, 39, 0,
  "OH", 2002, "H", 3, "G", 2004, "I", "I", "WILLIAMITIS, RONALD", 0, 14, 0,
  "AZ", 2002, "H", 8, "G", 2004, "Other", "W-I", "DORRANCE, JIM", 0, 28, 0,
  "CA", 2002, "H", 4, "G", 2004, "Other", "W-I (R)", "KIRBY, BILL", 0, 401, 0,
  "CA", 2002, "H", 17, "G", 2004, "Other", "W-I", "SHUGART, ALAN", 0, 27, 0,
  "CA", 2002, "H", 18, "G", 2004, "Other", "W-I", "CROWDER, DONNA", 0, 49, 0,
  "CA", 2002, "H", 53, "G", 2004, "Other", "W-I", "DORENKOTT, JIM", 0, 37, 0,
  "CT", 2002, "H", 1, "G", 2004, "Other", "W-I", "MASULLO, MIRIAM J.", 0, 18, 0,
  "FL", 2002, "H", 9, "G", 2004, "Other", "W-I", "PASAYAN, ANDREW", 0, 16, 0,
  "FL", 2002, "H", 13, "G", 2004, "Other", "W-I", "GENTHNER, WAYNE", 0, 22, 0,
  "FL", 2002, "H", 15, "G", 2004, "Other", "W-I", "GIBBENS, DONALD", 0, 10, 0,
  "FL", 2002, "H", 17, "G", 2004, "Other", "W-I", "ITALIE, MICHAEL", 0, 73, 0,
  "IL", 2002, "H", 8, "G", 2004, "Other", "W-I", "KELECIC, CHUCK", 0, 25, 0,
  "IN", 2002, "H", 7, "G", 2004, "Other", "W-I", "JEFFRIES, JAMES KELL (JIM)", 0, 64, 0,
  "MD", 2002, "H", 4, "G", 2004, "Other", "W-I", "ANDERSON, FLOYD W., JR.", 0, 33, 0,
  "TX", 2002, "H", 7, "G", 2004, "Other", "W-I", "SKONE-PALMER, JOHN R.", 0, 58, 0
)

cand <- cand |>
  rows_insert(adds_2002, by = c("year", "state", "office", "dist", "type", "name_snyder"), conflict = "error")

## Other fixes ----
# Sanger is not on the AL-4 ballot; Sherwood ran on both major lines; LA Senate and LA-5 rows are the December runoff
cand <- cand |>
  tidylog::filter(!(year == 2002 & state == "AL" & name_snyder == "SANGER, JOE")) |>
  tidylog::mutate(
    party_formal = replace(party_formal, year == 2002 & state == "PA" & name_snyder == "SHERWOOD, DONALD L. (DON)", "R,D"),
    runoff = replace(runoff, year == 2002 & state == "LA" & (office == "S" | (office == "H" & dist == 5)), 1)
  )

# 9. Clerk audit, 2004 ----

## Votes ----
# Snyder's 2004 figures predate certification in many states (AK, MI, NE, NH, NJ, NV, ...)
votes_2004 <- tribble(
  ~year, ~state, ~office, ~dist, ~type, ~name_snyder, ~vote_g,
  2004, "AK", "S", 3, "G", "MURKOWSKI, LISA A.", 149773,  # p. 3
  2004, "AK", "S", 3, "G", "KNOWLES, TONY", 140424,  # p. 3
  2004, "AK", "S", 3, "G", "SYKES, JAMES L. (JIM)", 3053,  # p. 3
  2004, "AK", "S", 3, "G", "MILLICAN, MARC JOSEPH", 8885,  # p. 3
  2004, "AK", "S", 3, "G", "GIANOUTSOS, TED", 732,  # p. 3
  2004, "AK", "S", 3, "G", "SANDERS, JERRY L.", 3785,  # p. 3
  2004, "AK", "S", 3, "G", "KOHLHAAS, SCOTT A.", 1240,  # p. 3
  2004, "DE", "H", 1, "G", "CASTLE, MICHAEL N.", 245978,  # p. 12
  2004, "DE", "H", 1, "G", "DONNELLY, PAUL", 105716,  # p. 12
  2004, "DE", "H", 1, "G", "MORRIS, WILLIAM E.", 2014,  # p. 12
  2004, "DE", "H", 1, "G", "BARROS, MAURICE J. (BUD)", 2337,  # p. 12
  2004, "HI", "S", 3, "G", "INOUYE, DANIEL KEN", 313629,  # p. 16
  2004, "IL", "S", 3, "G", "KEYES, ALAN", 1390690,  # p. 17
  2004, "IL", "S", 3, "G", "OBAMA, BARACK", 3597456,  # p. 17
  2004, "IL", "S", 3, "G", "FRANZEN, ALBERT J.", 81164,  # p. 17
  2004, "IL", "S", 3, "G", "KOHN, JERRY", 69253,  # p. 17
  2004, "IL", "H", 13, "G", "BIGGERT, JUDY", 200472,  # p. 17
  2004, "IL", "H", 13, "G", "ANDERSEN, GLORIA SCHOR", 107836,  # p. 17
  2004, "IL", "H", 14, "G", "HASTERT, JOHN DENNIS (DENNY)", 191618,  # p. 17
  2004, "IL", "H", 14, "G", "KANHAI-ZAMORA, RUBEN", 87590,  # p. 17
  2004, "LA", "S", 3, "G", "VITTER, DAVID B.", 943014,  # p. 22
  2004, "LA", "H", 1, "G", "JINDAL, BOBBY", 225708,  # p. 22
  2004, "LA", "H", 2, "G", "JEFFERSON, WILLIAM J. (BILL)", 173510,  # p. 22
  2004, "LA", "H", 5, "G", "ALEXANDER, RODNEY M.", 141495,  # p. 22
  2004, "LA", "H", 6, "G", "BAKER, RICHARD HUGH", 189106,  # p. 22
  2004, "MA", "H", 7, "G", "CHASE, KENNETH G. (KEN)", 60334,  # p. 26
  2004, "MI", "H", 1, "G", "HOOPER, DON", 105706,  # p. 26
  2004, "MI", "H", 1, "G", "STUPAK, BART T.", 211571,  # p. 26
  2004, "MI", "H", 1, "G", "LOOSEMORE, JOHN W.", 2292,  # p. 26
  2004, "MI", "H", 1, "G", "NEWLAND, DAVID J.", 3105,  # p. 26
  2004, "MI", "H", 2, "G", "HOEKSTRA, PETER (PETE)", 225343,  # p. 26
  2004, "MI", "H", 2, "G", "KOTOS, KIMON", 94040,  # p. 26
  2004, "MI", "H", 2, "G", "GRAESER, RONALD E.", 2746,  # p. 26
  2004, "MI", "H", 3, "G", "EHLERS, VERNON J. (VERN)", 214465,  # p. 26
  2004, "MI", "H", 3, "G", "HICKEY, PETER H.", 101395,  # p. 26
  2004, "MI", "H", 3, "G", "ADAMS, WARREN T.", 3695,  # p. 26
  2004, "MI", "H", 3, "G", "SALES, MARCEL J.", 2548,  # p. 26
  2004, "MI", "H", 4, "G", "CAMP, DAVID LEE (DAVE)", 205274,  # p. 27
  2004, "MI", "H", 4, "G", "HUCKLEBERRY, MICHAEL R. (MIKE)", 110885,  # p. 27
  2004, "MI", "H", 4, "G", "CHIA, AL, JR.", 2765,  # p. 27
  2004, "MI", "H", 5, "G", "KILDEE, DALE E.", 208163,  # p. 27
  2004, "MI", "H", 5, "G", "FOSTER, CLINT", 2350,  # p. 27
  2004, "MI", "H", 5, "G", "MIKKELSON, HARLEY G.", 2468,  # p. 27
  2004, "MI", "H", 6, "G", "UPTON, FREDERICK STEPHEN (FRED)", 197425,  # p. 27
  2004, "MI", "H", 6, "G", "HAAS, ERWIN J.", 2275,  # p. 27
  2004, "MI", "H", 6, "G", "FITZSIMONS, W. DENNIS", 2169,  # p. 27
  2004, "MI", "H", 6, "G", "MACPHEE, RANDALL", 2311,  # p. 27
  2004, "MI", "H", 7, "G", "SCHWARZ, JOHN H. (JOE)", 176053,  # p. 27
  2004, "MI", "H", 7, "G", "RENIER, SHARON MARIE", 109527,  # p. 27
  2004, "MI", "H", 7, "G", "PROCTOR, KENNETH L. (KEN)", 3034,  # p. 27
  2004, "MI", "H", 7, "G", "HORN, DAVID P.", 9032,  # p. 27
  2004, "MI", "H", 7, "G", "SEAGRAVES, JASON", 3996,  # p. 27
  2004, "MI", "H", 8, "G", "ROGERS, MICHAEL J. (MIKE)", 207925,  # p. 27
  2004, "MI", "H", 8, "G", "ALEXANDER, ROBERT D. (BOB)", 125619,  # p. 27
  2004, "MI", "H", 8, "G", "WHITE, WILLIAM TYLER (WILL)", 3591,  # p. 27
  2004, "MI", "H", 8, "G", "MANGOPOULOS, JOHN S.", 3288,  # p. 27
  2004, "MI", "H", 9, "G", "KNOLLENBERG, JOSEPH K. (JOE)", 199210,  # p. 27
  2004, "MI", "H", 9, "G", "REIFMAN, STEVEN", 134764,  # p. 27
  2004, "MI", "H", 10, "G", "MILLER, CANDICE S.", 227720,  # p. 27
  2004, "MI", "H", 10, "G", "CASEY, ROB", 98029,  # p. 27
  2004, "MI", "H", 10, "G", "AMERICA, ANTHONY", 2153,  # p. 27
  2004, "MI", "H", 11, "G", "MCCOTTER, THADDEUS G. (THAD)", 186431,  # p. 27
  2004, "MI", "H", 11, "G", "TRURAN, PHILLIP", 134301,  # p. 27
  2004, "MI", "H", 11, "G", "BASSO, CHARLES", 6484,  # p. 27
  2004, "MI", "H", 12, "G", "SHAFER, RANDELL J.", 88256,  # p. 27
  2004, "MI", "H", 12, "G", "LEVIN, SANDER M. (SANDY)", 210827,  # p. 27
  2004, "MI", "H", 12, "G", "GACH, RICHARD CARL (DICK)", 5051,  # p. 27
  2004, "MI", "H", 13, "G", "CASSELL, CYNTHIA G.", 40935,  # p. 27
  2004, "MI", "H", 13, "G", "KILPATRICK, CAROLYN CHEEKS", 173246,  # p. 27
  2004, "MI", "H", 13, "G", "GORDON, ERIC B.", 3211,  # p. 27
  2004, "MI", "H", 13, "G", "LAVIGNE, THOMAS M.J.", 4261,  # p. 27
  2004, "MI", "H", 14, "G", "PEDRAZA, VERONICA", 35089,  # p. 27
  2004, "MI", "H", 14, "G", "CONYERS, JOHN, JR.", 213681,  # p. 27
  2004, "MI", "H", 14, "G", "DONAHUE, MICHAEL L.", 2278,  # p. 27
  2004, "MI", "H", 14, "G", "SEARS, WILBERT (WILL)", 1307,  # p. 27
  2004, "MI", "H", 14, "G", "WELTMAN, LISA R.", 2224,  # p. 27
  2004, "MI", "H", 15, "G", "REAMER, DAWN", 81828,  # p. 27
  2004, "MI", "H", 15, "G", "DINGELL, JOHN DAVID, JR.", 218409,  # p. 27
  2004, "MI", "H", 15, "G", "ELLER, MIKE", 2508,  # p. 27
  2004, "MI", "H", 15, "G", "WHITE, JEROME S.", 1818,  # p. 27
  2004, "MS", "H", 2, "G", "O'HARA, SHAWN R.", 2596,  # p. 30
  2004, "MT", "H", 1, "G", "REHBERG, DENNIS R. (DENNY)", 286076,  # p. 32
  2004, "MT", "H", 1, "G", "VELAZQUEZ, TRACY E.", 145606,  # p. 32
  2004, "MT", "H", 1, "G", "FELLOWS, MIKE", 12548,  # p. 32
  2004, "NE", "H", 1, "G", "LARRICK, STEVEN R.", 7345,  # p. 32
  2004, "NE", "H", 2, "G", "SALVATIERRA, DANTE", 2208,  # p. 32
  2004, "NE", "H", 2, "G", "GRAZIANO, JOHN J. (JACK)", 4656,  # p. 32
  2004, "NE", "H", 3, "G", "OSBORNE, THOMAS WILLIAM (TOM)", 218751,  # p. 32
  2004, "NE", "H", 3, "G", "ANDERSON, DONNA J.", 26434,  # p. 32
  2004, "NE", "H", 3, "G", "GUISINGER, ROY", 1555,  # p. 32
  2004, "NE", "H", 3, "G", "ROSBERG, JOSEPH A.", 3396,  # p. 32
  2004, "NV", "H", 3, "G", "PORTER, JON C., SR.", 162240,  # p. 33
  2004, "NV", "H", 3, "G", "GALLAGHER, TOM", 120365,  # p. 33
  2004, "NV", "H", 3, "G", "SILVESTRI, JOSEPH P. (JOE)", 9260,  # p. 33
  2004, "NV", "H", 3, "G", "O'DELL, RICHARD", 6053,  # p. 33
  2004, "NH", "S", 3, "G", "GREGG, JUDD ALAN", 434847,  # p. 33
  2004, "NH", "S", 3, "G", "HADDOCK, DORIS", 221549,  # p. 33
  2004, "NH", "H", 1, "G", "BRADLEY, JOSEPH E. (JEB), III", 204836,  # p. 34
  2004, "NH", "H", 1, "G", "NADEAU, JUSTIN", 118226,  # p. 34
  2004, "NH", "H", 2, "G", "BASS, CHARLES F. (CHARLIE)", 191188,  # p. 34
  2004, "NH", "H", 2, "G", "HODES, PAUL W.", 125280,  # p. 34
  2004, "NH", "H", 2, "G", "KAHN, RICHARD B.", 11311,  # p. 34
  2004, "NJ", "H", 1, "G", "HUTCHISON, S. DANIEL", 66109,  # p. 34
  2004, "NJ", "H", 1, "G", "ANDREWS, ROBERT E. (ROB)", 201163,  # p. 34
  2004, "NJ", "H", 1, "G", "CROCE, ARTURO", 931,  # p. 34
  2004, "NJ", "H", 2, "G", "LOBIONDO, FRANK A.", 172779,  # p. 34
  2004, "NJ", "H", 2, "G", "ROBB, TIMOTHY J.", 86792,  # p. 34
  2004, "NJ", "H", 2, "G", "MATTHEWS, MICHAEL J., JR.", 1767,  # p. 34
  2004, "NJ", "H", 2, "G", "ALCANTARA, JOSE DAVID", 1516,  # p. 34
  2004, "NJ", "H", 2, "G", "NORWOOD, WILLIE", 1993,  # p. 34
  2004, "NJ", "H", 2, "G", "ROZZO, CONSTANTINO (TINOTEAK)", 595,  # p. 34
  2004, "NJ", "H", 3, "G", "SAXTON, H. JAMES (JIM)", 195938,  # p. 34
  2004, "NJ", "H", 3, "G", "CONAWAY, HERBERT C. (HERB), JR.", 107034,  # p. 34
  2004, "NJ", "H", 3, "G", "ORLAND, FRANK", 976,  # p. 34
  2004, "NJ", "H", 3, "G", "FORCHION, ROBERT EDWARD", 4914,  # p. 34
  2004, "NJ", "H", 4, "G", "SMITH, CHRISTOPHER H. (CHRIS)", 192671,  # p. 34
  2004, "NJ", "H", 4, "G", "VASQUEZ, AMY", 92826,  # p. 34
  2004, "NJ", "H", 4, "G", "EDGAR, RICHARD", 2056,  # p. 34
  2004, "NJ", "H", 5, "G", "GARRETT, E. SCOTT", 171220,  # p. 34
  2004, "NJ", "H", 5, "G", "WOLFE, DOROTHEA ANNE", 122259,  # p. 34
  2004, "NJ", "H", 5, "G", "KAPLAN, VICTOR", 1857,  # p. 34
  2004, "NJ", "H", 5, "G", "PHELAN, THOMAS A.", 1515,  # p. 34
  2004, "NJ", "H", 5, "G", "PASON, GREGORY", 574,  # p. 34
  2004, "NJ", "H", 6, "G", "FERNANDEZ, SYLVESTER", 70942,  # p. 34
  2004, "NJ", "H", 6, "G", "PALLONE, FRANK J., JR.", 153981,  # p. 34
  2004, "NJ", "H", 6, "G", "FLYNN, VIRGINIA A.", 2829,  # p. 34
  2004, "NJ", "H", 7, "G", "FERGUSON, MICHAEL A. (MIKE)", 162597,  # p. 34
  2004, "NJ", "H", 7, "G", "BROZAK, STEVE", 119081,  # p. 34
  2004, "NJ", "H", 7, "G", "WILLIAMS, MATTHEW ANGUS", 2016,  # p. 34
  2004, "NJ", "H", 7, "G", "ABRAMS, THOMAS D. (TOM)", 2153,  # p. 34
  2004, "NJ", "H", 8, "G", "AJJAN, GEORGE", 62747,  # p. 35
  2004, "NJ", "H", 8, "G", "PASCRELL, WILLIAM J. (BILL), JR.", 152001,  # p. 35
  2004, "NJ", "H", 8, "G", "FORTUNATO, JOSEPH A.", 4072,  # p. 35
  2004, "NJ", "H", 9, "G", "TRAWINSKI, EDWARD", 68564,  # p. 35
  2004, "NJ", "H", 9, "G", "ROTHMAN, STEVEN R.", 146038,  # p. 35
  2004, "NJ", "H", 9, "G", "DALY, DAVID E.", 1649,  # p. 35
  2004, "NJ", "H", 10, "G", "PAYNE, DONALD M.", 155697,  # p. 35
  2004, "NJ", "H", 10, "G", "WASHINGTON, TOY-LING", 2927,  # p. 35
  2004, "NJ", "H", 10, "G", "LOBMAN, SARA J.", 2089,  # p. 35
  2004, "NJ", "H", 11, "G", "FRELINGHUYSEN, RODNEY P.", 200915,  # p. 35
  2004, "NJ", "H", 11, "G", "BUELL, JAMES W.", 91811,  # p. 35
  2004, "NJ", "H", 11, "G", "LETT, AUSTIN S.", 1530,  # p. 35
  2004, "NJ", "H", 11, "G", "MELE, JOHN", 1746,  # p. 35
  2004, "NJ", "H", 12, "G", "SPADEA, BILL", 115014,  # p. 35
  2004, "NJ", "H", 12, "G", "HOLT, RUSH D.", 171691,  # p. 35
  2004, "NJ", "H", 12, "G", "CHAZOTTE, KENNETH J. (KEN)", 1562,  # p. 35
  2004, "NJ", "H", 12, "G", "BROOKS, DARYL MIKELL", 1518,  # p. 35
  2004, "NJ", "H", 13, "G", "PIATKOWSKI, RICHARD W.", 35288,  # p. 35
  2004, "NJ", "H", 13, "G", "MENENDEZ, ROBERT (BOB)", 121018,  # p. 35
  2004, "NJ", "H", 13, "G", "LARISCY, ANGELA L.", 887,  # p. 35
  2004, "NJ", "H", 13, "G", "HESTER, RICHARD S. (DICK), SR.", 1282,  # p. 35
  2004, "NJ", "H", 13, "G", "SHAW, HERBERT H.", 1066,  # p. 35
  2004, "NM", "H", 2, "G", "PEARCE, STEVAN E. (STEVE)", 130498,  # p. 37
  2004, "NM", "H", 2, "G", "KING, GARY K.", 86292,  # p. 37
  2004, "NY", "S", 3, "G", "KOPPEL, MARTIN", 14811,  # p. 37
  2004, "NY", "H", 5, "G", "POLICARPIO, GONZALO M. (JUN)", 1248,  # p. 38
  2004, "NY", "H", 6, "G", "MEEKS, GREGORY W.", 129688,  # p. 38
  2004, "NY", "H", 10, "G", "BLUME, MARIANNA", 1554,  # p. 38
  2004, "NY", "H", 11, "G", "STEVENS, LORRAINE", 4721,  # p. 38
  2004, "NY", "H", 11, "G", "LIEBERMAN, SOL", 4478,  # p. 38
  2004, "NY", "H", 12, "G", "RODRIGUEZ, PAUL", 17166,  # p. 39
  2004, "NY", "H", 16, "G", "MOHAMED, ALI", 5610,  # p. 39
  2004, "NY", "H", 17, "G", "BRAWLEY, KEVIN", 3482,  # p. 39
  2004, "NY", "H", 20, "G", "GULLER, MORRIS", 1353,  # p. 39
  2004, "NY", "H", 28, "G", "CARTONIA, FRANCINA J.", 5678,  # p. 40
  2004, "NY", "H", 29, "G", "CIAMPOLI, JOHN", 5819,  # p. 40
  2004, "NC", "H", 8, "G", "HAYES, ROBERT C. (ROBIN)", 125070,  # p. 42
  2004, "NC", "H", 8, "G", "TROUTMAN, NANCY ELIZABETH (BETH)", 100101,  # p. 42
  2004, "NC", "H", 9, "G", "MYRICK, SUELLEN WILKINS (SUE)", 210783,  # p. 42
  2004, "NC", "H", 9, "G", "FLYNN, JACK", 89318,  # p. 42
  2004, "NC", "H", 12, "G", "FISHER, ADA M.", 76898,  # p. 42
  2004, "NC", "H", 12, "G", "WATT, MELVIN LUTHER (MEL)", 154908,  # p. 42
  2004, "ND", "S", 3, "G", "DORGAN, BYRON L.", 212143,  # p. 43
  2004, "OH", "H", 15, "G", "PRYCE, DEBORAH D.", 166520,  # p. 44
  2004, "OR", "S", 3, "G", "KEANE, TERESA CATHERINE", 43053,  # p. 46
  2004, "PA", "H", 13, "G", "MOULTON, CHUCK", 3865,  # p. 47
  2004, "RI", "H", 2, "G", "MORABITO, EDWARD M.", 6196,  # p. 50
  2004, "RI", "H", 2, "G", "HAYES, DORMAN J., JR.", 3303,  # p. 50
  2004, "SC", "S", 3, "G", "NWANGAZA, EFIA", 4383,  # p. 50
  2004, "SC", "H", 6, "G", "MCLEOD, GARY M.", 79600,  # p. 51
  2004, "SD", "S", 3, "G", "THUNE, JOHN R.", 197848,  # p. 51
  2004, "SD", "S", 3, "G", "DASCHLE, THOMAS ANDREW (TOM)", 193340,  # p. 51
  2004, "SD", "H", 1, "G", "BEGAY, TERRY L.", 2808,  # p. 51
  2004, "TN", "H", 2, "G", "GREENE, JOHN", 52155,  # p. 52
  2004, "UT", "H", 1, "G", "JOHNSTON, CHARLES", 4510,  # p. 55
  2004, "UT", "H", 1, "G", "SODERBERG, RICHARD", 4206,  # p. 55
  2004, "UT", "H", 2, "G", "PETERSEN, JEREMY PAUL", 3541,  # p. 55
  2004, "UT", "H", 2, "G", "AMOS, RONALD", 1210,  # p. 55
  2004, "UT", "H", 2, "G", "DIEHL, PATRICK S.", 2189,  # p. 55
  2004, "UT", "H", 3, "G", "WINFIELD, RONALD G.", 5089,  # p. 55
  2004, "UT", "H", 3, "G", "JAMES, CURTIS DARRELL", 2390,  # p. 55
  2004, "UT", "H", 3, "G", "DEXTER, JIM", 3691,  # p. 55
  2004, "VT", "S", 3, "G", "MCMULLEN, JACK", 75398,  # p. 56
  2004, "SD", "H", 1, "G", "DIEDRICH, LARRY", 178823,  # p. 51
  2004, "SD", "H", 1, "G", "SANDLIN, STEPHANIE HERSETH", 207837,  # p. 51
  2004, "MI", "H", 2, "G", "VAN TIL, STEVEN S.", 2876,  # p. 26
  2004, "MI", "H", 6, "G", "ELLIOT, SCOTT C.", 97978   # p. 27
)

cand <- cand |>
  rows_update(votes_2004, by = c("year", "state", "office", "dist", "type", "name_snyder"), unmatched = "error")

## Candidates missing from Snyder ----
adds_2004 <- tribble(
  ~state, ~year, ~office, ~dist, ~type, ~nextup, ~party, ~party_formal, ~name_snyder, ~inc, ~vote_g, ~w_g,
  "VT", 2004, "H", 1, "G", 2006, "I", "I", "SANDERS, BERNARD (BERNIE)", 1, 205774, 1,
  "OH", 2004, "H", 2, "G", 2006, "Other", "W-I", "CONDIT, JAMES J. (JIM), JR.", 0, 60, 0,
  "NH", 2004, "S", 3, "G", 2010, "Other", "W-I", "BLEVENS, KEN", 0, 102, 0,
  "LA", 2004, "H", 1, "G", 2006, "D", "D", "MENDOZA, MANILLO V. (VINNY)", 0, 12779, 0,
  "VT", 2004, "H", 1, "G", 2006, "Other", "Liberty Union", "NEWTON, JANE E.", 0, 3018, 0,
  "LA", 2004, "S", 3, "G", 2010, "D", "D", "JOHN, CHRISTOPHER (CHRIS)", 0, 542150, 0,
  "KY", 2004, "H", 1, "G", 2006, "Other", "W-I", "BARLOW, THOMAS J. (TOM), III", 0, 186, 0,
  "MD", 2004, "H", 8, "G", 2006, "Other", "W-I", "YOUNG, LIH Y.", 0, 79, 0,
  "LA", 2004, "S", 3, "G", 2010, "D", "D", "KENNEDY, JOHN NEELY", 0, 275821, 0,
  "LA", 2004, "S", 3, "G", 2010, "D", "D", "MELTON, SAM HOUSTON, JR.", 0, 12289, 0,
  "LA", 2004, "S", 3, "G", 2010, "Other", "Other", "FONTANESI, RICHARD M.", 0, 15097, 0,
  "NM", 2004, "H", 1, "G", 2006, "Other", "W-I", "COLE, ORLIN G.", 0, 194, 0,
  "OH", 2004, "H", 1, "G", 2006, "Other", "W-I", "STEVENSON, RICHARD LEE (RICH)", 0, 198, 0,
  "LA", 2004, "S", 3, "G", 2010, "Other", "Other", "GALAN, RAOUL ARMANDO (SKIP)", 0, 12463, 0,
  "LA", 2004, "H", 6, "G", 2006, "D", "D", "CRAIG, RUFUS HOLT, JR.", 0, 50732, 0,
  "FL", 2004, "H", 6, "G", 2006, "Other", "W-I", "O'BRIEN, NEWELL WILLIAM", 0, 36, 0,
  "FL", 2004, "H", 9, "G", 2006, "Other", "W-I", "PASAYAN, ANDREW", 0, 243, 0,
  "FL", 2004, "H", 17, "G", 2006, "Other", "W-I", "MUSA, OMARI", 0, 734, 0,
  "MD", 2004, "S", 3, "G", 2010, "Other", "W-I", "BLY, RAYMOND J. (RAY)", 0, 109, 0,
  "TN", 2004, "H", 5, "G", 2006, "Other", "W-I", "KOVACH, THOMAS F. (TOM)", 0, 15, 0,
  "FL", 2004, "H", 3, "G", 2006, "Other", "W-I", "BROWN, JOHNNY M.", 0, 1323, 0,
  "FL", 2004, "H", 4, "G", 2006, "Other", "W-I", "GRAYSON, RICHARD", 0, 1170, 0,
  "FL", 2004, "H", 22, "G", 2006, "Other", "W-I", "KENNEDY, DON", 0, 627, 0,
  "LA", 2004, "H", 1, "G", 2006, "D", "D", "WATTS, JERRY", 0, 10034, 0,
  "FL", 2004, "S", 3, "G", 2010, "Other", "W-I", "BLASS, PETER", 0, 16, 0,
  "LA", 2004, "S", 3, "G", 2010, "D", "D", "MORRELL, ARTHUR A.", 0, 47222, 0,
  "LA", 2004, "H", 1, "G", 2006, "R", "R", "ROGERS, MIKE", 0, 7975, 0,
  "LA", 2004, "H", 1, "G", 2006, "D", "D", "ARMSTRONG, ROY", 0, 19266, 0,
  "LA", 2004, "H", 1, "G", 2006, "D", "D", "ZIMMERMAN, DANIEL", 0, 12135, 0,
  "LA", 2004, "H", 2, "G", 2006, "R", "R", "SCHWERTZ, ARTHUR L. (ART)", 0, 46097, 0,
  "LA", 2004, "H", 5, "G", 2006, "R", "R", "SCOTT, JOHN W. (JOCK)", 0, 37971, 0,
  "LA", 2004, "H", 5, "G", 2006, "D", "D", "BLAKES, ZELMA (TISA)", 0, 58591, 0,
  "LA", 2004, "H", 6, "G", 2006, "D", "D", "GALMON, EDWARD ANTHONY (SCOTT)", 0, 22031, 0,
  "VT", 2004, "H", 1, "G", 2006, "R", "R", "PARKE, GREG", 0, 74271, 0,
  "VT", 2004, "H", 1, "G", 2006, "D", "D", "DROWN, LARRY", 0, 21684, 0,
  "AR", 2004, "S", 3, "G", 2010, "Other", "W-I", "SCHWARZ, GLEN A.", 0, 212, 0,
  "AR", 2004, "S", 3, "G", 2010, "Other", "W-I", "MASON, GENE", 0, 128, 0,
  "CT", 2004, "H", 2, "G", 2006, "Other", "W-I", "LYON, DAVID R.", 0, 130, 0,
  "FL", 2004, "S", 3, "G", 2010, "Other", "W-I", "STUFFT, MARK", 0, 119, 0,
  "FL", 2004, "S", 3, "G", 2010, "Other", "W-I", "TRAVIS, NANCY", 0, 27, 0,
  "FL", 2004, "S", 3, "G", 2010, "Other", "W-I", "FRUIT, RACHELE", 0, 25, 0,
  "FL", 2004, "H", 2, "G", 2006, "Other", "W-I", "FREDERICK, T.A.", 0, 11, 0,
  "FL", 2004, "H", 11, "G", 2006, "Other", "W-I", "BUTTS, KARL M.", 0, 122, 0,
  "KY", 2004, "H", 3, "G", 2006, "Other", "W-I", "EVERETT, CORLEY", 0, 15, 0,
  "MD", 2004, "S", 3, "G", 2010, "Other", "W-I", "GEMMILL, ROBERT, II", 0, 204, 0,
  "MD", 2004, "S", 3, "G", 2010, "Other", "W-I", "GAYLE-EL, DENNARD A., SR.", 0, 47, 0,
  "MO", 2004, "H", 3, "G", 2006, "Other", "W-I", "BADARACCO, JOSEPH L.", 0, 11, 0,
  "NC", 2004, "S", 3, "G", 2010, "Other", "W-I", "RUCKER, WALKER F.", 0, 362, 0,
  "NC", 2004, "H", 4, "G", 2006, "Other", "W-I", "LONGLEY, MAXIMILIAN", 0, 76, 0,
  "OH", 2004, "S", 3, "G", 2010, "Other", "W-I", "MEYERS, HELEN", 0, 296, 0,
  "OH", 2004, "H", 6, "G", 2006, "Other", "W-I", "LUCHANSKY, JOHN STEPHEN", 0, 145, 0,
  "OH", 2004, "H", 12, "G", 2006, "Other", "W-I", "SPINGOLA, CHUCK", 0, 25, 0,
  "TN", 2004, "H", 8, "G", 2006, "Other", "W-I", "BERTRAND, DENNIS", 0, 91, 0,
  "TN", 2004, "H", 9, "G", 2006, "Other", "W-I", "MAYNARD, JIM", 0, 166, 0
)

cand <- cand |>
  rows_insert(adds_2004, by = c("year", "state", "office", "dist", "type", "name_snyder"), conflict = "error") |>
  # LA-3 and LA-7 rows are the December 4 runoff
  tidylog::mutate(runoff = replace(runoff, year == 2004 & state == "LA" & office == "H" & dist %in% c(3, 7), 1))

# 10. Clerk audit, 2006 ----

## Votes ----
votes_2006 <- tribble(
  ~year, ~state, ~office, ~dist, ~type, ~name_snyder, ~vote_g,
  2006, "IL", "H", 12, "G", "COSTELLO, JERRY F.", 157284,  # p. 15
  2006, "IL", "H", 16, "G", "AUMAN, RICHARD D.", 63462,  # p. 16
  2006, "IL", "H", 16, "G", "MANZULLO, DONALD A.", 125508,  # p. 16
  2006, "ME", "S", 1, "G", "HAY BRIGHT, JEAN M.", 113131,  # p. 19
  2006, "ME", "S", 1, "G", "SNOWE, OLYMPIA J.", 405596,  # p. 19
  2006, "ME", "H", 2, "G", "MICHAUD, MICHAEL H. (MIKE)", 179772,  # p. 19
  2006, "ME", "H", 2, "G", "D'AMBOISE, LAURENCE SCOTT", 75156,  # p. 19
  2006, "NJ", "H", 2, "G", "THOMAS-HUGHES, VIOLA", 64277,  # p. 28
  2006, "NY", "H", 11, "G", "FINGER, STEPHEN", 7447,  # p. 32
  2006, "OR", "H", 1, "G", "WU, DAVID (DAVE)", 169409,  # p. 38
  2006, "OR", "H", 2, "G", "WALDEN, GREGORY PAUL (GREG)", 181529,  # p. 38
  2006, "RI", "S", 1, "G", "WHITEHOUSE, SHELDON, II", 206043,  # p. 40
  2006, "RI", "S", 1, "G", "CHAFEE, LINCOLN D. (LINC)", 178950,  # p. 40
  2006, "RI", "H", 1, "G", "KENNEDY, PATRICK J.", 124634,  # p. 41
  2006, "RI", "H", 1, "G", "SCOTT, JONATHAN P.", 41836,  # p. 41
  2006, "RI", "H", 1, "G", "CAPALBO, KENNETH A. (KEN)", 13634,  # p. 41
  2006, "SD", "H", 1, "G", "WHALEN, BRUCE W.", 97864   # p. 42
)

cand <- cand |>
  rows_update(votes_2006, by = c("year", "state", "office", "dist", "type", "name_snyder"), unmatched = "error") |>
  # Healey was lieutenant governor, not governor (MA EOPS, December 2006).
  # https://archives.lib.state.ma.us/server/api/core/bitstreams/ac3d667e-1a6d-454b-bb79-e745f1fde212/content
  rows_update(tribble(
    ~year, ~state, ~office, ~dist, ~type, ~name_snyder, ~inc,
    2006, "MA", "G", NA_real_, "G", "HEALEY, KERRY MURPHY", 0
  ), by = c("year", "state", "office", "dist", "type", "name_snyder"), unmatched = "error")

## Candidates missing from Snyder ----
# TX-22 also held a special for DeLay's unexpired term on Nov 7
adds_2006 <- tribble(
  ~state, ~year, ~office, ~dist, ~type, ~nextup, ~party, ~party_formal, ~name_snyder, ~inc, ~vote_g, ~w_g,
  "OH", 2006, "H", 2, "G", 2008, "Other", "W-I", "CONDIT, JAMES J. (JIM), JR.", 0, 76, 0,
  "TN", 2006, "S", 1, "G", 2012, "I", "I", "HOOKER, JOHN JAY", 0, 13, 0,
  "TN", 2006, "S", 1, "G", 2012, "I", "I", "GRAY, JAMES ANTHONY", 0, 14, 0,
  "FL", 2006, "H", 9, "G", 2008, "Other", "W-I", "PASAYAN, ANDREW", 0, 19, 0,
  "CT", 2006, "H", 1, "G", 2008, "Other", "W-I", "FOURNIER, STEPHEN E.D.", 0, 43, 0,
  "FL", 2006, "S", 1, "G", 2012, "Other", "W-I", "SCOTT, LAWRENCE", 0, 78, 0,
  "FL", 2006, "H", 17, "G", 2008, "Other", "W-I", "SIMPSON, ERIC", 0, 23, 0,
  "IN", 2006, "H", 5, "G", 2008, "Other", "W-I (I)", "MILLER, JOHN", 0, 18, 0,
  "MI", 2006, "H", 13, "G", 2008, "Other", "W-I", "DAVENPORT, JOHN", 0, 15, 0,
  "FL", 2006, "H", 4, "G", 2008, "Other", "W-I", "BLADE, JOHN", 0, 16, 0,
  "FL", 2006, "H", 8, "G", 2008, "Other", "W-I", "MAURO, D.J.", 0, 17, 0,
  "FL", 2006, "H", 11, "G", 2008, "Other", "W-I", "GREENWALD, JIM", 0, 13, 0,
  "MI", 2006, "S", 1, "G", 2012, "Other", "W-I", "MCATEE, BRET", 0, 24, 0,
  "TX", 2006, "H", 22, "S", 2006, "R", "R", "SEKULA GIBBS, SHELLEY A.", 0, 76924, 1,
  "TX", 2006, "H", 22, "S", 2006, "Lbt", "Lbt", "SMITHER, M. BOB", 0, 23425, 0,
  "TX", 2006, "H", 22, "S", 2006, "R", "R", "STOCKMAN, STEPHEN E. (STEVE)", 0, 13600, 0,
  "TX", 2006, "H", 22, "S", 2006, "R", "R", "RICHARDSON, DONALD LUTHER (DON)", 0, 7405, 0,
  "TX", 2006, "H", 22, "S", 2006, "R", "R", "TRAN, GIANNIBICEGO HOA", 0, 2568, 0
)

cand <- cand |>
  rows_insert(adds_2006, by = c("year", "state", "office", "dist", "type", "name_snyder"), conflict = "error") |>
  # Roth and Mele ran in NJ-11
  tidylog::mutate(dist = replace(dist, year == 2006 & state == "NJ" & name_snyder %in% c("ROTH, RICHARD S.", "MELE, JOHN"), 11))

# 11. Verified large differences ----
# where the Clerk and Snyder differ by more than 2% and 500 votes, the state's official canvass decides;
# unverifiable NY 2004 rows follow the Clerk, which matched all 20 NY 2004 races that could be checked
verified <- tribble(
  ~year, ~state, ~office, ~dist, ~type, ~name_snyder, ~vote_g,
  1990, "NJ", "H", 9, "G", "RUSSO, PETER J.", 59758,  # state confirms neither
  1994, "DE", "H", 1, "G", "BEAVER, DANNY RAY", 3869,  # state confirms neither
  1994, "DE", "H", 1, "G", "HOCKMUTH, DONALD M.", 1405,  # state confirms neither
  1996, "RI", "S", 2, "G", "LOVEJOY, DONALD W.", 5327,  # state confirms snyder
  1996, "TN", "H", 7, "G", "BRYANT, EDWARD G.", 136643,  # unverified, Snyder kept
  1996, "TN", "H", 7, "G", "TROTTER, DONALD W. (DON)", 73629,  # unverified, Snyder kept
  1998, "IL", "H", 4, "G", "GUTIERREZ, LUIS V.", 54244,  # state confirms clerk
  1998, "MA", "H", 10, "G", "BLEICKEN, ERIC VAUGHN", 70466,  # state confirms clerk
  1998, "MA", "H", 10, "G", "DELAHUNT, WILLIAM D. (BILL)", 164917,  # state confirms clerk
  1998, "MN", "H", 7, "G", "EDIN, ALETA DAWN", 66562,  # state confirms clerk
  1998, "NC", "H", 6, "G", "COBLE, JOHN HOWARD", 112740,  # state confirms clerk
  1998, "OR", "H", 3, "G", "BROWN, WALTER F. (WALT)", 10199,  # state confirms clerk
  2004, "CA", "H", 8, "G", "DOWELL, LEILANI D.", 9527,  # state confirms clerk
  2004, "CT", "H", 5, "G", "RAMIREZ, FERNANDO", 3196,  # state confirms clerk
  2004, "IA", "S", 3, "G", "SMALL, ARTHUR A., JR.", 412365,  # state confirms clerk
  2004, "MO", "H", 4, "G", "NOLAND, JAMES A. (JIM), JR.", 93334,  # state confirms clerk
  2004, "MS", "H", 1, "G", "WICKER, ROGER F.", 219328,  # state confirms clerk
  2004, "MS", "H", 1, "G", "WASHER, BARBARA DALE", 58256,  # state confirms clerk
  2004, "MS", "H", 2, "G", "LESUEUR, CLINTON B.", 107647,  # state confirms clerk
  2004, "MS", "H", 2, "G", "THOMPSON, BENNIE G.", 154626,  # state confirms clerk
  2004, "MS", "H", 3, "G", "PICKERING, CHARLES W. (CHIP), JR.", 234874,  # state confirms clerk
  2004, "MS", "H", 3, "G", "GILES, JIM", 40426,  # state confirms clerk
  2004, "MS", "H", 3, "G", "MAGEE, LAMONICA L.", 18068,  # state confirms clerk
  2004, "MS", "H", 4, "G", "LOTT, MICHAEL (MIKE)", 96740,  # state confirms clerk
  2004, "MS", "H", 4, "G", "TAYLOR, GARY EUGENE (GENE)", 181614,  # state confirms neither
  2004, "MS", "H", 4, "G", "HILL, TRACELLA LOU O'HARA", 2028,  # state confirms neither
  2004, "NE", "H", 1, "G", "FORTENBERRY, JEFFREY (JEFF)", 143756,  # state confirms clerk
  2004, "NE", "H", 1, "G", "CONNEALY, MATT", 113971,  # state confirms clerk
  2004, "NE", "H", 2, "G", "TERRY, LEE R., JR.", 152608,  # state confirms clerk
  2004, "NE", "H", 2, "G", "THOMPSON, NANCY", 90292,  # state confirms clerk
  2004, "NM", "H", 1, "G", "WILSON, HEATHER A.", 147372,  # state confirms clerk
  2004, "NM", "H", 1, "G", "ROMERO, RICHARD M.", 123339,  # state confirms clerk
  2004, "NM", "H", 3, "G", "TUCKER, GREGORY M. (GREG)", 79935,  # state confirms clerk
  2004, "NM", "H", 3, "G", "UDALL, TOM S.", 175269,  # state confirms clerk
  2004, "NY", "H", 1, "G", "MANGER, WILLIAM", 121855,  # state confirms clerk
  2004, "NY", "H", 1, "G", "BISHOP, TIMOTHY H. (TIM)", 156354,  # state confirms clerk
  2004, "NY", "H", 2, "G", "HOFFMANN, RICHARD", 80950,  # Clerk (NY cluster)
  2004, "NY", "H", 2, "G", "ISRAEL, STEVE J.", 161593,  # Clerk (NY cluster)
  2004, "NY", "H", 3, "G", "KING, PETER T. (PETE)", 171259,  # Clerk (NY cluster)
  2004, "NY", "H", 3, "G", "MATHIES, BLAIR H.", 100737,  # Clerk (NY cluster)
  2004, "NY", "H", 4, "G", "GARNER, JAMES", 94141,  # Clerk (NY cluster)
  2004, "NY", "H", 4, "G", "MCCARTHY, CAROLYN", 159969,  # Clerk (NY cluster)
  2004, "NY", "H", 5, "G", "GRAVES, STEPHEN E.", 46867,  # Clerk (NY cluster)
  2004, "NY", "H", 5, "G", "ACKERMAN, GARY L.", 119726,  # Clerk (NY cluster)
  2004, "NY", "H", 7, "G", "CROWLEY, JOSEPH (JOE)", 104275,  # state confirms clerk
  2004, "NY", "H", 7, "G", "CINQUEMANI, JOSEPH", 24548,  # state confirms clerk
  2004, "NY", "H", 8, "G", "HORT, PETER", 39240,  # state confirms clerk
  2004, "NY", "H", 8, "G", "NADLER, JERROLD L. (JERRY)", 162082,  # state confirms clerk
  2004, "NY", "H", 9, "G", "CRONIN, GERARD", 45451,  # state confirms clerk
  2004, "NY", "H", 9, "G", "WEINER, ANTHONY DAVID", 113025,  # state confirms clerk
  2004, "NY", "H", 10, "G", "CLARKE, HARVEY", 11099,  # state confirms clerk
  2004, "NY", "H", 10, "G", "TOWNS, EDOLPHUS (ED)", 136113,  # state confirms clerk
  2004, "NY", "H", 11, "G", "OWENS, MAJOR ROBERT ODELL", 144999,  # state confirms clerk
  2004, "NY", "H", 12, "G", "VELAZQUEZ, NYDIA M.", 107796,  # state confirms clerk
  2004, "NY", "H", 13, "G", "FOSSELLA, VITO J., JR.", 112934,  # state confirms clerk
  2004, "NY", "H", 13, "G", "BARBARO, FRANK J.", 78500,  # state confirms clerk
  2004, "NY", "H", 14, "G", "SRDANOVIC, ANTON", 43623,  # state confirms clerk
  2004, "NY", "H", 14, "G", "MALONEY, CAROLYN B.", 186688,  # state confirms clerk
  2004, "NY", "H", 15, "G", "JEFFERSON, KENNETH", 12355,  # state confirms clerk
  2004, "NY", "H", 15, "G", "RANGEL, CHARLES B.", 161351,  # state confirms clerk
  2004, "NY", "H", 15, "G", "FIELDS, JESSIE A.", 3345,  # state confirms clerk
  2004, "NY", "H", 16, "G", "SERRANO, JOSE E.", 111638,  # state confirms clerk
  2004, "NY", "H", 17, "G", "BRENNAN, MATT", 40524,  # Clerk (NY cluster)
  2004, "NY", "H", 17, "G", "ENGEL, ELIOT L.", 140530,  # Clerk (NY cluster)
  2004, "NY", "H", 18, "G", "HOFFMAN, RICHARD A.", 73975,  # Clerk (NY cluster)
  2004, "NY", "H", 18, "G", "LOWEY, NITA M.", 170715,  # Clerk (NY cluster)
  2004, "NY", "H", 19, "G", "KELLY, SUE W.", 175401,  # Clerk (NY cluster)
  2004, "NY", "H", 19, "G", "JALIMAN, MICHAEL", 87429,  # Clerk (NY cluster)
  2004, "NY", "H", 20, "G", "SWEENEY, JOHN E.", 188753,  # Clerk (NY cluster)
  2004, "NY", "H", 20, "G", "KELLY, DORIS F.", 96630,  # Clerk (NY cluster)
  2004, "NY", "H", 21, "G", "REDLICH, WARREN", 80121,  # Clerk (NY cluster)
  2004, "NY", "H", 21, "G", "MCNULTY, MICHAEL R.", 194033,  # Clerk (NY cluster)
  2004, "NY", "H", 22, "G", "BRENNER, WILLIAM A.", 81881,  # Clerk (NY cluster)
  2004, "NY", "H", 22, "G", "HINCHEY, MAURICE D.", 167489,  # Clerk (NY cluster)
  2004, "NY", "H", 23, "G", "MCHUGH, JOHN M.", 160079,  # Clerk (NY cluster)
  2004, "NY", "H", 23, "G", "JOHNSON, ROBERT J.", 66448,  # Clerk (NY cluster)
  2004, "NY", "H", 24, "G", "BOEHLERT, SHERWOOD L. (SHERRY)", 143000,  # Clerk (NY cluster)
  2004, "NY", "H", 24, "G", "MILLER, JEFFREY A. (JEFF)", 85140,  # Clerk (NY cluster)
  2004, "NY", "H", 24, "G", "WALRATH, DAVID L.", 23228,  # Clerk (NY cluster)
  2004, "NY", "H", 25, "G", "WALSH, JAMES THOMAS (JIM)", 189063,  # Clerk (NY cluster)
  2004, "NY", "H", 25, "G", "HAWKINS, HOWIE", 20106,  # Clerk (NY cluster)
  2004, "NY", "H", 26, "G", "REYNOLDS, THOMAS M.", 157466,  # Clerk (NY cluster)
  2004, "NY", "H", 26, "G", "DAVIS, JOHN R. (JACK)", 125613,  # Clerk (NY cluster)
  2004, "NY", "H", 27, "G", "NAPLES, NANCY", 139558,  # Clerk (NY cluster)
  2004, "NY", "H", 27, "G", "HIGGINS, BRIAN M.", 143332,  # Clerk (NY cluster)
  2004, "NY", "H", 28, "G", "LABA, MICHAEL D.", 54543,  # Clerk (NY cluster)
  2004, "NY", "H", 28, "G", "SLAUGHTER, LOUISE MCINTOSH", 159655,  # Clerk (NY cluster)
  2004, "NY", "H", 29, "G", "KUHL, JOHN RANDY, JR.", 136883,  # Clerk (NY cluster)
  2004, "NY", "H", 29, "G", "BAREND, SAMARA", 110241,  # Clerk (NY cluster)
  2004, "NY", "H", 29, "G", "ASSINI, MARK W.", 17272,  # Clerk (NY cluster)
  2004, "NY", "S", 3, "G", "MILLS, HOWARD D., III", 1625069,  # Clerk (NY cluster)
  2004, "NY", "S", 3, "G", "SCHUMER, CHARLES E. (CHUCK)", 4769824,  # Clerk (NY cluster)
  2004, "NY", "S", 3, "G", "O'GRADY, MARILYN F.", 220960,  # Clerk (NY cluster)
  2004, "NY", "S", 3, "G", "MCREYNOLDS, DAVID", 36942,  # Clerk (NY cluster)
  2004, "NY", "S", 3, "G", "SILBERGER, DONALD", 19073,  # Clerk (NY cluster)
  2004, "NY", "S", 3, "G", "HIRSCHFELD, ABRAHAM", 16196,  # Clerk (NY cluster)
  2004, "RI", "H", 1, "G", "ROGERS, DAVID W. (DAVE)", 69819,  # state confirms clerk
  2004, "RI", "H", 1, "G", "KENNEDY, PATRICK J.", 124923,  # state confirms clerk
  2004, "RI", "H", 2, "G", "BARTON, ARTHUR CHUCK", 43139,  # state confirms clerk
  2004, "RI", "H", 2, "G", "LANGEVIN, JAMES R. (JIM)", 154392,  # state confirms clerk
  2004, "TX", "H", 5, "G", "BERNSTEIN, WILLIAM (BILL)", 75911,  # state confirms clerk
  2004, "TX", "H", 30, "G", "DAVIS, JOHN H.", 10821,  # state confirms clerk
  2004, "UT", "H", 1, "G", "BISHOP, ROBERT WILLIAM (ROB)", 199615,  # state confirms clerk
  2004, "UT", "H", 1, "G", "THOMPSON, STEVE", 85630,  # state confirms clerk
  2004, "UT", "H", 2, "G", "SWALLOW, JOHN E.", 147778,  # state confirms clerk
  2004, "UT", "H", 2, "G", "MATHESON, JAMES D. (JIM)", 187250,  # state confirms clerk
  2004, "UT", "H", 3, "G", "CANNON, CHRISTOPHER B. (CHRIS)", 173010,  # state confirms clerk
  2004, "UT", "H", 3, "G", "BABKA, BEAU", 88748,  # state confirms clerk
  2004, "UT", "S", 3, "G", "BENNETT, ROBERT F.", 626640,  # state confirms clerk
  2004, "UT", "S", 3, "G", "VAN DAM, R. PAUL", 258955,  # state confirms clerk
  2004, "UT", "S", 3, "G", "VAN HORN, GARY R.", 17289,  # state confirms clerk
  2004, "UT", "S", 3, "G", "LABONTE, JOSEPH K.", 8824,  # state confirms clerk
  2006, "ME", "S", 1, "G", "SLAVICK, WILLIAM H.", 29223,  # state confirms neither
  2006, "NJ", "H", 13, "G", "HESTER, RICHARD S. (DICK), SR.", 586,  # state confirms clerk
  2008, "OH", "H", 11, "S", "FUDGE, MARCIA L.", 8597,  # Nov 18 special; state confirms neither
  2012, "AL", "H", 3, "G", "HARRIS, JOHN ANDREW", 98141,  # state confirms clerk
  2012, "IN", "H", 2, "G", "RUIZ, JOSEPH WAYNE (JOE)", 9326,  # state confirms clerk
  2012, "IN", "S", 1, "G", "HORNING, ANDREW MICHAEL (ANDY)", 145282,  # state confirms clerk
  2012, "MI", "H", 9, "G", "FULNER, JIM", 6100,  # state confirms clerk
  2012, "OK", "H", 4, "G", "BEBO, DONNA MARIE", 71846,  # state confirms clerk
  2012, "PA", "H", 3, "G", "EATON, MELISSA ANN (MISSA)", 123933,  # state confirms clerk
  2012, "TN", "H", 1, "G", "BRACKETT, KAREN SHERRY", 4837,  # state confirms clerk
  2012, "VA", "H", 10, "G", "CHISHOLM, JAMES KEVIN", 9855,  # state confirms clerk
  2012, "VA", "H", 11, "G", "GALDO, JOSEPH F. (JOE), JR.", 2195,  # state confirms clerk
  2012, "WI", "H", 6, "G", "PETRI, THOMAS EVERT (TOM)", 223460,  # state confirms clerk
  2012, "WV", "H", 1, "G", "MCKINLEY, DAVID B.", 133809,  # state confirms clerk
  2012, "WV", "H", 1, "G", "THORN, SUSAN CAROL (SUE)", 80342,  # state confirms clerk
  2012, "WV", "H", 2, "G", "CAPITO, SHELLEY MOORE", 158206,  # state confirms clerk
  2012, "WV", "H", 2, "G", "SWINT, HOWARD", 68560,  # state confirms clerk
  2012, "WV", "H", 3, "G", "SNUFFER, RICHARD RAY (RICK)", 92238,  # state confirms clerk
  2012, "WV", "H", 3, "G", "RAHALL, NICK JOE, II", 108199,  # state confirms clerk
  2012, "WV", "S", 1, "G", "RAESE, JOHN REEVES", 240787,  # state confirms clerk
  2012, "WV", "S", 1, "G", "MANCHIN, JOE, III", 399908,  # state confirms neither
  2012, "WV", "S", 1, "G", "BABER, BOB HENRY", 19517,  # state confirms clerk
  2016, "SC", "H", 5, "G", "MULVANEY, JOHN MICHAEL (MICK)", 161669,  # state confirms clerk
  2016, "SC", "H", 5, "G", "PERSON, FRANCIS (FRAN)", 105772,  # state confirms clerk
  2016, "SC", "H", 5, "G", "BARNES, RUDY, JR.", 5388,  # state confirms clerk
  2020, "NY", "H", 17, "G", "MCARDLE-SCHULMAN, MAUREEN", 117309   # state confirms neither
)

cand <- cand |>
  rows_update(verified, by = c("year", "state", "office", "dist", "type", "name_snyder"), unmatched = "error")

# 12. Maine Senate 2012 and 2018 ----
# 03 adds the two King races under each other's year; King holds a class 1 seat, as does Sanders
cand <- cand |>
  tidylog::mutate(
    swap = state == "ME" & office == "S" & year %in% c(2012, 2018) & name_snyder != "KING, ANGUS S., JR.",
    year = case_when(
      swap & year == 2012 ~ 2018,
      swap & year == 2018 ~ 2012,
      .default = year
    ),
    nextup = if_else(swap, year + 6, nextup),
    name_snyder = replace(name_snyder, state == "ME" & name_snyder == "SUMNERS, CHARLES E., JR.", "SUMMERS, CHARLES EDWARD, JR."),
    inc = case_when(
      state == "ME" & office == "S" & name_snyder == "KING, ANGUS S., JR." & year == 2012 ~ 0,
      state == "ME" & office == "S" & name_snyder == "KING, ANGUS S., JR." & year == 2018 ~ 1,
      .default = inc
    ),
    vote_g = case_when(
      state == "ME" & office == "S" & name_snyder == "KING, ANGUS S., JR." & year == 2012 ~ 370580,
      state == "ME" & office == "S" & name_snyder == "KING, ANGUS S., JR." & year == 2018 ~ 344575,
      .default = vote_g
    ),
    dist = replace(dist, office == "S" & year == 2024 & state %in% c("ME", "VT"), 1)
  ) |>
  select(-swap)

# 13. Clerk audit, 2008 ----

## Votes ----
# OH-11 also held the Nov 18 special that Snyder's regular-race row carried
votes_2008 <- tribble(
  ~year, ~state, ~office, ~dist, ~type, ~name_snyder, ~vote_g,
  2008, "CA", "H", 37, "G", "DIBS, NICHOLAS (NICK)", 42774,  # p. 8
  2008, "CO", "S", 2, "G", "UDALL, MARK EMERY", 1230994,  # p. 12
  2008, "CO", "S", 2, "G", "SCHAFFER, ROBERT W. (BOB)", 990755,  # p. 12
  2008, "CO", "S", 2, "G", "CAMPBELL, DOUGLAS (DAYHORSE)", 59733,  # p. 12
  2008, "CO", "S", 2, "G", "KINSEY, ROBERT A. (BOB)", 50004,  # p. 12
  2008, "CO", "H", 1, "G", "DEGETTE, DIANA LOUISE", 203755,  # p. 12
  2008, "CO", "H", 1, "G", "LILLY, GEORGE C.", 67345,  # p. 12
  2008, "CO", "H", 1, "G", "BUCHANAN, MARTIN L.", 12135,  # p. 12
  2008, "CO", "H", 2, "G", "POLIS, JARED S.", 215571,  # p. 12
  2008, "CO", "H", 2, "G", "STARIN, SCOTT HOWARD", 116591,  # p. 12
  2008, "CO", "H", 2, "G", "CALHOUN, JOSEPH A. (JOE)", 10026,  # p. 12
  2008, "CO", "H", 3, "G", "SALAZAR, JOHN TONY", 203455,  # p. 12
  2008, "CO", "H", 4, "G", "MARKEY, ELIZABETH HELEN (BETSY)", 187347,  # p. 12
  2008, "CO", "H", 4, "G", "MUSGRAVE, MARILYN N.", 146028,  # p. 12
  2008, "CO", "H", 5, "G", "BIDLACK, HAROLD WILFORD (HAL)", 113025,  # p. 12
  2008, "CO", "H", 5, "G", "LAMBORN, DOUGLAS L. (DOUG)", 183178,  # p. 12
  2008, "CO", "H", 6, "G", "ENG, HENRY H. (HANK)", 162639,  # p. 12
  2008, "CO", "H", 7, "G", "PERLMUTTER, EDWIN G. (ED)", 173931,  # p. 12
  2008, "CO", "H", 7, "G", "LEREW, JOHN WILLIAM", 100055,  # p. 12
  2008, "PA", "H", 3, "G", "ENGLISH, PHILIP S. (PHIL)", 139707,  # p. 55
  2008, "OH", "H", 11, "G", "FUDGE, MARCIA L.", 212667   # p. 53
)

cand <- cand |>
  rows_update(votes_2008, by = c("year", "state", "office", "dist", "type", "name_snyder"), unmatched = "error")

## Candidates missing from Snyder ----
adds_2008 <- tribble(
  ~state, ~year, ~office, ~dist, ~type, ~nextup, ~party, ~party_formal, ~name_snyder, ~inc, ~vote_g, ~w_g,
  "OH", 2008, "H", 2, "G", 2010, "Other", "W-I", "CONDIT, JAMES J. (JIM), JR.", 0, 30, 0,
  "TN", 2008, "H", 8, "G", 2010, "Other", "W-I", "HART, JAMES L.", 0, 54, 0,
  "CO", 2008, "H", 1, "G", 2010, "Other", "W-I", "SWING, GARY WARREN", 0, 11, 0,
  "MD", 2008, "H", 8, "G", 2010, "Other", "W-I", "YOUNG, LIH Y.", 0, 28, 0,
  "MI", 2008, "H", 11, "G", 2010, "Other", "W-I", "DASHAIRYA, BHAGWAN (BOB)", 0, 23, 0,
  "OH", 2008, "H", 1, "G", 2010, "Other", "W-I", "STEVENSON, RICHARD LEE (RICH)", 0, 67, 0,
  "CA", 2008, "H", 1, "G", 2010, "Other", "W-I", "ELIZONDO, PAMELA (PAM)", 0, 14, 0,
  "MD", 2008, "H", 7, "G", 2010, "Other", "W-I", "BLY, RAYMOND J. (RAY)", 0, 13, 0,
  "MI", 2008, "H", 7, "G", 2010, "Other", "W-I", "RENIER, SHARON MARIE", 0, 89, 0,
  "MN", 2008, "S", 2, "G", 2014, "Other", "W-I", "CAVLAN, MICHAEL JAMES", 0, 13, 0,
  "CA", 2008, "H", 11, "G", 2010, "Other", "W-I", "CHRISTENSEN, DAVID", 0, 12, 0,
  "CO", 2008, "S", 2, "G", 2014, "Other", "W-I", "COOPER, GARY", 0, 10, 0,
  "CO", 2008, "H", 5, "G", 2010, "Other", "W-I", "HAND, RICHARD D. (RICH)", 0, 45, 0,
  "CT", 2008, "H", 5, "G", 2010, "Other", "W-I", "GENGARELLY, WALTER J.", 0, 10, 0,
  "IL", 2008, "H", 9, "G", 2010, "Other", "W-I", "ATANUS, SUSANNE", 0, 13, 0,
  "MN", 2008, "H", 6, "G", 2010, "Other", "W-I", "IMMELMAN, AUBREY", 0, 20, 0,
  "CA", 2008, "H", 8, "G", 2010, "Other", "W-I", "SHERMAN, LEA", 0, 11, 0,
  "CA", 2008, "H", 37, "G", 2010, "Other", "W-I", "DAVIS, LEE", 0, 10, 0,
  "OH", 2008, "H", 1, "G", 2010, "Other", "W-I", "WILSON, ERIC", 0, 85, 0,
  "CA", 2008, "H", 9, "G", 2010, "Other", "W-I", "HELLER, DAVID", 0, 37, 0,
  "MD", 2008, "H", 4, "G", 2010, "Other", "W-I", "BROADUS, BOBBY", 0, 48, 0,
  "TN", 2008, "H", 1, "G", 2010, "I", "I", "GOODMAN, JOEL", 0, 3988, 0,
  "CA", 2008, "H", 9, "G", 2010, "Other", "W-I", "KULA, CHRISTOPHER", 0, 27, 0,
  "CA", 2008, "H", 52, "G", 2010, "Other", "W-I", "RYAN, JOSEPH M.", 0, 47, 0,
  "CT", 2008, "H", 2, "G", 2010, "Other", "W-I", "VACHON, TODD", 0, 19, 0,
  "CT", 2008, "H", 4, "G", 2010, "Other", "W-I", "FLANAGAN, EUGENE", 0, 10, 0,
  "FL", 2008, "H", 23, "G", 2010, "Other", "W-I", "COOK, APRIL", 0, 40, 0,
  "GA", 2008, "H", 4, "G", 2010, "Other", "W-I", "COFFIELD, FAYE", 0, 35, 0,
  "GA", 2008, "H", 5, "G", 2010, "Other", "W-I", "KASH, SHIRA", 0, 81, 0,
  "GA", 2008, "H", 5, "G", 2010, "Other", "W-I", "FITZMAURICE, JEANNE", 0, 25, 0,
  "IL", 2008, "H", 13, "G", 2010, "Other", "W-I", "KNAPP, THEODORE", 0, 51, 0,
  "MD", 2008, "H", 4, "G", 2010, "Other", "W-I", "JACKSON, DARRYN O'SHEA, SR.", 0, 28, 0,
  "MD", 2008, "H", 4, "G", 2010, "Other", "W-I", "SCHULIN, STEVE", 0, 75, 0,
  "MD", 2008, "H", 8, "G", 2010, "Other", "W-I", "VOLLMER, DEBORAH A.", 0, 188, 0,
  "MN", 2008, "S", 2, "G", 2014, "Other", "W-I", "PRICE, ANTHONY KEITH", 0, 12, 0,
  "MN", 2008, "H", 2, "G", 2010, "Other", "W-I", "MASRUD, KEVIN", 0, 41, 0,
  "MO", 2008, "H", 7, "G", 2010, "Other", "W-I", "POTTS, MIDGELLE R.", 0, 49, 0,
  "OH", 2008, "H", 13, "G", 2010, "Other", "W-I", "CROW, ROBERT", 0, 37, 0,
  "WV", 2008, "S", 2, "G", 2014, "Other", "W-I", "BARTLETT, JOHN R. (RICK)", 0, 83, 0,
  "WV", 2008, "S", 2, "G", 2014, "Other", "W-I", "SHAFFER, CHAD", 0, 36, 0,
  "WV", 2008, "H", 1, "G", 2010, "Other", "W-I", "OSGOOD, TED", 0, 69, 0,
  "WV", 2008, "H", 1, "G", 2010, "Other", "W-I", "SMITH, R.J.", 0, 61, 0,
  "WV", 2008, "H", 2, "G", 2010, "Other", "W-I", "MILLS, AARON", 0, 16, 0
)

cand <- cand |>
  rows_insert(adds_2008, by = c("year", "state", "office", "dist", "type", "name_snyder"), conflict = "error")

# 14. Clerk audit, 2010 ----

## Votes ----
cand <- cand |>
  rows_update(tribble(
    ~year, ~state, ~office, ~dist, ~type, ~name_snyder, ~vote_g,
    2010, "AK", "S", 3, "G", "MILLER, JOE", 90839   # p. 3
  ), by = c("year", "state", "office", "dist", "type", "name_snyder"), unmatched = "error")

## Candidates missing from Snyder ----
adds_2010 <- tribble(
  ~state, ~year, ~office, ~dist, ~type, ~nextup, ~party, ~party_formal, ~name_snyder, ~inc, ~vote_g, ~w_g,
  "MD", 2010, "S", 3, "G", 2016, "Other", "W-I", "YOUNG, LIH Y.", 0, 80, 0,
  "CT", 2010, "H", 2, "G", 2012, "Other", "W-I", "REALE, DANIEL J. (DAN)", 0, 27, 0,
  "FL", 2010, "H", 4, "G", 2012, "Other", "W-I", "KONIZ, GARY LEE", 0, 27, 0,
  "CT", 2010, "S", 3, "G", 2016, "Other", "W-I", "RUSSELL, JEFFREY (JEFF)", 0, 45, 0,
  "OH", 2010, "H", 15, "G", 2012, "Other", "W-I", "BUCKEL, WILLIAM L. (BILL)", 0, 45, 0,
  "MD", 2010, "H", 7, "G", 2012, "Other", "W-I", "BLY, RAYMOND J. (RAY)", 0, 20, 0,
  "CA", 2010, "H", 17, "G", 2012, "Other", "W-I", "KABAT, RONALD PAUL", 0, 90, 0,
  "FL", 2010, "H", 2, "G", 2012, "Other", "W-I", "NETHERWOOD, RAY", 0, 16, 0,
  "IL", 2010, "S", 3, "G", 2016, "Other", "W-I", "ATANUS, SUSANNE", 0, 19, 0,
  "AR", 2010, "H", 2, "G", 2012, "Other", "W-I", "SUITS, DANIAL", 0, 14, 0,
  "CO", 2010, "S", 3, "G", 2016, "Other", "W-I", "LOHMILLER, BRUCE EDWARD", 0, 10, 0,
  "CT", 2010, "S", 3, "G", 2016, "Other", "W-I", "VACHON, TODD", 0, 45, 0,
  "CT", 2010, "S", 3, "G", 2016, "Other", "W-I", "VASSAR, CARL E.", 0, 31, 0,
  "CT", 2010, "S", 3, "G", 2016, "Other", "W-I", "TRACESKI, JOHN M.", 0, 15, 0,
  "CT", 2010, "H", 4, "G", 2012, "Other", "W-I", "FLANAGAN, EUGENE", 0, 10, 0,
  "CT", 2010, "H", 5, "G", 2012, "Other", "W-I", "PISTONE, JOHN J.", 0, 20, 0,
  "FL", 2010, "S", 3, "G", 2016, "Other", "W-I", "BLASS, PIOTR", 0, 47, 0,
  "GA", 2010, "S", 3, "G", 2016, "Other", "W-I", "BECKWORTH, RAYMOND D.", 0, 24, 0,
  "KY", 2010, "H", 6, "G", 2012, "Other", "W-I", "VANCE, RANDOLPH S.", 0, 22, 0,
  "MO", 2010, "H", 6, "G", 2012, "Other", "W-I", "YARBER, KYLE", 0, 47, 0,
  "TX", 2010, "H", 22, "G", 2012, "Other", "W-I", "WILLIAMS, JOHNNY", 0, 66, 0,
  "GA", 2010, "S", 3, "G", 2016, "Other", "W-I", "DAVIS, STEVE", 0, 52, 0,
  "CA", 2010, "S", 3, "G", 2016, "Other", "W-I", "HARRIS, JAMES E.", 0, 41, 0,
  "CO", 2010, "S", 3, "G", 2016, "Other", "W-I", "NEWMAN, MICHELE M.", 0, 19, 0,
  "MI", 2010, "H", 7, "G", 2012, "Other", "W-I", "DAVIS, DANNY", 0, 21, 0,
  "TX", 2010, "H", 18, "G", 2012, "Other", "W-I", "MEYER, CHARLES B. (CHUCK M)", 0, 28, 0,
  "LA", 2010, "H", 2, "G", 2012, "Other", "Other", "RADOSTA, JACK", 0, 645, 0,
  "AZ", 2010, "S", 3, "G", 2016, "Other", "W-I", "CHAVEZ, SANTOS", 0, 39, 0,
  "AZ", 2010, "S", 3, "G", 2016, "Other", "W-I", "DUDIKOFF, SYDNEY", 0, 14, 0,
  "CA", 2010, "S", 3, "G", 2016, "Other", "W-I", "VLAKANCIC, CONNOR", 0, 11, 0,
  "CA", 2010, "S", 3, "G", 2016, "Other", "W-I", "CARROLL, JERRY LEON", 0, 10, 0,
  "CA", 2010, "H", 2, "G", 2012, "Other", "W-I", "JENSEN, MARK A.", 0, 11, 0,
  "CA", 2010, "H", 5, "G", 2012, "Other", "W-I", "LACY, TONY", 0, 19, 0,
  "CA", 2010, "H", 8, "G", 2012, "Other", "W-I", "SHIELDS, SUMMER", 0, 24, 0,
  "CA", 2010, "H", 12, "G", 2012, "Other", "W-I", "HARDING, JOSEPH MICHAEL", 0, 32, 0,
  "CA", 2010, "H", 41, "G", 2012, "Other", "W-I", "ZANDER, PAMELA", 0, 35, 0,
  "CA", 2010, "H", 46, "G", 2012, "Other", "W-I", "SHAH, JAY", 0, 20, 0,
  "CO", 2010, "S", 3, "G", 2016, "Other", "W-I", "RANK, ROBERT", 0, 67, 0,
  "CO", 2010, "H", 3, "G", 2012, "Other", "W-I", "HARGIS, JOHN W., SR.", 0, 23, 0,
  "CO", 2010, "H", 3, "G", 2012, "Other", "W-I", "FRITZ, JAMES", 0, 11, 0,
  "CT", 2010, "S", 3, "G", 2016, "Other", "W-I", "GILES, JAY J.", 0, 17, 0,
  "CT", 2010, "H", 2, "G", 2012, "Other", "W-I", "BIANCHI, MURIEL P.", 0, 19, 0,
  "DE", 2010, "S", 2, "S", 2014, "Other", "W-I", "BOURGEOIS, MAURICE F.", 0, 25, 0,
  "DE", 2010, "S", 2, "S", 2014, "Other", "W-I", "DEVARD, SAMTRA", 0, 10, 0,
  "FL", 2010, "S", 3, "G", 2016, "Other", "W-I", "LOCK, RICHARD", 0, 18, 0,
  "FL", 2010, "S", 3, "G", 2016, "Other", "W-I", "QUARTERMAN-NOAH, BELINDA GAIL", 0, 18, 0,
  "FL", 2010, "S", 3, "G", 2016, "Other", "W-I", "DRAKE, GEORGE", 0, 13, 0,
  "FL", 2010, "H", 4, "G", 2012, "Other", "W-I", "PUESCHEL, DEBORAH KATZ (DEB)", 0, 40, 0,
  "FL", 2010, "H", 8, "G", 2012, "Other", "W-I", "GERRITZEN, STEVEN J.", 0, 11, 0,
  "GA", 2010, "S", 3, "G", 2016, "Other", "W-I", "BROWN, BRIAN RUSSELL", 0, 12, 0,
  "ID", 2010, "S", 3, "G", 2016, "Other", "W-I", "REINISCH, FREDERICK R., JR.", 0, 91, 0,
  "IL", 2010, "S", 3, "G", 2016, "Other", "W-I", "DABNEY, COREY", 0, 33, 0,
  "IL", 2010, "S", 3, "G", 2016, "Other", "W-I", "HORTON, SHON-TIYON (SANTIAGO)", 0, 16, 0,
  "IL", 2010, "S", 3, "G", 2016, "Other", "W-I", "NAGER, AVNER", 0, 15, 0,
  "IL", 2010, "S", 3, "G", 2016, "Other", "W-I", "JAGLA, STAN", 0, 12, 0,
  "IL", 2010, "H", 14, "G", 2012, "Other", "W-I", "MARKS, DOUG", 0, 50, 0,
  "MD", 2010, "S", 3, "G", 2016, "Other", "W-I", "WHITTINGTON, DENISE L.", 0, 56, 0,
  "MD", 2010, "S", 3, "G", 2016, "Other", "W-I", "EVANS, ROBERT J.", 0, 14, 0,
  "MD", 2010, "H", 1, "G", 2012, "Other", "W-I", "KENNEDY, MICHAEL", 0, 18, 0,
  "MD", 2010, "H", 7, "G", 2012, "Other", "W-I", "DICKSON, FRED DONALD, JR.", 0, 55, 0,
  "MI", 2010, "H", 8, "G", 2012, "Other", "W-I", "HOUSTON, KATHERINE", 0, 11, 0,
  "MN", 2010, "H", 4, "G", 2012, "Other", "W-I", "GARLAN, AMBER", 0, 39, 0,
  "MN", 2010, "H", 4, "G", 2012, "Other", "W-I", "SHEPARD, JACK", 0, 10, 0,
  "MO", 2010, "S", 3, "G", 2016, "Other", "W-I", "HOINOSKI, DALE M.", 0, 14, 0,
  "MO", 2010, "H", 7, "G", 2012, "Other", "W-I", "LADENDORF, NICHOLAS IVAN", 0, 10, 0,
  "MO", 2010, "H", 9, "G", 2012, "Other", "W-I", "BURRUS, RON", 0, 69, 0,
  "OH", 2010, "H", 16, "G", 2012, "Other", "W-I", "ROSS, ROBERT L.", 0, 67, 0,
  "OH", 2010, "H", 18, "G", 2012, "Other", "W-I", "PITRONE, MARK", 0, 20, 0,
  "TX", 2010, "H", 3, "G", 2012, "Other", "W-I", "PIERCE, HARRY", 0, 22, 0
)

cand <- cand |>
  rows_insert(adds_2010, by = c("year", "state", "office", "dist", "type", "name_snyder"), conflict = "error")

# 15. Clerk audit, 2012 ----

## Votes ----
votes_2012 <- tribble(
  ~year, ~state, ~office, ~dist, ~type, ~name_snyder, ~vote_g,
  2012, "GA", "H", 4, "G", "VAUGHN, JEFFERY CHRIS", 75041,  # p. 17
  2012, "IA", "H", 3, "G", "BOSWELL, LEONARD LEROY", 168632,  # p. 22
  2012, "KS", "H", 2, "G", "HAWVER, DENNIS", 12520,  # p. 22
  2012, "ME", "S", 1, "G", "DALTON, DANNY FRANCIS", 5807,  # p. 26
  2012, "MA", "H", 1, "G", "NEAL, RICHARD E.", 261936,  # p. 30
  2012, "MA", "H", 3, "G", "GOLNIK, JONATHAN A. (JON)", 109372,  # p. 30
  2012, "MS", "H", 3, "G", "HARPER, GREGG", 234717,  # p. 35
  2012, "MO", "S", 1, "G", "DINE, JONATHAN", 165468,  # p. 35
  2012, "MO", "H", 3, "G", "WILSON, STEVEN (STEVE)", 12353,  # p. 36
  2012, "NJ", "S", 1, "G", "KYRILLOS, JOSEPH M. (JOE), JR.", 1329534,  # p. 39
  2012, "NJ", "S", 1, "G", "MENENDEZ, ROBERT (BOB)", 1987680,  # p. 39
  2012, "NJ", "S", 1, "G", "WOLSKI, KENNETH (KEN)", 15801,  # p. 39
  2012, "NJ", "S", 1, "G", "DIAKOS, GWEN", 9359,  # p. 39
  2012, "NJ", "S", 1, "G", "LAVERGNE, EUGENE MARTIN", 2198,  # p. 39
  2012, "NJ", "S", 1, "G", "DRANIKOFF, J. DAVID", 3834,  # p. 39
  2012, "NJ", "S", 1, "G", "SONI, INDER JIT (ANDY)", 3593,  # p. 39
  2012, "NJ", "S", 1, "G", "TURKAVAGE, ROBERT D. (TURK)", 3532,  # p. 39
  2012, "NJ", "S", 1, "G", "PASON, GREGORY", 2249,  # p. 39
  2012, "NJ", "S", 1, "G", "BROOKS, DARYL MIKELL", 2066,  # p. 39
  2012, "NJ", "H", 2, "G", "LOBIONDO, FRANK A.", 166677,  # p. 39
  2012, "NJ", "H", 2, "G", "SHOBER, CASSANDRA", 116462,  # p. 39
  2012, "NJ", "H", 3, "G", "RUNYAN, JON", 174253,  # p. 39
  2012, "NJ", "H", 3, "G", "ADLER, SHELLEY", 145506,  # p. 39
  2012, "NJ", "H", 4, "G", "SMITH, CHRISTOPHER H. (CHRIS)", 195145,  # p. 39
  2012, "NJ", "H", 4, "G", "FROELICH, BRIAN P.", 107991,  # p. 39
  2012, "NJ", "H", 5, "G", "GARRETT, E. SCOTT", 167501,  # p. 40
  2012, "NJ", "H", 5, "G", "GUSSEN, ADAM JEFFREY", 130100,  # p. 40
  2012, "NJ", "H", 5, "G", "ALESSANDRINI, PATRICIA", 6770,  # p. 40
  2012, "NJ", "H", 7, "G", "LANCE, LEONARD", 175662,  # p. 40
  2012, "NJ", "H", 7, "G", "CHIVUKULA, UPENDRA J.", 123057,  # p. 40
  2012, "NJ", "H", 7, "G", "MCKNIGHT, PATRICK", 4078,  # p. 40
  2012, "NJ", "H", 7, "G", "BREEN, DENNIS ARTHUR", 4518,  # p. 40
  2012, "NJ", "H", 8, "G", "KARCZEWSKI, MARIA PINEIRO", 31763,  # p. 40
  2012, "NJ", "H", 8, "G", "SIRES, ALBIO", 130853,  # p. 40
  2012, "NJ", "H", 8, "G", "SHAW, HERBERT H.", 1839,  # p. 40
  2012, "NJ", "H", 9, "G", "BOTEACH, SHMULEY", 55091,  # p. 40
  2012, "NJ", "H", 9, "G", "PASCRELL, WILLIAM J. (BILL), JR.", 162822,  # p. 40
  2012, "NJ", "H", 11, "G", "FRELINGHUYSEN, RODNEY P.", 182237,  # p. 40
  2012, "NJ", "H", 11, "G", "ARVANITES, JOHN", 123897,  # p. 40
  2012, "NJ", "H", 12, "G", "BECK, ERIC A.", 80906,  # p. 40
  2012, "NJ", "H", 12, "G", "HOLT, RUSH D.", 189926,  # p. 40
  2012, "NY", "H", 10, "G", "CHAN, MICHAEL W.", 39311,  # p. 43
  2012, "NY", "H", 18, "G", "HAYWORTH, NAN A.S.", 133049,  # p. 44
  2012, "NY", "H", 25, "G", "BROOKS, MAGGIE", 133389,  # p. 45
  2012, "NC", "H", 7, "G", "ROUZER, DAVID CHESTON", 168041,  # p. 47
  2012, "NC", "H", 7, "G", "MCINTYRE, MIKE", 168695,  # p. 47
  2012, "SC", "H", 5, "G", "KNOTT, JOYCE", 123443,  # p. 55
  2012, "WA", "H", 1, "G", "DELBENE, SUZAN K.", 177025   # p. 65
)

cand <- cand |>
  rows_update(votes_2012, by = c("year", "state", "office", "dist", "type", "name_snyder"), unmatched = "error")

## Candidates missing from Snyder ----
adds_2012 <- tribble(
  ~state, ~year, ~office, ~dist, ~type, ~nextup, ~party, ~party_formal, ~name_snyder, ~inc, ~vote_g, ~w_g,
  "MD", 2012, "S", 1, "G", 2018, "Other", "W-I", "YOUNG, LIH Y.", 0, 163, 0,
  "FL", 2012, "H", 4, "G", 2014, "Other", "W-I", "KONIZ, GARY LEE", 0, 246, 0,
  "CT", 2012, "S", 1, "G", 2018, "Other", "W-I", "RUSSELL, JEFFREY (JEFF)", 0, 40, 0,
  "MD", 2012, "H", 2, "G", 2014, "Other", "W-I", "BLY, RAYMOND J. (RAY)", 0, 22, 0,
  "IL", 2012, "H", 2, "G", 2014, "Other", "W-I", "WILLIAMS, ANTHONY W.", 0, 288, 0,
  "IL", 2012, "H", 2, "G", 2014, "I", "I", "LEWIS, MARCUS", 0, 40006, 0,
  "CT", 2012, "S", 1, "G", 2018, "Other", "W-I", "TRACESKI, JOHN M.", 0, 19, 0,
  "CT", 2012, "H", 5, "G", 2014, "Other", "W-I", "PISTONE, JOHN J.", 0, 12, 0,
  "MO", 2012, "H", 4, "G", 2014, "Other", "Constitution", "COWAN, GREG", 0, 2959, 0,
  "FL", 2012, "S", 1, "G", 2018, "Other", "W-I", "SCOTT, LAWRENCE", 0, 18, 0,
  "IN", 2012, "S", 1, "G", 2018, "Other", "W-I", "JOHNSON, JAMES L., JR.", 0, 15, 0,
  "MD", 2012, "H", 7, "G", 2014, "Other", "W-I", "SMITH, CHARLES ULYSSES", 0, 28, 0,
  "TN", 2012, "S", 1, "G", 2018, "Other", "W-I", "MAYNARD, JIM", 0, 117, 0,
  "FL", 2012, "H", 2, "G", 2014, "Other", "W-I", "MILLER, FLOYD PATRICK", 0, 228, 0,
  "TN", 2012, "S", 1, "G", 2018, "Other", "W-I", "DAVIS, GARY GENE", 0, 18, 0,
  "IL", 2012, "H", 11, "G", 2014, "Other", "W-I", "MICHEL, CHRIS", 0, 19, 0,
  "TX", 2012, "H", 35, "G", 2014, "Other", "W-I", "ALVARADO, SIMON", 0, 37, 0,
  "TN", 2012, "S", 1, "G", 2018, "Other", "W-I", "STEWART, LEE", 0, 14, 0,
  "TN", 2012, "H", 3, "G", 2014, "Other", "W-I", "HALL, RICHARD", 0, 18, 0,
  "ID", 2012, "H", 1, "G", 2014, "I", "I", "RICHARDSON, MARVIN (PRO-LIFE)", 0, 7607, 0,
  "MI", 2012, "S", 1, "G", 2018, "Other", "Natural Law", "LITLE, JOHN D.", 0, 11229, 0,
  "MO", 2012, "H", 2, "G", 2014, "Other", "Constitution", "ZORIKOVA, ANATOL", 0, 2012, 0,
  "AZ", 2012, "S", 1, "G", 2018, "Other", "W-I", "WATTS, STEVEN", 0, 290, 0,
  "AZ", 2012, "S", 1, "G", 2018, "Other", "W-I", "MANSPEAKER, DON", 0, 24, 0,
  "AZ", 2012, "H", 2, "G", 2014, "Other", "W-I", "PROWELL, ANTHONY", 0, 57, 0,
  "CT", 2012, "S", 1, "G", 2018, "Other", "W-I", "ADAMS, MICHAEL D.", 0, 388, 0,
  "CT", 2012, "S", 1, "G", 2018, "Other", "W-I", "COLEMAN, MATTHEW", 0, 6414, 0,
  "CT", 2012, "H", 5, "G", 2014, "Other", "W-I", "JAEGER, RUSS", 0, 10, 0,
  "FL", 2012, "S", 1, "G", 2018, "Other", "W-I", "CRAINE, NAOMI", 0, 19, 0,
  "FL", 2012, "S", 1, "G", 2018, "Other", "W-I", "MONROE, ROBERT", 0, 12, 0,
  "FL", 2012, "H", 7, "G", 2014, "Other", "W-I", "MARRA, FRED", 0, 13, 0,
  "FL", 2012, "H", 10, "G", 2014, "Other", "W-I", "SEEGOLAM, NAIPAUL", 0, 46, 0,
  "FL", 2012, "H", 17, "G", 2014, "Other", "W-I", "BAUMANN, TOM", 0, 12, 0,
  "FL", 2012, "H", 18, "G", 2014, "Other", "W-I", "HOLLOMAN, MARILYN DAVIS", 0, 55, 0,
  "ID", 2012, "H", 2, "G", 2014, "Other", "W-I", "CHAPPELL, JACK WAYNE", 0, 235, 0,
  "IL", 2012, "H", 17, "G", 2014, "Other", "W-I", "REYES, ERIC", 0, 10, 0,
  "MD", 2012, "S", 1, "G", 2018, "Other", "W-I", "BAKER, BRANDY", 0, 151, 0,
  "MD", 2012, "S", 1, "G", 2018, "Other", "W-I", "TINUS, ED", 0, 48, 0,
  "MD", 2012, "S", 1, "G", 2018, "Other", "W-I", "PODLESAK, MARY", 0, 21, 0,
  "MD", 2012, "H", 1, "G", 2014, "Other", "W-I", "CALPINO, MICHAEL", 0, 71, 0,
  "MD", 2012, "H", 1, "G", 2014, "Other", "W-I", "RAE, DOUGLAS DRYDEN", 0, 26, 0,
  "MD", 2012, "H", 7, "G", 2014, "Other", "W-I", "BUSCH, TY GLEN", 0, 10, 0,
  "MI", 2012, "S", 1, "G", 2018, "Other", "W-I", "GHOSH, AMITABH", 0, 48, 0,
  "MI", 2012, "S", 1, "G", 2018, "Other", "W-I", "HOLLOWAY, DANIEL", 0, 10, 0,
  "MI", 2012, "H", 11, "G", 2014, "Other", "W-I", "VAN GILDER, JAMES", 0, 14, 0,
  "MO", 2012, "S", 1, "G", 2018, "Other", "W-I", "KIMZEY, TED", 0, 15, 0,
  "OH", 2012, "H", 14, "G", 2014, "Other", "W-I", "ZURBRUGG, AARON J.", 0, 20, 0,
  "TN", 2012, "S", 1, "G", 2018, "Other", "W-I", "STINNETT, ANGELIA", 0, 470, 0,
  "TN", 2012, "S", 1, "G", 2018, "Other", "W-I", "MAURER, JACOB", 0, 409, 0,
  "TN", 2012, "S", 1, "G", 2018, "Other", "W-I", "CRIM, LARRY", 0, 218, 0,
  "TN", 2012, "S", 1, "G", 2018, "Other", "W-I", "PENN, MARGIE NELL", 0, 23, 0,
  "TN", 2012, "S", 1, "G", 2018, "Other", "W-I", "SCHAPPERT, CHRISTOPHER", 0, 12, 0,
  "TN", 2012, "H", 5, "G", 2014, "Other", "W-I", "PUCKETT, SEAN", 0, 12, 0,
  "TX", 2012, "H", 4, "G", 2014, "Other", "W-I", "ROSTEK, FRED", 0, 188, 0
)

cand <- cand |>
  rows_insert(adds_2012, by = c("year", "state", "office", "dist", "type", "name_snyder"), conflict = "error")

## Misplaced and duplicate rows ----
# WY's minor House candidates sit in the Senate race; LA-6 has both 03's and Snyder's spelling of two candidates
cand <- cand |>
  tidylog::mutate(
    move = year == 2012 & state == "WY" & name_snyder %in% c("BRUBAKER, RICHARD P. (BRU)", "WILLS, DON", "CUMMINGS, DANIEL CLYDE (DAN)"),
    office = if_else(move, "H", office),
    nextup = if_else(move, 2014, nextup),
    name_snyder = replace(name_snyder, state == "VT" & name_snyder == "Laframboise", "LAFRAMBOISE, LAUREL")
  ) |>
  select(-move) |>
  tidylog::filter(!(year == 2012 & state == "LA" & name_snyder %in% c("CRAIG, RUFUS HOLT CRAIG, JR.", "TORREGANO, RICHARD P. (RPT)")))

# 16. Clerk audit, 2014 ----

## Votes ----
votes_2014 <- tribble(
  ~year, ~state, ~office, ~dist, ~type, ~name_snyder, ~vote_g,
  2014, "UT", "H", 1, "G", "BOWDEN, CRAIG RICHARD", 4847,  # p. 48
  2014, "WV", "H", 2, "G", "JONES, DAVY H.", 7682,  # p. 51
  2014, "WV", "H", 2, "G", "RABEL, ED", 6250,  # p. 51
  2014, "LA", "S", 2, "G", "LANDRIEU, MARY L.", 561210   # December 6 runoff, LA SOS
)

cand <- cand |>
  rows_update(votes_2014, by = c("year", "state", "office", "dist", "type", "name_snyder"), unmatched = "error")

## Candidates missing from Snyder ----
# Snyder's 2014 House races omit most minor-party candidates
adds_2014 <- tribble(
  ~state, ~year, ~office, ~dist, ~type, ~nextup, ~party, ~party_formal, ~name_snyder, ~inc, ~vote_g, ~w_g,
  "VT", 2014, "H", 1, "G", 2016, "I", "I", "ERICSON, CRIS", 0, 2750, 0,
  "IN", 2014, "H", 8, "G", 2016, "Lbt", "Lbt", "HORNING, ANDREW MICHAEL (ANDY)", 0, 6587, 0,
  "NJ", 2014, "H", 8, "G", 2016, "Other", "Politicians Are Crooks", "SHAW, HERBERT H.", 0, 1192, 0,
  "MD", 2014, "H", 8, "G", 2016, "Other", "W-I", "YOUNG, LIH Y.", 0, 16, 0,
  "MT", 2014, "H", 1, "G", 2016, "Lbt", "Lbt", "FELLOWS, MIKE", 0, 15402, 0,
  "AZ", 2014, "H", 9, "G", 2016, "Lbt", "Lbt", "GAMMILL, POWELL E.", 0, 5612, 0,
  "NJ", 2014, "H", 8, "G", 2016, "Other", "Wake Up USA", "OLIVERA, PABLO R.", 0, 1022, 0,
  "NJ", 2014, "H", 12, "G", 2016, "Grn", "Grn", "WELZER, STEVEN W.", 0, 890, 0,
  "TN", 2014, "S", 2, "G", 2020, "I", "I", "TYLER, RICK", 0, 5759, 0,
  "TX", 2014, "H", 18, "G", 2016, "I", "I", "DUNCAN, VINCE", 0, 2362, 0,
  "VT", 2014, "H", 1, "G", 2016, "Other", "Energy Independence", "TRUDELL, JERRY P.", 0, 2024, 0,
  "NJ", 2014, "H", 2, "G", 2016, "I", "I", "STEIN, GARY", 0, 612, 0,
  "NJ", 2014, "H", 12, "G", 2016, "Other", "Truth Vision Hope", "CODY, KENNETH J. (KEN)", 0, 567, 0,
  "IL", 2014, "H", 2, "G", 2016, "Other", "W-I", "LEWIS, MARCUS", 0, 130, 0,
  "MI", 2014, "H", 11, "G", 2016, "Other", "W-I", "BENTIVOLIO, KERRY", 0, 1411, 0,
  "AZ", 2014, "H", 7, "G", 2016, "Other", "Americans Elect", "DEWITT, REBECCA S.", 0, 3858, 0,
  "DE", 2014, "H", 1, "G", 2016, "Lbt", "Lbt", "GESTY, SCOTT A.", 0, 4419, 0,
  "MS", 2014, "H", 1, "G", 2016, "Lbt", "Lbt", "BEDWELL, DANNY", 0, 3830, 0,
  "MS", 2014, "H", 2, "G", 2016, "I", "I", "RAY, TROY", 0, 36465, 0,
  "MS", 2014, "H", 3, "G", 2016, "Other", "Reform", "WASHER, BARBARA DALE", 0, 1541, 0,
  "NJ", 2014, "H", 3, "G", 2016, "Other", "D-R Party", "LAVERGNE, FREDERICK JOHN", 0, 3095, 0,
  "NJ", 2014, "H", 12, "G", 2016, "Other", "Start The Conversation", "FREUDENHEIM, JACK", 0, 531, 0,
  "TX", 2014, "H", 36, "G", 2016, "Grn", "Grn", "RIDLEY, HAL J., JR.", 0, 685, 0,
  "FL", 2014, "H", 21, "G", 2016, "Other", "W-I", "TROUT, W. MICHAEL (MIKE)", 0, 575, 0,
  "AZ", 2014, "H", 8, "G", 2016, "Other", "Americans Elect", "DOLGOS, STEPHEN M.", 0, 41066, 0,
  "DE", 2014, "H", 1, "G", 2016, "Grn", "Grn", "AUGUST, BERNARD", 0, 4801, 0,
  "MN", 2014, "H", 8, "G", 2016, "Grn", "Grn", "SANDMAN, RAY SKIP", 0, 11450, 0,
  "MS", 2014, "H", 3, "G", 2016, "I", "I", "GERRARD, ROGER I.", 0, 3890, 0,
  "NJ", 2014, "H", 5, "G", 2016, "Other", "For Americans", "QUICK, MARK D.", 0, 2435, 0,
  "NJ", 2014, "H", 12, "G", 2016, "Other", "D-R Party", "CANNON, ALLEN J.", 0, 450, 0,
  "AZ", 2014, "H", 2, "G", 2016, "Other", "W-I", "DUDIKOFF, SYDNEY", 0, 48, 0,
  "AZ", 2014, "H", 3, "G", 2016, "Other", "W-I", "SANCHEZ, FEDERICO A.", 0, 43, 0,
  "FL", 2014, "H", 4, "G", 2016, "Other", "W-I", "PUESCHEL, DEBORAH KATZ (DEB)", 0, 13, 0,
  "FL", 2014, "H", 13, "G", 2016, "Other", "W-I", "LEVINSON, MICHAEL S.", 0, 86, 0,
  "GA", 2014, "H", 8, "G", 2016, "Other", "W-I", "CAIN, RONALD L.", 0, 119, 0,
  "IL", 2014, "S", 2, "G", 2020, "Other", "W-I", "DAVIS, ROGER K.", 0, 31, 0,
  "IL", 2014, "H", 17, "G", 2016, "Other", "W-I", "FAWELL, WILLIAM W. (BILL)", 0, 16, 0,
  "MD", 2014, "H", 8, "G", 2016, "Other", "W-I", "WILDMAN, ANDREW J.", 0, 102, 0,
  "NM", 2014, "H", 2, "G", 2016, "Other", "W-I", "MCGRANN, JACK A.", 0, 69, 0,
  "WI", 2014, "H", 1, "G", 2016, "Other", "W-I", "DESCHLER, KEITH R.", 0, 29, 0,
  "WI", 2014, "H", 7, "G", 2016, "Other", "W-I", "TAYLOR, ROBERT (ROB)", 0, 30, 0,
  "NJ", 2014, "H", 2, "G", 2016, "Other", "American Labor", "ROZZO, COSTANTINO", 0, 501, 0,
  "IL", 2014, "H", 9, "G", 2016, "Other", "W-I", "COLLINS, PHIL", 0, 66, 0,
  "FL", 2014, "H", 2, "G", 2016, "Other", "W-I", "LEE, LUTHER", 0, 422, 0,
  "GA", 2014, "H", 4, "G", 2016, "Other", "W-I", "DAVIS, RAYMOND L.", 0, 109, 0,
  "MO", 2014, "H", 4, "G", 2016, "Other", "W-I", "COWAN, GREGORY A.", 0, 15, 0,
  "OH", 2014, "H", 3, "G", 2016, "Other", "W-I", "APPLEGATE, RALPH A.", 0, 17, 0,
  "OH", 2014, "H", 9, "G", 2016, "Other", "W-I", "HOFFMAN, CORY", 0, 112, 0,
  "NJ", 2014, "H", 7, "G", 2016, "Lbt", "Lbt", "GAWRON, JAMES", 0, 3478, 0,
  "TX", 2014, "H", 17, "G", 2016, "Lbt", "Lbt", "HAMILTON, SHAWN MICHAEL", 0, 4009, 0,
  "MO", 2014, "H", 3, "G", 2016, "Other", "W-I", "DAVIS, HAROLD", 0, 66, 0,
  "AZ", 2014, "H", 4, "G", 2016, "Lbt", "Lbt", "RIKE, CHRIS", 0, 7440, 0,
  "AZ", 2014, "H", 7, "G", 2016, "I", "I", "PENALOSA, JOSE", 0, 3496, 0,
  "AR", 2014, "H", 2, "G", 2016, "Lbt", "Lbt", "STANDIFORD, DEBBIE", 0, 10590, 0,
  "AR", 2014, "H", 3, "G", 2016, "Lbt", "Lbt", "BRAND, GRANT", 0, 39305, 0,
  "IN", 2014, "H", 9, "G", 2016, "Lbt", "Lbt", "FREY, R. MIKE", 0, 6777, 0,
  "ME", 2014, "H", 2, "G", 2016, "I", "I", "RICHARDSON, BLAINE", 0, 31337, 0,
  "MN", 2014, "H", 4, "G", 2016, "Other", "Independence", "THOMAS, DAVE", 0, 14059, 0,
  "MN", 2014, "H", 5, "G", 2016, "Other", "Independence", "BAUER, LEE", 0, 12001, 0,
  "MS", 2014, "H", 1, "G", 2016, "Other", "Reform", "WALLEY, LAJENA", 0, 946, 0,
  "MS", 2014, "H", 2, "G", 2016, "Other", "Reform", "SHOEMAKE, SHELLEY", 0, 11493, 0,
  "MS", 2014, "H", 4, "G", 2016, "I", "I", "BURLESON, CINDY", 0, 3684, 0,
  "MS", 2014, "H", 4, "G", 2016, "Lbt", "Lbt", "ROBINSON, JOEY", 0, 3473, 0,
  "MS", 2014, "H", 4, "G", 2016, "Other", "Reform", "JACKSON, ELI (SARGE)", 0, 917, 0,
  "MS", 2014, "H", 4, "G", 2016, "I", "I", "REICH, ED", 0, 857, 0,
  "NJ", 2014, "H", 2, "G", 2016, "Other", "D-R Party", "SPANO, ALEXANDER H.", 0, 663, 0,
  "NJ", 2014, "H", 2, "G", 2016, "I", "I", "OLABISI, BAYODE", 0, 471, 0,
  "NJ", 2014, "H", 4, "G", 2016, "Other", "D-R Party", "NEUMAN, SCOTT", 0, 1608, 0,
  "NJ", 2014, "H", 6, "G", 2016, "Lbt", "Lbt", "GOIKHMAN, DORIT", 0, 1376, 0,
  "NJ", 2014, "H", 8, "G", 2016, "Other", "911 Truth Needed", "THORNE, ROBERT", 0, 653, 0,
  "NJ", 2014, "H", 9, "G", 2016, "Other", "Seeking Inclusion", "MONTILLA, NESTOR", 0, 1715, 0,
  "NJ", 2014, "H", 12, "G", 2016, "Other", "Legalize Marijuana", "DEZARN, DON", 0, 1330, 0,
  "TN", 2014, "S", 2, "G", 2020, "I", "I", "SCHECHTER, ERIC", 0, 1673, 0,
  "TX", 2014, "H", 36, "G", 2016, "Lbt", "Lbt", "VEACH, RODNEY", 0, 1951, 0,
  "VT", 2014, "H", 1, "G", 2016, "I", "I", "MEYER, RANDALL", 0, 1685, 0,
  "AZ", 2014, "H", 2, "G", 2016, "Other", "W-I", "RAMIREZ, SAMPSON U.", 0, 56, 0,
  "AZ", 2014, "H", 7, "G", 2016, "Other", "W-I", "DUNN, GARY", 0, 129, 0,
  "AZ", 2014, "H", 7, "G", 2016, "Other", "W-I", "ORTEGA, GUSTAVO", 0, 17, 0,
  "CO", 2014, "S", 2, "G", 2020, "Other", "W-I", "WILLOUGHBY, ", 0, 21, 0,
  "CO", 2014, "S", 2, "G", 2020, "Other", "W-I", "CUNNINGHAM, KATHLEEN ROSEWATER", 0, 17, 0,
  "FL", 2014, "H", 8, "G", 2016, "Other", "W-I", "DUNCAN, CHRISTOPHER L.", 0, 61, 0,
  "FL", 2014, "H", 10, "G", 2016, "Other", "W-I", "FALSTAD, DAVID B.", 0, 20, 0,
  "FL", 2014, "H", 16, "G", 2016, "Other", "W-I", "NEWMAN, JOE", 0, 220, 0,
  "GA", 2014, "S", 2, "G", 2020, "Other", "W-I", "MUSCU, ANANTHA REDDY", 0, 21, 0,
  "GA", 2014, "S", 2, "G", 2020, "Other", "W-I", "SCHRODER, MARY H.", 0, 14, 0,
  "IL", 2014, "S", 2, "G", 2020, "Other", "W-I", "SHIOURA, HILAIRE F.", 0, 12, 0,
  "MD", 2014, "H", 5, "G", 2016, "Other", "W-I", "FRITZ, DENNIS L.", 0, 227, 0,
  "MI", 2014, "H", 14, "G", 2016, "Other", "W-I", "PRUDEN, CALVIN", 0, 30, 0,
  "MO", 2014, "H", 7, "G", 2016, "Other", "W-I", "BRUCE, NIKOLAS", 0, 32, 0,
  "NM", 2014, "H", 3, "G", 2016, "Other", "W-I", "HOOK, THOMAS F.", 0, 52, 0,
  "NC", 2014, "H", 9, "G", 2016, "Other", "W-I", "ECKLES, SHAWN", 0, 2369, 0,
  "OH", 2014, "H", 9, "G", 2016, "Other", "W-I", "SKALSKY, GEORGE A.", 0, 29, 0,
  "OH", 2014, "H", 13, "G", 2016, "Other", "W-I", "PASTORIUS, DAVID ALLEN", 0, 86, 0,
  "TX", 2014, "S", 2, "G", 2020, "Other", "W-I", "TAHIRO, MOHAMMED", 0, 988, 0,
  "TX", 2014, "H", 19, "G", 2016, "Other", "W-I", "VANCE, DONALD L.", 0, 54, 0,
  "UT", 2014, "H", 2, "G", 2016, "Other", "W-I", "ROGERS, WARREN", 0, 117, 0,
  "UT", 2014, "H", 3, "G", 2016, "Other", "W-I", "ELSE, DAVID A.", 0, 280, 0,
  "WV", 2014, "S", 2, "G", 2020, "Other", "W-I", "WEINSTEIN, ALEX WOLF", 0, 10, 0,
  "WV", 2014, "H", 1, "G", 2016, "Other", "W-I", "WOOD, CHUCK", 0, 137, 0,
  "WI", 2014, "H", 3, "G", 2016, "Other", "W-I", "VAN DOREN, KEN", 0, 128, 0
)

cand <- cand |>
  rows_insert(adds_2014, by = c("year", "state", "office", "dist", "type", "name_snyder"), conflict = "error")

# 17. MS-1 special runoff, 2015 ----
# Kelly (R) beat Zinn (D) in the June 2 runoff; Snyder swaps their parties and votes. Figures are unofficial election-night
cand <- cand |>
  rows_update(tribble(
    ~year, ~state, ~office, ~dist, ~type, ~name_snyder, ~party, ~party_formal, ~vote_g, ~w_g, ~runoff,
    2015, "MS", "H", 1, "S", "KELLY, JOHN TRENT", "R", "R", 68852, 1, 1,
    2015, "MS", "H", 1, "S", "ZINN, WALTER", "D", "D", 29402, 0, 1
  ), by = c("year", "state", "office", "dist", "type", "name_snyder"), unmatched = "error")

# 18. Clerk audit, 2016 ----

## Votes ----
# 03 sets Abraham to 208,345; the Clerk and LA SOS have 208,545
votes_2016 <- tribble(
  ~year, ~state, ~office, ~dist, ~type, ~name_snyder, ~vote_g,
  2016, "LA", "H", 5, "G", "ABRAHAM, RALPH LEE", 208545,  # p. 27
  2016, "MN", "H", 1, "G", "HAGEDORN, JAMES (JIM)", 166524,  # p. 36
  2016, "MN", "H", 1, "G", "WALZ, TIMOTHY J. (TIM)", 169071,  # p. 36
  2016, "MN", "H", 2, "G", "OVERBY, PAULA M.", 28870,  # p. 36
  2016, "MN", "H", 3, "G", "PAULSEN, ERIK", 223075,  # p. 36
  2016, "MN", "H", 3, "G", "BONOFF, TERRI E.", 169238,  # p. 36
  2016, "MN", "H", 4, "G", "RYAN, GREG", 121033,  # p. 36
  2016, "MN", "H", 5, "G", "ELLISON, KEITH MAURICE", 249957,  # p. 36
  2016, "MN", "H", 6, "G", "EMMER, THOMAS (TOM)", 235385,  # p. 36
  2016, "MN", "H", 6, "G", "SNYDER, DAVID", 123010,  # p. 36
  2016, "MN", "H", 7, "G", "HUGHES, DAVE", 156944,  # p. 36
  2016, "MN", "H", 7, "G", "PETERSON, COLLIN CLARK", 173572,  # p. 36
  2016, "MN", "H", 8, "G", "MILLS, STEWART", 177088,  # p. 36
  2016, "MN", "H", 8, "G", "NOLAN, RICHARD MICHAEL (RICK)", 179097,  # p. 36
  2016, "NH", "H", 2, "G", "LAWRENCE, JAMES H. (JIM), III", 158825,  # p. 43
  2016, "NH", "H", 2, "G", "KUSTER, ANN MCLANE (ANNIE)", 174371,  # p. 43
  2016, "NH", "H", 2, "G", "BABIARZ, JOHN J.", 17076,  # p. 43
  2016, "NM", "H", 2, "G", "PEARCE, STEVAN E. (STEVE)", 143515,  # p. 49
  2016, "NY", "S", 3, "G", "LONG, WENDY", 2009353,  # p. 51
  2016, "NY", "S", 3, "G", "SCHUMER, CHARLES E. (CHUCK)", 5221949,  # p. 51
  2016, "NY", "S", 3, "G", "MERCED, ALEX", 48121,  # p. 51
  2016, "NY", "H", 12, "G", "ARDINI, ROBERT", 49398,  # p. 52
  2016, "NY", "H", 20, "G", "VITOLLO, FRANCIS JOSEPH (JOE)", 100738,  # p. 54
  2016, "NY", "H", 20, "G", "TONKO, PAUL DAVID", 213021,  # p. 54
  2016, "RI", "H", 1, "G", "CICILLINE, DAVID N.", 130534,  # p. 65
  2016, "RI", "H", 2, "G", "JOHNSON, JEFFREY C. (JEFF)", 16253,  # p. 65
  2016, "SC", "H", 3, "G", "DUNCAN, JEFFREY D. (JEFF)", 196325,  # p. 66
  2016, "SC", "H", 3, "G", "CLEVELAND, HOSEA", 72933,  # p. 66
  2016, "SD", "S", 3, "G", "THUNE, JOHN R.", 265516,  # p. 68
  2016, "SD", "S", 3, "G", "WILLIAMS, JAY H.", 104140,  # p. 68
  2016, "SD", "H", 1, "G", "NOEM, KRISTI LYNN", 237163,  # p. 68
  2016, "SD", "H", 1, "G", "HAWKS, PAULA", 132810,  # p. 68
  2016, "UT", "S", 3, "G", "LEE, MICHAEL (MIKE)", 760220,  # p. 72
  2016, "UT", "S", 3, "G", "SNOW, MISTY KATHERINE", 301858,  # p. 72
  2016, "UT", "H", 1, "G", "BISHOP, ROBERT WILLIAM (ROB)", 182925,  # p. 72
  2016, "UT", "H", 1, "G", "CLEMENS, PETER CONOVER", 73380,  # p. 72
  2016, "UT", "H", 2, "G", "STEWART, CHRIS", 170524,  # p. 72
  2016, "UT", "H", 2, "G", "ALBARRAN, CHARLENE MCARTHUR", 93778   # p. 72
)

cand <- cand |>
  rows_update(votes_2016, by = c("year", "state", "office", "dist", "type", "name_snyder"), unmatched = "error")

## Candidates missing from Snyder ----
adds_2016 <- tribble(
  ~state, ~year, ~office, ~dist, ~type, ~nextup, ~party, ~party_formal, ~name_snyder, ~inc, ~vote_g, ~w_g,
  "MD", 2016, "S", 3, "G", 2022, "Other", "W-I", "YOUNG, LIH Y.", 0, 77, 0,
  "FL", 2016, "S", 3, "G", 2022, "Other", "W-I", "KNEPPER, HOWARD", 0, 50, 0,
  "CT", 2016, "S", 3, "G", 2022, "Other", "W-I", "TRACESKI, JOHN M.", 0, 12, 0,
  "CT", 2016, "H", 5, "G", 2018, "Other", "W-I", "PISTONE, JOHN J.", 0, 28, 0,
  "IL", 2016, "H", 9, "G", 2018, "Other", "W-I", "ATANUS, SUSANNE", 0, 13, 0,
  "AZ", 2016, "S", 3, "G", 2022, "Other", "W-I", "DUDIKOFF, SYDNEY", 0, 494, 0,
  "MD", 2016, "S", 3, "G", 2022, "Other", "W-I", "SMITH, CHARLES ULYSSES", 0, 26, 0,
  "MO", 2016, "S", 3, "G", 2022, "Other", "W-I", "BUFE, GINA", 0, 21, 0,
  "NM", 2016, "H", 2, "G", 2018, "Other", "W-I", "MCGRANN, JACK A.", 0, 70, 0,
  "AZ", 2016, "S", 3, "G", 2022, "Other", "W-I", "CLARK, LEONARD", 0, 83, 0,
  "FL", 2016, "H", 19, "G", 2018, "Other", "W-I", "ROSSANO, TIMOTHY", 0, 20, 0,
  "MD", 2016, "S", 3, "G", 2022, "Other", "W-I", "TINUS, ED", 0, 138, 0,
  "MO", 2016, "H", 3, "G", 2018, "Other", "W-I", "DAVIS, HAROLD", 0, 10, 0,
  "OH", 2016, "H", 1, "G", 2018, "Other", "W-I", "KIANI, KIUMARS", 0, 16, 0,
  "FL", 2016, "H", 4, "G", 2018, "Other", "W-I", "MURPHY, DANIEL", 0, 11, 0,
  "MI", 2016, "H", 2, "G", 2018, "Other", "W-I", "ARNOLD, JOSHUA", 0, 18, 0,
  "GA", 2016, "S", 3, "G", 2022, "Other", "W-I", "GATES, MICHELLE", 0, 813, 0,
  "KS", 2016, "S", 3, "G", 2022, "Other", "W-I", "SMITH, D. J.", 0, 46, 0,
  "AZ", 2016, "S", 3, "G", 2022, "Other", "W-I", "QUINN, PAT", 0, 694, 0,
  "AZ", 2016, "S", 3, "G", 2022, "Other", "W-I", "LOPEZ, SELENA", 0, 223, 0,
  "AZ", 2016, "S", 3, "G", 2022, "Other", "W-I", "CAMBONI, ANTHONY", 0, 45, 0,
  "AZ", 2016, "S", 3, "G", 2022, "Other", "W-I", "BILYEU, SHEILA", 0, 34, 0,
  "CT", 2016, "S", 3, "G", 2022, "Other", "W-I", "RULE, ANDREW", 0, 26, 0,
  "CT", 2016, "H", 3, "G", 2018, "Other", "W-I", "SCHAEFER, CHRISTOPHER", 0, 18, 0,
  "FL", 2016, "H", 7, "G", 2018, "Other", "W-I", "PLASKON, MIKE", 0, 33, 0,
  "GA", 2016, "H", 1, "G", 2018, "Other", "W-I", "RUSSO, NATHAN", 0, 869, 0,
  "GA", 2016, "H", 10, "G", 2018, "Other", "W-I", "WARE, LEONARD", 0, 886, 0,
  "GA", 2016, "H", 10, "G", 2018, "Other", "W-I", "BOGGS, PATRICK A.", 0, 210, 0,
  "IL", 2016, "S", 3, "G", 2022, "Other", "W-I", "SANDOVAL, SUSANA", 0, 42, 0,
  "IN", 2016, "H", 1, "G", 2018, "Other", "W-I", "MEYER, JOHN", 0, 17, 0,
  "MD", 2016, "S", 3, "G", 2022, "Other", "W-I", "ROBINSON, BOB", 0, 15, 0,
  "MD", 2016, "H", 3, "G", 2018, "Other", "W-I", "DALRYMPLE, ANN", 0, 63, 0,
  "MD", 2016, "H", 4, "G", 2018, "Other", "W-I", "PETRUS, ADRIAN", 0, 18, 0,
  "MD", 2016, "H", 7, "G", 2018, "Other", "W-I", "NEWTON, WILLIAM T.", 0, 202, 0,
  "MD", 2016, "H", 7, "G", 2018, "Other", "W-I", "PEARSON, MICHAEL T.", 0, 17, 0,
  "MI", 2016, "H", 12, "G", 2018, "Other", "W-I", "VIRK, EJAZ", 0, 11, 0,
  "MO", 2016, "S", 3, "G", 2022, "Other", "W-I", "LEE, PATRICK", 0, 53, 0,
  "MO", 2016, "S", 3, "G", 2022, "Other", "W-I", "MALONE, NATHANIEL (NATE)", 0, 12, 0,
  "NH", 2016, "H", 1, "G", 2018, "Other", "W-I", "POTUCEK, JOHN", 0, 163, 0,
  "OH", 2016, "H", 13, "G", 2018, "Other", "W-I", "HILL, CALVIN, SR.", 0, 17, 0
)

cand <- cand |>
  rows_insert(adds_2016, by = c("year", "state", "office", "dist", "type", "name_snyder"), conflict = "error") |>
  # LA-1 has both 03's and Snyder's spelling of Kearney
  tidylog::filter(!(year == 2016 & state == "LA" & name_snyder == "KEARNEY, HOWARD L."))

# 19. Clerk audit, 2018 ----

## Votes ----
# CT candidates lacked their Working Families and Independent lines; ME-2 is the final ranked-choice round
votes_2018 <- tribble(
  ~year, ~state, ~office, ~dist, ~type, ~name_snyder, ~vote_g,
  2018, "CT", "S", 1, "G", "MURPHY, CHRISTOPHER SCOTT (CHRIS)", 825579,  # p. 9
  2018, "CT", "H", 1, "G", "LARSON, JOHN B.", 175087,  # p. 10
  2018, "CT", "H", 2, "G", "COURTNEY, JOSEPH D. (JOE)", 179731,  # p. 10
  2018, "CT", "H", 3, "G", "DELAURO, ROSA L.", 174572,  # p. 10
  2018, "CT", "H", 4, "G", "ARORA, HARRY", 106921,  # p. 10
  2018, "CT", "H", 5, "G", "HAYES, JAHANA", 151225,  # p. 10
  2018, "CT", "H", 5, "G", "SANTOS, MANNY", 119426,  # p. 10
  2018, "LA", "H", 1, "G", "SCALISE, STEPHEN J. (STEVE)", 192555,  # p. 18
  2018, "LA", "H", 2, "G", "RICHMOND, CEDRIC L.", 190182,  # p. 19
  2018, "LA", "H", 3, "G", "HIGGINS, CLAY", 136876,  # p. 19
  2018, "LA", "H", 4, "G", "JOHNSON, JAMES MICHAEL (MIKE)", 139326,  # p. 19
  2018, "LA", "H", 5, "G", "ABRAHAM, RALPH LEE", 149018,  # p. 19
  2018, "LA", "H", 6, "G", "GRAVES, GARRET", 186553,  # p. 19
  2018, "MA", "H", 7, "G", "PRESSLEY, AYANNA S.", 216557,  # p. 22
  2018, "PA", "S", 1, "G", "CASEY, ROBERT P. (BOB), JR.", 2792437,  # p. 44
  2018, "VT", "S", 1, "G", "PEACOCK, BRAD J.", 3665,  # p. 52
  2018, "ME", "H", 2, "G", "GOLDEN, JARED FORREST", 142440,  # final RCV round, p. 19
  2018, "ME", "H", 2, "G", "POLIQUIN, BRUCE L.", 138931   # final RCV round, p. 19
)

cand <- cand |>
  rows_update(votes_2018, by = c("year", "state", "office", "dist", "type", "name_snyder"), unmatched = "error")

## Candidates missing from Snyder ----
# ME-2: Bond and Hoar were eliminated after the first round (Maine SOS RCV report, Nov 21, 2018);
# AZ: the Clerk prints only the Green line, Angela Green's; NY-14: Crowley stayed on two minor lines
adds_2018 <- tribble(
  ~state, ~year, ~office, ~dist, ~type, ~nextup, ~party, ~party_formal, ~name_snyder, ~inc, ~vote_g, ~w_g,
  "CT", 2018, "H", 5, "G", 2020, "Other", "W-I", "PISTONE, JOHN J.", 0, 13, 0,
  "TX", 2018, "H", 5, "G", 2020, "Other", "W-I", "GRAY, PHIL", 0, 224, 0,
  "LA", 2018, "H", 1, "G", 2020, "D", "D", "DUGAS, LEE ANN", 0, 18560, 0,
  "LA", 2018, "H", 6, "G", 2020, "I", "I", "GRAHAM, DEVIN LANCE", 0, 5256, 0,
  "LA", 2018, "H", 1, "G", 2020, "Lbt", "Lbt", "KEARNEY, HOWARD", 0, 2806, 0,
  "UT", 2018, "H", 4, "G", 2020, "Other", "W-I", "PETERSON, JONATHAN L.", 0, 37, 0,
  "LA", 2018, "H", 2, "G", 2020, "I", "I", "BATISTE, BELDEN (NOONIE MAN)", 0, 17260, 0,
  "LA", 2018, "H", 3, "G", 2020, "D", "D", "ANDERSON, ROB", 0, 13477, 0,
  "LA", 2018, "H", 4, "G", 2020, "D", "D", "TRUNDLE, RYAN", 0, 72934, 0,
  "LA", 2018, "H", 5, "G", 2020, "I", "I", "BURKETTE, BILLY", 0, 4799, 0,
  "AZ", 2018, "S", 1, "G", 2024, "Other", "W-I", "BILYEU, SHEILA", 0, 42, 0,
  "FL", 2018, "H", 20, "G", 2020, "Other", "W-I", "BONNER, JAY", 0, 165, 0,
  "GA", 2018, "H", 8, "G", 2020, "Other", "W-I", "COOPER, JAMES L., III", 0, 564, 0,
  "GA", 2018, "H", 13, "G", 2020, "Other", "W-I", "COWEN, MARTIN", 0, 93, 0,
  "MI", 2018, "H", 13, "G", 2020, "Other", "W-I", "DUDENHOEFER, DAVID ANTHONY", 0, 75, 0,
  "OH", 2018, "H", 9, "G", 2020, "Other", "W-I", "LEVINDOFSKE, MCKENZIE", 0, 48, 0,
  "WI", 2018, "S", 1, "G", 2024, "Other", "W-I", "SCHIESS, JOHN", 0, 14, 0,
  "TX", 2018, "H", 16, "G", 2020, "Other", "W-I", "WILLIAMS, SAM", 0, 43, 0,
  "IL", 2018, "H", 3, "G", 2020, "Other", "W-I", "HANSON, JUSTIN", 0, 1353, 0,
  "KY", 2018, "H", 5, "G", 2020, "Other", "W-I", "WILSON, BILL RAY", 0, 34, 0,
  "MI", 2018, "H", 13, "G", 2020, "Other", "W-I", "JONES, BRENDA", 0, 633, 0,
  "LA", 2018, "H", 1, "G", 2020, "I", "I", "JONES, FREDERICK (FERD)", 0, 2443, 0,
  "LA", 2018, "H", 2, "G", 2020, "I", "No Party", "RODRIGUEZ, SHAWNDRA", 0, 8075, 0,
  "LA", 2018, "H", 3, "G", 2020, "R", "R", "GUILLORY, JOSH", 0, 31387, 0,
  "CO", 2018, "H", 1, "G", 2020, "Other", "W-I", "LOPEZ, MIGUEL", 0, 22, 0,
  "FL", 2018, "H", 15, "G", 2020, "Other", "W-I", "JOHNSON, DAVE", 0, 15, 0,
  "LA", 2018, "H", 1, "G", 2020, "D", "D", "SAVOIE, TAMMY M.", 0, 44273, 0,
  "LA", 2018, "H", 1, "G", 2020, "D", "D", "FRANCIS, JIM", 0, 8688, 0,
  "LA", 2018, "H", 2, "G", 2020, "I", "No Party", "SCHMIDT, JESSE", 0, 20465, 0,
  "LA", 2018, "H", 3, "G", 2020, "D", "D", "METHVIN, MILDRED (MIMI)", 0, 43729, 0,
  "LA", 2018, "H", 3, "G", 2020, "D", "D", "RADER, LARRY", 0, 9692, 0,
  "LA", 2018, "H", 3, "G", 2020, "D", "D", "THOMAS, VERONE", 0, 7815, 0,
  "LA", 2018, "H", 3, "G", 2020, "Lbt", "Lbt", "ANDRUS, AARON J.", 0, 2967, 0,
  "LA", 2018, "H", 4, "G", 2020, "I", "No Party", "HALVERSON, MARK DAVID", 0, 4612, 0,
  "LA", 2018, "H", 5, "G", 2020, "D", "D", "FLEENOR, JESSEE CARLTON", 0, 67118, 0,
  "LA", 2018, "H", 5, "G", 2020, "Lbt", "Lbt", "RANDOL, KYLE", 0, 3011, 0,
  "LA", 2018, "H", 6, "G", 2020, "D", "D", "DEWITT, JUSTIN", 0, 55089, 0,
  "LA", 2018, "H", 6, "G", 2020, "D", "D", "SAIZAN, ANDIE", 0, 21627, 0,
  "AZ", 2018, "S", 1, "G", 2024, "Other", "W-I", "B, JONATHAN RINGHAM", 0, 46, 0,
  "AZ", 2018, "S", 1, "G", 2024, "Other", "W-I", "DECARLO, MICHAEL", 0, 45, 0,
  "AZ", 2018, "S", 1, "G", 2024, "Other", "W-I", "KAY, ROBERT", 0, 37, 0,
  "AZ", 2018, "S", 1, "G", 2024, "Other", "W-I", "PEARCE, JEFF", 0, 29, 0,
  "CO", 2018, "H", 2, "G", 2020, "Other", "W-I", "ALUMBAUGH, KEVIN", 0, 151, 0,
  "CO", 2018, "H", 4, "G", 2020, "Other", "W-I", "VIGIL, JOHN B.", 0, 26, 0,
  "CO", 2018, "H", 4, "G", 2020, "Other", "W-I", "WEIL, RICHARD LYONS", 0, 13, 0,
  "CO", 2018, "H", 5, "G", 2020, "Other", "W-I", "FURSTENBERG, LORI", 0, 55, 0,
  "CO", 2018, "H", 5, "G", 2020, "Other", "W-I", "CROOM, JOHN", 0, 16, 0,
  "CO", 2018, "H", 7, "G", 2020, "Other", "W-I", "ZORN, STEVE", 0, 45, 0,
  "CO", 2018, "H", 7, "G", 2020, "Other", "W-I", "HAUGHEY, MICHAEL", 0, 16, 0,
  "CT", 2018, "S", 1, "G", 2024, "Other", "W-I", "LINCK, FRED", 0, 70, 0,
  "CT", 2018, "S", 1, "G", 2024, "Other", "W-I", "TALMADGE, KRISTI L.", 0, 18, 0,
  "DE", 2018, "H", 1, "G", 2020, "Other", "W-I", "JOHNSTON, PAUL A.", 0, 637, 0,
  "DE", 2018, "H", 1, "G", 2020, "Other", "W-I", "WEBB, ANDREW C.", 0, 417, 0,
  "DE", 2018, "H", 1, "G", 2020, "Other", "W-I", "DAVIS, MARVIN", 0, 23, 0,
  "FL", 2018, "H", 11, "G", 2020, "Other", "W-I", "SALDANA, LUIS", 0, 58, 0,
  "FL", 2018, "H", 19, "G", 2020, "Other", "W-I", "POLLARD, PETE", 0, 36, 0,
  "GA", 2018, "H", 6, "G", 2020, "Other", "W-I", "STUBBS, JEREMY (CARLTON HESTON)", 0, 18, 0,
  "ID", 2018, "H", 1, "G", 2020, "Other", "W-I", "RATH, MICHAEL J.", 0, 91, 0,
  "IL", 2018, "H", 3, "G", 2020, "Other", "W-I", "YERKES, KENNETH", 0, 1039, 0,
  "IN", 2018, "H", 2, "G", 2020, "Other", "W-I", "WOLF, RICHARD", 0, 27, 0,
  "KY", 2018, "H", 4, "G", 2020, "Other", "W-I", "GOODWIN, DAVID", 0, 12, 0,
  "MI", 2018, "H", 13, "G", 2020, "Other", "W-I", "POMMERVILLE, JONATHAN LEE", 0, 61, 0,
  "OH", 2018, "H", 3, "G", 2020, "Other", "W-I", "MILAM, MILLIE", 0, 62, 0,
  "OH", 2018, "H", 11, "G", 2020, "Other", "W-I", "BELL, JAMES JEROME", 0, 36, 0,
  "UT", 2018, "S", 1, "G", 2024, "Other", "W-I", "REEVE, CALEB DAN", 0, 29, 0,
  "UT", 2018, "S", 1, "G", 2024, "Other", "W-I", "JENSEN, TYRONE", 0, 17, 0,
  "WI", 2018, "S", 1, "G", 2024, "Other", "W-I", "WALTERS, MARY JO", 0, 28, 0,
  "WI", 2018, "H", 2, "G", 2020, "Other", "W-I", "REED, JOEY WAYNE", 0, 29, 0,
  "ME", 2018, "H", 2, "G", 2020, "I", "I", "BOND, TIFFANY", 0, 16552, 0,
  "ME", 2018, "H", 2, "G", 2020, "I", "I", "HOAR, WILL", 0, 6875, 0,
  "AZ", 2018, "S", 1, "G", 2024, "Grn", "Grn", "GREEN, ANGELA", 0, 57442, 0,
  "NY", 2018, "H", 14, "G", 2020, "Other", "Wk Fam, Women's Equality", "CROWLEY, JOSEPH (JOE)", 1, 9348, 0
)

cand <- cand |>
  rows_insert(adds_2018, by = c("year", "state", "office", "dist", "type", "name_snyder"), conflict = "error")

## Mississippi Senate special runoff ----
# Hyde-Smith beat Espy in the Nov 27 runoff (MS SOS certified results)
cand <- cand |>
  tidylog::filter(!(year == 2018 & state == "MS" & office == "S" & type == "S")) |>
  bind_rows(tribble(
    ~state, ~year, ~office, ~dist, ~type, ~nextup, ~party, ~party_formal, ~name_snyder, ~inc, ~vote_g, ~w_g, ~runoff,
    "MS", 2018, "S", 2, "S", 2020, "R", "R", "HYDE-SMITH, CINDY", 3, 486769, 1, 1,
    "MS", 2018, "S", 2, "S", 2020, "D", "D", "ESPY, ALPHONSO MICHAEL (MIKE)", 0, 420819, 0, 1
  ))

# 20. Clerk audit, 2020 ----

## Votes ----
# Snyder puts Smith's Serve America Movement line on Farley
cand <- cand |>
  rows_update(tribble(
    ~year, ~state, ~office, ~dist, ~type, ~name_snyder, ~vote_g,
    2020, "NY", "H", 18, "G", "FARLEY, CHELE CHIAVACCI", 145098,  # p. 51
    2020, "NY", "H", 18, "G", "SMITH, SCOTT A.", 3162             # p. 51
  ), by = c("year", "state", "office", "dist", "type", "name_snyder"), unmatched = "error")

## Candidates missing from Snyder ----
# the Clerk prints VA-2's independent without a name; David Bruce Foster (VA ELECT)
cand <- cand |>
  rows_insert(tribble(
    ~state, ~year, ~office, ~dist, ~type, ~nextup, ~party, ~party_formal, ~name_snyder, ~inc, ~vote_g, ~w_g,
    "TN", 2020, "H", 5, "G", 2022, "Other", "W-I", "BROOKS, NATISHA", 0, 13, 0,
    "VA", 2020, "H", 2, "G", 2022, "I", "I", "FOSTER, DAVID BRUCE", 0, 9170, 0
  ), by = c("year", "state", "office", "dist", "type", "name_snyder"), conflict = "error")

## Duplicates and runoff flag ----
# fusion_candidates.csv and Snyder spell three minor candidates differently; LA-5 rows are the Dec 5 runoff
cand <- cand |>
  tidylog::filter(!(year == 2020 & name_snyder %in% c("MCCORMICK, THOMAS", "POPKIN, GARY", "ANABILAH-AZUMAH, JOEL"))) |>
  tidylog::mutate(runoff = replace(runoff, year == 2020 & state == "LA" & office == "H" & dist == 5, 1))

# 21. Presidential names ----
# MEDSL rows that are party labels or lumped minor candidates are dropped; names follow each candidate's
# House, Senate or Governor record (e.g. Bush's Texas governor rows)
cand <- cand |>
  tidylog::filter(!(office == "P" & name_snyder %in% c("OTHER", "SCATTERING", "NOT DESIGNATED", "NONPARTISAN",
                                                       "WORKERS WORLD", "PROHIBITION", "UNAFFILIATED"))) |>
  tidylog::mutate(name_snyder = if_else(office == "P", recode_values(
    name_snyder,
    "THE BETTER LIFE," ~ "NADER, RALPH",
    "CLINTON, BILL" ~ "CLINTON, WILLIAM J. (BILL)",
    "BUSH, GEORGE W." ~ "BUSH, GEORGE W., JR.",
    "DOLE, ROBERT" ~ "DOLE, ROBERT JOSEPH (BOB)",
    "GORE, AL" ~ "GORE, ALBERT, JR.",
    "KERRY, JOHN" ~ "KERRY, JOHN F.",
    "BARR, BOB" ~ "BARR, ROBERT LAWRENCE (BOB)",
    "JOHNSON, GARY" ~ "JOHNSON, GARY E.",
    "STEIN, JILL" ~ "STEIN, JILL E.",
    "KENNEDY, ROBERT F. JR" ~ "KENNEDY, ROBERT F., JR.",
    "EMIDIO (MIMI\"\", SOLTYSIK" ~ "SOLTYSIK, EMIDIO (MIMI)",
    "DENNIS (DENNY\"\", LANE" ~ "LANE, DENNIS (DENNY)",
    "JADE SIMMONS" ~ "SIMMONS, JADE",
    "JACOB?FAMBRO, PRINCESS KHADIJAH MARYAM" ~ "JACOB-FAMBRO, PRINCESS KHADIJAH MARYAM",
    default = name_snyder
  ), name_snyder))

# 22. Ranked-choice final rounds, 2022 and 2024 ----
# last tabulation round each candidate reached (AK Division of Elections and Maine SOS RCV reports)
cand <- cand |>
  rows_update(tribble(
    ~year, ~state, ~office, ~dist, ~type, ~name_snyder, ~vote_g,
    2022, "AK", "S", 3, "G", "MURKOWSKI, LISA A.", 136330,
    2022, "AK", "S", 3, "G", "TSHIBAKA, KELLY C.", 117534,
    2022, "AK", "S", 3, "G", "CHESBRO, PATRICIA R.", 29134,
    2022, "AK", "S", 3, "G", "KELLEY, BUZZ A.", 8575,
    2022, "AK", "H", 1, "G", "PELTOLA, MARY SATTLER", 137263,
    2022, "AK", "H", 1, "G", "PALIN, SARAH H.", 112471,
    2022, "AK", "H", 1, "G", "BEGICH, NICK", 64499,
    2022, "AK", "H", 1, "G", "BYE, CHRIS", 4999,
    2022, "ME", "H", 2, "G", "GOLDEN, JARED FORREST", 165136,
    2022, "ME", "H", 2, "G", "POLIQUIN, BRUCE L.", 146142,
    2024, "AK", "H", 1, "G", "BEGICH, NICK", 164861,
    2024, "AK", "H", 1, "G", "PELTOLA, MARY SATTLER", 156985,
    2024, "AK", "H", 1, "G", "HOWE, JOHN WAYNE", 13871,
    2024, "AK", "H", 1, "G", "HAFNER, ERIC", 3558
  ), by = c("year", "state", "office", "dist", "type", "name_snyder"), unmatched = "error")

# 23. Clerk audit, 2022 ----
# Snyder has two spellings of one LA Senate candidate
cand <- cand |>
  tidylog::filter(!(year == 2022 & state == "LA" & name_snyder == "OLSON, W. THOMAS LA FONTAINE"))

# 24. Names ----

## Same person, different strings ----
# each merged to its most complete string; Romney and Keyes also ran in other states, Keyes and Romney for President
names_same <- tribble(
  ~state, ~name_snyder, ~name_new,
  "ME", "CARTER, JONATHAN", "CARTER, JONATHAN K.",
  "MI", "CLARKE, LLOYD", "CLARKE, LLOYD C.",
  "NJ", "FLYNN, LEN", "FLYNN, LEN T.",
  "NY", "HIRSCHFELD, ABRAHAM", "HIRSCHFELD, ABRAHAM J.",
  "WI", "BARCA, PETER", "BARCA, PETER W.",
  "ME", "MICHAEL, JOHN", "MICHAEL, JOHN M.",
  "NY", "LEIGHTON, THOMAS", "LEIGHTON, THOMAS K.",
  "MI", "ELLISON, BRIAN", "ELLISON, BRIAN D.",
  "PA", "DINICOLA, RONALD", "DINICOLA, RONALD A.",
  "LA", "JENKINS, WOODY", "JENKINS, LOUIS (WOODY)",
  "TN", "WATSON, ROBERT", "WATSON, ROBERT O.",
  "VT", "WILLIAMS, JOEL", "WILLIAMS, JOEL W.",
  "IA", "HENNAGER, JIM", "HENNAGER, JAMES C. (JIM)",
  "KY", "CONWAY, JACK", "CONWAY, JOHN WILLIAM (JACK)",
  "TX", "BELL, CHRIS", "BELL, R. CHRISTOPHER (CHRIS)",
  "NY", "DAVIS, JACK", "DAVIS, JOHN R. (JACK)",
  "TX", "PETERSON, CHIP", "PETERSON, RICHARD (CHIP)",
  "AK", "MURKOWSKI, LISA ANN", "MURKOWSKI, LISA A.",
  "VT", "HILL, CRAIG", "HILL, CRAIG BARCLAY",
  "CT", "LAMONT, NED", "LAMONT, EDWARD M. (NED)",
  "FL", "MCGOVERN, JAY", "MCGOVERN, JAMES JOSEPH (JAY)",
  "UT", "BUCHMAN, JOSEPH", "BUCHMAN, JOSEPH GEDDES",
  "AK", "HAASE, FREDRICK (DAVID)", "HAASE, FREDERICK D. (DAVID)",
  "AK", "MILLER, JOE", "MILLER, JOSEPH W. (JOE)",
  "IA", "ELWORTH, MARK, JR.", "ELWORTH, MARK G., JR.",
  "NE", "ELWORTH, MARK, JR.", "ELWORTH, MARK G., JR.",
  "NJ", "BERMAN, MIKE", "BERMAN, MICHAEL (MIKE)",
  "LA", "KEARNEY, HOWARD L.", "KEARNEY, HOWARD",
  "LA", "JOHNSON, MIKE", "JOHNSON, JAMES MICHAEL (MIKE)",
  "CT", "MCCORMICK, THOMAS", "MCCORMICK, THOMAS E.",
  "NY", "ANABILAH-AZUMAH, JOEL", "ANABILAH-AZUMAH, JOEL B.",
  "WV", "MORRISEY, PATRICK", "MORRISEY, PATRICK J.",
  "NC", "BISHOP, DAN", "BISHOP, JAMES DANIEL (DAN)",
  "UT", "COX, SPENCER J.", "COX, SPENCER JAMES",
  "TX", "ROYAL, CHRIS", "ROYAL, CHRIS B.",
  "LA", "BILLIOT, BERYL", "BILLIOT, BERYL A.",
  "CT", "DARWISH, AHMAD A J", "DARWISH, AHMAD A.J.",
  "CT", "DARWISH, AHMAD A. J.", "DARWISH, AHMAD A.J.",
  "FL", "KUNST, ROBERT P.", "KUNST, ROBERT P. (BOB)",
  "MN", "CAVLAN, MICHAEL JAMES (FIGHTING IRISH)", "CAVLAN, MICHAEL JAMES",
  "VT", "DIAMONDSTONE, PETER", "DIAMONDSTONE, PETER ISAAC (PETE)",
  "LA", "CASH, ARTIS R., SR.", "CASH, ARTIS R. (DOC), SR.",
  "MD", "DAVIS, RICHARD JAMES", "DAVIS, RICHARD JAMES (RIK)",
  "LA", "MENDOZA, M.V.", "MENDOZA, MANILLO V. (VINNY)",
  "VT", "MACGOVERN, JOHN", "MACGOVERN, JOHN F.",
  "FL", "CRAINE, NAOMI", "CRAINE, NAOMI L.")

cand <- cand |>
  left_join(names_same, by = c("state", "name_snyder"), relationship = "many-to-one") |>
  tidylog::mutate(
    name_snyder = coalesce(name_new, name_snyder),
    name_snyder = recode_values(
      name_snyder,
      "ROMNEY, MITT" ~ "ROMNEY, W. MITT",
      "KEYES, ALAN" ~ "KEYES, ALAN L.",
      default = name_snyder
    )
  ) |>
  select(-name_new)

## Malformed strings ----
# Willoughby (CO 2014) is printed without a given name
cand <- cand |>
  tidylog::filter(!(name_snyder %in% c("WILLOUGHBY, ", "W-I"))) |>
  tidylog::mutate(
    name_snyder = str_squish(str_replace_all(name_snyder, "’", "'")),
    name_snyder = recode_values(
      name_snyder,
      "CUNNINGHAM, RANDALL H. (RANDY) (DUKE)" ~ "CUNNINGHAM, RANDALL H. (DUKE)",
      "BURNS, ROBERT ()" ~ "BURNS, ROBERT",
      "DUPAUL MICHAEL O." ~ "DUPAUL, MICHAEL O.",
      "FISH, HAMILTON, V." ~ "FISH, HAMILTON, V",
      "CROCKER, RONALD E. (ED?)" ~ "CROCKER, RONALD E. (ED)",
      "KERN, BOBBY S. HIDALGO (BOB) (CITIZEN)" ~ "KERN, BOBBY S. HIDALGO (BOB)",
      "B, JONATHAN RINGHAM" ~ "RINGHAM, JONATHAN",
      "MEYER, MATTHEW (MATT) STEPHEN" ~ "MEYER, MATTHEW STEPHEN (MATT)",
      "RAMONE, MICHAEL (MIKE) J." ~ "RAMONE, MICHAEL J. (MIKE)",
      "LYMAN, PHILLIP (PHIL) KAY" ~ "LYMAN, PHILLIP KAY (PHIL)",
      "FERGUSON, ROBERT (BOB) W." ~ "FERGUSON, ROBERT W. (BOB)",
      "WILLIAMS, STEPHEN (STEVE) T." ~ "WILLIAMS, STEPHEN T. (STEVE)",
      "STAMPER, KATY (KATE) L." ~ "STAMPER, KATY L. (KATE)",
      default = name_snyder
    )
  )

# 25. Other internal fixes ----
# Grimes is not on the CO 1998 Senate ballot; LA-6's May 2008 special per LA SOS (Snyder has 56,741 for Jenkins)
cand <- cand |>
  tidylog::filter(!(year == 1998 & state == "CO" & office == "S" & name_snyder == "GRIMES, RICHARD O.")) |>
  rows_update(tribble(
    ~year, ~state, ~office, ~dist, ~type, ~name_snyder, ~vote_g,
    2008, "LA", "H", 6, "S", "CAZAYOUX, DONALD (DON), JR.", 49703,
    2008, "LA", "H", 6, "S", "JENKINS, LOUIS (WOODY)", 46746
  ), by = c("year", "state", "office", "dist", "type", "name_snyder"), unmatched = "error")

# 26. Party codes ----
# party follows the first party_formal label for Greens and independents; stray codes go to the six categories
indep <- c("I", "Independent", "No Party", "No Party Affiliation", "Nonaffiliated", "Unaffiliated")
cand <- cand |>
  mutate(first_formal = str_trim(str_split_i(party_formal, ",", 1))) |>
  tidylog::mutate(party = case_when(
    party == "G" ~ "Grn",
    party == "L" ~ "Lbt",
    party %in% c("W", "LaRouche", "SAM") ~ "Other",
    party == "Other" & first_formal %in% c("Grn", "Green") ~ "Grn",
    party == "Other" & first_formal %in% indep ~ "I",
    party == "Other" & first_formal == "Lbt" & state != "NY" ~ "Lbt",
    year == 2024 & state == "VT" & name_snyder == "COESTER, MARK" ~ "R",
    .default = party
  )) |>
  select(-first_formal) |>
  # Snyder's NY "L" is the Liberal Party, which 02 recodes as Libertarian; Darwish's write-in appears twice
  tidylog::mutate(party_formal = replace(party_formal, state == "NY" & party == "Other" & party_formal == "Lbt", "Liberal")) |>
  tidylog::filter(!(year == 2024 & state == "CT" & name_snyder == "DARWISH, AHMAD A.J." & party_formal == "Write-in"))

# 27. Disambiguation markers ----
# Snyder's (1)/(2) suffixes replaced by middle names from FEC filings and the Clerk, or dropped when none is on record
cand <- cand |>
  tidylog::mutate(name_snyder = recode_values(
    name_snyder,
    "WILLIAMS, WILLIAM R. (2)" ~ "WILLIAMS, WILLIAM R.",
    "DAVIS, DAVID R. (1)" ~ "DAVIS, DAVID RANDALL",
    "DAVIS, DAVID R. (2)" ~ "DAVIS, DAVID R.",
    "MILLER, BOB (1)" ~ "MILLER, ROBERT L. (BOB)",
    "NEWTON, JON (2)" ~ "NEWTON, JONATHAN (JON)",
    "HOGAN, JOHN (2)" ~ "HOGAN, JOHN P.",
    "ADAIR, ROBIN (2)" ~ "ADAIR, ROBIN",
    "WHITE, BILL (2)" ~ "WHITE, WILLIAM H. (BILL)",
    "GARZA, JULIO (1)" ~ "GARZA, JULIO",
    default = name_snyder
  ))

# 28. Missing governor races ----
# Snyder drops most governor races won by independents, plus NJ 2017 and 2023; figures from each state's
# official returns, linked above each race. AK 1990, AK 2014 and MN 1998 print surnames only; given names are the
# commonly reported ones. MN 1998 uses the SOS results book, which amends the canvass report
gov_missing <- tribble(
  ~state, ~year, ~office, ~dist, ~type, ~nextup, ~party, ~party_formal, ~name_snyder, ~inc, ~vote_g, ~w_g,
  # https://www.elections.alaska.gov/results/90GENR/90genr.pdf
  "AK", 1990, "G", NA, "G", 1994, "Other", "AK Indep", "HICKEL, WALTER J. (WALLY)", 0, 75721, 1,
  "AK", 1990, "G", NA, "G", 1994, "D", "D", "KNOWLES, TONY", 0, 60201, 0,
  "AK", 1990, "G", NA, "G", 1994, "R", "R", "STURGULEWSKI, ARLISS", 0, 50991, 0,
  "AK", 1990, "G", NA, "G", 1994, "Grn", "Grn", "SYKES, JAMES L. (JIM)", 0, 6563, 0,
  "AK", 1990, "G", NA, "G", 1994, "Other", "The Political Party", "O'CALLAGHAN, MICHAEL", 0, 942, 0,
  # https://ct.elstats.civera.com/api/download_contest/10648_table.csv?split_party=false
  "CT", 1990, "G", NA, "G", 1994, "Other", "CT Pty", "WEICKER, LOWELL P., JR.", 0, 460576, 1,
  "CT", 1990, "G", NA, "G", 1994, "R", "R", "ROWLAND, JOHN G.", 0, 427840, 0,
  "CT", 1990, "G", NA, "G", 1994, "D", "D", "MORRISON, BRUCE A.", 0, 236641, 0,
  "CT", 1990, "G", NA, "G", 1994, "Other", "Concerned Cit", "ZDONCZYK, JOSEPH A.", 0, 16044, 0,
  # https://lldc.mainelegislature.org/Open/Elections/Returns/1994/19941108/elec_19941108c.pdf
  "ME", 1994, "G", NA, "G", 1998, "I", "I", "KING, ANGUS S., JR.", 0, 180829, 1,
  "ME", 1994, "G", NA, "G", 1998, "D", "D", "BRENNAN, JOSEPH E.", 0, 172951, 0,
  "ME", 1994, "G", NA, "G", 1998, "R", "R", "COLLINS, SUSAN M.", 0, 117990, 0,
  "ME", 1994, "G", NA, "G", 1998, "Grn", "Grn", "CARTER, JONATHAN K.", 0, 32695, 0,
  "ME", 1994, "G", NA, "G", 1998, "Other", "W-I", "FINKS, MARK S.", 0, 6576, 0,
  # https://lldc.mainelegislature.org/Open/Elections/Returns/1998/19981103/elec_19981103c.pdf
  "ME", 1998, "G", NA, "G", 2002, "I", "I", "KING, ANGUS S., JR.", 1, 246772, 1,
  "ME", 1998, "G", NA, "G", 2002, "R", "R", "LONGLEY, JAMES B., JR.", 0, 79716, 0,
  "ME", 1998, "G", NA, "G", 2002, "D", "D", "CONNOLLY, THOMAS J.", 0, 50506, 0,
  "ME", 1998, "G", NA, "G", 2002, "Grn", "Grn", "LAMARCHE, PATRICIA H. (PAT)", 0, 28722, 0,
  "ME", 1998, "G", NA, "G", 2002, "Other", "Taxpayers", "CLARKE, WILLIAM P.", 0, 15293, 0,
  # https://www.lrl.mn.gov/archive/sessions/electionresults/1998-11-03-g-sec.pdf
  "MN", 1998, "G", NA, "G", 2002, "Other", "Reform", "VENTURA, JESSE", 0, 773403, 1,
  "MN", 1998, "G", NA, "G", 2002, "R", "R", "COLEMAN, NORMAN (NORM)", 0, 716880, 0,
  "MN", 1998, "G", NA, "G", 2002, "D", "DFL", "HUMPHREY, HUBERT H. (SKIP), III", 0, 587060, 0,
  "MN", 1998, "G", NA, "G", 2002, "Grn", "Grn", "PENTEL, KEN", 0, 7034, 0,
  "MN", 1998, "G", NA, "G", 2002, "Lbt", "Lbt", "GERMANN, FRANK", 0, 1932, 0,
  "MN", 1998, "G", NA, "G", 2002, "Other", "Grassroots", "WRIGHT, CHRIS", 0, 1727, 0,
  "MN", 1998, "G", NA, "G", 2002, "Other", "The People's Champion", "MCCLONEY, RAY", 0, 919, 0,
  "MN", 1998, "G", NA, "G", 2002, "Other", "SWP", "FISKE, THOMAS A.", 0, 787, 0,
  # https://elections.ri.gov/sites/g/files/xkgbur756/files/publications/Election_Publications/Countbooks/2010-Countbook.pdf
  "RI", 2010, "G", NA, "G", 2014, "I", "I", "CHAFEE, LINCOLN D. (LINC)", 0, 123571, 1,
  "RI", 2010, "G", NA, "G", 2014, "R", "R", "ROBITAILLE, JOHN F.", 0, 114911, 0,
  "RI", 2010, "G", NA, "G", 2014, "D", "D", "CAPRIO, FRANK T.", 0, 78896, 0,
  "RI", 2010, "G", NA, "G", 2014, "Other", "Moderate", "BLOCK, KENNETH J.", 0, 22146, 0,
  "RI", 2010, "G", NA, "G", 2014, "I", "I", "LUSI, JOSEPH M.", 0, 1091, 0,
  "RI", 2010, "G", NA, "G", 2014, "I", "I", "GIROUX, TODD", 0, 882, 0,
  "RI", 2010, "G", NA, "G", 2014, "I", "I", "ALGIERI, RONALD", 0, 793, 0,
  # https://www.elections.alaska.gov/results/14GENR/data/results.htm
  "AK", 2014, "G", NA, "G", 2018, "I", "I", "WALKER, WILLIAM M. (BILL)", 0, 134658, 1,
  "AK", 2014, "G", NA, "G", 2018, "R", "R", "PARNELL, SEAN R.", 1, 128435, 0,
  "AK", 2014, "G", NA, "G", 2018, "Lbt", "Lbt", "CLIFT, ROBERT EDWARD (ROB)", 0, 8985, 0,
  "AK", 2014, "G", NA, "G", 2018, "Other", "Const", "MYERS, J.R.", 0, 6987, 0,
  # https://nj.gov/state/elections/assets/pdf/election-results/2017/2017-official-general-election-results-governor.pdf
  "NJ", 2017, "G", NA, "G", 2021, "D", "D", "MURPHY, PHILIP", 0, 1203110, 1,
  "NJ", 2017, "G", NA, "G", 2021, "R", "R", "GUADAGNO, KIM", 0, 899583, 0,
  "NJ", 2017, "G", NA, "G", 2021, "Other", "Reduce Property Taxes", "GENOVESE, GINA", 0, 12294, 0,
  "NJ", 2017, "G", NA, "G", 2021, "Lbt", "Lbt", "ROHRMAN, PETER J.", 0, 10531, 0,
  "NJ", 2017, "G", NA, "G", 2021, "Grn", "Grn", "KAPER-DALE, SETH", 0, 10053, 0,
  "NJ", 2017, "G", NA, "G", 2021, "Other", "Const", "RICCARDI, MATTHEW", 0, 6864, 0,
  "NJ", 2017, "G", NA, "G", 2021, "Other", "We the People", "ROSS, VINCENT", 0, 4980, 0,
  # https://elect.ky.gov/results/2020-2029/Documents/Certification%20of%20Election%20Results%20for%202023%20General%20Election%20Final.pdf
  "KY", 2023, "G", NA, "G", 2027, "D", "D", "BESHEAR, ANDY", 1, 694482, 1,
  "KY", 2023, "G", NA, "G", 2027, "R", "R", "CAMERON, DANIEL", 0, 627457, 0,
  "KY", 2023, "G", NA, "G", 2027, "Other", "W-I", "FISHBACK, BRIAN", 0, 83, 0,
  # https://sos.ms.gov/elections/electionresults/2023General/State%20Certified%20Results/2023%20Statewide%20Recapitulation%20Sheet.pdf
  "MS", 2023, "G", NA, "G", 2027, "R", "R", "REEVES, JONATHAN (TATE)", 1, 418233, 1,
  "MS", 2023, "G", NA, "G", 2027, "D", "D", "PRESLEY, BRANDON", 0, 391614, 0,
  "MS", 2023, "G", NA, "G", 2027, "I", "I", "GRAY, GWENDOLYN", 0, 11153, 0,
  # https://voterportal.sos.la.gov/ElectionResults/ElectionResults/Data?blob=20231014/RacesCandidates_Multiparish.htm
  "LA", 2023, "G", NA, "G", 2027, "R", "R", "LANDRY, JEFFREY M. (JEFF)", 0, 547827, 1,
  "LA", 2023, "G", NA, "G", 2027, "D", "D", "WILSON, SHAWN D.", 0, 275525, 0,
  "LA", 2023, "G", NA, "G", 2027, "R", "R", "WAGUESPACK, STEPHEN (WAGS)", 0, 62287, 0,
  "LA", 2023, "G", NA, "G", 2027, "R", "R", "SCHRODER, JOHN", 0, 56654, 0,
  "LA", 2023, "G", NA, "G", 2027, "I", "I", "LUNDY, HUNTER W.", 0, 52165, 0,
  "LA", 2023, "G", NA, "G", 2027, "D", "D", "COLE, DANIEL M. (DANNY)", 0, 27662, 0,
  "LA", 2023, "G", NA, "G", 2027, "R", "R", "HEWITT, SHARON W.", 0, 18468, 0,
  "LA", 2023, "G", NA, "G", 2027, "I", "I", "BARNES, BENJAMIN", 0, 5190, 0,
  "LA", 2023, "G", NA, "G", 2027, "R", "R", "BARTHEL, PATRICK HENRY (DAT)", 0, 4426, 0,
  "LA", 2023, "G", NA, "G", 2027, "R", "R", "NELSON, RICHARD", 0, 3605, 0,
  "LA", 2023, "G", NA, "G", 2027, "I", "I", "ISTRE, JEFFERY", 0, 3400, 0,
  "LA", 2023, "G", NA, "G", 2027, "R", "R", "ELLIS, XAVIER", 0, 1734, 0,
  "LA", 2023, "G", NA, "G", 2027, "I", "No Party", "GAGNON, KEITRON", 0, 1260, 0,
  "LA", 2023, "G", NA, "G", 2027, "R", "R", "JOHN, XAN", 0, 1164, 0,
  "LA", 2023, "G", NA, "G", 2027, "I", "I", "SCURLOCK, FRANK", 0, 1131, 0
)

cand <- cand |>
  rows_insert(gov_missing, by = c("year", "state", "office", "dist", "type", "name_snyder"), conflict = "error")

# 29. Same person across offices ----
# presidential rows take the name of the candidate's House, Senate or Governor record; recalls are not general elections
cand <- cand |>
  tidylog::filter(!(year == 2003 & state == "CA" & office == "G" & type == "S")) |>
  tidylog::mutate(
    name_snyder = recode_values(
      name_snyder,
      "FULANI, LENORA" ~ "FULANI, LENORA B.",
      "GOODE, VIRGIL HAMLIN, JR." ~ "GOODE, VIRGIL H., JR.",
      "FORBES, RALPH" ~ "FORBES, RALPH PERRY",
      "FEINLAND, MARSHA" ~ "FEINLAND, MARSHA J.",
      "DODGE, EARL" ~ "DODGE, EARL F.",
      "WHITE, JEROME (JERRY)" ~ "WHITE, JEROME S. (JERRY)",
      "BADNARIK, MICHAEL" ~ "BADNARIK, MICHAEL J.",
      "MOORE, BRIAN" ~ "MOORE, BRIAN PATRICK",
      "WEILL, TED" ~ "WEILL, TED C.",
      "LA RIVA, GLORIA ESTELLA" ~ "LA RIVA, GLORIA ESTELA",
      "VACEK, DAN" ~ "VACEK, DAN R.",
      "HAMMONS, BILL" ~ "HAMMONS, WILLIAM ROBERT (BILL)",
      "TERRY, RANDALL" ~ "TERRY, RANDALL A.",
      "WHITE, JERRY" ~ "WHITE, JEROME S. (JERRY)",
      "HEDGES, JIM" ~ "HEDGES, JAMES (JIM)",
      "HEDGES, JAMES" ~ "HEDGES, JAMES (JIM)",
      default = name_snyder
    ),
    name_snyder = case_when(
      state == "UT" & name_snyder == "ANDERSON, ROSS CARL" ~ "ANDERSON, ROSS CARL (ROCKY)",
      state == "MI" & name_snyder == "WHITE, JEROME S." ~ "WHITE, JEROME S. (JERRY)",
      state == "NJ" & name_snyder == "STACKHOUSE, EDWARD C." ~ "STACKHOUSE, EDWARD C., JR.",
      .default = name_snyder
    )
  )

# 30. Idaho Constitution/Libertarian candidate ----
# one person printed as Idaho Sierra Law (2020), Carta Reale Sierra (2022) and Idaho Law - Carta Sierra (2024);
# the earliest name is kept and the duplicate 2022 Senate row dropped
cand <- cand |>
  tidylog::filter(!(year == 2022 & state == "ID" & name_snyder == "SIERRA, CARTA REALE")) |>
  tidylog::mutate(name_snyder = replace(name_snyder, state == "ID" & name_snyder == "SIERRA, IDAHO LAW - CARTA", "LAW, IDAHO SIERRA"))

# 31. Fusion labels (NY, CT, SC) ----
# one format for every year, as in 2018-2024: lines in printed order, comma-space separated, the same
# abbreviation for each party. 1998-2024 House and Senate lines follow the Clerk volumes (2004 and 2016 had kept
# only the major-party line); 1990-1996 keep Snyder's lines. Olchin, Vitanza and McLaughlin ran as Libertarians;
# Snyder's CT "Lbt?" rows are Libertarians on the Clerk's page
fusion_abbrev <- c(
  C = "Conservative", Cons = "Conservative", CR = "Conservative", CRV = "Conservative",
  WF = "Wk Fam", Indep = "I", IDP = "I", In = "I", IN = "I", "Women's Equality" = "WE",
  Rfm = "Reform", Green = "Grn", Libertarian = "Lbt", "Lbt?" = "Lbt", L = "Liberal", "Right To Life" = "RTL",
  Constitution = "Const", "Write-in" = "W-I", "Serve Amer Movement" = "SAM", "Save Amer Movement" = "SAM",
  Tax = "Tax Revolt", "I Fus" = "Independence Fusion", "Indep Fus" = "Independence Fusion",
  "Concerned Citizens" = "Concerned Cit", "Connecticut for Lieberman" = "CT for Lieberman",
  UC = "United Citizens", "United Cit" = "United Citizens", "UNITED CITIZENS" = "United Citizens",
  NL = "Natural Law", "Nat Law" = "Natural Law", SW = "SWP", SocWk = "SWP"
)

fusion_lines <- tribble(
  ~year, ~state, ~office, ~dist, ~type, ~name_snyder, ~party_formal,
  1998, "NY", "H", 1, "G", "FORBES, MICHAEL P.", "R, Conservative, RTL, I",
  1998, "NY", "H", 27, "G", "COOK, WILLIAM R. (BILL)", "D, RTL, I",
  1998, "NY", "H", 5, "G", "ROBINSON, ANNE T.", "Liberal",
  1998, "NY", "H", 6, "G", "MEEKS, GREGORY W.", "D, I, Liberal",
  2000, "CT", "H", 2, "G", "SIMMONS, ROBERT R. (ROB)", "R, I",
  2000, "NY", "H", 1, "G", "GRUCCI, FELIX J., JR.", "R, Conservative, I, RTL",
  2000, "NY", "H", 15, "G", "RANGEL, CHARLES B.", "D, Wk Fam, Liberal",
  2000, "NY", "H", 19, "G", "GRAHAM, LARRY OTIS", "D, Wk Fam, Liberal",
  2000, "NY", "H", 2, "G", "BISHOP, DAVID A.", "Grn, Wk Fam, I",
  2000, "NY", "H", 20, "G", "FEINER, PAUL J.", "D, Grn, Wk Fam, Liberal",
  2000, "NY", "H", 22, "G", "MCCALLION, KENNETH F.", "D, Grn, Wk Fam",
  2000, "NY", "H", 24, "G", "SMITH, WILLARD E.", "Grn, I",
  2000, "NY", "H", 26, "G", "HINCHEY, MAURICE D.", "D, Wk Fam, Liberal, I",
  2000, "NY", "H", 29, "G", "LAFALCE, JOHN J.", "D, Liberal, I",
  2000, "NY", "H", 3, "G", "LAMAGNA, DAL ANTHONY", "D, Grn, Wk Fam",
  2000, "NY", "H", 30, "G", "FEE, JOHN", "D, Grn, Wk Fam, Liberal",
  2000, "NY", "H", 4, "G", "MCCARTHY, CAROLYN", "D, Wk Fam, I",
  2000, "NY", "H", 5, "G", "ACKERMAN, GARY L.", "D, Wk Fam, Liberal, I",
  2000, "NY", "H", 8, "G", "NADLER, JERROLD L. (JERRY)", "D, Wk Fam, Liberal",
  2000, "NY", "S", 1, "G", "CLINTON, HILLARY RODHAM", "D, Wk Fam, Liberal",
  2002, "NY", "H", 1, "G", "GRUCCI, FELIX J., JR.", "R, Conservative, I, RTL",
  2002, "NY", "H", 13, "G", "MATTSSON, ARNE M.", "D, Wk Fam, Liberal",
  2002, "NY", "H", 14, "G", "MALONEY, CAROLYN B.", "D, I, Wk Fam, Liberal",
  2002, "NY", "H", 17, "G", "ENGEL, ELIOT L.", "D, Wk Fam, Liberal",
  2002, "NY", "H", 17, "G", "VANDERHOEF, C. SCOTT", "R, Conservative, I",
  2002, "NY", "H", 19, "G", "KELLY, SUE W.", "R, Conservative, I",
  2002, "NY", "H", 21, "G", "MCNULTY, MICHAEL R.", "D, Conservative, I, Wk Fam",
  2002, "NY", "H", 22, "G", "HINCHEY, MAURICE D.", "D, I, Wk Fam, Liberal",
  2002, "NY", "H", 25, "G", "WALSH, JAMES THOMAS (JIM)", "R, Conservative, I",
  2002, "NY", "H", 26, "G", "REYNOLDS, THOMAS M.", "R, Conservative, I",
  2002, "NY", "H", 28, "G", "WOJTASZEK, HENRY F.", "R, Conservative, I",
  2002, "NY", "H", 4, "G", "MCCARTHY, CAROLYN", "D, I, Wk Fam, Liberal",
  2002, "NY", "H", 5, "G", "ACKERMAN, GARY L.", "D, I, Wk Fam, Liberal",
  2002, "NY", "H", 6, "G", "MEEKS, GREGORY W.", "D, Wk Fam, Liberal",
  2002, "NY", "H", 8, "G", "NADLER, JERROLD L. (JERRY)", "D, Wk Fam, Liberal",
  2002, "NY", "H", 9, "G", "WEINER, ANTHONY DAVID", "D, Wk Fam, Liberal",
  2004, "NY", "H", 1, "G", "BISHOP, TIMOTHY H. (TIM)", "D, I, Wk Fam",
  2004, "NY", "H", 1, "G", "MANGER, WILLIAM", "R, Conservative",
  2004, "NY", "H", 10, "G", "TOWNS, EDOLPHUS (ED)", "D, Wk Fam",
  2004, "NY", "H", 11, "G", "OWENS, MAJOR ROBERT ODELL", "D, Wk Fam",
  2004, "NY", "H", 12, "G", "RODRIGUEZ, PAUL", "R, Conservative",
  2004, "NY", "H", 12, "G", "VELAZQUEZ, NYDIA M.", "D, Wk Fam",
  2004, "NY", "H", 13, "G", "BARBARO, FRANK J.", "D, I, Wk Fam",
  2004, "NY", "H", 13, "G", "FOSSELLA, VITO J., JR.", "R, Conservative",
  2004, "NY", "H", 14, "G", "MALONEY, CAROLYN B.", "D, I, Wk Fam",
  2004, "NY", "H", 14, "G", "SRDANOVIC, ANTON", "R, Conservative",
  2004, "NY", "H", 15, "G", "RANGEL, CHARLES B.", "D, Wk Fam",
  2004, "NY", "H", 16, "G", "MOHAMED, ALI", "R, Conservative",
  2004, "NY", "H", 16, "G", "SERRANO, JOSE E.", "D, Wk Fam",
  2004, "NY", "H", 17, "G", "ENGEL, ELIOT L.", "D, Wk Fam",
  2004, "NY", "H", 18, "G", "LOWEY, NITA M.", "D, I, Wk Fam",
  2004, "NY", "H", 19, "G", "KELLY, SUE W.", "R, Conservative, I",
  2004, "NY", "H", 2, "G", "HOFFMANN, RICHARD", "R, Conservative",
  2004, "NY", "H", 2, "G", "ISRAEL, STEVE J.", "D, I, Wk Fam",
  2004, "NY", "H", 20, "G", "SWEENEY, JOHN E.", "R, Conservative, I",
  2004, "NY", "H", 21, "G", "MCNULTY, MICHAEL R.", "D, Conservative, I, Wk Fam",
  2004, "NY", "H", 22, "G", "HINCHEY, MAURICE D.", "D, I, Wk Fam",
  2004, "NY", "H", 23, "G", "MCHUGH, JOHN M.", "R, Conservative, I",
  2004, "NY", "H", 24, "G", "BOEHLERT, SHERWOOD L. (SHERRY)", "R, I",
  2004, "NY", "H", 25, "G", "WALSH, JAMES THOMAS (JIM)", "R, Conservative, I",
  2004, "NY", "H", 26, "G", "DAVIS, JOHN R. (JACK)", "D, Wk Fam",
  2004, "NY", "H", 26, "G", "REYNOLDS, THOMAS M.", "R, Conservative, I",
  2004, "NY", "H", 27, "G", "HIGGINS, BRIAN M.", "D, I, Wk Fam",
  2004, "NY", "H", 27, "G", "NAPLES, NANCY", "R, Conservative",
  2004, "NY", "H", 28, "G", "LABA, MICHAEL D.", "R, Conservative",
  2004, "NY", "H", 28, "G", "SLAUGHTER, LOUISE MCINTOSH", "D, Wk Fam",
  2004, "NY", "H", 29, "G", "BAREND, SAMARA", "D, Wk Fam",
  2004, "NY", "H", 3, "G", "KING, PETER T. (PETE)", "R, Conservative, I",
  2004, "NY", "H", 4, "G", "GARNER, JAMES", "R, Conservative",
  2004, "NY", "H", 4, "G", "MCCARTHY, CAROLYN", "D, I, Wk Fam",
  2004, "NY", "H", 5, "G", "ACKERMAN, GARY L.", "D, I, Wk Fam",
  2004, "NY", "H", 5, "G", "GRAVES, STEPHEN E.", "R, Conservative",
  2004, "NY", "H", 6, "G", "MEEKS, GREGORY W.", "D, Wk Fam",
  2004, "NY", "H", 7, "G", "CROWLEY, JOSEPH (JOE)", "D, Wk Fam",
  2004, "NY", "H", 8, "G", "HORT, PETER", "R, Conservative, I",
  2004, "NY", "H", 8, "G", "NADLER, JERROLD L. (JERRY)", "D, Wk Fam",
  2004, "NY", "H", 9, "G", "CRONIN, GERARD", "R, Conservative, I",
  2004, "NY", "H", 9, "G", "WEINER, ANTHONY DAVID", "D, Wk Fam",
  2004, "NY", "S", 3, "G", "SCHUMER, CHARLES E. (CHUCK)", "D, I, Wk Fam",
  2004, "SC", "H", 6, "G", "MCLEOD, GARY M.", "R, Const",
  2004, "SC", "S", 3, "G", "NWANGAZA, EFIA", "Grn, I",
  2006, "NY", "H", 1, "G", "BISHOP, TIMOTHY H. (TIM)", "D, Wk Fam, I",
  2006, "NY", "H", 11, "G", "FINGER, STEPHEN", "R, Lbt",
  2006, "NY", "H", 14, "G", "MAIO, DANNIEL S.", "R, Let's Do It",
  2006, "NY", "H", 14, "G", "MALONEY, CAROLYN B.", "D, Wk Fam, I",
  2006, "NY", "H", 2, "G", "ISRAEL, STEVE J.", "D, Wk Fam, I",
  2006, "NY", "H", 21, "G", "MCNULTY, MICHAEL R.", "D, Conservative, Wk Fam, I",
  2006, "NY", "H", 22, "G", "HINCHEY, MAURICE D.", "D, Wk Fam, I",
  2006, "NY", "H", 24, "G", "ARCURI, MICHAEL A.", "D, Wk Fam, I",
  2006, "NY", "H", 26, "G", "DAVIS, JOHN R. (JACK)", "D, Wk Fam, I",
  2006, "NY", "H", 27, "G", "HIGGINS, BRIAN M.", "D, Wk Fam, I",
  2006, "NY", "H", 28, "G", "SLAUGHTER, LOUISE MCINTOSH", "D, Wk Fam, I",
  2006, "NY", "H", 4, "G", "MCCARTHY, CAROLYN", "D, Wk Fam, I",
  2006, "NY", "H", 5, "G", "ACKERMAN, GARY L.", "D, Wk Fam, I",
  2006, "NY", "S", 1, "G", "CLINTON, HILLARY RODHAM", "D, Wk Fam, I",
  2008, "NY", "H", 1, "G", "BISHOP, TIMOTHY H. (TIM)", "D, Wk Fam, I",
  2008, "NY", "H", 3, "G", "KING, PETER T. (PETE)", "R, Conservative, I",
  2010, "NY", "H", 27, "G", "ROBERTO, LEONARD A.", "R, Conservative, Taxp",
  2012, "NY", "H", 2, "G", "KING, PETER T. (PETE)", "R, Tax Revolt, Conservative, I",
  2012, "NY", "H", 3, "G", "LABATE, STEPHEN A.", "R, Tax Revolt, Conservative",
  2012, "NY", "H", 6, "G", "HALLORAN, DANIEL J. (DAN), III", "R, Conservative, Lbt",
  2014, "NY", "H", 2, "G", "KING, PETER T. (PETE)", "R, Tax Revolt, Conservative, I",
  2014, "NY", "H", 3, "G", "LALLY, GRANT M.", "R, Conservative, Lbt",
  2016, "CT", "H", 1, "G", "LARSON, JOHN B.", "D, Wk Fam",
  2016, "CT", "H", 2, "G", "COURTNEY, JOSEPH D. (JOE)", "D, Wk Fam",
  2016, "CT", "H", 3, "G", "DELAURO, ROSA L.", "D, Wk Fam",
  2016, "CT", "H", 4, "G", "SHABAN, JOHN T.", "R, I",
  2016, "CT", "H", 5, "G", "COPE, CLAY", "R, I",
  2016, "CT", "H", 5, "G", "ESTY, ELIZABETH H.", "D, Wk Fam",
  2016, "CT", "S", 3, "G", "BLUMENTHAL, RICHARD", "D, Wk Fam",
  2016, "NY", "H", 1, "G", "THRONE-HOLST, ANNA E.", "D, Wk Fam, WE",
  2016, "NY", "H", 1, "G", "ZELDIN, LEE M.", "R, Conservative, I, Reform",
  2016, "NY", "H", 10, "G", "NADLER, JERROLD L. (JERRY)", "D, Wk Fam, WE",
  2016, "NY", "H", 10, "G", "ROSENTHAL, PHILIP J. (PHIL)", "R, Conservative, I, Stop Iran Deal",
  2016, "NY", "H", 11, "G", "DONOVAN, DANIEL M. (DAN), JR.", "R, Conservative, I, Reform",
  2016, "NY", "H", 12, "G", "MALONEY, CAROLYN B.", "D, Wk Fam",
  2016, "NY", "H", 13, "G", "EVANS, ROBERT ANTHONY (TONY)", "R, I",
  2016, "NY", "H", 14, "G", "CROWLEY, JOSEPH (JOE)", "D, Wk Fam, WE",
  2016, "NY", "H", 14, "G", "SPOTORNO, FRANK J.", "R, Conservative",
  2016, "NY", "H", 16, "G", "ENGEL, ELIOT L.", "D, Wk Fam, WE",
  2016, "NY", "H", 17, "G", "LOWEY, NITA M.", "D, Wk Fam, WE",
  2016, "NY", "H", 18, "G", "MALONEY, SEAN PATRICK", "D, I, Wk Fam, WE",
  2016, "NY", "H", 18, "G", "OLIVA, PHIL", "R, Conservative, Reform",
  2016, "NY", "H", 19, "G", "FASO, JOHN J.", "R, Conservative, I, Reform",
  2016, "NY", "H", 19, "G", "TEACHOUT, ZEPHYR R.", "D, Wk Fam",
  2016, "NY", "H", 2, "G", "GREGORY, DUWAYNE", "D, Wk Fam, I, WE",
  2016, "NY", "H", 2, "G", "KING, PETER T. (PETE)", "R, Tax Revolt, Conservative, Reform",
  2016, "NY", "H", 20, "G", "TONKO, PAUL DAVID", "D, Wk Fam, I, WE",
  2016, "NY", "H", 20, "G", "VITOLLO, FRANCIS JOSEPH (JOE)", "R, Conservative, Reform",
  2016, "NY", "H", 21, "G", "DERRICK, WILLIAM MIKE", "D, Wk Fam",
  2016, "NY", "H", 21, "G", "STEFANIK, ELISE M.", "R, Conservative, I, Reform",
  2016, "NY", "H", 22, "G", "BABINEC, MARTIN", "Reform, Upstate Jobs",
  2016, "NY", "H", 22, "G", "MYERS, KIM A.", "D, Wk Fam",
  2016, "NY", "H", 22, "G", "TENNEY, CLAUDIA", "R, Conservative",
  2016, "NY", "H", 23, "G", "PLUMB, JOHN F.", "D, Wk Fam",
  2016, "NY", "H", 23, "G", "REED, THOMAS W. (TOM), II", "R, Conservative, I, Reform",
  2016, "NY", "H", 24, "G", "DEACON, COLLEEN", "D, Wk Fam",
  2016, "NY", "H", 24, "G", "KATKO, JOHN M.", "R, Conservative, I, Reform",
  2016, "NY", "H", 25, "G", "ASSINI, MARK W.", "R, Conservative, I, Reform",
  2016, "NY", "H", 25, "G", "SLAUGHTER, LOUISE MCINTOSH", "D, Wk Fam, WE",
  2016, "NY", "H", 26, "G", "HIGGINS, BRIAN M.", "D, Wk Fam, WE",
  2016, "NY", "H", 26, "G", "SCHRATZ, SHELLY", "R, Conservative",
  2016, "NY", "H", 27, "G", "COLLINS, CHRISTOPHER C. (CHRIS)", "R, Conservative, I, Reform",
  2016, "NY", "H", 3, "G", "MARTINS, JACK M.", "R, Conservative, Reform",
  2016, "NY", "H", 4, "G", "GURFEIN, DAVID H. (BULL)", "R, Conservative, Reform",
  2016, "NY", "H", 4, "G", "RICE, KATHLEEN M.", "D, WE",
  2016, "NY", "H", 5, "G", "MEEKS, GREGORY W.", "D, WE",
  2016, "NY", "H", 5, "G", "O'REILLY, MICHAEL A.", "R, Conservative",
  2016, "NY", "H", 6, "G", "MAIO, DANNIEL S.", "R, Conservative, Blue Lives Matter",
  2016, "NY", "H", 6, "G", "MENG, GRACE", "D, WE",
  2016, "NY", "H", 7, "G", "ROMAGUERA, ALLAN E. (AL)", "R, Conservative",
  2016, "NY", "H", 8, "G", "CAVANAGH, DANIEL J.", "Conservative",
  2016, "NY", "H", 8, "G", "JEFFRIES, HAKEEM S.", "D, Wk Fam",
  2016, "NY", "H", 9, "G", "BELLONE, ALAN", "Conservative",
  2016, "NY", "H", 9, "G", "CLARKE, YVETTE DIANE", "D, Wk Fam",
  2016, "NY", "S", 3, "G", "LONG, WENDY", "R, Conservative, Reform",
  2016, "NY", "S", 3, "G", "SCHUMER, CHARLES E. (CHUCK)", "D, Wk Fam, I, WE",
  2016, "SC", "H", 1, "G", "CHERNY, DIMITRI", "D, Wk Fam, Grn",
  2016, "SC", "H", 2, "G", "BJORN, ARIK M.", "D, Grn",
  2016, "SC", "H", 7, "G", "HYMAN, MAL", "D, Wk Fam, Grn",
  2016, "SC", "S", 3, "G", "BLEDSOE, BILL", "Const, Lbt",
  2016, "SC", "S", 3, "G", "DIXON, THOMAS", "D, Wk Fam, Grn",
  2018, "CT", "H", 1, "G", "LARSON, JOHN B.", "D, Wk Fam",
  2018, "CT", "H", 2, "G", "COURTNEY, JOSEPH D. (JOE)", "D, Wk Fam",
  2018, "CT", "H", 3, "G", "DELAURO, ROSA L.", "D, Wk Fam",
  2018, "CT", "H", 4, "G", "ARORA, HARRY", "R, I",
  2018, "CT", "H", 5, "G", "HAYES, JAHANA", "D, Wk Fam",
  2018, "CT", "H", 5, "G", "SANTOS, MANNY", "R, I",
  2018, "CT", "S", 1, "G", "MURPHY, CHRISTOPHER SCOTT (CHRIS)", "D, Wk Fam",
  2020, "NY", "H", 18, "G", "SMITH, SCOTT A.", "Lbt, SAM"
)

cand <- cand |>
  tidylog::mutate(party_formal = if_else(
    state %in% c("NY", "CT", "SC"),
    map_chr(str_split(party_formal, ",\\s*"), \(l) paste(coalesce(fusion_abbrev[l], l), collapse = ", ")),
    party_formal
  )) |>
  rows_update(fusion_lines, by = c("year", "state", "office", "dist", "type", "name_snyder"), unmatched = "error") |>
  rows_update(tribble(
    ~year, ~state, ~office, ~dist, ~type, ~name_snyder, ~party, ~party_formal,
    2000, "NY", "H", 3, "G", "OLCHIN, SELMA", "Lbt", "Lbt",
    2000, "NY", "H", 4, "G", "VITANZA, BARBARA", "Lbt", "Lbt",
    2014, "SC", "H", 4, "G", "MCLAUGHLIN, CURTIS E., JR.", "Lbt", "Lbt",
    1990, "CT", "H", 5, "G", "HARE, WILLIAM GEORGE", "Lbt", "Lbt",
    1996, "NY", "H", 14, "G", "LEIGHTON, THOMAS K.", "I", "I, Grn",
    1998, "CT", "S", 3, "G", "MOORE, WILDEY JUDD", "Lbt", "Lbt",
    2000, "CT", "H", 4, "G", "GISLAO, DANIEL", "Lbt", "Lbt",
    2000, "CT", "S", 1, "G", "MOORE, WILDEY JUDD", "Lbt", "Lbt",
    2002, "CT", "H", 5, "G", "GENGARELLY, WALTER J.", "Lbt", "Lbt"
  ), by = c("year", "state", "office", "dist", "type", "name_snyder"), unmatched = "error") |>
  # vote_g is the sum of every line; these three carried one line or a stray figure
  rows_update(tribble(
    ~year, ~state, ~office, ~dist, ~type, ~name_snyder, ~vote_g,
    2000, "NY", "H", 23, "G", "VICKERS, DAVID B.", 32854,
    2012, "NY", "H", 6, "G", "HALLORAN, DANIEL J. (DAN), III", 50845,
    2014, "NY", "H", 3, "G", "LALLY, GRANT M.", 74269
  ), by = c("year", "state", "office", "dist", "type", "name_snyder"), unmatched = "error")

## Governors ----
# certified state returns: votes summed over every line, lines in printed order. NY 1990 has no official
# source online and keeps Snyder's figures. Several races also lacked minor and write-in candidates
# CT 1994: https://web.archive.org/web/20101007091016/http://www.sots.ct.gov/sots/lib/sots/electionservices/statementofvote_pdfs/1994_sov.pdf
# CT 1998: https://web.archive.org/web/20250901200716/https://portal.ct.gov/sots/register-manual/section-viii---old-version/election-statistics-vote-for-governor--lt-governor--november-1998
# CT 2002: https://web.archive.org/web/20250326205158/https://portal.ct.gov/sots/register-manual/section-viii---old-version/vote-for-governor-and-lt-governor-2002
# CT 2006: https://web.archive.org/web/20250324090851/https://portal.ct.gov/sots/register-manual/section-viii---old-version/vote-for-governor-and-lieutenant-governor-2006
# CT 2014: https://electionhistory.ct.gov/eng/contests/view/5743
# CT 2018: https://electionhistory.ct.gov/eng/contests/view/3781
# CT 2022: https://electionhistory.ct.gov/eng/contests/view/35080
# NY 1994: https://web.archive.org/web/20120823020419/http://www.elections.ny.gov/NYSBOE/elections/1994/gov94.pdf
# NY 1998: https://web.archive.org/web/20120823020649/http://www.elections.ny.gov/NYSBOE/elections/1998/GOVWEB.pdf
# NY 2002: https://web.archive.org/web/20120823003957/http://www.elections.ny.gov/NYSBOE/elections/2002/general/2002_gov.pdf
# NY 2006: https://web.archive.org/web/20120822221126/http://www.elections.ny.gov/NYSBOE/elections/2006/general/2006_gov.pdf
# NY 2010: https://web.archive.org/web/20121004081140/http://www.elections.ny.gov/NYSBOE/elections/2010/general/2010GovernorRecertified09122012.pdf
# NY 2014: https://web.archive.org/web/20150410011028/http://www.elections.ny.gov/NYSBOE/elections/2014/general/2014Governor.xlsx
# NY 2018: https://web.archive.org/web/20181215022233/https://www.elections.ny.gov/NYSBOE/elections/2018/general/2018Governor.pdf
# NY 2022: https://web.archive.org/web/20221230031610/https://www.elections.ny.gov/NYSBOE/elections/2022/General/2022CertifiedStatewideGenElectionResults.xlsx
gov_fusion <- tribble(
  ~year, ~state, ~name_snyder, ~party_formal, ~vote_g,
  1994, "NY", "GOLISANO, B. THOMAS", "Independence Fusion", 217490,
  1998, "NY", "PATAKI, GEORGE E.", "R, Conservative", 2571991,
  1998, "NY", "VALLONE, PETER", "D, Wk Fam", 1570317,
  2002, "CT", "ROWLAND, JOHN G.", "R", 573958,
  2002, "CT", "CURRY, WILLIAM E., JR.", "D", 448984,
  2002, "NY", "ARONOWITZ, STANLEY", "Grn", 41797,
  2006, "NY", "MCMILLAN, JIMMY", "Rent 2 Damn High", 13355,
  2010, "NY", "CUOMO, ANDREW M.", "D, I, Wk Fam", 2910876,
  2010, "NY", "PALADINO, CARL P.", "R, Conservative, Taxp", 1547857,
  2010, "NY", "HAWKINS, HOWIE", "Grn", 59906,
  2010, "NY", "REDLICH, WARREN", "Lbt", 48359,
  2010, "NY", "MCMILLAN, JIMMY", "Rent 2 Damn High", 41129,
  2010, "NY", "BARRON, CHARLES", "Freedom", 24571,
  2010, "NY", "DAVIS, KRISTIN M.", "Anti-Prohib", 20421,
  2014, "CT", "MALLOY, DANNEL P. (DAN)", "D, Wk Fam", 554314,
  2014, "CT", "FOLEY, THOMAS C. (TOM)", "R, I", 526295,
  2014, "NY", "CUOMO, ANDREW M.", "D, Wk Fam, I, WE", 2069480,
  2014, "NY", "ASTORINO, ROB", "R, Conservative, SCC", 1536879,
  2014, "NY", "MCDERMOTT, MICHAEL (MIKE)", "Lbt", 16967,
  2018, "CT", "LAMONT, EDWARD M. (NED)", "D, Wk Fam", 694510,
  2018, "CT", "STEFANOWSKI, BOB", "R, I", 650138,
  2018, "NY", "CUOMO, ANDREW M.", "D, Wk Fam, I, WE", 3635340,
  2018, "NY", "MOLINARO, MARCUS J. (MARC)", "R, Conservative, Reform", 2207602,
  2022, "CT", "LAMONT, EDWARD M. (NED)", "D, Griebel Frank for CT, Wk Fam", 710186,
  2022, "NY", "HOCHUL, KATHLEEN COURTNEY (KATHY)", "D, Wk Fam", 3140415,
  2022, "NY", "ZELDIN, LEE M.", "R, Conservative", 2762581
) |>
  mutate(office = "G", dist = NA_real_, type = "G")

gov_added <- tribble(
  ~state, ~year, ~nextup, ~party, ~party_formal, ~name_snyder, ~vote_g,
  "CT", 1994, 1998, "I", "I", "SCOTT, THOMAS", 130128,
  "CT", 1994, 1998, "Other", "Concerned Cit", "ZDONCZYK, JOSEPH A.", 10007,
  "CT", 1994, 1998, "Other", "W-I", "LOOMIS, ROBERT S.", 29,
  "CT", 1998, 2002, "Other", "Concerned Cit", "ZDONCZYK, JOSEPH A.", 8792,
  "CT", 1998, 2002, "Lbt", "Lbt", "VARE, NED", 5637,
  "CT", 1998, 2002, "Other", "Term Limits", "SCAGLIONE, ROBERTA M.", 2212,
  "CT", 2002, 2006, "Other", "W-I", "GOSSELIN, PHILIP", 29,
  "CT", 2002, 2006, "Other", "W-I", "COLONNA, WILLIAM M.", 16,
  "CT", 2006, 2010, "Other", "W-I", "JOY, JOHN M.", 54,
  "CT", 2018, 2022, "Other", "W-I", "WHITNUM, LEE", 74,
  "CT", 2022, 2026, "Other", "W-I", "BICKING, MICHELLE LOUISE", 98,
  "NY", 1998, 2002, "I", "I", "GOLISANO, B. THOMAS", 364056,
  "NY", 1998, 2002, "Other", "Liberal", "MCCAUGHEY ROSS, ELIZABETH (BETSY)", 77915,
  "NY", 1998, 2002, "Other", "RTL", "REYNOLDS, MICHAEL J.", 56683,
  "NY", 1998, 2002, "Grn", "Grn", "LEWIS, AL", 52533,
  "NY", 1998, 2002, "Other", "Marij Rfm", "LEIGHTON, THOMAS K.", 24788,
  "NY", 1998, 2002, "Other", "Unity", "FRANCE, MARY ALICE", 9692,
  "NY", 1998, 2002, "Lbt", "Lbt", "GARVEY, CHRISTOPHER B.", 4722,
  "NY", 1998, 2002, "Other", "SWP", "DUNCAN, AL", 2539
) |>
  mutate(office = "G", dist = NA_real_, type = "G", inc = 0, w_g = 0)

cand <- cand |>
  rows_update(gov_fusion, by = c("year", "state", "office", "dist", "type", "name_snyder"), unmatched = "error") |>
  rows_insert(gov_added, by = c("year", "state", "office", "dist", "type", "name_snyder"), conflict = "error")

# 32. Write-ins ----
# the guide excludes candidates under 10 votes; the 2020-2024 sources kept some.
# Ohio 2024 Senate write-ins printed by the Clerk were missing
cand <- cand |>
  tidylog::filter(is.na(vote_g) | vote_g >= 10) |>
  rows_insert(tribble(
    ~state, ~year, ~office, ~dist, ~type, ~nextup, ~party, ~party_formal, ~name_snyder, ~inc, ~vote_g, ~w_g,
    "OH", 2024, "S", 1, "G", 2030, "Other", "W-I", "SHABAZZ, TARIQ K.", 0, 524, 0,
    "OH", 2024, "S", 1, "G", 2030, "Other", "W-I", "FARIS, STEPHEN", 0, 84, 0,
    "OH", 2024, "S", 1, "G", 2030, "Other", "W-I", "RUSSELL, NATHAN", 0, 19, 0,
    "OH", 2024, "S", 1, "G", 2030, "Other", "W-I", "PASTORIUS, DAVID ALLEN", 0, 13, 0
  ), by = c("year", "state", "office", "dist", "type", "name_snyder"), conflict = "error")

# 33. Totals ----

# rows added here miss 04's runoff default for GA and LA
cand <- cand |>
  mutate(runoff = if_else(state %in% c("GA", "LA") & is.na(runoff), 0, runoff)) |>
  mutate(totalvotes = if (all(is.na(vote_g))) NA_real_ else sum(vote_g, na.rm = TRUE), .by = c(state, year, office, dist, type)) |>
  arrange(year, office, state, dist)

write_rds(cand, "data/intermediate/candidates_1990-2024.rds")
