---
name: clerk-election-returns
description: Fill a new election year's official U.S. House and Senate results (candidatevotes, totalvotes, won) from the Clerk of the House "Statistics of the Congressional Election" PDF at history.house.gov, into the CAGE roster kept in the project's Google Sheet (names, party, incumbency already coded). Claude reads the page images and transcribes ballot lines; R matches them to the roster and flags everything it cannot fill. Use when the Clerk volume for a new year (e.g. 2026) is out; the steps are in TASK.md.
---

# Clerk of the House election returns → CAGE roster

The Clerk of the House publishes one PDF per election cycle at
<https://history.house.gov/Institution/Election-Statistics/>. It is the
authoritative source for House and Senate `candidatevotes`. Before the election,
the CAGE roster for the year is kept in a Google Sheet (tab `Results_<year>` of
sheet `1otML47xPsa9Wf1A_XEISLJFHEm6wpv5D3FzRe2-xZXY`): every ballot-qualified
House, Senate and Governor candidate with `name_snyder`, `party`,
`party_formal`, `inc`, `type`, `dist` and `nextup` already coded, and the
official columns (`candidatevotes`, `totalvotes`, `won`) blank. This skill fills
those official columns for House and Senate rows from the Clerk volume.

**To run it:** "Execute `.claude/skills/clerk-election-returns/TASK.md` for 2026".

## The one rule that matters

**Never write a vote total you did not read off the page.** A wrong number is
far worse than a missing one, because a missing one is visible and a wrong one
is not. When the document is ambiguous, the cell is left empty and the row is
flagged. Leaving `candidatevotes` blank is an accepted outcome; the CAGE
codebook already blanks vote counts for uncontested Florida races.

Concretely, no step may:

- read a number off a chart, a percentage, or a neighbouring row;
- carry a number over from another source (the sheet's live columns, MEDSL,
  Ballotpedia, Wikipedia, a Secretary of State page) into a cell sourced from
  this PDF;
- infer a total by subtraction from a recapitulation row;
- repair a flagged row from memory of the election.

The roster's names and party codes are not re-coded from the PDF. A Clerk
candidate missing from the roster gets a proposed name built only from the
printed name and is flagged for review.

## Pipeline

| Step | Done by | Writes |
|---|---|---|
| 1. Fetch the volume, check the cover year | shell | `data/clerk/<year>election.pdf` |
| 2. Prepare the reader folder and roster | `scripts/prepare_reading.R` | `data/clerk/claude_read/<year>/`: page images, `roster.csv` (from the sheet), `reader.md`, `batches.csv` |
| 3. Transcribe the pages | Claude subagents, one per 10-page batch, following `reference/reader.md` | `data/clerk/claude_read/<year>/out/pages_*.tsv` |
| 4. Check the readings | `scripts/check_readings.R` | `data/clerk/claude_rows_<year>.tsv`, `data/<year>/candidates_<year>_discrepancies.csv`, `data/<year>/candidates_<year>_hand-entry.csv` |
| 5. Fill the roster | `scripts/fill_roster.R` | `data/<year>/candidates_<year>_skeleton.csv` |
| Validation (past years) | `scripts/make_test_roster.R`, then steps 2–5, then `scripts/validate_pipeline.R` | `data/<year>/roster_test.csv`, `data/clerk/validation/<year>/` |

Readers check their own figures against each state's recapitulation table and
flag any race that does not add up. They never see the roster.

## How the roster is filled

Within each race (state, office, House district, special or regular), each
printed candidate line is matched to one roster row, in this order:

1. the last word of the surname (hyphens split), when it is unique in the race;
2. surname plus first initial or nickname initial (two candidates sharing a
   surname);
3. a compound surname found anywhere in the printed name;
4. the only unmatched roster row against the only unmatched printed candidate
   (`match = only unmatched pair in race`; check these).

Then:

- `candidatevotes` is the sum of all the candidate's printed party lines.
- `totalvotes` is the sum over the race's printed candidates, and `won` marks
  the most votes, only when every printed candidate has a figure; a lone
  candidate with no printed count (unopposed) gets `won = 1`.
- A roster row with no printed line is kept, blank, flagged
  `on roster, not printed by the Clerk` (withdrew, or the Clerk omitted them).
- A printed named candidate with 10+ votes and no roster row is added, flagged
  `not in roster`, with `name_snyder` and `party` built from the printed line
  (party from the first label: Democrat D, Republican R, Libertarian Lbt,
  Green Grn, independent/no-party labels I, write-ins and everything else
  Other) and `inc = 0`.
- Races on the hand-entry list keep blank official columns.
- Governor rows pass through blank (`governor: from state returns`).
- When the sheet's `votes_live` is filled, a row whose official figure differs
  from it by more than 10% is flagged.

## Outputs

### `candidates_<year>_skeleton.csv`

Every roster row plus added rows, in CAGE columns (`year state office dist type
nextup party party_formal name_snyder inc candidatevotes totalvotes won
runoff`) followed by provenance: `cage_row_id`, `candidate` (from the sheet),
`name_printed`, `party_lines`, `line_votes`, `source_page`, `match`, `flags`,
`line_flags` (the reader's flag).

### `candidates_<year>_discrepancies.csv`

Rows a person should check on the PDF page named in `page`: unexpected codes,
duplicated lines, party lines and figures that do not line up, candidate totals
that are not the sum of their lines, blank figures without a flag, and every
reader flag.

### `candidates_<year>_hand-entry.csv`

Races coded by hand from Secretary of State returns, not taken from this volume:

| `reason` | Why |
|---|---|
| `special election` | printed under "For unexpired term ending …"; specials are coded from state returns |
| `runoff state with no majority` | Georgia or Louisiana race where nobody cleared 50%; CAGE keeps the deciding runoff |
| `figure may not be the November first round` | a footnote or ranked-choice lines; CAGE keeps the final ranked-choice round |
| `ballot line with no candidate name printed` (`scope = line`) | e.g. just "Green .... 57,442"; identify the candidate |

House special elections held on other dates are not printed in the volume at
all.

## Scope

**In scope:** House, Delegate and Senate races held on the November general
election date, as printed. Delegates and DC are dropped (CAGE excludes them).

**Not in scope, coded by hand:** special elections, runoff rounds (GA, LA, MS
specials), ranked-choice final rounds (AK, ME), Governor (state returns),
President (MEDSL).

## After the run

1. Work the discrepancies file against the PDF pages.
2. Review rows matched only by elimination, rows added `not in roster`, and
   roster rows not printed.
3. Code the hand-entry races from Secretary of State returns.
4. Append the skeleton at `01_stack-sources.R` (dropping the provenance columns)
   and run the pipeline through `07_checks.R`.

Keep `claude_rows_<year>.tsv`, the readers' `out/` files and the `data/<year>/`
files: together they are the provenance for every figure.

## Files

- `TASK.md`: the step-by-step task Claude executes for a year.
- `reference/reader.md`: transcription rules for the readers.
- `scripts/prepare_reading.R`, `scripts/check_readings.R`,
  `scripts/fill_roster.R`: steps 2, 4 and 5.
- `scripts/make_test_roster.R`, `scripts/validate_pipeline.R`: end-to-end test
  on a year CAGE already covers.

## Validation status

End-to-end runs on 2024 and 2022 (October 2026), with test rosters built from
the audited CAGE data (named write-ins removed) and the readers seeing only the
page images:

| | 2024 | 2022 |
|---|---|---|
| Ballot lines read | 1,518 | 1,524 |
| Roster rows matched to a Clerk line, with `candidatevotes` identical to CAGE | 1,173 of 1,173 | 1,168 of 1,168 |
| Roster rows left blank: House specials not in the volume | 21 | 22 |
| Rows in hand-entry races (runoffs, ranked-choice rounds) | 10 | 20 |
| Clerk candidates added (`not in roster`) | 65 | 78 |
| … of which CAGE has under the same name | 58 | 70 |

With the House specials set aside, `totalvotes` and `won` now match CAGE in
every race. The runs led to fixes in `04b_audit-corrections.R`: CAGE's
sub-10-vote write-ins were dropped (the guide excludes them), four 2024 Ohio
Senate write-ins the Clerk prints were added, and a duplicate 2022 Idaho Senate
row was removed. The remaining differences are added candidates whose name is
formatted differently from CAGE's (always flagged) and the Idaho
Constitution/Libertarian candidate, whose test roster predates the name merge.
