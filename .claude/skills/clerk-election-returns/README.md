# Filling CAGE House and Senate results with Claude Code

Before an election, the CAGE roster for the year lives in a Google Sheet (tab
`Results_<year>`): every candidate with `name_snyder`, party and incumbency
coded, and the official columns blank. Once the Clerk of the House publishes its
*Statistics of the Congressional Election* volume, Claude Code reads it and fills
`candidatevotes`, `totalvotes` and `won` for the House and Senate rows. `TASK.md`
holds the steps Claude follows; `SKILL.md` and `reference/reader.md` hold the
rules.

## Requirements

- R with `tidyverse`, `fs`, `pdftools` and `googlesheets4`; poppler (`pdftoppm`,
  `pdftotext`); `curl`.
- A cached Google login for the sheet. Once per machine, type this into the
  Claude Code prompt (the `!` runs it inside the session):

  ```
  ! Rscript -e 'options(rlang_interactive = TRUE, browser = function(url) cat(url, "\n")); googlesheets4::gs4_auth(cache = TRUE)'
  ```

  Open the printed link in a browser on the same machine, choose the account
  that can see the sheet, and tick the Google Sheets permission box (it is
  unticked by default; without it reads fail with "insufficient scopes").
- The Clerk volume must be published, usually about four months after the
  election (the 2024 volume came out 10 March 2025).
- For validation only: `data/intermediate/candidates_1990-2024.rds` (built by
  `00a_scripts-cand.R`; gitignored).

## Fill a new year

Open Claude Code in the repo root and type:

> Execute `.claude/skills/clerk-election-returns/TASK.md` for 2026

Claude then:

1. downloads the volume to `data/clerk/2026election.pdf` and checks the cover year;
2. reads the roster from the sheet and prepares `data/clerk/claude_read/2026/`;
3. launches one reader agent per 10 pages, in parallel, to transcribe ballot lines;
4. merges and checks their rows (`check_readings.R`);
5. matches lines to roster rows and fills the official columns (`fill_roster.R`);
6. reports what was filled, added, left blank and flagged.

The sheet itself is never edited.

### Outputs

| File | Contents |
|---|---|
| `data/2026/candidates_2026_skeleton.csv` | Every roster row with official columns filled where possible, plus Clerk candidates missing from the roster (`match = not in roster`), with provenance columns |
| `data/2026/candidates_2026_discrepancies.csv` | Rows to check on the PDF page: internal inconsistencies and reader flags |
| `data/2026/candidates_2026_hand-entry.csv` | Specials, runoffs and ranked-choice rounds to code from Secretary of State returns |
| `data/clerk/claude_rows_2026.tsv` | Every printed ballot line as read: the provenance for every figure |

### After the run

1. Check each discrepancy against the PDF page it names.
2. Review `match = only unmatched pair in race`, `not in roster` rows and roster
   rows `not printed by the Clerk`.
3. Code the hand-entry races; governors come from state returns.
4. Append the skeleton at `01_stack-sources.R` (drop the provenance columns) and
   run the pipeline through `07_checks.R`.

## Test on a past year

> Execute `.claude/skills/clerk-election-returns/TASK.md` for 2024 with a test roster, then validate it

Claude builds `data/2024/roster_test.csv` from CAGE (official columns blank,
named write-ins dropped so the "not in roster" path is exercised), runs the
steps with `--roster data/2024/roster_test.csv`, then `validate_pipeline.R 2024`,
which scores the skeleton against the audited CAGE rows and writes
`data/clerk/validation/2024/`.

## Running the scripts yourself

```bash
Rscript .claude/skills/clerk-election-returns/scripts/prepare_reading.R 2026
# step 3, the reading, needs Claude Code (see TASK.md)
Rscript .claude/skills/clerk-election-returns/scripts/check_readings.R 2026
Rscript .claude/skills/clerk-election-returns/scripts/fill_roster.R 2026

# validation on a past year
Rscript .claude/skills/clerk-election-returns/scripts/make_test_roster.R 2024
Rscript .claude/skills/clerk-election-returns/scripts/prepare_reading.R 2024 --roster data/2024/roster_test.csv
Rscript .claude/skills/clerk-election-returns/scripts/validate_pipeline.R 2024
```

## What to commit

- **New year:** `data/clerk/claude_read/<year>/out/`, `batches.csv`,
  `data/clerk/claude_rows_<year>.tsv` and the three `data/<year>/candidates_*`
  files. They are the record of what was read.
- **Validation year:** keep the record in `data/clerk/validation/<year>/`: move
  `out/*.tsv` and `batches.csv` into `transcriptions/`, plus
  `claude_rows_<year>.tsv`, `roster_test.csv` and the three
  `candidates_<year>_*` files, next to `summary.csv` and `candidates.csv`. Then
  delete `data/clerk/claude_read/<year>/`. 2022 and 2024 are kept this way.
- Page images, zoom renders and `roster.csv` copies are gitignored.
