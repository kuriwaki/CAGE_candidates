This folder contains the final data to be released and uploaded to Dataverse.

- `candidates_1990-2024.dta`: candidate-level data.
- `candidates_1990-2024-by-contest.dta`: one row per contest with exactly one Democratic and one Republican candidate, including their names, incumbency, votes, and the Democratic share of the two-party vote.

Run the temporary 2026 election-night script `08_upload-2026-election-night.R` from the project root to upload both releases to the `1990-2024-CAGE_by-cand` and `1990-2024-CAGE_by-contest` tabs in [2026 Election Returns - CAGE](https://docs.google.com/spreadsheets/d/1otML47xPsa9Wf1A_XEISLJFHEm6wpv5D3FzRe2-xZXY/edit), sorted by descending year and office. Re-running the script refreshes those two tabs.

Run the temporary `09_pivot-2026-election-night.R` script to create `2026_by-contest` with the same columns and one-D/one-R rule as `1990-2024-CAGE_by-contest`, sorted by descending year and office. Names, incumbency, and live vote counts link to `Results_2026` by candidate ID; each `share_D_twoparty` cell contains its own formula. The `candidatevotes_*` columns use unofficial live counts from `votes_live`, with missing counts and shares left blank. Re-run the script to refresh the eligible contest roster.

Run `10_chart-nc-google-sheets.R` to create or update the native scatterplot in `NC-Republican-share`: Republican two-party vote shares in North Carolina general elections, with separate House, Senate, and Governor series. House votes are pooled across all districts, including uncontested races; the chart's supporting table contains formulas linked to the candidate-level release tab.
