# Reading a Clerk of the House election-statistics volume

You are transcribing candidate lines from page images of the Clerk of the House
*Statistics of the Congressional Election* for one election year. You work on an
assigned range of pages and write one TSV file. You only transcribe: party codes
and standardized names come from the CAGE roster later, not from you.

## Ground rules

- Read the page images yourself. Do not run text-extraction tools on the PDF
  (`pdftotext`, `pdfinfo`, `pypdf`, OCR libraries) and do not look anything up
  online. `pdftoppm` is allowed for re-rendering a page at higher resolution.
- Use only the files named in your task. Put any helper files in your own folder
  `work/<first page of your range>/`; other readers share the working directory.
- **Never write a vote figure you did not read on the page.** Do not compute a
  figure by subtraction, carry one over from elsewhere, or fill one from memory.
  If a figure is unreadable, leave it blank and say so in `flags`. A blank is
  visible; a wrong number is not.
- Copy names exactly as printed. Do not expand, correct or reorder them.

## How the volume is laid out

```
STATE                                       (bold, centred; "STATE—Continued" on later pages)
  FOR PRESIDENTIAL ELECTORS                 (skip: parties, not people)
  FOR UNITED STATES SENATOR
    (For unexpired term ending January 3, YYYY)   <- special election rows follow
    Name, Party .................... 123,456
    (For full term beginning January 3, YYYY)     <- regular seat rows follow
  FOR UNITED STATES REPRESENTATIVE
    AT LARGE                                (single-district state: district 1)
 1. Name, Party .................... 123,456   <- district number left of the first candidate
    Name, Party .................... 98,765
          Party .................... 4,321    <- indented, no name: cross-endorsement of the line above
    Write-in ....................... 12       <- ballot line that is not a person
  FOR DELEGATE / FOR RESIDENT COMMISSIONER  (district 0)
  Recapitulation of Votes Cast in STATE     (a table: skip, do not transcribe)
```

`page` is always the PDF page number (the cover is page 1), not the number
printed at the top of the page. The back of the volume holds national summary
tables (presidential vote, Senate and House recapitulations, party divisions):
those pages produce no rows.

## Reading traps

1. **Footnote markers are not digits.** A small raised number next to a name or
   a figure is a footnote marker. `¹1,234,567` is 1234567 with footnote 1. Read
   the footnote text and put it in `footnote`.
2. **A footnote or the ballot lines can change what the figure is.** Some
   footnotes say a figure is from a runoff or a ranked-choice round, and
   ranked-choice races may print "Continuing Ballots" or "Exhausted Ballots"
   lines. Add `flags: figure is not the November first round` to every line of
   such a race.
3. **`(1)` in the vote column is not a count.** Unopposed candidates often show a
   footnote reference instead of a number. Leave the figure blank and record the
   footnote.
4. **Indentation decides cross-endorsements.** An indented line with only a party
   name belongs to the candidate above it. A line with a name is a separate
   candidate. Several parties on one line with a single figure
   ("Name, Republican, Libertarian") is one line.
5. **Continued pages.** Use the context pages to know which state, office and
   district you are in when a page starts mid-race.
6. **The two image halves overlap by a few lines.** Do not output a row twice.
7. **Lines that are not people:** Write-in (without a name), Scattering,
   Blank(s), Void, Over/Under Votes, Miscellaneous, None of These Candidates,
   Continuing or Exhausted Ballots, and the printed Total line are
   `kind = nonvote`. Give each its own row with its figure in `line_votes` and
   `candidatevotes`.
8. **A party label with no name on its own line** (not indented under a
   candidate, e.g. just "Green .... 12,345") is `kind = unnamed`, with a flag.
9. **Check each race against the recapitulation table.** Add the race's lines and
   compare with the state's table. If they disagree, re-read at higher
   resolution; if they still disagree, keep what the listing shows and flag it.
   Never take a figure from the table.

## Output

One tab-separated file, UTF-8, with this header and one row per printed ballot
line in your pages. Leave a cell empty when it does not apply. No tabs or line
breaks inside cells; write every column even when the trailing ones are empty.

| column | content |
|---|---|
| `year` | election year |
| `page` | PDF page number where the line is printed |
| `state` | two-letter postal code |
| `office` | `S` or `H` (House, delegate, resident commissioner); skip presidential electors |
| `dist` | House district number; at large = 1; delegate or resident commissioner = 0; empty for Senate |
| `special` | 1 under an "(For unexpired term ending …)" caption, else 0 |
| `kind` | `candidate` (named person, not a write-in); `writein` (named person whose label starts with "Write-in"); `unnamed`; `nonvote` |
| `name_printed` | the name exactly as printed (curly quotes and accents included), without the party; empty for `unnamed` and `nonvote` |
| `party_lines` | each printed line's party label for this candidate, own line first, joined with ` ; `; for `unnamed` and `nonvote`, the line's label |
| `line_votes` | the figure on each of those lines, digits only, same order, joined with ` ; ` |
| `candidatevotes` | sum of `line_votes`; empty if any figure is blank |
| `footnote` | footnote text that applies to this line |
| `flags` | anything uncertain: unreadable figures, recap disagreements, runoff or ranked-choice notes |

Rows appear in printed order. A cross-endorsement line is never its own row.

Before finishing, re-check every figure against the image once more, and confirm
every line has a figure or a flag and that no line appears twice.
