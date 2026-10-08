# Golf tee time pricing and utilization

The Module 1 proposal asks: when does a course have unused capacity, and what pricing or promotional adjustments might be worth testing?

`golfops` 0.3.0 answers that question for Dobson Ranch with the City of Mesa daily operating extract. Operating charts share one daily sample. The package prepares the file, shows utilization of the daylight estimate, summarizes revenue relationships, runs pricing scenarios whose volume changes are typed in by the user, and compares a seasonal baseline with one interpretable forecast of observed daily rounds.

The extract has no tee time, posted rate, or booking timestamp. Scenarios are accounting illustrations. They are not price elasticities and not recommended tee-time prices. Forecasts predict observed rounds. They do not estimate latent demand on days at or above the daylight capacity.

## Course stages

The syllabus grades the final project in three stages. They are not one deadline.

| Stage | What it asks for | This package |
| --- | --- | --- |
| Module 1 proposal | The written proposal | Already the proposal document. The package does not replace it. |
| Module 5 checkpoint | A checkpoint whose rubric is not in the local course files | Not claimed. Do not treat the proposal date as this checkpoint. |
| Module 10 submission | An R package with documentation | In progress: functions, tests, example data, and two vignettes. No website or CRAN release. |

`course_requirements()` returns that map from inside R. Module 6 is a separate documentation assignment, not the final submission.

## Why this dataset

The course proposal started from municipal tee-sheet operations. Tee-sheet files from course operators were not available for this version. Dobson Ranch is public, has a published data dictionary, and is usable for participation and revenue once the grain of the file is respected.

Catalog: [Dobson Ranch Golf Rounds](https://data.mesaaz.gov/Parks-Recreation-and-Community-Facilities/Dobson-Ranch-Golf-Rounds/24g5-hb8k) (`24g5-hb8k`). The catalog says the course is operated by Paradigm Golf Group. On October 3, 2026 the SODA endpoint returned:

- 1,838 rows and 23 fields, matching the catalog and this extract
- dates from 2004-12-10 through 2026-04-30
- 868,646 rounds played, matching this extract
- a last update timestamp of October 2, 2026

The 2026-04-30 API total is \$55,134.93. The CSV total is \$55,135. Component sums in the CSV stay within \$0.50 of the printed total, which fits dollar rounding. The package keeps the CSV as the example file and does not silently replace it with the API.

## What the validation changes

`audit_golf_rounds()` is the check to read before any chart. The findings that control the analysis:

- 1,766 rows are daily. 72 rows, July 2015 through June 2021, are monthly totals dated the 1st. The largest daily total is 675 rounds and the smallest monthly total is 2,963, so a cutoff of 1,000 separates them in this file. Do not add the two grains together.
- The continuous daily series runs from 2021-07-01 through 2026-04-30 and is missing 2024-12-10. 2004-12-10 is a separate one-day row and stays in the audit. Operating charts use `dobson_operating_daily()`, which omits that isolated day. 2022-04-24 is duplicated exactly, and one copy is kept.
- The city's `Revenue/Round` equals total revenue divided by rounds played. It is blank when rounds are zero. It is facility revenue per played round, not a green fee.
- `Round Booking Rate` equals rounds played divided by `Rounds Possible`. The catalog defines `Rounds Possible` from daylight, not from the tee sheet. Utilization can exceed 1.
- Cart revenue is blank on early rows. Where it is blank and the other five categories are present, those five already equal total revenue. A blank is not a hidden cart amount, and it should not be read as a measured zero unless the source says zero.
- Zero-round days still show facility revenue and cluster in October. Per-played-round yield is undefined on those days. The file does not give the reason play stopped.

2022, 2023, and 2025 are complete calendar years in the daily series. 2024 is missing one day. 2021 and 2026 are partial.

## What this version delivers from the proposal

| Proposal function | This package | Status |
| --- | --- | --- |
| `prepare_golf_data()` | `prepare_dobson_rounds()` | Delivered for the Mesa CSV |
| `summarize_utilization()` | `summarize_golf_rounds()`, `plot_utilization_heatmap()` | Delivered at day, week, weekday, month, season, and year. Capacity is labeled as the daylight estimate unless an operating calendar supplies a better basis |
| `plot_tee_patterns()` | `plot_participation()`, `plot_revenue()`, `plot_seasonal_overlay()`, `plot_utilization_heatmap()` | Delivered for weekday, week, month-of-year, and a daily utilization calendar. Not a tee-time heatmap |
| `simulate_pricing()` | `simulate_price_volume()` | Delivered with user-supplied demand assumptions |
| `compare_course_rates()`, `compare_pass_value()` |  | Not in this version. No rate card or pass file is in the extract |
| `estimate_event_tradeoff()` | `tournament_breakeven()` | A full-day or user-supplied partial-day baseline. Contribution is omitted until costs and margins are supplied. Not an estimated tradeoff and not a quote |

`audit_golf_rounds()` and `metric_definitions()` support the four delivered functions. They record grain, duplicates, and what each revenue metric is not.

The forecast workflow is shared. `forecast_dobson_rounds()` fits observed rounds on weekday and month. `forecast_tournament_tickets()` fits a separate model for a fictional spectator-ticket series, on weekday and days until the event. Both use `compare_demand_forecasts()`, a seasonal baseline, and a chronological holdout. The ticket series is generated and labeled synthetic. It is not the course-tournament break-even calculator. That calculator, `tournament_breakeven()`, compares supplied event revenue with observed regular-play green fees and does not quote a price.

`revenue_relationships()` reports category revenue per dollar of green fees as a ratio of sums. `course_period()` and `course_revenue()` are the small tables an importer should produce. foreUP, Lightspeed Golf, and GolfNow are named in `future_operator_exports()` as possible authorized exports. No importer is claimed for them.

Period yields are ratios of sums. They are not averages of daily yields. `simulate_price_volume()` rescales realized green-fee yield and rounds. Ancillary revenue, total minus green fees, can be held constant or scaled with rounds. Neither choice is an estimated demand response. The assumptions are part of the returned object.

## Worked example

The vignette `dobson-ranch` loads the bundled extract, prints the audit, and then uses the shared daily sample for seasonal overlays, a utilization heatmap, revenue ratios, the 2025 scenarios, and an illustrative tournament baseline. One scenario raises realized green-fee yield 10 percent and lowers rounds 5 percent. That 5 percent is an assumption, not a result.

```r
library(golfops)
rounds <- prepare_dobson_rounds()
audit_golf_rounds(rounds)

daily <- rounds[rounds$grain == "day", ]
summarize_golf_rounds(daily, by = "weekday", duplicates = "drop_extra")
```

From this project directory, with the package dependencies installed. RStudio supplies pandoc for the vignette. In a plain terminal, set `RSTUDIO_PANDOC` to the directory that contains RStudio's pandoc before building vignettes.

```r
devtools::load_all()
devtools::test()
vignette("dobson-ranch", package = "golfops")
```

The raw export lives in `sources/` and is read-only. The package reads `inst/extdata/dobson_ranch_golf_rounds.csv`, a byte copy of that CSV.

## Outside this deadline

Country-club equity, cart GPS, and a live scoring app are outside the proposal's current scope. A tee sheet would be required before a posted-price test. Peoria's pass comparison remains the fallback named in the proposal if a second course file is added later.

## Course note

This package is the MSDS 457 project toolkit. The syllabus asks for a documented R package built around a sports-business question. Development of the code was assisted by Cursor's coding agent. The data checks, metric definitions, and scenario assumptions were reviewed against the city catalog and this extract.

City of Mesa. "Dobson Ranch Golf Rounds." Mesa Open Data, dataset 24g5-hb8k. Accessed October 3, 2026. https://data.mesaaz.gov/Parks-Recreation-and-Community-Facilities/Dobson-Ranch-Golf-Rounds/24g5-hb8k.
