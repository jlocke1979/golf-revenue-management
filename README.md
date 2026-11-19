# Golf participation, utilization, and revenue

`golfops` 0.4.0 summarizes unused capacity and revenue for Dobson Ranch from the City of Mesa daily operating extract. It is for a golf course director, an owner or operator, or a revenue analyst counseling the course. A later check can use a tee sheet once start times and players per group are available. Operating charts share one daily sample. The package prepares the file, shows utilization of the daylight estimate, summarizes revenue relationships, runs pricing scenarios whose volume changes are typed in by the user, and compares a seasonal baseline with one interpretable forecast of observed daily rounds.

The extract has no tee time, posted rate, or booking timestamp. Scenarios are accounting illustrations. They are not price elasticities and not recommended tee-time prices. Forecasts predict observed rounds. They do not estimate latent demand on days at or above the daylight capacity.

## Why this dataset

Tee-sheet files from course operators were not available for this version. Dobson Ranch is public, has a published data dictionary, and is usable for participation and revenue once the grain of the file is respected.

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
- Zero-round days still show facility revenue and cluster in October. Those October days are course closures. The extract does not label them closed. The capacity vignette marks them closed and leaves them out of the utilization ranking. Per-played-round yield is undefined on those days.

2022, 2023, and 2025 are complete calendar years in the daily series. 2024 is missing one day. 2021 and 2026 are partial.

## What this version delivers from the proposal

| Proposal function | This package | Status |
| --- | --- | --- |
| `prepare_golf_data()` | `prepare_dobson_rounds()` | Delivered for the Mesa CSV |
| `summarize_utilization()` | `summarize_golf_rounds()`, `plot_utilization_heatmap()` | Delivered at day, week, weekday, month, season, and year. Capacity is labeled as the daylight estimate unless an operating calendar supplies a better basis |
| `plot_tee_patterns()` | `plot_participation()`, `plot_daylight_share()`, `plot_revenue()`, `plot_seasonal_overlay()`, `plot_utilization_heatmap()` | Delivered for weekday, week, month-of-year, and a daily utilization calendar. The daylight-share charts are box plots of each open day. Time of day is an empty panel until tee times exist. Not a tee-time heatmap |
| `simulate_pricing()` | `simulate_price_volume()` | Delivered with user-supplied demand assumptions |
| `compare_course_rates()`, `compare_pass_value()` |  | Not in this version. No rate card or pass file is in the extract |
| `estimate_event_tradeoff()` | `tournament_breakeven()` | A full-day or user-supplied partial-day baseline. Contribution is omitted until costs and margins are supplied. Not an estimated tradeoff and not a quote |

`audit_golf_rounds()` and `metric_definitions()` support the four delivered functions. They record grain, duplicates, and what each revenue metric is not.

The forecast workflow is shared. `forecast_dobson_rounds()` fits observed rounds on weekday and month. `forecast_tournament_tickets()` fits a separate model for a fictional spectator-ticket series, on weekday and days until the event. Both use `compare_demand_forecasts()`, a seasonal baseline, and a chronological holdout. The ticket series is generated and labeled synthetic. It is not the course-tournament break-even calculator. That calculator, `tournament_breakeven()`, compares supplied event revenue with observed regular-play green fees and does not quote a price.

`revenue_relationships()` reports category revenue per dollar of green fees as a ratio of sums. `course_period()` and `course_revenue()` are the small tables an importer should produce. foreUP, Lightspeed Golf, and GolfNow are named in `future_operator_exports()` as possible authorized exports. No importer is claimed for them.

Period yields are ratios of sums. They are not averages of daily yields. `simulate_price_volume()` rescales realized green-fee yield and rounds. Ancillary revenue, total minus green fees, can be held constant or scaled with rounds. Neither choice is an estimated demand response. The assumptions are part of the returned object.

## Worked example

The vignette `dobson-ranch` loads the bundled extract and uses the shared daily sample for the capacity charts and the weekday, month, and season utilization tables. October closures are left out of those tables. The same sample is used for seasonal overlays, a utilization heatmap, revenue ratios, the 2025 scenarios, and an illustrative tournament baseline. One scenario raises realized green-fee yield 10 percent and lowers rounds 5 percent. That 5 percent is an assumption, not a result.

```r
library(golfops)
rounds <- prepare_dobson_rounds()
audit_golf_rounds(rounds)

daily <- rounds[rounds$grain == "day", ]
summarize_golf_rounds(daily, by = "weekday", duplicates = "drop_extra")
```

A tee sheet is a separate check. `course_capacity()` counts starts in a window the caller supplies. There is no default interval. `daily_utilization()` compares groups on that sheet with those starts. The bundled `sample_tee_sheet` is synthetic. Dobson Ranch has no tee time, so the capacity vignette does not use these two functions for the daylight ranking.

```r
capacity <- course_capacity(
  date = as.Date("2024-06-01"),
  window_start = "07:00",
  window_end = "07:40",
  tee_interval_minutes = 10,
  players_per_start = 4
)
bookings <- data.frame(
  date = as.Date(c("2024-06-01", "2024-06-01", "2024-06-01")),
  tee_time = c("07:00", "07:00", "07:10"),
  players = c(4, 0, 1)
)
daily_utilization(bookings, capacity)
```

From this project directory, with the package dependencies installed. RStudio supplies pandoc for the vignette. In a plain terminal, set `RSTUDIO_PANDOC` to the directory that contains RStudio's pandoc before building vignettes.

```r
devtools::load_all()
devtools::test()
vignette("dobson-ranch", package = "golfops")
```

The raw export lives in `sources/` and is read-only. The package reads `inst/extdata/dobson_ranch_golf_rounds.csv`, a byte copy of that CSV.

## Weather and a synthetic price association

`prepare_dobson_weather()` reads a NOAA GHCND extract retrieved on 2026-11-19. Temperature and wind are Phoenix Sky Harbor, 13.91 km from the course centroid. Precipitation is the Mesa 4.3 SW gauge, 0.81 km away, with Sky Harbor precipitation kept in a separate column. The vignette reads the bundled file. It does not download weather.

`playable_capacity()` keeps the supplied theoretical capacity. A confirmed closure, a stated unavailable fraction, or stated unavailable hours can reduce it. Hours are proportional to daylight hours and are not inferred from a daily weather total. A precipitation or wind threshold is a separate scenario and is labeled as an assumption. High temperature does not reduce capacity.

`estimate_price_elasticity()` fits log rounds on log price. The standard errors are ordinary least squares from `lm()`. They are not the course-clustered standard errors in Enz and Canina (2016). `simulate_elasticity_sample()` builds four synthetic samples, including two customer-mix cases. `simulate_price_volume()` is unchanged and still applies a volume change supplied by the user. The forecast formula is unchanged and still refuses observed weather when the role is a forecast.

Dobson Ranch still has no posted price. The elasticity vignette does not regress rounds on realized green-fee yield.

Enz, Cathy A., and Linda Canina. 2016. "Competitive Pricing in the Golf Industry." Cornell University, School of Hotel Administration. https://hdl.handle.net/1813/72428. Journal version: *Journal of Revenue and Pricing Management* 16 (2): 139–153. https://doi.org/10.1057/s41272-016-0075-8.

## Outside this version

Country-club equity, cart GPS, and a live scoring app are outside this version. A tee sheet would be required before a posted-price test.

City of Mesa. "Dobson Ranch Golf Rounds." Mesa Open Data, dataset 24g5-hb8k. Accessed October 3, 2026. https://data.mesaaz.gov/Parks-Recreation-and-Community-Facilities/Dobson-Ranch-Golf-Rounds/24g5-hb8k.
