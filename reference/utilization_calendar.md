# Calendar of daily utilization, including dates the extract skipped

Builds every date from the first through the last row. A date inside
that span with no row is `missing`. It is not given zero rounds.
Zero-round days already in the extract keep their operating status.
Utilization above 1 is retained.

## Usage

``` r
utilization_calendar(data, calendar = NULL)
```

## Arguments

- data:

  Daily `golfops_rounds` rows, usually
  [`dobson_operating_daily()`](https://jlocke1979.github.io/golf-revenue-management/reference/dobson_operating_daily.md).

- calendar:

  Optional
  [`operating_calendar()`](https://jlocke1979.github.io/golf-revenue-management/reference/operating_calendar.md).

## Value

A tibble with one row per calendar date in the sample span.
