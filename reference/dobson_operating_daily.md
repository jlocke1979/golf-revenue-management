# Daily Dobson Ranch rows used for operating charts and forecasts

This is the shared analytical sample. It keeps daily rows from
2021-07-01 through the last date in the extract, drops the identical
extra copy of 2022-04-24, and leaves 2024-12-10 missing. The isolated
2004-12-10 row stays in
[`prepare_dobson_rounds()`](https://jlocke1979.github.io/golf-revenue-management/reference/prepare_dobson_rounds.md)
and in the audit. It is not plotted here. Monthly totals are not
included.

## Usage

``` r
dobson_operating_daily(rounds = NULL)
```

## Arguments

- rounds:

  Optional result of
  [`prepare_dobson_rounds()`](https://jlocke1979.github.io/golf-revenue-management/reference/prepare_dobson_rounds.md).

## Value

A `golfops_rounds` table with capacity columns from
[`apply_operating_capacity()`](https://jlocke1979.github.io/golf-revenue-management/reference/apply_operating_capacity.md).

## Details

Zero-round days remain in the sample. Their status is unknown unless an
operating calendar says otherwise. Nothing in this function invents a
closure, a tee sheet, or the missing December day.

## Examples

``` r
sample <- dobson_operating_daily()
range(sample$date)
#> [1] "2021-07-01" "2026-04-30"
```
