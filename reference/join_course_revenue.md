# Attach revenue to participation without adding a month to a day

The join keys are course, date, and frequency. A repeated key is an
error. A monthly revenue row does not attach to a daily participation
row on the same calendar date.

## Usage

``` r
join_course_revenue(periods, revenue)
```

## Arguments

- periods:

  A
  [`course_period()`](https://jlocke1979.github.io/golf-revenue-management/reference/course_period.md)
  table.

- revenue:

  A
  [`course_revenue()`](https://jlocke1979.github.io/golf-revenue-management/reference/course_revenue.md)
  table.

## Value

The participation rows with revenue columns added. Unmatched revenue is
not silently summed into another period.
