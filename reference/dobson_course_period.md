# Dobson operating rows in the course-period shape

Currency is USD because the city extract is in dollars. Tax treatment,
refund treatment, 9-hole versus 18-hole counts, and membership versus
daily-fee rounds are not identified in the file, so those fields stay
unknown. Capacity is the daylight estimate.

## Usage

``` r
dobson_course_period(rounds = NULL)
```

## Arguments

- rounds:

  Optional result of
  [`prepare_dobson_rounds()`](https://jlocke1979.github.io/golf-revenue-management/reference/prepare_dobson_rounds.md).

## Value

A `golfops_course_period` table.
