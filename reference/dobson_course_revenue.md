# Dobson revenue categories in the course-revenue shape

The city publishes cart, club, food and beverage, merchandise, range,
and green fees as separate columns, and complete rows sum to total
revenue. That supports `published_as_separate_categories`. It does not
say whether a posted green fee included a cart.

## Usage

``` r
dobson_course_revenue(rounds = NULL)
```

## Arguments

- rounds:

  Optional result of
  [`prepare_dobson_rounds()`](https://jlocke1979.github.io/golf-revenue-management/reference/prepare_dobson_rounds.md).

## Value

A `golfops_course_revenue` table.
