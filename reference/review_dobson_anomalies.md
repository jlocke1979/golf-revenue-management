# Review the largest Dobson participation and range-revenue days

The review measures two patterns in the operating sample. It does not
assign a cause. A busy day is not labeled a tournament, a holiday, or a
broadcast weekend unless an external event table says so.

## Usage

``` r
review_dobson_anomalies(rounds = NULL, events = NULL)
```

## Arguments

- rounds:

  Optional result of
  [`prepare_dobson_rounds()`](https://jlocke1979.github.io/golf-revenue-management/reference/prepare_dobson_rounds.md).

- events:

  Optional
  [`course_events()`](https://jlocke1979.github.io/golf-revenue-management/reference/course_events.md)
  table. Listed names are reported. They are not used to explain a day
  when the table is empty.

## Value

A tibble with the measured pattern and what the extract does not
establish.
