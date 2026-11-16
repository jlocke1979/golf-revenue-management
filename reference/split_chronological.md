# Split a demand series on a date, without shuffling

Training rows are strictly before `holdout_start`. Later rows are the
test.

## Usage

``` r
split_chronological(data, holdout_start)
```

## Arguments

- data:

  A `golfops_demand` table.

- holdout_start:

  First date of the test period.

## Value

A list with `train` and `test`.
