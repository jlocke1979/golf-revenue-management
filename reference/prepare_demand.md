# Prepare a demand series for a shared forecast workflow

The outcome is the observed count. Rows flagged as closures stay in the
table so they can be scored, and they are left out of model fitting.
Capacity, when supplied, is stored for documentation. It is not used to
invent unobserved demand.

## Usage

``` r
prepare_demand(
  data,
  date,
  outcome,
  closure = NULL,
  capacity = NULL,
  keep = character(),
  series = "demand",
  synthetic = FALSE,
  decisions = character()
)
```

## Arguments

- data:

  A data frame.

- date, outcome:

  Column names for the date and the observed outcome.

- closure:

  Optional column name. TRUE marks a row to keep out of the fit.

- capacity:

  Optional column name for a capacity estimate.

- keep:

  Extra column names to retain.

- series:

  Short name for this application.

- synthetic:

  TRUE when the rows were generated rather than observed.

- decisions:

  Character vector of data decisions to store on the result.

## Value

A `golfops_demand` tibble.
