# Synthetic ticket sales for a hypothetical golf tournament

Builds ninety advance-sale days for each of three fictional editions of
the Cedar Municipal Classic. Counts rise toward the event and are higher
on weekends, plus noise. This is not a PGA Tour series and not Dobson
Ranch.

## Usage

``` r
synthetic_tournament_tickets(seed = 457)
```

## Arguments

- seed:

  Random seed used before the noise is drawn.

## Value

A `golfops_demand` table. The `days_to_event` column is retained for the
ticket model.
