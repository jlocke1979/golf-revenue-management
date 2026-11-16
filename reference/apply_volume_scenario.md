# Rescale a forecast by an assumed volume change

Rescale a forecast by an assumed volume change

## Usage

``` r
apply_volume_scenario(predicted, volume_change = 0)
```

## Arguments

- predicted:

  Numeric predictions.

- volume_change:

  Fractional change. A value of 0.05 means 5 percent more volume.

## Value

A `golfops_scenario` list whose totals are the assumed counts.
