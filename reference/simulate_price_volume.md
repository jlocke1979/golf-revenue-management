# Apply a user-specified price and volume scenario

This rescales observed green-fee yield and observed rounds. It does not
estimate how many additional rounds a price change would produce. Set
`price_change = 0.10` to mean a 10 percent higher realized green-fee
yield, not a 10 percent higher posted rack rate. Set `volume_change`
yourself.

## Usage

``` r
simulate_price_volume(
  data,
  price_change = 0,
  volume_change = 0,
  ancillary = c("hold_constant", "scale_with_rounds"),
  respect_daylight_capacity = TRUE
)
```

## Arguments

- data:

  Daily rows from
  [`prepare_dobson_rounds()`](https://jlocke1979.github.io/golf-revenue-management/reference/prepare_dobson_rounds.md).
  Monthly rows are rejected.

- price_change:

  Fractional change in realized green-fee yield. Must be greater than
  -1.

- volume_change:

  Fractional change in played rounds. Must be greater than -1.

- ancillary:

  How to treat revenue other than green fees.

- respect_daylight_capacity:

  Whether to cap scenario rounds at `rounds_possible` when observed play
  did not already exceed it.

## Value

A `golfops_scenario` list with `totals`, `daily`, and `assumptions`.

## Details

Ancillary revenue is total revenue minus green fees. `hold_constant`
leaves that remainder at its observed dollar amount. `scale_with_rounds`
multiplies it by the same ratio applied to rounds after any daylight
cap.

The daylight cap uses `rounds_possible`. It is applied only when the
scenario rounds exceed that estimate and the observed rounds did not
already exceed it. Days that already exceed the estimate are left
uncapped, because forcing them down would change the baseline. Days with
zero played rounds are not rescaled, because yield is undefined.

## Examples

``` r
rounds <- prepare_dobson_rounds()
y2025 <- rounds[rounds$calendar_year == 2025 & rounds$grain == "day", ]
simulate_price_volume(y2025, price_change = 0.10, volume_change = -0.05)
#> Price and volume scenario
#> The totals apply the assumptions below. They are not an estimated demand response.
#> 
#> - price_change rescales realized green-fee revenue per played round. It is not a change in a posted rack rate.
#> - volume_change is an assumption supplied for this scenario. It is not estimated from these rows.
#> - Ancillary revenue, defined as total revenue minus green fees, stays at the observed dollar amount.
#> - Scenario rounds are capped at rounds possible only when observed rounds did not already exceed that daylight estimate.
#> - Rows with zero played rounds keep their observed revenue. Yield is undefined when the denominator is zero.
#> - This scenario is not an estimate of price elasticity and not an optimal tee-time price.
#> 
#> # A tibble: 1 × 19
#>   price_change volume_change ancillary     respect_daylight_capacity n_days
#>          <dbl>         <dbl> <chr>         <lgl>                      <int>
#> 1          0.1         -0.05 hold_constant TRUE                         365
#> # ℹ 14 more variables: n_days_capped <int>, n_zero_round_days <int>,
#> #   n_days_already_above_daylight <int>, baseline_rounds <dbl>,
#> #   scenario_rounds <dbl>, baseline_green_fee_revenue <dbl>,
#> #   scenario_green_fee_revenue <dbl>, baseline_ancillary_revenue <dbl>,
#> #   scenario_ancillary_revenue <dbl>, baseline_total_revenue <dbl>,
#> #   scenario_total_revenue <dbl>, change_total_revenue <dbl>,
#> #   green_fee_yield_baseline <dbl>, green_fee_yield_scenario <dbl>
```
