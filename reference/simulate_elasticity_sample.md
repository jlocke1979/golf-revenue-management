# Synthetic samples with a known price elasticity

Builds one of four daily samples. The first two are a single series. The
segment samples have one resident row and one visitor row per date.
[`aggregate_segment_rounds()`](https://jlocke1979.github.io/golf-revenue-management/reference/aggregate_segment_rounds.md)
turns those rows into a daily total and a round-weighted average price.

## Usage

``` r
simulate_elasticity_sample(
  experiment = c("correct_specification", "endogenous_price", "segment_mix",
    "segment_mix_price_variation"),
  n_days = 400,
  elasticity = -0.8,
  seed = 457
)
```

## Arguments

- experiment:

  One of `correct_specification`, `endogenous_price`, `segment_mix`, or
  `segment_mix_price_variation`.

- n_days:

  Number of dates.

- elasticity:

  Structural price coefficient. Ignored when `experiment` is
  `segment_mix`, because those prices do not vary and the rounds are not
  generated from a price term.

- seed:

  Integer seed. The generator restores the previous random seed when it
  finishes.

## Value

A `golfops_elasticity_sample` tibble. Attributes `description` and `dgp`
state the experiment and the generating equation.

## Details

`correct_specification` draws price independently of the demand shock
and generates log rounds from `elasticity`, weekend, temperature, and
precipitation. `endogenous_price` raises price when anticipated demand
is higher. Part of that demand is the weekend and the temperature, and
part is not returned in the table. `segment_mix` keeps each segment
price constant and shifts the visitor share by season. The `elasticity`
argument is not used to generate those rounds.
`segment_mix_price_variation` gives both segments the same negative
elasticity and modest price noise that is independent of the demand
shock, while the visitor share still shifts.

These rows are synthetic. They are not Dobson Ranch, and they are not
the Enz and Canina transaction file.

## Examples

``` r
simulate_elasticity_sample("correct_specification", n_days = 40, seed = 457)
#> Price is drawn independently of the demand shock. Log rounds use the stated elasticity plus weekend, temperature, and precipitation.
#> 
#> # A tibble: 40 × 9
#>    date       rounds price weekend temp_min_c precip_mm  year true_elasticity
#>    <date>      <dbl> <dbl>   <int>      <dbl>     <dbl> <int>           <dbl>
#>  1 2022-01-01   27.6  30.5       1       3.35      0     2022            -0.8
#>  2 2022-01-02   24.5  36.6       1       2.65      0     2022            -0.8
#>  3 2022-01-03   23.3  23.5       0       5.99      0     2022            -0.8
#>  4 2022-01-04   23.7  25.5       0       3.78      0     2022            -0.8
#>  5 2022-01-05   27.3  20.5       0       3.09      0     2022            -0.8
#>  6 2022-01-06   22.3  23.3       0       5.33      0     2022            -0.8
#>  7 2022-01-07   31.9  18.7       0       6.03      0     2022            -0.8
#>  8 2022-01-08   20.1  38.8       1       2.79      2.43  2022            -0.8
#>  9 2022-01-09   22.9  38.6       1       5.93      0     2022            -0.8
#> 10 2022-01-10   19.6  28.0       0       3.96      0     2022            -0.8
#> # ℹ 30 more rows
#> # ℹ 1 more variable: experiment <chr>
```
