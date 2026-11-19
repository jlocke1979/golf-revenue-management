# Estimate a log-log price association

Fits `log(rounds)` on `log(price)` with optional linear controls and
optional group fixed effects. The price coefficient is the percent
change in rounds associated with a 1 percent higher price in the rows
that remain, under this specification.

## Usage

``` r
estimate_price_elasticity(
  data,
  rounds,
  price,
  controls = NULL,
  group = NULL,
  min_n = 30,
  conf_level = 0.95,
  exclusion_warn = 0.1
)
```

## Arguments

- data:

  A table.

- rounds, price:

  Column names. Both must be positive in a row that enters the fit.

- controls:

  Optional column names entered as linear terms. Numeric or logical
  columns only. Pass categories through `group`.

- group:

  Optional column names entered as fixed effects.

- min_n:

  Minimum number of rows after exclusions.

- conf_level:

  Confidence level for the ordinary least-squares interval.

- exclusion_warn:

  Warn when the unique excluded share of the supplied rows is at least
  this fraction. The default is 10 percent.

## Value

A `golfops_elasticity` list. `model` is the
[`stats::lm()`](https://rdrr.io/r/stats/lm.html) fit. `limitations`
states the deviations from the Enz and Canina specification and the
inferential limits of this fit.

## Details

The standard error and confidence interval come from
[`stats::lm()`](https://rdrr.io/r/stats/lm.html). They are ordinary
least-squares statistics. Enz and Canina clustered standard errors by
course. These are not clustered estimates. Passing a course identifier
to `group` adds a fixed effect. It does not cluster the standard errors.

Their working-paper equation, page 8, regresses log rounds on log
average daily price, course age, rating, slope, number of holes,
regular-tee par, water features, lodging, minimum temperature,
precipitation, location indicators, and a year fixed effect. Temperature
in that equation is degrees Fahrenheit and precipitation is inches. This
function adds a control or a group only when you name the column. It
does not convert units. A numeric control enters as a straight line. A
column named in `group` enters as a set of fixed effects.

The logarithm is defined for positive numbers. Rows with a missing,
zero, or negative rounds or price value are excluded, as are rows with a
missing control or group value. `n_excluded` counts each excluded row
once. `exclusions` counts each reason, so one row can appear in more
than one reason. A warning is issued when the unique excluded share
reaches `exclusion_warn`. The published transaction file is not public,
and this function does not re-estimate the published coefficients. A
coefficient is not a causal demand elasticity and not a recommendation
to change a posted price.

## References

Enz, Cathy A., and Linda Canina. 2016. "Competitive Pricing in the Golf
Industry." Cornell University, School of Hotel Administration.
<https://hdl.handle.net/1813/72428>. The log-log specification and
course-clustered standard errors are on page 8 of that working paper.
Journal version: *Journal of Revenue and Pricing Management* 16 (2):
139–153.
[doi:10.1057/s41272-016-0075-8](https://doi.org/10.1057/s41272-016-0075-8)
.

## Examples

``` r
sample <- simulate_elasticity_sample(
  "correct_specification",
  n_days = 80,
  seed = 457
)
estimate_price_elasticity(
  sample,
  rounds = "rounds",
  price = "price",
  controls = c("weekend", "temp_min_c", "precip_mm")
)
#> Log-log price association
#> log(rounds) ~ log(price) + weekend + temp_min_c + precip_mm
#> Elasticity -0.801; OLS standard error 0.046; 95% interval [-0.891, -0.710]
#> Rows used 80 of 80 (0.0% excluded).
#> 
#> A coefficient of -0.801 is the ordinary least-squares association between log rounds and log price in the rows that remain. It is the percent change in rounds associated with a 1 percent higher price under this specification. The standard error is not clustered by course. The coefficient is not a causal demand elasticity and not a recommendation to change a posted price.
#> 
#> - This is a methodological analogue of Enz and Canina (2016), working paper page 8. It does not re-estimate their published coefficients. Their transaction data are not public.
#> - The published equation uses log rounds, log average daily price, course characteristics, minimum temperature in degrees Fahrenheit, precipitation in inches, location indicators, and a year fixed effect.
#> - The standard error and confidence interval are ordinary least-squares statistics from lm(). They are not standard errors clustered by course.
#> - A column in group is a fixed effect. It does not cluster the standard errors. Course characteristics, weather, location, and year are included only when those columns are named.
#> - Rows with missing, zero, or negative rounds or prices are excluded because a logarithm is defined for positive numbers. The excluded share can change the sample when the omitted rows are closures or missing prices.
#> - The coefficient is the association in the rows that remain under this specification. It is not a causal price elasticity. It is not a recommendation to change a posted price. A positive coefficient is not evidence that demand rises with price.
#> - Rounds played can be censored at capacity. This fit does not correct for that.
#> - An average of prices paid is not a posted rack rate. A shift in customer mix can move that average and total rounds together.
#> - A synthetic sample can recover a known coefficient when the data are generated from the fitted equation. That check is not a description of Dobson Ranch. Dobson realized green-fee yield is not a posted price.
```
