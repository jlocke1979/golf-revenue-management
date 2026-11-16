# Correlations and relationships

``` r

library(golfops)
sample <- dobson_operating_daily()
played <- sample[!is.na(sample$rounds_played) & sample$rounds_played > 0, ]
categories <- c(
  "Total revenue" = "total_revenue",
  "Green fees" = "green_fee_revenue",
  "Food and beverage" = "food_beverage_revenue",
  "Merchandise" = "merchandise_revenue",
  "Range" = "range_revenue",
  "Cart" = "cart_revenue",
  "Club rental" = "club_revenue"
)
revenue_correlation <- function(block) {
  round(vapply(categories, function(column) {
    stats::cor(block$rounds_played, block[[column]], use = "complete.obs")
  }, numeric(1)), 2)
}
```

Each number is the correlation of golfers with that day’s recorded
revenue, on days with at least one golfer. The charts below are the same
days. Each point is one day. The horizontal axis is golfers, and the
line is a straight fit to that year’s days. These are associations. They
are not evidence that an extra golfer causes the extra spending, and
they are not a price.

|                   | 2021\* | 2022 | 2023 | 2024 | 2025 | 2026\* |
|:------------------|-------:|-----:|-----:|-----:|-----:|-------:|
| Total revenue     |   0.59 | 0.66 | 0.50 | 0.51 | 0.20 |  -0.05 |
| Green fees        |   0.66 | 0.49 | 0.36 | 0.30 | 0.20 |   0.11 |
| Food and beverage |   0.39 | 0.46 | 0.45 | 0.39 | 0.08 |  -0.29 |
| Merchandise       |   0.25 | 0.14 | 0.01 | 0.22 | 0.08 |  -0.25 |
| Range             |   0.36 | 0.20 | 0.30 | 0.40 | 0.09 |   0.02 |
| Cart              |   0.85 | 0.83 | 0.83 | 0.79 | 0.40 |   0.59 |
| Club rental       |   0.32 | 0.29 | 0.25 | 0.20 | 0.01 |   0.12 |

Correlation of golfers with each daily revenue amount. \* 2021 and 2026
are half years: 2021 is July through December, and 2026 is January
through April. {.table}

Each section is one calendar year. A panel uses that revenue type’s
color, the same colors as the annual revenue bars. 2021 starts on July
1, and the latest year ends with the extract.

## 2021

``` r

plot_revenue_by_golfers(sample, year = 2021)
```

![2021 daily revenue against golfers, seven panels, from July
1.](correlations-and-relationships_files/figure-html/year-2021-1.png)

## 2022

``` r

plot_revenue_by_golfers(sample, year = 2022)
```

![2022 daily revenue against golfers, seven
panels.](correlations-and-relationships_files/figure-html/year-2022-1.png)

## 2023

``` r

plot_revenue_by_golfers(sample, year = 2023)
```

![2023 daily revenue against golfers, seven
panels.](correlations-and-relationships_files/figure-html/year-2023-1.png)

## 2024

``` r

plot_revenue_by_golfers(sample, year = 2024)
```

![2024 daily revenue against golfers, seven
panels.](correlations-and-relationships_files/figure-html/year-2024-1.png)

## 2025

``` r

plot_revenue_by_golfers(sample, year = 2025)
```

![2025 daily revenue against golfers, seven
panels.](correlations-and-relationships_files/figure-html/year-2025-1.png)

## 2026

``` r

plot_revenue_by_golfers(sample, year = 2026)
```

![2026 daily revenue against golfers, seven panels, through the last day
in the
extract.](correlations-and-relationships_files/figure-html/year-2026-1.png)
