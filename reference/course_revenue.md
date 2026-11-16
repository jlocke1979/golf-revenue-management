# Revenue categories for the same course period

One row per course, date, and frequency. Categories that the source does
not report stay missing. A missing cart amount is not a measured zero.
`cart_green_fee_treatment` records whether those two amounts are known
to be separate. The default is `"unknown"`, so a later ratio does not
assume a bundled cart fee has already been removed from green fees.

## Usage

``` r
course_revenue(
  course_id,
  date,
  frequency,
  green_fee = NA_real_,
  food_beverage = NA_real_,
  range = NA_real_,
  merchandise = NA_real_,
  cart = NA_real_,
  club = NA_real_,
  currency = "unknown",
  tax_treatment = "unknown",
  refund_treatment = "unknown",
  service_or_posting = "unknown",
  cart_green_fee_treatment = "unknown"
)
```

## Arguments

- course_id, date, frequency:

  Identifiers. `frequency` cannot mix `day` and `month`.

- green_fee, food_beverage, range, merchandise, cart, club:

  Revenue amounts.

- currency, tax_treatment, refund_treatment, service_or_posting:

  Source definitions. Unknown stays `"unknown"`.

- cart_green_fee_treatment:

  `"separated"`, `"bundled"`, `"published_as_separate_categories"`, or
  `"unknown"`.

## Value

A `golfops_course_revenue` tibble.
