# One row per course and period for participation

The reusable analysis table. Source files are converted into this shape
by an importer. Daily and monthly rows cannot share a table: adding them
would count a month as a day. Unknown definitions stay unknown. This
constructor does not infer tee-sheet capacity, cart bundling, taxes, or
which rounds were membership play.

## Usage

``` r
course_period(
  course_id,
  date,
  frequency,
  rounds,
  capacity = NA_real_,
  capacity_basis = "unknown",
  operating_status = "unknown",
  currency = "unknown",
  tax_treatment = "unknown",
  refund_treatment = "unknown",
  holes = "unknown",
  player_type = "unknown",
  service_or_posting = "unknown"
)
```

## Arguments

- course_id:

  Course identifier.

- date:

  Period date. A daily row is the service date when that is known. A
  monthly row is the period start used by the source.

- frequency:

  `day` or `month`. Every row in one table must match.

- rounds:

  Rounds played. Missing stays missing.

- capacity:

  Optional capacity count in rounds.

- capacity_basis:

  Why `capacity` is that number.

- operating_status:

  Open, closed, unknown, or a status already assigned by
  [`apply_operating_capacity()`](https://jlocke1979.github.io/golf-revenue-management/reference/apply_operating_capacity.md).

- currency:

  ISO currency code, or `"unknown"`.

- tax_treatment:

  How tax sits in the amounts, or `"unknown"`.

- refund_treatment:

  How refunds sit in the amounts, or `"unknown"`.

- holes:

  `"9"`, `"18"`, `"mixed"`, or `"unknown"`.

- player_type:

  `"daily_fee"`, `"membership"`, `"mixed"`, or `"unknown"`.

- service_or_posting:

  `"service_date"`, `"posting_date"`, or `"unknown"`.

## Value

A `golfops_course_period` tibble.
