test_that("the bundled extract matches the checks recorded for this file", {
  audit <- audit_golf_rounds(prepare_dobson_rounds())
  checks <- audit$checks
  detail <- function(name) checks$detail[checks$check == name]
  status <- function(name) checks$status[checks$check == name]

  expect_match(detail("rows_and_dates"), "1,838 rows")
  expect_match(detail("rows_and_dates"), "868,646")
  expect_equal(status("grain_separation"), "pass")
  expect_match(detail("grain_separation"), "1,766 daily rows")
  expect_match(detail("grain_separation"), "72 monthly rows")
  expect_match(detail("daily_coverage"), "2024-12-10")
  expect_match(detail("daily_coverage"), "2004-12-10")
  expect_equal(status("duplicate_dates"), "watch")
  expect_match(detail("duplicate_dates"), "2022-04-24")
  expect_equal(status("component_sum"), "pass")
  expect_equal(status("revenue_per_round_source"), "pass")
  expect_equal(status("booking_rate"), "pass")
  expect_equal(status("calendar_labels"), "pass")
  expect_match(detail("zero_round_rows"), "October")
  expect_match(detail("cart_revenue_coverage"), "already equal")
})
