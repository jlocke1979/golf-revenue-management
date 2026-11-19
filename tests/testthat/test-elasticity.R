log_log_sample <- function(n = 200, elasticity = -0.7, seed = 11) {
  set.seed(seed)
  price <- exp(stats::rnorm(n, log(40), 0.25))
  rounds <- exp(4.2 + elasticity * log(price) + stats::rnorm(n, 0, 0.02))
  data.frame(rounds = rounds, price = price)
}

test_that("a correctly generated log-log sample recovers the price coefficient", {
  sample <- log_log_sample()
  fit <- estimate_price_elasticity(sample, "rounds", "price")
  expect_s3_class(fit, "golfops_elasticity")
  expect_s3_class(fit$model, "lm")
  expect_lt(abs(fit$elasticity - (-0.7)), 0.05)
  expect_equal(fit$n_supplied, 200)
  expect_equal(fit$n_used, 200)
  expect_equal(fit$n_excluded, 0)
  expect_equal(fit$pct_excluded, 0)
  expect_lt(fit$conf_low, fit$elasticity)
  expect_gt(fit$conf_high, fit$elasticity)
  expect_equal(
    fit$std_error,
    unname(summary(fit$model)$coefficients["log_price", "Std. Error"])
  )
  expect_match(paste(fit$limitations, collapse = " "), "not standard errors clustered by course")
  expect_match(fit$interpretation, "not a causal demand elasticity")
  expect_match(fit$formula, "log(rounds) ~ log(price)", fixed = TRUE)
})

test_that("missing columns, small samples, and unidentified prices are rejected", {
  sample <- log_log_sample(n = 40)
  expect_error(
    estimate_price_elasticity(sample, "rounds", "rate"),
    "Missing columns",
    class = "golfops_input_error"
  )
  expect_error(
    estimate_price_elasticity(sample, "rounds", "price", min_n = 100),
    "positive",
    class = "golfops_input_error"
  )
  constant <- sample
  constant$price <- 25
  expect_error(
    estimate_price_elasticity(constant, "rounds", "price", min_n = 30),
    "does not vary",
    class = "golfops_input_error"
  )
  grouped <- sample
  grouped$segment <- rep(c("resident", "visitor"), length.out = nrow(grouped))
  grouped$price <- ifelse(grouped$segment == "resident", 20, 60)
  expect_error(
    estimate_price_elasticity(grouped, "rounds", "price", group = "segment"),
    "within the supplied groups",
    class = "golfops_input_error"
  )
  expect_error(
    estimate_price_elasticity(sample, "rounds", "price", controls = "segment"),
    class = "golfops_input_error"
  )
})

test_that("non-positive and missing rows are counted and a large exclusion warns", {
  sample <- log_log_sample(n = 100, seed = 19)
  sample$rounds[1:8] <- 0
  sample$price[9:12] <- -5
  sample$rounds[13:15] <- NA_real_
  sample$extra <- 1
  sample$extra[16:18] <- NA_real_
  expect_warning(
    fit <- estimate_price_elasticity(
      sample,
      "rounds",
      "price",
      controls = "extra",
      min_n = 30
    ),
    class = "golfops_exclusion_warning"
  )
  expect_equal(fit$n_supplied, 100)
  expect_equal(fit$n_excluded, 18)
  expect_equal(fit$pct_excluded, 18)
  expect_equal(fit$n_used, 82)
  reasons <- fit$exclusions
  expect_equal(reasons$n_rows[reasons$reason == "nonpositive_rounds"], 8)
  expect_equal(reasons$n_rows[reasons$reason == "nonpositive_price"], 4)
  expect_equal(reasons$n_rows[reasons$reason == "missing_value"], 6)
  expect_match(
    paste(fit$limitations, collapse = " "),
    "logarithm is defined for positive numbers"
  )
})

test_that("group fixed effects still recover a within-group price slope", {
  set.seed(23)
  n <- 180
  group <- rep(c("2022", "2023"), each = n / 2)
  price <- exp(stats::rnorm(n, log(35), 0.2))
  rounds <- exp(4 + (-0.9) * log(price) + ifelse(group == "2023", 0.2, 0) + stats::rnorm(n, 0, 0.02))
  sample <- data.frame(rounds = rounds, price = price, year = group)
  fit <- estimate_price_elasticity(sample, "rounds", "price", group = "year")
  expect_lt(abs(fit$elasticity - (-0.9)), 0.08)
  expect_match(fit$formula, "factor(year)", fixed = TRUE)
})

test_that("synthetic experiments are reproducible and match their designs", {
  set.seed(123)
  before <- stats::rnorm(1)
  one <- simulate_elasticity_sample("correct_specification", n_days = 80, seed = 457)
  after <- stats::rnorm(1)
  two <- simulate_elasticity_sample("correct_specification", n_days = 80, seed = 457)
  other <- simulate_elasticity_sample("correct_specification", n_days = 80, seed = 458)
  expect_equal(one$rounds, two$rounds)
  expect_false(isTRUE(all.equal(one$rounds, other$rounds)))
  set.seed(123)
  expect_equal(stats::rnorm(1), before)
  expect_equal(stats::rnorm(1), after)
  expect_equal(attr(one, "true_elasticity"), -0.8)
  expect_equal(one$weekend[[1]], 1)

  fit <- estimate_price_elasticity(
    one,
    "rounds",
    "price",
    controls = c("weekend", "temp_min_c", "precip_mm")
  )
  expect_lt(abs(fit$elasticity - attr(one, "true_elasticity")), 0.25)

  endogenous <- simulate_elasticity_sample("endogenous_price", seed = 457)
  naive <- estimate_price_elasticity(endogenous, "rounds", "price")
  controlled <- estimate_price_elasticity(
    endogenous,
    "rounds",
    "price",
    controls = c("weekend", "temp_min_c")
  )
  expect_gt(naive$elasticity, -0.2)
  expect_lt(controlled$elasticity, naive$elasticity - 0.15)
  expect_gt(controlled$elasticity, -0.55)
  expect_lt(controlled$elasticity, -0.15)
  expect_gt(controlled$elasticity, attr(endogenous, "true_elasticity") + 0.2)
  expect_match(
    paste(attr(endogenous, "dgp"), collapse = " "),
    "do not establish a causal elasticity",
    fixed = TRUE
  )

  constant <- simulate_elasticity_sample("segment_mix", seed = 457)
  expect_true(is.na(attr(constant, "true_elasticity")))
  expect_equal(length(unique(constant$price[constant$segment == "resident"])), 1)
  daily <- aggregate_segment_rounds(constant)
  expect_equal(nrow(daily), 400)
  aggregate_fit <- estimate_price_elasticity(daily, "rounds", "price", controls = "weekend")
  expect_gt(aggregate_fit$elasticity, 0.5)
  expect_error(
    estimate_price_elasticity(
      constant[constant$segment == "resident", ],
      "rounds",
      "price"
    ),
    "does not vary",
    class = "golfops_input_error"
  )

  varied <- simulate_elasticity_sample("segment_mix_price_variation", seed = 457)
  resident <- estimate_price_elasticity(
    varied[varied$segment == "resident", ],
    "rounds",
    "price",
    controls = c("weekend", "visitor_index")
  )
  visitor <- estimate_price_elasticity(
    varied[varied$segment == "visitor", ],
    "rounds",
    "price",
    controls = c("weekend", "visitor_index")
  )
  mixed <- aggregate_segment_rounds(varied)
  mixed_fit <- estimate_price_elasticity(mixed, "rounds", "price", controls = "weekend")
  expect_lt(abs(resident$elasticity - (-0.8)), 0.2)
  expect_lt(abs(visitor$elasticity - (-0.8)), 0.2)
  expect_gt(mixed_fit$elasticity, -0.8 + 0.4)
})

test_that("segment aggregation uses round weights", {
  days <- data.frame(
    date = as.Date(c("2024-01-01", "2024-01-01", "2024-01-02", "2024-01-02")),
    segment = c("resident", "visitor", "resident", "visitor"),
    rounds = c(10, 30, 0, 0),
    price = c(20, 40, 10, 10),
    weekend = c(1, 1, 0, 0)
  )
  daily <- aggregate_segment_rounds(days)
  expect_equal(daily$rounds, c(40, 0))
  expect_equal(daily$price, c(35, NA_real_))
  expect_equal(daily$weekend, c(1, 0))
  expect_false("segment" %in% names(daily))
  clash <- days
  clash$note <- c("a", "b", "a", "a")
  expect_error(
    aggregate_segment_rounds(clash),
    "varies within a date",
    class = "golfops_input_error"
  )
})
