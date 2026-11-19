#' Estimate a log-log price association
#'
#' Fits `log(rounds)` on `log(price)` with optional linear controls and
#' optional group fixed effects. The price coefficient is the percent change
#' in rounds associated with a 1 percent higher price in the rows that remain,
#' under this specification.
#'
#' The standard error and confidence interval come from [stats::lm()]. They
#' are ordinary least-squares statistics. Enz and Canina clustered standard
#' errors by course. These are not clustered estimates. Passing a course
#' identifier to `group` adds a fixed effect. It does not cluster the
#' standard errors.
#'
#' Their working-paper equation, page 8, regresses log rounds on log average
#' daily price, course age, rating, slope, number of holes, regular-tee par,
#' water features, lodging, minimum temperature, precipitation, location
#' indicators, and a year fixed effect. Temperature in that equation is
#' degrees Fahrenheit and precipitation is inches. This function adds a
#' control or a group only when you name the column. It does not convert
#' units. A numeric control enters as a straight line. A column named in
#' `group` enters as a set of fixed effects.
#'
#' The logarithm is defined for positive numbers. Rows with a missing, zero,
#' or negative rounds or price value are excluded, as are rows with a missing
#' control or group value. `n_excluded` counts each excluded row once.
#' `exclusions` counts each reason, so one row can appear in more than one
#' reason. A warning is issued when the unique excluded share reaches
#' `exclusion_warn`. The published transaction file is not public, and this
#' function does not re-estimate the published coefficients. A coefficient is
#' not a causal demand elasticity and not a recommendation to change a posted
#' price.
#'
#' @param data A table.
#' @param rounds,price Column names. Both must be positive in a row that
#'   enters the fit.
#' @param controls Optional column names entered as linear terms. Numeric or
#'   logical columns only. Pass categories through `group`.
#' @param group Optional column names entered as fixed effects.
#' @param min_n Minimum number of rows after exclusions.
#' @param conf_level Confidence level for the ordinary least-squares interval.
#' @param exclusion_warn Warn when the unique excluded share of the supplied
#'   rows is at least this fraction. The default is 10 percent.
#' @return A `golfops_elasticity` list. `model` is the [stats::lm()] fit.
#'   `limitations` states the deviations from the Enz and Canina specification
#'   and the inferential limits of this fit.
#' @references
#' Enz, Cathy A., and Linda Canina. 2016. "Competitive Pricing in the Golf
#' Industry." Cornell University, School of Hotel Administration.
#' <https://hdl.handle.net/1813/72428>. The log-log specification and
#' course-clustered standard errors are on page 8 of that working paper.
#' Journal version: *Journal of Revenue and Pricing Management* 16 (2):
#' 139–153. \doi{10.1057/s41272-016-0075-8}.
#' @export
#' @examples
#' sample <- simulate_elasticity_sample(
#'   "correct_specification",
#'   n_days = 80,
#'   seed = 457
#' )
#' estimate_price_elasticity(
#'   sample,
#'   rounds = "rounds",
#'   price = "price",
#'   controls = c("weekend", "temp_min_c", "precip_mm")
#' )
estimate_price_elasticity <- function(
    data,
    rounds,
    price,
    controls = NULL,
    group = NULL,
    min_n = 30,
    conf_level = 0.95,
    exclusion_warn = 0.10) {
  if (!is.data.frame(data)) {
    abort("`data` must be a table.", class = "golfops_input_error")
  }
  rounds <- .column_name(rounds, "rounds")
  price <- .column_name(price, "price")
  controls <- .optional_names(controls, "controls")
  group <- .optional_names(group, "group")
  if (rounds == price || any(c(rounds, price) %in% c(controls, group))) {
    abort(
      "`rounds` and `price` must be distinct from each other and from controls and groups.",
      class = "golfops_input_error"
    )
  }
  if (length(intersect(controls, group)) > 0) {
    abort(
      "Name a column in `controls` or in `group`, not both.",
      class = "golfops_input_error"
    )
  }
  reserved <- intersect(c(controls, group), c("log_rounds", "log_price"))
  if (length(reserved) > 0) {
    abort(
      "Rename a column called log_rounds or log_price before using it as a control or a group.",
      class = "golfops_input_error"
    )
  }
  needed <- c(rounds, price, controls, group)
  missing <- setdiff(needed, names(data))
  if (length(missing) > 0) {
    abort(
      paste0("Missing columns: ", paste(missing, collapse = ", "), "."),
      class = "golfops_input_error"
    )
  }
  if (!is.numeric(min_n) || length(min_n) != 1 || is.na(min_n) || min_n < 3) {
    abort("`min_n` must be one number of at least 3.", class = "golfops_input_error")
  }
  if (!is.numeric(conf_level) || length(conf_level) != 1 ||
      is.na(conf_level) || conf_level <= 0 || conf_level >= 1) {
    abort("`conf_level` must be one number between 0 and 1.", class = "golfops_input_error")
  }
  if (!is.numeric(exclusion_warn) || length(exclusion_warn) != 1 ||
      is.na(exclusion_warn) || exclusion_warn <= 0 || exclusion_warn > 1) {
    abort(
      "`exclusion_warn` must be one fraction from just above 0 through 1.",
      class = "golfops_input_error"
    )
  }
  rounds_value <- data[[rounds]]
  price_value <- data[[price]]
  if (!is.numeric(rounds_value) || !is.numeric(price_value)) {
    abort("`rounds` and `price` must be numeric.", class = "golfops_input_error")
  }
  for (column in controls) {
    if (!is.numeric(data[[column]]) && !is.logical(data[[column]])) {
      abort(
        paste0(
          "`", column, "` is not numeric. Pass a category through `group` ",
          "so it enters as a fixed effect."
        ),
        class = "golfops_input_error"
      )
    }
  }

  n_supplied <- nrow(data)
  if (n_supplied == 0) {
    abort("`data` has no rows.", class = "golfops_input_error")
  }
  missing_value <- !is.finite(rounds_value) | !is.finite(price_value)
  for (column in c(controls, group)) {
    column_value <- data[[column]]
    missing_value <- missing_value | is.na(column_value)
  }
  nonpositive_rounds <- is.finite(rounds_value) & rounds_value <= 0
  nonpositive_price <- is.finite(price_value) & price_value <= 0
  excluded <- missing_value | nonpositive_rounds | nonpositive_price
  n_excluded <- sum(excluded)
  pct_excluded <- 100 * n_excluded / n_supplied
  exclusions <- tibble(
    reason = c("missing_value", "nonpositive_rounds", "nonpositive_price"),
    n_rows = c(sum(missing_value), sum(nonpositive_rounds), sum(nonpositive_price))
  )
  exclusions$pct_of_supplied <- 100 * exclusions$n_rows / n_supplied

  kept <- data[!excluded, , drop = FALSE]
  n_used <- nrow(kept)
  if (n_used < min_n) {
    abort(
      paste0(
        "The log-log fit needs at least ", min_n, " rows with positive rounds and a positive price. ",
        n_used, " of ", n_supplied, " rows remain (",
        format(round(pct_excluded, 1), nsmall = 1),
        "% excluded). A logarithm is undefined at zero or a negative number, ",
        "so those rows cannot enter."
      ),
      class = "golfops_input_error"
    )
  }
  if (pct_excluded + 1e-9 >= 100 * exclusion_warn) {
    warn(
      paste0(
        "Excluded ", format(round(pct_excluded, 1), nsmall = 1),
        "% of the supplied rows. A log-log model uses log(rounds) and log(price), ",
        "which are defined only for positive numbers. Missing, zero, and negative values ",
        "cannot enter the fit. An exclusion of this size can change the result when the ",
        "omitted rows are closures, unplayable days, or missing prices."
      ),
      class = "golfops_exclusion_warning"
    )
  }

  kept_price <- kept[[price]]
  if (length(unique(kept_price)) < 2) {
    abort(
      "Price does not vary in the rows that remain, so the price coefficient is not identified.",
      class = "golfops_input_error"
    )
  }
  if (length(group) > 0) {
    group_key <- interaction(
      lapply(group, function(column) kept[[column]]),
      drop = TRUE,
      lex.order = TRUE
    )
    within <- tapply(kept_price, group_key, function(x) length(unique(x)) > 1)
    if (!any(within)) {
      abort(
        paste0(
          "Price does not vary within the supplied groups, so the price coefficient ",
          "is not identified once those group effects are included."
        ),
        class = "golfops_input_error"
      )
    }
  }

  model_data <- data.frame(
    log_rounds = log(kept[[rounds]]),
    log_price = log(kept_price)
  )
  for (column in controls) {
    model_data[[column]] <- kept[[column]]
  }
  for (column in group) {
    model_data[[column]] <- factor(as.character(kept[[column]]))
  }
  control_terms <- if (length(controls) == 0) {
    character()
  } else {
    vapply(controls, .formula_name, character(1))
  }
  group_terms <- if (length(group) == 0) {
    character()
  } else {
    paste0("factor(", vapply(group, .formula_name, character(1)), ")")
  }
  rhs <- c("log_price", control_terms, group_terms)
  model <- stats::lm(stats::reformulate(rhs, response = "log_rounds"), data = model_data)
  coefs <- stats::coef(model)
  if (!"log_price" %in% names(coefs) || is.na(coefs[["log_price"]])) {
    abort(
      paste0(
        "The price coefficient is not identified. It is collinear with the controls ",
        "or the groups."
      ),
      class = "golfops_input_error"
    )
  }
  sm <- summary(model)
  tab <- sm$coefficients
  if (!"log_price" %in% rownames(tab)) {
    abort(
      "The price coefficient is not identified.",
      class = "golfops_input_error"
    )
  }
  estimate <- tab["log_price", "Estimate"]
  std_error <- tab["log_price", "Std. Error"]
  df <- model$df.residual
  if (df < 1) {
    abort(
      "The model has no residual degrees of freedom.",
      class = "golfops_input_error"
    )
  }
  critical <- stats::qt(1 - (1 - conf_level) / 2, df)
  displayed_terms <- paste0("log(", rounds, ") ~ log(", price, ")")
  if (length(controls) > 0) {
    displayed_terms <- c(displayed_terms, controls)
  }
  if (length(group) > 0) {
    displayed_terms <- c(displayed_terms, paste0("factor(", group, ")"))
  }
  displayed <- paste(displayed_terms, collapse = " + ")
  limitations <- .elasticity_limitations()
  if (any(is.na(coefs))) {
    limitations <- c(
      limitations,
      "Some controls or groups were dropped because they were collinear with the other terms. The reported price coefficient is the one lm() kept."
    )
  }

  structure(
    list(
      elasticity = unname(estimate),
      std_error = unname(std_error),
      conf_low = unname(estimate - critical * std_error),
      conf_high = unname(estimate + critical * std_error),
      conf_level = conf_level,
      statistic = unname(tab["log_price", "t value"]),
      p_value = unname(tab["log_price", "Pr(>|t|)"]),
      n_supplied = n_supplied,
      n_used = n_used,
      n_excluded = n_excluded,
      pct_excluded = pct_excluded,
      exclusions = exclusions,
      r_squared = sm$r.squared,
      adj_r_squared = sm$adj.r.squared,
      degrees_of_freedom = df,
      formula = displayed,
      model = model,
      interpretation = paste0(
        "A coefficient of ", format(round(estimate, 3), nsmall = 3),
        " is the ordinary least-squares association between log rounds and log price ",
        "in the rows that remain. It is the percent change in rounds associated with a ",
        "1 percent higher price under this specification. The standard error is not ",
        "clustered by course. The coefficient is not a causal demand elasticity and ",
        "not a recommendation to change a posted price."
      ),
      limitations = limitations
    ),
    class = "golfops_elasticity"
  )
}

#' @export
print.golfops_elasticity <- function(x, ...) {
  cat("Log-log price association\n")
  cat(x$formula, "\n", sep = "")
  cat(sprintf(
    "Elasticity %.3f; OLS standard error %.3f; %.0f%% interval [%.3f, %.3f]\n",
    x$elasticity, x$std_error, 100 * x$conf_level, x$conf_low, x$conf_high
  ))
  cat(sprintf(
    "Rows used %s of %s (%.1f%% excluded).\n\n",
    format(x$n_used, big.mark = ","),
    format(x$n_supplied, big.mark = ","),
    x$pct_excluded
  ))
  cat(x$interpretation, "\n\n", sep = "")
  cat(paste0("- ", x$limitations), sep = "\n")
  invisible(x)
}

#' Synthetic samples with a known price elasticity
#'
#' Builds one of four daily samples. The first two are a single series.
#' The segment samples have one resident row and one visitor row per date.
#' `aggregate_segment_rounds()` turns those rows into a daily total and a
#' round-weighted average price.
#'
#' `correct_specification` draws price independently of the demand shock and
#' generates log rounds from `elasticity`, weekend, temperature, and
#' precipitation. `endogenous_price` raises price when anticipated demand is
#' higher. Part of that demand is the weekend and the temperature, and part
#' is not returned in the table. `segment_mix` keeps each segment price
#' constant and shifts the visitor share by season. The `elasticity` argument
#' is not used to generate those rounds. `segment_mix_price_variation` gives
#' both segments the same negative elasticity and modest price noise that is
#' independent of the demand shock, while the visitor share still shifts.
#'
#' These rows are synthetic. They are not Dobson Ranch, and they are not the
#' Enz and Canina transaction file.
#'
#' @param experiment One of `correct_specification`, `endogenous_price`,
#'   `segment_mix`, or `segment_mix_price_variation`.
#' @param n_days Number of dates.
#' @param elasticity Structural price coefficient. Ignored when
#'   `experiment` is `segment_mix`, because those prices do not vary and the
#'   rounds are not generated from a price term.
#' @param seed Integer seed. The generator restores the previous random seed
#'   when it finishes.
#' @return A `golfops_elasticity_sample` tibble. Attributes `description` and
#'   `dgp` state the experiment and the generating equation.
#' @export
#' @examples
#' simulate_elasticity_sample("correct_specification", n_days = 40, seed = 457)
simulate_elasticity_sample <- function(
    experiment = c(
      "correct_specification",
      "endogenous_price",
      "segment_mix",
      "segment_mix_price_variation"
    ),
    n_days = 400,
    elasticity = -0.8,
    seed = 457) {
  experiment <- match.arg(experiment)
  if (!is.numeric(n_days) || length(n_days) != 1 || is.na(n_days) ||
      n_days < 40 || n_days != floor(n_days)) {
    abort("`n_days` must be one integer of at least 40.", class = "golfops_input_error")
  }
  if (!is.numeric(elasticity) || length(elasticity) != 1 || !is.finite(elasticity)) {
    abort("`elasticity` must be one finite number.", class = "golfops_input_error")
  }
  n_days <- as.integer(n_days)
  .with_seed(seed, {
    date <- seq(as.Date("2022-01-01"), by = "day", length.out = n_days)
    weekend <- .weekend_indicator(date)
    doy <- as.integer(format(date, "%j"))
    year <- as.integer(format(date, "%Y"))
    if (experiment == "correct_specification") {
      temp_min_c <- 16 + 12 * sin(2 * pi * (doy - 110) / 365) + stats::rnorm(n_days, 0, 1.2)
      precip_mm <- stats::rbinom(n_days, 1, 0.15) * stats::rexp(n_days, rate = 1 / 3)
      log_price <- 3.2 + 0.25 * weekend + 0.15 * stats::rnorm(n_days)
      log_rounds <- 5.5 + elasticity * log_price + 0.30 * weekend -
        0.02 * (temp_min_c - 16) - 0.05 * precip_mm + stats::rnorm(n_days, 0, 0.06)
      out <- tibble(
        date = date,
        rounds = exp(log_rounds),
        price = exp(log_price),
        weekend = weekend,
        temp_min_c = temp_min_c,
        precip_mm = precip_mm,
        year = year,
        true_elasticity = elasticity,
        experiment = experiment
      )
      description <- paste0(
        "Price is drawn independently of the demand shock. ",
        "Log rounds use the stated elasticity plus weekend, temperature, and precipitation."
      )
      dgp <- c(
        "log(price) = 3.2 + 0.25 * weekend + 0.15 * z, with z standard normal and independent of the demand shock.",
        "log(rounds) = 5.5 + elasticity * log(price) + 0.30 * weekend - 0.02 * (temp_min_c - 16) - 0.05 * precip_mm + e.",
        "e is normal with mean 0 and standard deviation 0.06."
      )
      true <- elasticity
    } else if (experiment == "endogenous_price") {
      temp_min_c <- 16 + 12 * sin(2 * pi * (doy - 110) / 365) + stats::rnorm(n_days, 0, 1.2)
      cool <- (16 - temp_min_c) / 10
      unobserved <- stats::rnorm(n_days)
      log_price <- 3.15 + 0.28 * stats::rnorm(n_days) + 0.85 * weekend +
        0.30 * cool + 0.12 * unobserved
      log_rounds <- 5.6 + elasticity * log_price + 0.85 * weekend + 0.30 * cool +
        0.40 * unobserved + 0.05 * stats::rnorm(n_days)
      out <- tibble(
        date = date,
        rounds = exp(log_rounds),
        price = exp(log_price),
        weekend = weekend,
        temp_min_c = temp_min_c,
        year = year,
        true_elasticity = elasticity,
        experiment = experiment
      )
      description <- paste0(
        "Price is higher when anticipated demand is higher. ",
        "Weekend and temperature are observed pieces of that demand. ",
        "Another piece is used to set the price and the rounds and is not a column in this table."
      )
      dgp <- c(
        "Let cool = (16 - temp_min_c) / 10. u and z are standard normal. e is normal with standard deviation 0.05.",
        "log(price) = 3.15 + 0.28 * z + 0.85 * weekend + 0.30 * cool + 0.12 * u.",
        "log(rounds) = 5.6 + elasticity * log(price) + 0.85 * weekend + 0.30 * cool + 0.40 * u + e.",
        "z is independent of demand. u is not returned. Weekend and temperature remove only the observed part of the price setting. They do not remove u, and they do not establish a causal elasticity."
      )
      true <- elasticity
    } else {
      visitor_index <- (cos(2 * pi * (doy - 15) / 365) + 1) / 2
      resident_price <- rep(28, n_days)
      visitor_price <- rep(75, n_days)
      if (experiment == "segment_mix_price_variation") {
        resident_price <- 28 * exp(0.12 * stats::rnorm(n_days))
        visitor_price <- 75 * exp(0.12 * stats::rnorm(n_days))
        resident_log <- 5.25 + elasticity * log(resident_price / 28) -
          0.10 * visitor_index + 0.05 * weekend + stats::rnorm(n_days, 0, 0.04)
        visitor_log <- 3.10 + elasticity * log(visitor_price / 75) +
          2.00 * visitor_index + 0.15 * weekend + stats::rnorm(n_days, 0, 0.04)
        description <- paste0(
          "Resident and visitor rounds each use the stated elasticity and a small exogenous price shock. ",
          "The visitor share is higher in winter. An aggregate price is the round-weighted average of the two segment prices."
        )
        dgp <- c(
          "log(resident rounds) = 5.25 + elasticity * log(resident price / 28) - 0.10 * visitor_index + 0.05 * weekend + e.",
          "log(visitor rounds) = 3.10 + elasticity * log(visitor price / 75) + 2.00 * visitor_index + 0.15 * weekend + e.",
          "The price shocks are independent of visitor_index, weekend, and e. Both segments use the same elasticity.",
          "visitor_index is high near the middle of January and low near the middle of July."
        )
        true <- elasticity
      } else {
        resident_log <- 5.25 - 0.10 * visitor_index + 0.05 * weekend +
          stats::rnorm(n_days, 0, 0.04)
        visitor_log <- 3.10 + 2.00 * visitor_index + 0.15 * weekend +
          stats::rnorm(n_days, 0, 0.04)
        description <- paste0(
          "Resident price stays at 28 and visitor price stays at 75. ",
          "Segment rounds do not depend on a price coefficient. ",
          "The visitor share is higher in winter, so the daily average price and total rounds can rise together. ",
          "The elasticity argument is not used."
        )
        dgp <- c(
          "Resident price is 28 and visitor price is 75 on every date.",
          "log(resident rounds) = 5.25 - 0.10 * visitor_index + 0.05 * weekend + e.",
          "log(visitor rounds) = 3.10 + 2.00 * visitor_index + 0.15 * weekend + e.",
          "No price term enters either equation. A positive aggregate coefficient is a composition pattern."
        )
        true <- NA_real_
      }
      resident <- tibble(
        date = date,
        segment = "resident",
        rounds = exp(resident_log),
        price = resident_price,
        weekend = weekend,
        visitor_index = visitor_index,
        year = year,
        true_elasticity = true,
        experiment = experiment
      )
      visitor <- tibble(
        date = date,
        segment = "visitor",
        rounds = exp(visitor_log),
        price = visitor_price,
        weekend = weekend,
        visitor_index = visitor_index,
        year = year,
        true_elasticity = true,
        experiment = experiment
      )
      out <- rbind(resident, visitor)
      out <- out[order(out$date, out$segment), , drop = FALSE]
    }
    .as_elasticity_sample(out, experiment, true, seed, description, dgp)
  })
}

#' Round-weighted daily price from segment rows
#'
#' Sums rounds by date and sets price to the round-weighted average of the
#' segment prices. That average is an average of prices paid. It is not a
#' posted rack rate. Other columns are kept when they do not vary within the
#' date. `segment` is dropped.
#'
#' @param data A table with `date`, `segment`, `rounds`, and `price`, such as
#'   a segment sample from [simulate_elasticity_sample()].
#' @return A tibble with one row per date.
#' @export
#' @examples
#' days <- data.frame(
#'   date = as.Date(c("2024-01-01", "2024-01-01")),
#'   segment = c("resident", "visitor"),
#'   rounds = c(10, 30),
#'   price = c(20, 40)
#' )
#' aggregate_segment_rounds(days)
aggregate_segment_rounds <- function(data) {
  if (!is.data.frame(data)) {
    abort("`data` must be a table.", class = "golfops_input_error")
  }
  required <- c("date", "segment", "rounds", "price")
  missing <- setdiff(required, names(data))
  if (length(missing) > 0) {
    abort(
      paste0("Missing columns: ", paste(missing, collapse = ", "), "."),
      class = "golfops_input_error"
    )
  }
  date <- as.Date(data$date)
  if (any(is.na(date))) {
    abort("`date` must be complete.", class = "golfops_input_error")
  }
  if (any(is.na(data$segment) | !nzchar(as.character(data$segment)))) {
    abort("`segment` must be complete.", class = "golfops_input_error")
  }
  if (!is.numeric(data$rounds) || any(is.na(data$rounds) | data$rounds < 0)) {
    abort("`rounds` must be zero or positive.", class = "golfops_input_error")
  }
  if (!is.numeric(data$price) || any(!is.finite(data$price))) {
    abort("`price` must be complete and finite.", class = "golfops_input_error")
  }
  key <- paste(date, as.character(data$segment))
  if (anyDuplicated(key)) {
    abort(
      "Each date and segment pair must appear once.",
      class = "golfops_input_error"
    )
  }
  keep_names <- setdiff(names(data), c("date", "segment", "rounds", "price"))
  pieces <- split(seq_len(nrow(data)), date)
  rows <- lapply(pieces, function(idx) {
    block <- data[idx, , drop = FALSE]
    total_rounds <- sum(block$rounds)
    average_price <- if (total_rounds > 0) {
      sum(block$rounds * block$price) / total_rounds
    } else {
      NA_real_
    }
    row <- tibble(
      date = as.Date(block$date[[1]]),
      rounds = total_rounds,
      price = average_price
    )
    for (column in keep_names) {
      values <- block[[column]]
      if (length(unique(values)) != 1) {
        abort(
          paste0("`", column, "` varies within a date, so it cannot be kept on the aggregate row."),
          class = "golfops_input_error"
        )
      }
      row[[column]] <- values[[1]]
    }
    row
  })
  out <- as_tibble(do.call(rbind, rows))
  out[order(out$date), , drop = FALSE]
}

.elasticity_limitations <- function() {
  c(
    "This is a methodological analogue of Enz and Canina (2016), working paper page 8. It does not re-estimate their published coefficients. Their transaction data are not public.",
    "The published equation uses log rounds, log average daily price, course characteristics, minimum temperature in degrees Fahrenheit, precipitation in inches, location indicators, and a year fixed effect.",
    "The standard error and confidence interval are ordinary least-squares statistics from lm(). They are not standard errors clustered by course.",
    "A column in group is a fixed effect. It does not cluster the standard errors. Course characteristics, weather, location, and year are included only when those columns are named.",
    "Rows with missing, zero, or negative rounds or prices are excluded because a logarithm is defined for positive numbers. The excluded share can change the sample when the omitted rows are closures or missing prices.",
    "The coefficient is the association in the rows that remain under this specification. It is not a causal price elasticity. It is not a recommendation to change a posted price. A positive coefficient is not evidence that demand rises with price.",
    "Rounds played can be censored at capacity. This fit does not correct for that.",
    "An average of prices paid is not a posted rack rate. A shift in customer mix can move that average and total rounds together.",
    "A synthetic sample can recover a known coefficient when the data are generated from the fitted equation. That check is not a description of Dobson Ranch. Dobson realized green-fee yield is not a posted price."
  )
}

.optional_names <- function(x, arg) {
  if (is.null(x)) {
    return(character())
  }
  if (!is.character(x) || any(is.na(x) | !nzchar(x)) || anyDuplicated(x)) {
    abort(
      paste0("`", arg, "` must be column names, without duplicates."),
      class = "golfops_input_error"
    )
  }
  x
}

.weekend_indicator <- function(date) {
  # 1970-01-01 was a Thursday. This stays on the calendar date, without a time zone.
  as.integer(((as.integer(date) + 4L) %% 7L) %in% c(0L, 6L))
}

.formula_name <- function(name) {
  if (grepl("^[A-Za-z.][A-Za-z0-9._]*$", name)) {
    name
  } else {
    paste0("`", gsub("`", "", name, fixed = TRUE), "`")
  }
}

.with_seed <- function(seed, expr) {
  if (!is.numeric(seed) || length(seed) != 1 || !is.finite(seed) ||
      seed < 0 || seed != floor(seed)) {
    abort("`seed` must be one non-negative integer.", class = "golfops_input_error")
  }
  seed <- as.integer(seed)
  if (is.na(seed)) {
    abort("`seed` must be one non-negative integer.", class = "golfops_input_error")
  }
  has_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (has_seed) {
    old <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    on.exit(assign(".Random.seed", old, envir = .GlobalEnv), add = TRUE)
  } else {
    on.exit(rm(".Random.seed", envir = .GlobalEnv), add = TRUE)
  }
  set.seed(seed)
  force(expr)
}

.as_elasticity_sample <- function(data, experiment, true_elasticity, seed, description, dgp) {
  data <- as_tibble(data)
  class(data) <- unique(c("golfops_elasticity_sample", class(data)))
  attr(data, "experiment") <- experiment
  attr(data, "true_elasticity") <- true_elasticity
  attr(data, "seed") <- as.integer(seed)
  attr(data, "description") <- description
  attr(data, "dgp") <- dgp
  data
}

#' @export
print.golfops_elasticity_sample <- function(x, ...) {
  cat(attr(x, "description"), "\n\n", sep = "")
  y <- x
  class(y) <- setdiff(class(y), "golfops_elasticity_sample")
  print(y, ...)
  invisible(x)
}
