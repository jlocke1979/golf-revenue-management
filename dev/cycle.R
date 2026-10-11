#!/usr/bin/env Rscript

# The devtools cycle from R Packages:
#   document(), load_all(), run examples, test(), and, when asked, check().
#
#   Rscript dev/cycle.R
#   Rscript dev/cycle.R check
#
# Run the short form after editing files under R/.
# Run the check form before a commit.
# This file is listed in .Rbuildignore. It is not part of the installed package.
#
# A timer every 30 minutes is a poor way to run check(). check() rebuilds the
# package and the vignettes, and a half-written file will fail it. Repeat
# document() and test() while coding. Save check() for a clean tree.

local_lib <- Sys.getenv("GOLFOPS_LIB", unset = "")
if (nzchar(local_lib)) {
  .libPaths(c(local_lib, .libPaths()))
}

if (!requireNamespace("devtools", quietly = TRUE)) {
  stop("Install devtools before running this cycle.", call. = FALSE)
}

args <- commandArgs(trailingOnly = TRUE)
do_check <- "check" %in% args

message("\n== document() ==")
devtools::document()

message("\n== load_all() ==")
devtools::load_all()

message("\n== run_examples() ==")
old_device <- getOption("device")
options(device = function(...) grDevices::pdf(file = nullfile()))
on.exit(options(device = old_device), add = TRUE)
devtools::run_examples()

message("\n== test() ==")
test_result <- as.data.frame(devtools::test())
if (sum(test_result$failed, na.rm = TRUE) > 0 ||
    sum(test_result$error, na.rm = TRUE) > 0) {
  stop("Tests failed.", call. = FALSE)
}

if (!do_check) {
  message("\nShort cycle done. Run `Rscript dev/cycle.R check` before a commit.")
  quit(status = 0)
}

message("\n== check() ==")
check_result <- devtools::check(
  document = FALSE,
  cran = FALSE,
  error_on = "never"
)
status <- check_result$status
message(sprintf(
  "check status %s: %s errors, %s warnings, %s notes",
  status,
  sum(check_result$errors != ""),
  sum(check_result$warnings != ""),
  sum(check_result$notes != "")
))
if (!identical(as.integer(status), 0L)) {
  quit(status = 1)
}
