#' Map this package to the posted MSDS 457 requirements
#'
#' The syllabus splits the final project into a proposal, a checkpoint, and a
#' final package submission. Those are different deadlines. This table records
#' what is in the package now. It does not invent checkpoint instructions that
#' are not in the local course files.
#'
#' @return A tibble with one row per requirement.
#' @export
#' @examples
#' course_requirements()
course_requirements <- function() {
  tibble(
    stage = c(
      "Module 1 proposal",
      "Module 1 proposal",
      "Module 1 proposal",
      "Module 5 checkpoint",
      "Module 6 documentation assignment",
      "Module 10 final submission",
      "Module 10 final submission",
      "Module 10 final submission",
      "Course objective"
    ),
    requirement = c(
      "Written proposal on unused capacity and pricing tests",
      "At least three core functions matched to the available data",
      "State that elasticity and an optimal tee-time price are out of reach without a tee sheet",
      "Final project checkpoint",
      "Preliminary R package documentation",
      "R package complete with documentation",
      "Sports-specific functions and an example dataset",
      "Tests, input checks, and a worked example",
      "A production-ready package a manager can rerun"
    ),
    status = c(
      "Met by the submitted proposal. The package is evidence for later stages, not a substitute for that document.",
      "Met: prepare, summarize, plot, utilization, revenue-ratio, and scenario functions are implemented for Dobson Ranch.",
      "Met in the function documentation, the audit, and the scenario assumptions.",
      "Not yet specified in the local course files. Do not treat the proposal deadline as this checkpoint.",
      "Not the same assignment as the final package. Local files name it and do not include its rubric.",
      "In progress. Documentation, tests, and two worked examples exist. A website, continuous integration, and CRAN release are not claimed.",
      "Met for Dobson Ranch. The ticket series is synthetic and labeled as hypothetical.",
      "Met: testthat, validation errors, the Dobson vignette, and the forecast vignette.",
      "Partly met. R CMD check is clean. Later modules in R Packages cover packaging and release steps that are not in this version."
    )
  )
}
