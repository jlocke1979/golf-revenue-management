# Rebuild data/dobson_rounds.rda from the City of Mesa CSV saved 2026-10-03.
# This script does not download a later catalog refresh.

devtools::load_all(quiet = TRUE)

raw <- readr::read_csv(
  dobson_rounds_example(),
  col_types = readr::cols(.default = readr::col_character()),
  na = character(),
  progress = FALSE,
  show_col_types = FALSE
)

blank_to_na <- function(x) {
  x <- as.character(x)
  x[!nzchar(x)] <- NA_character_
  x
}

dobson_rounds <- prepare_dobson_rounds()
dobson_rounds <- tibble::add_column(
  dobson_rounds,
  sunrise = blank_to_na(raw$Sunrise),
  sunset = blank_to_na(raw$Sunset),
  .before = "daylight_hours"
)

if (!inherits(dobson_rounds, "golfops_rounds")) {
  class(dobson_rounds) <- unique(c("golfops_rounds", class(dobson_rounds)))
}

attr(dobson_rounds, "source_path") <- "inst/extdata/dobson_ranch_golf_rounds.csv"
attr(dobson_rounds, "source_url") <- paste0(
  "https://data.mesaaz.gov/Parks-Recreation-and-Community-Facilities/",
  "Dobson-Ranch-Golf-Rounds/24g5-hb8k"
)
attr(dobson_rounds, "source_accessed") <- "2026-10-03"

calendar_year_source <- as.numeric(gsub(",", "", raw$`Calendar Year`, fixed = TRUE))
stopifnot(
  nrow(dobson_rounds) == 1838L,
  ncol(dobson_rounds) == 38L,
  identical(dobson_rounds$calendar_year, calendar_year_source),
  identical(sum(dobson_rounds$date == as.Date("2004-12-10")), 1L),
  !any(dobson_rounds$date == as.Date("2024-12-10"))
)

daily <- sort(unique(
  dobson_rounds$date[
    dobson_rounds$grain == "day" & dobson_rounds$date >= as.Date("2021-07-01")
  ]
))
full <- seq(as.Date("2021-07-01"), as.Date("2026-04-30"), by = "day")
missing_days <- as.Date(setdiff(as.character(full), as.character(daily)))
stopifnot(identical(missing_days, as.Date("2024-12-10")))

dups <- dobson_rounds[dobson_rounds$flag_duplicate_date, ]
stopifnot(
  nrow(dups) == 2L,
  all(dups$date == as.Date("2022-04-24")),
  nrow(unique(dups[, setdiff(names(dups), "row_id"), drop = FALSE])) == 1L
)

usethis::use_data(dobson_rounds, overwrite = TRUE)
