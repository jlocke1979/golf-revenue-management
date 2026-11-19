# Rebuild inst/extdata/dobson_weather.csv from sources/weather.
#
# The source CSVs are the NCEI GHCND access files retrieved on 2026-11-19,
# restricted to 2021-07-01 through 2026-04-30. This script converts GHCND
# tenths to degrees Celsius, millimeters, and meters per second. It does not
# download a file and it does not call a weather API.
#
# Optional refresh, which replaces sources/weather and then rebuilds:
#   Rscript data-raw/dobson_weather.R --download
#
# Run from the package root:
#   Rscript data-raw/dobson_weather.R

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
if (length(file_arg) != 1) {
  stop("Run this script with Rscript so its path can be resolved.")
}
root <- normalizePath(file.path(dirname(sub("^--file=", "", file_arg)), ".."))
raw_dir <- file.path(root, "sources", "weather")
download <- "--download" %in% commandArgs(trailingOnly = TRUE)

stations <- list(
  airport = list(
    id = "USW00023183",
    url = paste0(
      "https://www.ncei.noaa.gov/data/",
      "global-historical-climatology-network-daily/access/USW00023183.csv"
    )
  ),
  mesa = list(
    id = "US1AZMR0260",
    url = paste0(
      "https://www.ncei.noaa.gov/data/",
      "global-historical-climatology-network-daily/access/US1AZMR0260.csv"
    )
  )
)

if (download) {
  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
  start <- as.Date("2021-07-01")
  end <- as.Date("2026-04-30")
  for (station in stations) {
    tmp <- tempfile(fileext = ".csv")
    message("Downloading ", station$url)
    download.file(station$url, tmp, quiet = TRUE, mode = "wb")
    incoming <- utils::read.csv(tmp, colClasses = "character", check.names = FALSE)
    incoming$DATE <- as.Date(incoming$DATE)
    incoming <- incoming[incoming$DATE >= start & incoming$DATE <= end, , drop = FALSE]
    incoming$DATE <- format(incoming$DATE, "%Y-%m-%d")
    utils::write.csv(
      incoming,
      file.path(raw_dir, paste0(station$id, ".csv")),
      row.names = FALSE,
      na = "",
      quote = TRUE
    )
  }
}

read_station <- function(id) {
  path <- file.path(raw_dir, paste0(id, ".csv"))
  if (!file.exists(path)) {
    stop("Missing ", path, ". Run with --download or restore sources/weather.")
  }
  utils::read.csv(path, colClasses = "character", check.names = FALSE, na.strings = character())
}

blank_na <- function(x) {
  x <- trimws(as.character(x))
  empty <- is.na(x) | !nzchar(x)
  x[empty] <- NA_character_
  x
}

ghcn_number <- function(x) {
  x <- blank_na(x)
  bad <- !is.na(x) & grepl("[^0-9.\\-]", x)
  if (any(bad)) {
    stop("A GHCND value is not numeric.")
  }
  out <- suppressWarnings(as.numeric(x))
  out[!is.na(out) & out <= -9999] <- NA_real_
  out
}

ghcn_attribute <- function(x) {
  x <- ifelse(is.na(x), "", x)
  parts <- strsplit(x, ",", fixed = TRUE)
  measurement <- vapply(parts, function(p) if (length(p) >= 1) p[[1]] else "", character(1))
  quality <- vapply(parts, function(p) if (length(p) >= 2) p[[2]] else "", character(1))
  list(measurement = measurement, quality = quality)
}

# Tenths in the GHCND file. A non-blank quality flag withdraws the value.
element <- function(frame, name) {
  value <- ghcn_number(frame[[name]]) / 10
  attr_field <- ghcn_attribute(frame[[paste0(name, "_ATTRIBUTES")]])
  failed <- nzchar(attr_field$quality)
  value[failed] <- NA_real_
  list(
    value = value,
    trace = attr_field$measurement == "T" & !is.na(value),
    quality = attr_field$quality
  )
}

haversine_km <- function(lat1, lon1, lat2, lon2) {
  rad <- pi / 180
  dlat <- (lat2 - lat1) * rad
  dlon <- (lon2 - lon1) * rad
  a <- sin(dlat / 2)^2 + cos(lat1 * rad) * cos(lat2 * rad) * sin(dlon / 2)^2
  6371.0088 * 2 * atan2(sqrt(a), sqrt(1 - a))
}

airport <- read_station("USW00023183")
mesa <- read_station("US1AZMR0260")
airport$date <- as.Date(airport$DATE)
mesa$date <- as.Date(mesa$DATE)
if (anyDuplicated(airport$date) || anyDuplicated(mesa$date)) {
  stop("A station file repeats a date.")
}

start <- as.Date("2021-07-01")
end <- as.Date("2026-04-30")
dates <- seq(start, end, by = "day")
if (!all(dates %in% airport$date)) {
  stop("Sky Harbor is missing a date in the operating window.")
}
airport <- airport[match(dates, airport$date), , drop = FALSE]
mesa_row <- match(dates, mesa$date)

tmin <- element(airport, "TMIN")
tmax <- element(airport, "TMAX")
apr <- element(airport, "PRCP")
awnd <- element(airport, "AWND")

mesa_aligned <- mesa[mesa_row, , drop = FALSE]
mesa_present <- !is.na(mesa_row)
precip_value <- rep(NA_real_, length(dates))
precip_trace <- rep(FALSE, length(dates))
precip_quality <- rep("", length(dates))
precip_multiday <- rep(FALSE, length(dates))
local <- element(mesa_aligned[mesa_present, , drop = FALSE], "PRCP")
precip_value[mesa_present] <- local$value
precip_trace[mesa_present] <- local$trace
precip_quality[mesa_present] <- local$quality
dapr <- rep(NA_real_, length(dates))
dapr[mesa_present] <- ghcn_number(mesa_aligned$DAPR[mesa_present])
precip_multiday[mesa_present] <- !is.na(dapr[mesa_present]) & dapr[mesa_present] > 1

temp_avg <- (tmin$value + tmax$value) / 2
temp_missing <- is.na(tmin$value) | is.na(tmax$value)
precip_missing <- is.na(precip_value)
precip_airport_missing <- is.na(apr$value)
wind_missing <- is.na(awnd$value)

course_lat <- 33.3717596
course_lon <- -111.8696999
mesa_lat <- as.numeric(mesa$LATITUDE[[1]])
mesa_lon <- as.numeric(mesa$LONGITUDE[[1]])
airport_lat <- as.numeric(airport$LATITUDE[[1]])
airport_lon <- as.numeric(airport$LONGITUDE[[1]])

daily <- data.frame(
  date = format(dates, "%Y-%m-%d"),
  temp_min_c = round(tmin$value, 1),
  temp_max_c = round(tmax$value, 1),
  temp_avg_c = round(temp_avg, 2),
  temp_min_quality_flag = tmin$quality,
  temp_max_quality_flag = tmax$quality,
  precip_mm = round(precip_value, 1),
  precip_trace = precip_trace,
  precip_multiday = precip_multiday,
  precip_quality_flag = precip_quality,
  precip_mm_airport = round(apr$value, 1),
  precip_trace_airport = apr$trace,
  precip_airport_quality_flag = apr$quality,
  wind_speed_ms = round(awnd$value, 1),
  wind_quality_flag = awnd$quality,
  temp_missing = temp_missing,
  precip_missing = precip_missing,
  precip_airport_missing = precip_airport_missing,
  wind_missing = wind_missing,
  stringsAsFactors = FALSE
)

meta <- data.frame(
  role = c("course", "precipitation", "temperature", "wind", "airport_precipitation"),
  station_id = c("dobson_ranch", "US1AZMR0260", "USW00023183", "USW00023183", "USW00023183"),
  station_name = c(
    "Dobson Ranch Golf Course",
    mesa$NAME[[1]],
    airport$NAME[[1]],
    airport$NAME[[1]],
    airport$NAME[[1]]
  ),
  latitude = c(course_lat, mesa_lat, airport_lat, airport_lat, airport_lat),
  longitude = c(course_lon, mesa_lon, airport_lon, airport_lon, airport_lon),
  elevation_m = c(
    NA_real_,
    as.numeric(mesa$ELEVATION[[1]]),
    as.numeric(airport$ELEVATION[[1]]),
    as.numeric(airport$ELEVATION[[1]]),
    as.numeric(airport$ELEVATION[[1]])
  ),
  distance_km = c(
    NA_real_,
    haversine_km(course_lat, course_lon, mesa_lat, mesa_lon),
    haversine_km(course_lat, course_lon, airport_lat, airport_lon),
    haversine_km(course_lat, course_lon, airport_lat, airport_lon),
    haversine_km(course_lat, course_lon, airport_lat, airport_lon)
  ),
  variables = c(
    "course centroid",
    "PRCP",
    "TMIN,TMAX",
    "AWND",
    "PRCP"
  ),
  coordinate_note = c(
    "OpenStreetMap relation 326342 centroid, retrieved 2026-11-19",
    "GHCND station coordinate",
    "GHCND station coordinate",
    "GHCND station coordinate",
    "GHCND station coordinate"
  ),
  source_url = c(
    "https://www.openstreetmap.org/relation/326342",
    stations$mesa$url,
    stations$airport$url,
    stations$airport$url,
    stations$airport$url
  ),
  retrieved = "2026-11-19",
  stringsAsFactors = FALSE
)
meta$distance_km <- round(meta$distance_km, 2)

out_daily <- file.path(root, "inst", "extdata", "dobson_weather.csv")
out_meta <- file.path(root, "inst", "extdata", "dobson_weather_stations.csv")
utils::write.csv(daily, out_daily, row.names = FALSE, na = "", quote = TRUE)
utils::write.csv(meta, out_meta, row.names = FALSE, na = "", quote = TRUE)

message("Days: ", nrow(daily))
message("Temperature missing: ", sum(daily$temp_missing))
message("Local precipitation missing: ", sum(daily$precip_missing))
message("Airport precipitation missing: ", sum(daily$precip_airport_missing))
message("Wind missing: ", sum(daily$wind_missing))
message("Local traces: ", sum(daily$precip_trace))
message("Multiday precip flags: ", sum(daily$precip_multiday))
message("Distances km: ", paste(meta$role, meta$distance_km, sep = "=", collapse = "; "))
recent <- dates >= as.Date("2026-01-01")
message(
  "2026 local precip missing share: ",
  round(mean(daily$precip_missing[recent]), 3)
)

stopifnot(
  nrow(daily) == 1765L,
  sum(daily$temp_missing) == 0L,
  sum(daily$precip_airport_missing) == 0L,
  sum(daily$wind_missing) == 2L,
  sum(daily$precip_missing) == 139L,
  all(daily$temp_max_c >= daily$temp_min_c),
  max(daily$temp_max_c) < 55,
  min(daily$temp_min_c) > 0
)
