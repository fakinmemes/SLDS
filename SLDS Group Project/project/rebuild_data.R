# R/build.R
library(tidyverse); library(lubridate)

build_pm_long <- function(dir) {
  list.files(dir, pattern = "\\.csv$", full.names = TRUE) |>
    map_dfr(~ read_csv(.x, col_types = cols(.default = col_character()))) |>
    transmute(
      region = region,
      ts     = with_tz(ymd_hms(timestamp), "Asia/Singapore"),
      upd    = ymd_hms(update_timestamp),
      pm25   = as.numeric(pm25_one_hourly)
    ) |>
    mutate(date = as_date(ts)) |>
    arrange(region, ts, desc(upd)) |>
    distinct(region, ts, .keep_all = TRUE)
}

haversine_km <- function(lat, lon, lat0 = 1.35, lon0 = 103.82) {
  r <- pi / 180
  a <- sin((lat - lat0) * r / 2)^2 +
    cos(lat0 * r) * cos(lat * r) * sin((lon - lon0) * r / 2)^2
  6371 * 2 * asin(sqrt(a))
}

build_pm_daily <- function(pm_long, min_hours = 18) {
  pm_long |>
    group_by(region, date) |>
    summarise(n_hours = sum(!is.na(pm25)),
              pm25 = if_else(n_hours >= min_hours, mean(pm25, na.rm = TRUE), NA_real_),
              .groups = "drop")
}

build_fire_daily <- function(path, product = c("viirs", "modis"),
                             keep_low = FALSE, zones = TRUE) {
  product <- match.arg(product)
  f <- read_csv(path, col_types = cols(acq_time = col_character(),
                                       confidence = col_character()))
  if (!keep_low) {
    f <- if (product == "viirs") filter(f, confidence != "l")
    else filter(f, as.numeric(confidence) >= 30)
  }
  if ("type" %in% names(f)) f <- filter(f, type == 0)
  
  f |>
    mutate(ts_utc = ymd_hm(paste(acq_date, str_pad(acq_time, 4, pad = "0")), tz = "UTC"),
           date   = as_date(with_tz(ts_utc, "Asia/Singapore")),
           zone   = if (zones) paste(if_else(longitude < 106, "w", "e"),
                                     cut(haversine_km(latitude, longitude),
                                         c(0, 500, 1000, Inf),
                                         labels = c("near", "mid", "far")), sep = "_")
           else "all") |>
    group_by(date, zone) |>
    summarise(n = n(), frp = sum(frp), .groups = "drop") |>
    pivot_wider(names_from = zone, values_from = c(n, frp), values_fill = 0) |>
    complete(date = seq(ymd("2016-12-01"), ymd("2024-12-31"), by = "day")) |>
    mutate(across(-date, ~ replace_na(.x, 0)))
}

build_analysis <- function(pm_daily, weather_daily, fire_daily, T_HIGH) {
  monsoon <- function(m) case_when(m %in% c(12, 1, 2, 3) ~ "NE", m %in% 6:9 ~ "SW",
                                   TRUE ~ "inter")
  pm_daily |>
    complete(region, date = seq(ymd("2016-12-01"), ymd("2024-12-31"), by = "day")) |>
    left_join(weather_daily, by = c("region", "date")) |>
    left_join(fire_daily, by = "date") |>
    arrange(region, date) |>
    group_by(region) |>
    mutate(pm25_next  = lead(pm25, 1),
           pm25_lag1  = lag(pm25, 1),
           pm25_roll3 = slider::slide_dbl(pm25, mean, na.rm = TRUE, .before = 2),
           across(starts_with(c("n_", "frp_")),
                  list(roll3 = ~ slider::slide_dbl(.x, sum, .before = 2),
                       roll7 = ~ slider::slide_dbl(.x, sum, .before = 6)))) |>
    ungroup() |>
    mutate(pm25_roll3 = if_else(is.nan(pm25_roll3), NA_real_, pm25_roll3),
           doy_sin = sin(2 * pi * yday(date) / 365.25),
           doy_cos = cos(2 * pi * yday(date) / 365.25),
           monsoon = monsoon(month(date)),
           year    = year(date)) |>
    filter(year >= 2017, !is.na(pm25_next)) |>
    mutate(high_next = factor(pm25_next > T_HIGH, levels = c(TRUE, FALSE),
                              labels = c("high", "normal")))
}