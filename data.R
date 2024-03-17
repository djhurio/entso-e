# Get data

rm(list = ls())

library(data.table)
library(gt)
library(lubridate)

source("functions.R")

xml_data <- get_prices_xml(
  dateStart = Sys.Date() %m-% years(1),
  dateEnd = Sys.Date()
)

dat <- convert_prices_dt(xml_data) |> setDT(key = "datetime")

dat[, date := as.Date(format(datetime, format = "%Y-%m-%d"))]
dat[, time := format(datetime, format = "%H:%M")]

dat[, weekday := lubridate::wday(date, week_start = 1)]


# By weekdays
dat_agg_wdays <- dat[, .(price = mean(price * 1.21 / 10)),
                     keyby = .(weekday, time)]

setorder(dat_agg_wdays, weekday, price)

dat_agg_wdays[, hi := factor(1:.N <= 12, c(TRUE, FALSE), c("on", "off")),
              by = .(weekday)]
dat_agg_wdays[, .N, keyby = .(hi)]


# All days
dat_agg_all <- dat[, .(price = mean(price * 1.21 / 10)),
                   keyby = .(time)]
dat_agg_all[, weekday := 0L]

setorder(dat_agg_all, weekday, price)

dat_agg_all[, hi := factor(1:.N <= 12, c(TRUE, FALSE), c("on", "off")),
            by = .(weekday)]
dat_agg_all[, .N, keyby = .(hi)]

setorder(dat_agg_all, weekday, time)
dat_agg_all


# Add
dat_agg <- rbindlist(list(dat_agg_wdays, dat_agg_all), use.names = TRUE)

schedule <- dcast.data.table(
  data = dat_agg,
  formula = time ~ weekday,
  value.var = "hi"
) |>
  gt() |>
  sub_missing(missing_text = "") |>
  data_color(
    columns = matches("[0-9]"),
    target_columns = matches("[0-9]"),
    palette = "Accent"
  )
schedule

schedule |> gtsave(filename = "schedule.html")


schedule.price <- dcast.data.table(
  data = dat_agg,
  formula = time ~ weekday,
  value.var = "price"
) |>
  gt() |>
  sub_missing(missing_text = "") |>
  fmt_number(decimals = 0) |>
  cols_width(
    time ~ px(60),
    everything() ~ px(40)
  ) |>
  data_color(
    columns = matches("[0-9]"),
    target_columns = matches("[0-9]"),
    palette = "Reds"
  )
schedule.price

schedule.price |> gtsave(filename = "schedule.price.html")


dat_agg_all[, mean(price)]
dat_agg_all[, mean(price), keyby = .(hi)]

dat_agg_all[, 1 - mean(price[hi == "on"]) / mean(price)]
dat_agg_wdays[, 1 - mean(price[hi == "on"]) / mean(price)]
