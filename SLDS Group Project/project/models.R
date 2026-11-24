library(tidyverse)
fire_vars <- c("n_w_near", "n_w_mid", "frp_w_near", "frp_w_mid")   # replace with your zone columns + _roll3/_roll7
base_vars <- c("pm25", "pm25_lag1", "pm25_roll3", "rain", "temp_mean",
               "wind_mean", "wind_max", "doy_sin", "doy_cos", "monsoon", "region")

prep <- function(df) {             # same rows for every model
  df |>
    select(date, year, pm25_next, high_next, all_of(c(base_vars, fire_vars))) |>
    mutate(across(all_of(fire_vars), log1p),
           across(c(monsoon, region), factor)) |>
    drop_na()
}

make_X <- function(d, use_fire) {
  vars <- c(base_vars, if (use_fire) fire_vars)
  model.matrix(~ . - 1, d[, vars])
}

fit_lasso <- function(d, use_fire) {
  X <- make_X(d, use_fire)
  cv <- glmnet::cv.glmnet(X, d$high_next == "high", family = "binomial",
                          alpha = 1, foldid = as.integer(factor(d$year)))  # year-blocked tuning
  list(cv = cv, use_fire = use_fire)
}
predict_lasso <- function(m, d) {
  as.numeric(predict(m$cv, make_X(d, m$use_fire), s = "lambda.1se", type = "response"))
}

fit_rf <- function(d, use_fire, mtry = 5, min_node = 20) {
  vars <- c(base_vars, if (use_fire) fire_vars)
  ranger::ranger(x = d[, vars], y = d$high_next, probability = TRUE,
                 num.trees = 500, mtry = min(mtry, length(vars)),
                 min.node.size = min_node, importance = "permutation", seed = 42)
}
predict_rf <- function(m, d) {
  predict(m, d[, m$forest$independent.variable.names])$predictions[, "high"]
}