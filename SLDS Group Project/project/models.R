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

MIN_POS <- 10   # minimum high days needed to fit a classifier

fit_lasso <- function(d, use_fire) {
  X <- make_X(d, use_fire)
  y <- d$high_next == "high"
  # Year-blocked CV only if every leave-one-year-out subset still has >= 2 positives
  yr <- as.integer(factor(d$year))
  ok_blocks <- all(map_lgl(unique(yr), \(k) sum(y[yr != k]) >= 2))
  cv <- if (ok_blocks) {
    glmnet::cv.glmnet(X, y, family = "binomial", alpha = 1, foldid = yr)
  } else {
    glmnet::cv.glmnet(X, y, family = "binomial", alpha = 1, nfolds = 5)
  }
  list(cv = cv, use_fire = use_fire, blocked = ok_blocks)
}

score_models <- function(train, test) {
  n_pos <- sum(train$high_next == "high")
  clim  <- train |> group_by(wk = week(date)) |> summarise(clim = mean(pm25_next))
  
  out <- test |>
    mutate(wk = week(date)) |>
    left_join(clim, by = "wk") |>
    transmute(date, region, truth = high_next, pm25_next,
              B1 = pm25, B2 = coalesce(clim, mean(train$pm25_next)))
  
  if (n_pos < MIN_POS) {
    message("Only ", n_pos, " high days in training: classifiers skipped (NA).")
    return(mutate(out, M1 = NA_real_, M2 = NA_real_, M3 = NA_real_, M4 = NA_real_))
  }
  out |>
    mutate(M1 = predict_lasso(fit_lasso(train, FALSE), test),
           M2 = predict_lasso(fit_lasso(train, TRUE),  test),
           M3 = predict_rf(fit_rf(train, FALSE), test),
           M4 = predict_rf(fit_rf(train, TRUE),  test))
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