library(tidymodels)
library(modeltime)
library(tidyverse)
library(lubridate)
library(timetk)

# This toggles plots from plotly (interactive) to ggplot (static)
interactive <- FALSE

df <- read_csv("AirPassengers.csv")
df %>% view()
df %>% glimpse()

df$Month <- df$Month %>% 
  str_replace_all("-"," ")

for (n in ncol(df)) {
  df$Month <- df$Month %>% paste("01")
}

df$Month <- df$Month %>% 
  str_replace_all(" ","-")

df$Month <- df$Month %>% as.Date()

names(df) <- names(df) %>% 
  str_replace_all("#","")

df %>%
  plot_time_series(Month, Passengers, .interactive = interactive)

splits <- initial_time_split(df, prop = 0.9)

# 1.Use arima_boost(), exp_smoothing(), prophet_reg() models;
model_fit_arima_boosted <- arima_boost(
  min_n = 2,
  learn_rate = 0.015
) %>%
  set_engine(engine = "auto_arima_xgboost") %>%
  fit(Passengers ~ Month + as.numeric(Month) + factor(month(Month, label = TRUE), ordered = F),
      data = training(splits))

model_fit_ets <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(Passengers ~ Month, data = training(splits))

model_fit_prophet <- prophet_reg() %>%
  set_engine(engine = "prophet") %>%
  fit(Passengers ~ Month, data = training(splits))

# 2.Compare RMSE scores on test set;
models_tbl <- modeltime_table(
  model_fit_arima_boosted,
  model_fit_ets,
  model_fit_prophet
)

models_tbl

calibration_tbl <- models_tbl %>%
  modeltime_calibrate(new_data = testing(splits))

calibration_tbl
calibration_tbl %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = m750
  ) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25, # For mobile screens
    .interactive      = interactive
  )

calibration_tbl %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = interactive
  )

# 3.Make forecast on lowest RMSE score model;
# En asagi rmse Arima-da oldugu ucun (19.36) onu secirik
models_tbl <- modeltime_table(
  model_fit_arima_boosted)
calibration_tbl <- models_tbl %>%
  modeltime_calibrate(new_data = testing(splits))

refit_tbl <- calibration_tbl %>%
  modeltime_refit(data = df)

# 4.Visualize past data and forecast values on one plot; make separation with two different colors.
refit_tbl %>%
  modeltime_forecast(h = "3 years", actual_data = df) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25, # For mobile screens
    .interactive      = interactive
  )
