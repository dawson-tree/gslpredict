
#' @export
lagged_data <- function(var,
                        lag,
                        start = "1942-1943") {
  var_historic <- NULL
  if (var == "gsl_change_cal") {
    var_historic <- gsl_change_historic_cal
  } else if (var == "gsl_change_water"){
    var_historic <- gsl_change_historic_water
  } else if (var == "prcp_cal"){
    var_historic <- clean_prcp_historic_cal
  } else if (var == "prcp_water"){
    var_historic <- clean_prcp_historic_water
  } else if (var == "sst_cal"){
    var_historic <- sst_historic_cal
  } else if (var == "sst_water"){
    var_historic <- sst_historic_water
  }

  # ref <-

  var_historic[
    (which(names(gsl_change_historic) == start) - lag):
      (length(gsl_change_historic) - lag)
  ]

}

# lagged_data <- function(var,
#                         lag,
#                         start = "1942-1943") {
#   var_historic <- NULL
#   if (var == "gsl_change") {
#     var_historic <- gsl_change_historic
#   } else if (var == "prcp"){
#     var_historic <- cleaned_prcp_historic
#   } else if (var == "sst"){
#     var_historic <- sst_historic[-1]
#   }
#
#   var_historic[
#     (which(names(gsl_change_historic) == start) - lag):
#       (length(gsl_change_historic) - lag)
#   ]
#
# }

#' @export
data_split <- function(df) {
  n_col <- ncol(df)
  train <- df[1:65,]
  test <- df[66:83,]

  full_x <- as.matrix(df[, 1:(n_col-1)])
  rownames(full_x) <- NULL

  train_x <- as.matrix(train[, 1:(n_col-1)])
  rownames(train_x) <- NULL

  test_x = as.matrix(test[, 1:(n_col-1)])
  rownames(test_x) <- NULL

  list(
    full_x = full_x,
    full_y = df$gsl_change_target,
    train_x = train_x,
    train_y = train$gsl_change_target,
    test_x = test_x,
    test_y = test$gsl_change_target,
    test_years = seq(from = 2008, length.out = 18)
  )
}




#' @export
predict_arima <- function(arima_model,
                          data) {
  validation_filter <- Arima(data$full_y,
                             model = arima_model,
                             xreg = data$full_x)
  all_fitted <- fitted(validation_filter)
  test_pred_y <- tail(all_fitted, length(data$test_y))
  test_pred_y
}

#' @export
rmse <- function(actual_y,
                 pred_y) {
  rmse <- sqrt(mean((actual_y - pred_y)^2))
  rmse
}


#' @export
plot_test <- function(arima_model,
                      data,
                      title = "Actual vs Predicted GSL Elevation Change by Year") {
  test_pred_y <- predict_arima(arima_model, data)
  test_rmse <- rmse(data$test_y,
                    test_pred_y)

  plot(data$test_years, data$test_y, type = "l", ylim = c(-5.25, 4.25),
       xlab = "Year",
       ylab = "GSL Elevation Change",
       main = title)
  lines(data$test_years, test_pred_y, lty = 2)
  legend("bottomleft",
         legend = c("Actual", paste0("Predicted (RMSE: ",
                                     round(test_rmse, 3),
                                     ")")),
         lty = c(1, 2))

}



#' @export
forecast_arima <- function(arima_model,
                           data,
                           n_years = 8) {
  lapply(1:11, function(start_i) {
    end_i <- start_i + n_years - 1

    # Shift the training window forward by appending test data up to the current start_i - 1
    # (When start_i = 1, this gracefully defaults back to just your original training data)
    current_train_y <- c(data$train_y, data$test_y[seq_len(start_i - 1)])
    current_train_x <- rbind(data$train_x, data$test_x[seq_len(start_i - 1), , drop = FALSE])

    # Refit the model to the expanded data USING THE FROZEN COEFFICIENTS from spec_arima
    current_model <- Arima(current_train_y,
                           model = arima_model,
                           xreg = current_train_x)

    # Forecast the n_years-step window ahead using the correct test xreg slice
    forecast_n <- forecast(current_model,
                           xreg = data$test_x[start_i:end_i, , drop = FALSE])
    predicted <- as.numeric(forecast_n$mean)

    data.frame(
      forecast = start_i,
      test_year = (start_i:end_i) + 2007,
      forecast_idx = 1:n_years,
      actual = data$test_y[start_i:end_i],
      predicted = predicted,
      error = data$test_y[start_i:end_i] - predicted,
      abs_error = abs(data$test_y[start_i:end_i] - predicted)
    )
  })

}



#' @export
forecast_rmse <- function(forecast_df) {
  lapply(forecast_df, function(df) {
    sqrt(mean(df$error^2))
  }) |>
    unlist() |>
    mean()
}






