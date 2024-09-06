source("cbproj.R", local=T)

library(forecast)
library(xts)
library(zoo)
library(mice)

ts_list <- list()

evsales_bymaker$date <- as.Date(evsales_bymaker$date, format = "%d-%b-%Y")
evsales_bymaker$date <- as.Date(format(evsales_bymaker$date, "20%y-%m-%d"))

evsales_bystate$date <- as.Date(evsales_bystate$date, format = "%d-%b-%Y")
evsales_bystate$date <- as.Date(format(evsales_bystate$date, "20%y-%m-%d"))


ts_bystate_plot <- function(states) {
  states <- c("Delhi", "Andhra Pradesh")
  # Check if states is empty
  if (length(states) == 0) {
    return(NULL)
  }
  filtered_data <- evsales_bystate %>% filter(state %in% states) %>% group_by(state, date, year, month) %>% 
    reframe(electric_vehicles_sold = sum(electric_vehicles_sold))
  
  filtered_data$date <- as.Date(filtered_data$date, format = "%d-%b-%Y")
  filtered_data$date <- as.Date(format(filtered_data$date, "20%y-%m-%d"))
  filtered_data <- filtered_data %>% arrange(state, date)
  
  ts_plot1 <- ggplot(filtered_data, aes(x = date, y = electric_vehicles_sold, color = state, group = state)) +
    geom_line(linewidth = 1) +  # Add lines
    geom_point(size = 2) +   # Optional: Add points
    scale_color_manual(values = rainbow(length(states))) +  # Set custom colors
    labs(title = "Monthly Electric Vehicles Sold Every Year by State",
         x = "Year",
         y = "Electric Vehicles Sold") +
    theme_minimal() + 
    theme(plot.background = element_blank(),
      axis.text.x = element_text(family = 'Helvetica', face = 'italic', angle = 45, hjust = 1),
      axis.title = element_text(family = 'Comic Sans MS', size = 12),
      legend.position = c(0.95, 0.95)) 
  
  return (ts_plot1)
}

ts_bymaker_plot <- function (makers) {
  if(length(makers) == 0) {
    return (NULL)
  }
  filtered_data <- evsales_bymaker %>% filter(maker %in% makers) %>% group_by(maker, date, year, month) %>% 
    reframe(electric_vehicles_sold = sum(electric_vehicles_sold))
  
  filtered_data$date <- as.Date(filtered_data$date, format = "%d-%b-%Y")
  filtered_data$date <- as.Date(format(filtered_data$date, "20%y-%m-%d")) 
  filtered_data <- filtered_data %>% arrange(maker, date)
  
  ts_plot1 <- ggplot(filtered_data, aes(x = date, y = electric_vehicles_sold, color = maker, group = maker)) +
    geom_line(linewidth = 1) +  # Add lines
    geom_point(size = 2) +   # Optional: Add points
    scale_color_manual(values = rainbow(length(makers))) +  # Set custom colors
    labs(title = "Monthly Electric Vehicles Sold Every Year by Maker",
         x = "Year",
         y = "Electric Vehicles Sold") +
    theme_minimal() + 
    theme(plot.background = element_blank(),
          axis.text.x = element_text(family = 'Helvetica', face = 'italic', angle = 45, hjust = 1),
          axis.title = element_text(family = 'Comic Sans MS', size = 12),
          legend.position = c(0.95, 0.95)) 
  
  return (ts_plot1)
}


# Time Series Models: Makers
ts_models_maker <- function(maker, train) {
  ts_data <- evsales_bymaker[evsales_bymaker$maker == maker, ] 
  ts_data <- ts_data[order(ts_data$date, decreasing = F), ]
  # Impute data
  ts_data$electric_vehicles_sold <- ifelse(ts_data$electric_vehicles_sold == 0, median(ts_data$electric_vehicles_sold, na.rm = TRUE), 
                                           ts_data$electric_vehicles_sold)
  # Create Time Series Object
  maker_ts <- ts(ts_data$electric_vehicles_sold, start = c(2021, 4), frequency = 12)
  nTrain <- as.numeric(train)
  nValid <- length(maker_ts) - nTrain
  train_maker <- window(maker_ts, start = c(2021,4), end = c(2021, nTrain))
  vali_maker <- window(maker_ts, start = c(2021, nTrain + 1), end = c(2021, nTrain + nValid))
  
  # Fitting different Models
  # Trend Model
  trend_maker <- tslm(train_maker ~ trend)
  trend_pred <- forecast(trend_maker, h = nValid, level = 0)
  
  # Seasonality Model
  season_maker <- tslm(train_maker ~ trend + season)
  season_pred <- forecast(season_maker, h = nValid, level = 0)
  
  # Polynomial Season Model
  poly_season_maker <- tslm(train_maker ~ trend + I(trend)^2 + season)
  poly_season_pred <- forecast(poly_season_maker, h = nValid, level = 0)
  
  # Exponential Season Model
  expo_season_maker <- tslm(train_maker ~ trend + season, lambda = 0)
  expo_season_pred <- forecast(expo_season_maker, h=nValid, level = 0)
  
  # Add Smoothing Methods
  # Simple Exponential Smoothing
  ses_maker <- ets(train_maker, model = "ANN")
  ses_model_pred <- forecast(ses_maker, h=nValid, level = 0)
  
  # ARIMA Model
  arima_model <- auto.arima(train_maker)
  arima_pred <- forecast(arima_model, h = nValid, level = 0)
  
  # Computing accuracies
  trend_accr <- accuracy(trend_pred$mean, vali_maker)
  season_accr <- accuracy(season_pred$mean, vali_maker)
  poly_season_accr <- accuracy(poly_season_pred$mean, vali_maker)
  exp_season_accr <- accuracy(expo_season_pred$mean, vali_maker)
  ses_accr <- accuracy(ses_model_pred$mean, vali_maker)
  arima_accr <- accuracy(arima_pred$mean, vali_maker)
  
  # Storing accuracies in a dataframe
  accr_df <- rbind.data.frame(trend_accr, season_accr, poly_season_accr, exp_season_accr, 
                               ses_accr,  arima_accr)
  rownames(accr_df) <- c("Trend Accuracy", "Season Accuracy", "Polynomial Trend +Season Accuracy", 
                         "Exponential Season Accuracy", 
                         "Simple Exponential Smoothing Accuracy",
                         "Arima Model Accuracy")
  
  return (accr_df)
  
}

# Time Series Models: States
ts_models_state <- function(state, train){
  ts_data <- evsales_bystate[evsales_bystate$state == state, ] 
  ts_data <- ts_data[order(ts_data$date, decreasing = F), ]
  ts_data[ts_data == 0] <- NA
  imputed_data <- mice(ts_data[, c(4:5)], m = 1, method = 'pmm', maxit = 10, print = F)
  ts_data[, c(4:5)] <- complete(imputed_data)
  state_ts <- ts(ts_data$electric_vehicles_sold, start = c(2021, 4), frequency = 12)
  nTrain <- as.numeric(train)
  nValid <- length(state_ts) - nTrain
  train_state <- window(state_ts, start = c(2021,4), end = c(2021, nTrain))
  vali_state <- window(state_ts, start = c(2021, nTrain + 1), end = c(2021, nTrain + nValid))
  
  
  # Fitting different Models
  # Trend Model
  trend_state <- tslm(train_state ~ trend)
  trend_pred <- forecast(trend_state, h = nValid, level = 0)
  
  # Seasonality Model
  season_state <- tslm(train_state ~ trend + season)
  season_pred <- forecast(season_state, h = nValid, level = 0)
  
  # Polynomial Season Model
  poly_season_state <- tslm(train_state ~ trend + I(trend)^2 + season)
  poly_season_pred <- forecast(poly_season_state, h = nValid, level = 0)
  
  # Exponential Season Model
  expo_season_state <- tslm(train_state ~ trend + season, lambda = 0)
  expo_season_pred <- forecast(expo_season_state, h=nValid, level = 0)
  
  # Add Smoothing Methods
  # Simple Exponential Smoothing
  ses_state <- ets(train_state, model = "ANN")
  ses_model_pred <- forecast(ses_state, h=nValid, level = 0)
  
  # ARIMA Model
  arima_model <- auto.arima(train_state)
  arima_pred <- forecast(arima_model, h = nValid, level = 0)
  
  # Computing accuracies
  trend_accr <- accuracy(trend_pred$mean, vali_state)
  season_accr <- accuracy(season_pred$mean, vali_state)
  poly_season_accr <- accuracy(poly_season_pred$mean, vali_state)
  exp_season_accr <- accuracy(expo_season_pred$mean, vali_state)
  ses_accr <- accuracy(ses_model_pred$mean, vali_state)
  arima_accr <- accuracy(arima_pred$mean, vali_state)
  
  # Storing accuracies in a dataframe
  accr_df <- rbind.data.frame(trend_accr, season_accr, poly_season_accr, exp_season_accr, 
                              ses_accr,  arima_accr)
  rownames(accr_df) <- c("Trend Accuracy", "Season Accuracy", "Polynomial Trend +Season Accuracy", 
                         "Exponential Season Accuracy", 
                         "Simple Exponential Smoothing Accuracy",
                         "Arima Model Accuracy")
  
  return (accr_df)
}
