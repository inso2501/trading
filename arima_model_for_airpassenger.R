AirPassengers
library(forecast)
mydata <- ts(AirPassengers)
AirPassengers
str(mydata)
plot.ts(AirPassengers)
plot.ts(mydata)
AirPassengers_diff1 <- diff(AirPassengers, differences = 1)
plot.ts(AirPassengers_diff1)

AirPassengers_diff2 <- diff(AirPassengers,differences = 2)
plot.ts(AirPassengers_diff2)

acf(AirPassengers_diff2,lag.max = 20)
pacf(AirPassengers_diff2,lag.max = 20)

passenger_model <- arima(AirPassengers, order = c(2,2,2))
passenger_model
Box.test(passenger_model$residuals, lag = 20, type = "Ljung-Box")
accuracy(passenger_model)
passenger_model_forecast <- forecast(passenger_model, h=4)
plot(passenger_model_forecast)
passenger_model_forecast


AirPassengers_arima <- auto.arima(AirPassengers)
AirPassengers_arima
AirPassengers_arima_forecast <- forecast(AirPassengers_arima, h=4)
AirPassengers_arima_forecast
plot(AirPassengers_arima_forecast)
