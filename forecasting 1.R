#simple forecasting
AirPassengers
passenger <- ts(AirPassengers, start = c(1949))
passenger
plot.ts(passenger)

passenger_hw <-HoltWinters(passenger,beta = F,gamma = F)
passenger_hw

passenger_hw$fitted

library(forecast)
passenger_hw_forecast <- forecast:::forecast.HoltWinters(passenger_hw,h=5)
passenger_hw_forecast
forecast:::plot.forecast(passenger_hw_forecast)
#alpha value close to 1 means relatively recent value was taken to smoothen the time plot
#data having only trend component
sales_ts
sales_ts <- ts(sales, start = c(2000),frequency = 1)
plot.ts(sales_ts)
sales_yearly_forecst <-HoltWinters(sales_ts,gamma = F)
sales_yearly_forecst
plot(sales_yearly_forecst)#red line indicates forecasted values
#forecasting vlaues
sale_forecast <- forecast:::forecast.HoltWinters(sales_yearly_forecst,h=10)
forecast:::plot.forecast(sale_forecast)
#dark blue is actual value forecast, lighter blue is 80% confidence region, even lighter blue is 95% confidence

#for data with both trend and seasonal values
AirPassengers
plot.ts(AirPassengers)
AirPassengers_hw <-HoltWinters(AirPassengers)
plot(AirPassengers_hw)

AirPassengers_hw
AirPassengers_forecast <- forecast:::forecast.HoltWinters(AirPassengers_hw,h=25)
forecast:::plot.forecast(AirPassengers_forecast)
#simple testing for validation of the model
plot.ts(AirPassengers_forecast$residuals)
#if it shows constant variance and trend then it is a good model to use

#for testing
acf(na.exclude(AirPassengers_forecast$residuals,lag.max = 25))
pacf(AirPassengers_forecast, type = "o")
Box.test(AirPassengers_forecast$residuals,lag = 25,type = "Ljung-Box")# A small p-value is evidence that there is dependence. So you want to see large p-values.But a large p-value is not really evidence of independence -- merely a lack of evidence of dependence.
hist(AirPassengers_forecast$residuals)
