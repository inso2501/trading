library(ggplot2)
library(forecast)
library(tseries)
daily_data <- read.csv(file.choose())
daily_data
daily_data$Date = as.Date(daily_data$dteday)
ggplot(daily_data, aes(Date, cnt)) + geom_line() + scale_x_date('month')  + ylab("Daily Bike Checkouts") + xlab("")
count_ts = ts(daily_data[,c('cnt')])
count_ts
daily_data$clean_nt = tsclean(count_ts)
ggplot() + geom_line(data = daily_data, aes(x = Date, y = clean_nt)) + ylab('cleaned Bicylce count')
daily_data$ma = ma(daily_data$clean_nt, order = 7)
daily_data$ma30 = ma(daily_data$clean_nt, order = 30)

ggplot() +
  geom_line(data = daily_data, aes(x = Date, y = clean_nt, colour = "Counts")) +
  geom_line(data = daily_data, aes(x = Date, y = ma,   colour = "Weekly Moving Average"))  +
  geom_line(data = daily_data, aes(x = Date, y = ma30, colour = "Monthly Moving Average"))  +
  ylab('Bicycle Count')

count_ma = ts(na.omit(daily_data$ma),frequency = 30)
decomp = stl(count_ma, s.window = "periodic")
deseasonal_cnt <- seasadj(decomp)
plot(decomp)

adf.test(count_ma) #there is 99% chance that the series has unit root and is non stationary

acf(count_ma)
pacf(count_ma)

count_d1 = diff(deseasonal_cnt, differences = 1)
plot(count_d1)
adf.test(count_d1)
acf(count_d1, main = 'ACF for differenced series')
pacf(count_d1, main = 'PACF for differenced series')

auto.arima(deseasonal_cnt, seasonal = F) #Using the ARIMA notation introduced above, the fitted model can be written as Y{d_t} = 0.551 Y{t-1} - 0.2496 e{t-1} + E where E is some error and the original series is differenced with order 1. AR(1) coefficient p = 0.551 tells us that the next value in the series is taken as a dampened previous value by a factor of 0.55 and depends on previous error lag.

fit1 <- auto.arima(deseasonal_cnt, seasonal = F)
tsdisplay(residuals(fit1), lag.max = 45, main = '(1,1,1) Model Residuals')

fit2 = arima(deseasonal_cnt, order = c(1,1,7))
fit2
tsdisplay(residuals(fit2),lag.max = 15, main = 'seasonal model residuals')

fit3 = arima(deseasonal_cnt, order = c(7,1,1))
fit3
tsdisplay(residuals(fit3),lag.max = 15, main = 'seasonal model residuals')

#ACF and PACF for arima 1,1,7 is between accepted lines so fit2 is better model

frcst <-forecast(fit1, h=30)
plot(frcst)
frcst2 <-forecast(fit2, h=30)
plot(frcst2) #but plot show linear forecasting which is not gonna happen, since we used no seasonal component thats why its inaccurate

hold <- window(ts(deseasonal_cnt), start=700)

fit_no_holdout = arima(ts(deseasonal_cnt[-c(700:725)]), order=c(1,1,7))

fcast_no_holdout <- forecast(fit_no_holdout,h=25)
plot(fcast_no_holdout, main=" ")
lines(ts(deseasonal_cnt))

#with seasonality
fit_season <- auto.arima(deseasonal_cnt, seasonal = T)
fit_season
frcst_season <- forecast(fit_season, h= 15)
plot(frcst_season)
tsdisplay(residuals(frcst_season),lag.max = 15, main = 'seasonal model residuals')
