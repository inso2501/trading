library(forecast)
rates <- read.csv("eurofxref-hist.csv", header = T)
rates[1:2,]
str(rates$Date)
start <- as.Date("2009-01-02")
rates$Date <- as.Date(rates$Date,"%Y-%m-%d")
str(rates$Date)
range(rates$Date)
rates <- rates[order(rates$Date), ]

plot(rates$Date, rates$HKD, type = "l")
head(rates$Date,20)

years <- format(rates$Date, "%Y")
tab <- table(years)
tab

mean(tab[1:(length(tab) - 1)])

## Forecast with ARIMA model
forecastArima <- function(x, n.ahead=30)
{
  yolo <- ts(x$HKD, start=1, frequency=256)
  fit.arima <- arima(yolo, order=c(0,0,1))
  fore <- forecast(fit.arima, h=n.ahead)
  plot(fore)
  upper <- fore$upper[,'95%']
  lower <- fore$lower[,'95%']
  trend <- as.numeric(fore$fitted)
  pred <- as.numeric(fore$mean)
  output <- data.frame(actual = c(x$AUD, rep(NA, n.ahead)),
                       trend = c(trend, rep(NA, n.ahead)),
                       #pred = c(trend, pred),
                       pred = c(rep(NA, nrow(x)), pred),
                       lower = c(rep(NA, nrow(x)), lower),                       
                       upper = c(rep(NA, nrow(x)), upper),                       
                       date = c(x$Date, max(x$Date) + (1:n.ahead))  
  )
  return(output)
}
result.arima <- forecastArima(rates, n.ahead = 90)

plotForecastResult <- function(x, title=NULL) {
  x <- x[order(x$date),]
  max.val <- max(c(x$actual, x$upper), na.rm=T)
  min.val <- min(c(x$actual, x$lower), na.rm=T)
  plot(x$date, x$actual, type="l", col="grey", main=title,
       xlab="Time", ylab="Exchange Rate",
       xlim=range(x$date), ylim=c(min.val, max.val))
  grid()
  lines(x$date, x$trend, col="yellowgreen")
  lines(x$date, x$pred, col="green")
  lines(x$date, x$lower, col="blue")
  lines(x$date, x$upper, col="blue")
  legend("bottomleft", col=c("grey", "yellowgreen", "green", "blue"), lty= 1,
         c("Actual", "Trend", "Forecast", "Lower/Upper Bound"), cex = 0.5)
}

plotForecastResult(result.arima, title = "Exchange rate forecasting with ARIMA")

## Forecast with STL model
forecastStl <- function(x, n.ahead=30){
  myTs <- ts(x$HKD, start=1, frequency=256)
  fit.stl <- stl(myTs, s.window=256)
  sts <- fit.stl$time.series
  trend <- sts[,"trend"]
  fore <- forecast(fit.stl, h=n.ahead, level=95)
  plot(fore)
  pred <- fore$mean
  upper <- fore$upper
  lower <- fore$lower
  output <- data.frame(actual = c(x$AUD, rep(NA, n.ahead)),
                       trend = c(trend, rep(NA, n.ahead)),
                       #pred = c(trend, pred),
                       pred = c(rep(NA, nrow(x)), pred),
                       lower = c(rep(NA, nrow(x)), lower),                       
                       upper = c(rep(NA, nrow(x)), upper),                       
                       date = c(x$Date, max(x$Date) + (1:n.ahead))
  )
  return(output)
}

result.stl <- forecastStl(rates, n.ahead = 90)
plotForecastResult(result.stl, title = "Exchange rate forecasting with STL")

result <- subset(result.stl, date >= "2014-01-01")
plotForecastResult(result, title = "Exchange rate forecasting with STL (2014)")


