library(forecast)
nifty <- read.csv(file.choose())
nifty
candleChart(nifty_ts, theme = "white")
nifty_ts <- ts(nifty)
nifty_ts
str(nifty_ts)
plot.ts(nifty_ts)

acf(nifty_ts,lag.max = 10)
nifty_arima <- auto.arima(nifty_ts)


library(quantmod)
start <- as.Date("2016-01-01")
end <- as.Date("2017-01-31")
getSymbols("AAPL", src = "yahoo", from = start, to = end)
class(AAPL)
head(AAPL)
plot(AAPL)
plot(AAPL[, "AAPL.Close"], main = "AAPL")
candleChart(AAPL, theme = "white")

getSymbols(c("MSFT","GOOG"), from = start, to = end)
stocks <- as.xts(data.frame(AAPL = AAPL[, "AAPL.Close"], MSFT = MSFT[, "MSFT.Close"], 
                            GOOG = GOOG[, "GOOG.Close"]))
head(stocks)
plot(as.zoo(stocks), screens = 1, lty = 1:3, xlab = "Date", ylab = "Price")
legend("right", c("AAPL", "MSFT", "GOOG"), lty = 1:3, cex = 0.5)
library(magrittr)
importFrom(magrittr,"%>%")
stock_return = apply(stocks, 1, function(x) {x / stocks[1,]}) %>% 
  t %>% 

head(stock_return)
