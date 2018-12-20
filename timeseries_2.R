sales <- c(453735,465404,474742,477841,501775,503578,521750,562246,572453,592955,607816,614864,656448,658781,690422,708860)
sales_ts <- ts(sales, start = 2004,end = 2017, frequency = 1)
str(sales_ts)
plot(sales_ts)


#transformation of time series in case of seasonal or random fluctuations
sales_ts_log <- log(sales_ts)
plot(sales_ts_log)

#Decomposing time series

library(TTR)

#In case fluctuations are present and you want to smooth the time series

orderval <- c(1580,1560,1750,1407,1309,1424,1676,1936,1684,1488,1562,1618,1686,1840,1865,1636,1652,1699,1696,1545)

orderval_ts <- ts(orderval, start = 1996,end = 2015,frequency = 1)

plot.ts(orderval_ts)


#to see a trend component use SMA-simple moving average, here 3 is no. of period
#last 3 periods, the highr the period value more data points you lose
orderval_sma3 <- SMA(orderval_ts,3)
plot(orderval_sma3)

orderval_sma3 <- SMA(orderval_ts,5)
plot(orderval_sma3)

#Exponential smoothing
#here ratio implies how much  importance is given to recent price data
orderval_ema3_25 <- EMA(orderval_ts,3,ratio = .25)

plot.ts(orderval_ema3_25)



orderval_ema3_5 <- EMA(orderval_ts,3, ratio = .5)

plot.ts(orderval_ema3_5)



orderval_ema3_75 <- EMA(orderval_ts,3, ratio = .75)

plot.ts(orderval_ema3_75)


#decomposition
sales <- c(453735,465404,474742,477841,501775,503578,521750,562246,572453,592955,607816,614864,656448,658781,690422,708860)
sales_ts <- ts(sales, start = 2000,end = 2017, frequency = 4)
sales_ts

plot.ts(sales_ts)
sales_ts_decomp <- decompose(sales_ts)
plot(sales_ts_decomp)

#if you want to remove a component from the graph
sales_ts_wo_random <- sales_ts - sales_ts_decomp$random

