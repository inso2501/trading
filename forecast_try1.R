library(quantmod)
USDAUD <- getSymbols("DEXUSAL", src = "FRED")


library(forecast)
fit1 <- rwf(EuStockMarkets[1:200,1],h=100)
fit2 <- meanf(EuStockMarkets[1:200,1],h=100)
accuracy(fit1)
accuracy(fit2)
accuracy(fit1,EuStockMarkets[201:300,1])
accuracy(fit2,EuStockMarkets[201:300,1])
plot(fit1)
lines(EuStockMarkets[1:300,1])
plot(fit2)
lines(EuStockMarkets[1:300,1])
