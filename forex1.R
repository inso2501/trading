library(quantmod)
start <- as.Date("2016-01-01")
end <- as.Date("2017-01-31")
USDAUD <- getSymbols("DEXUSEU", src = "FRED", from = start, to = end)
View(DEXUSEU)
EURUSD<-unclass(DEXUSEU)
View(DEXUSEU)
class(DEXUSEU)
head(DEXUSEU)
tail(DEXUSEU)
plot(DEXUSEU)

library(forecast)

X1=ts(DEXUSEU,frequency(12))  
plot.ts(X1)
p1=auto.arima(X1)
summary(p1)
q=forecast(p1,h=30)
q

plot(q)



EUUSA <- getSymbols("DEXUSEU", src = "FRED")
GBRUSA <- getSymbols("DEXUSUK", src = "FRED")
USAJPN <- getSymbols("DEXJPUS", src = "FRED")
EUUSA <- unclass("DEXUSEU")
GBRUSA <- unclass("DEXUSUK")
USAJPN <- unclass("DEXJPUS")

JPNUSA=1/USAJPN
