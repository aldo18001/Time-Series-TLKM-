#Cek Stasioner Data dalam Varian
library(lmtest)
library(tseries)
library(forecast)
adf.test(saham_tseries)

#differencing sekali
t1=diff(saham_tseries,1)
adf.test(t1)

#differencing sekali lagi
t2=diff(t1,1)
adf.test(t2)

#Penentuan Orde ARIMA
acf(t2)
pacf(t2)
arima(3,1,2)

#ARIMA cari dari plot acf pacf
fit1=arima(sts1,order=c(1,1,0))
fit1
fit2=arima(sts1,order=c(0,1,1))
fit2
fit3=arima(sts1,order=c(1,1,1))
fit3
fit4=arima(sts1,order=c(3,1,0))
fit4
#tidak signifikan
fit5=arima(sts1,order=c(3,1,1))
fit5
#tidak signifikan
fit6=arima(sts1,order=c(3,1,2))
fit6
#tidak signifikan
fit7=arima(sts1,order=c(0,1,2))
fit7
#tidak signifikan
fit8=arima(sts1,order=c(1,1,2))
jfit8
