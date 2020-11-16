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
adf.test(t2) #Data stasioner setelah di differencing 2 kali

#Penentuan Orde ARIMA
acf(t2)
pacf(t2)
arima(1,2,2)

#ARIMA cari dari plot acf pacf
fit1=arima(saham_tseries,order=c(0,2,0))
fit1
fit2=arima(saham_tseries,order=c(0,2,1))
fit2
fit3=arima(saham_tseries,order=c(0,2,2))
fit3
fit4=arima(saham_tseries,order=c(1,2,0))
fit4
fit5=arima(saham_tseries,order=c(1,2,1))
fit5
fit6=arima(saham_tseries,order=c(1,2,2))
fit6

#Uji Signifikansi Parameter
n=length(saham_tseries)
p=1
ttabel=qt(c(0.05/2),df=n-1,lower.tail=FALSE)


#cek fit2
thit=fit2$coef/0.1519	#|thitung|=koefisien taksiran/standard error
abs(thit)>ttabel
##fit2 signifikan

#cek fit3
thit=fit3$coef[1]/0.2074	#|thitung|=koefisien taksiran/standard error
thit=fit3$coef[2]/0.2260	#|thitung|=koefisien taksiran/standard error
abs(thit)>ttabel
##fit3 tidak signifikan

#cek fit4
thit=fit4$coef/0.1757	#|thitung|=koefisien taksiran/standard error
abs(thit)>ttabel
## fit4 signifikan

#cek fit5
thit=fit5$coef[1]/0.2489	#|thitung|=koefisien taksiran/standard error
thit=fit5$coef[2]/0.1928	#|thitung|=koefisien taksiran/standard error
abs(thit)>ttabel
##fit5 tidak signifikan

#cek fit6
thit=fit6$coef[1]/0.9858	#|thitung|=koefisien taksiran/standard error
thit=fit6$coef[2]/0.9552
thit=fit6$coef[3]/0.8602#|thitung|=koefisien taksiran/standard error
abs(thit)>ttabel
##fit6 tidak signifikan

##fit 2 dan fit 4 signifikan

#Diagnostik Check

##Residual
res1=residuals(fit2)
res2=residuals(fit4)

##Uji Normalitas
#menggunakan kolmogorof smirnov
#Hipotesis
#H0 : Data mengikuti distribusi tertentu
#H1 : Data tidak mengikuti distribusi tertentu

#Tolak H0 Jika Pvalue <= 0.05
n1=length(res1)
mean1=mean(res1)
sd1=sd(res1)
resn1=rnorm(n1,mean1,sd1)
ks.test(res1,resn1)

n2=length(res2)
mean2=mean(res2)
sd2=sd(res2)
resn2=rnorm(n2,mean2,sd2)
ks.test(res2,resn2)

#White Noise gabungan dari normalitas dan homogenitas
library(lmtest)

#homoskedastisitas
#Hipotesis H0:Homoskedastisitas H1:heteroskedastisitas
Box.test(res1^2,lag=1,type="Ljung-Box")
Box.test(res2^2,lag=1,type="Ljung-Box")

#Pemilihan Model Terbaik
##error
e1=res1
e2=res2
##MSE
mse1=sum(e1^2)/25
mse1
mse2=sum(e2^2)/25
mse2
##MAPE
mape1=(sum(abs(e1/saham_tseries))/25)*100
mape1
mape2=(sum(abs(e2/saham_tseries))/25)*100
mape2

##AIC
AIC1=fit2$aic
AIC1
AIC2=fit4$aic
AIC2


#Peramalan
##misalnya yg paling kecil MAPE,MSE,AIC
fit2=arima(saham_tseries,order=c(0,2,1))
peramalan=forecast(fit2,3) #untuk meramal 3 periode ke depan
peramalan
plot(peramalan)
