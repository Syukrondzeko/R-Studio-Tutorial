data1=arima.sim(n=50,model=list(order=c(0,0,1),ma=0.1,sd=0.1))
#write.csv(data1, file = "data_arima.csv", fileEncoding = "macroman")
data1=read.csv("C:/Users/HP PC/Downloads/8. Arima/data_arima.csv")
View(data1)
data1=data1$x

library(timeSeries)
data1=as.ts(data1)

str(data1)

plot.ts(data1)


#stasioneitas terhadap varian 
#library(EnvStats)
#box.cox()
#atau log(data1)
#transformasi jika tidak stasioner

library(tseries)
adf.test(data1)

#jika tidak stasioner di diff1=diff(data1)

acf(data1)
pacf(data1)

fit=arima(data1,c(0,0,1))
fit

#Uji Asumsi
myresid=fit$residuals
qqnorm(myresid)

library(FitAR)
boxresult<-LjungBoxTest(myresid)
plot(boxresult[,3],main="Ljung-Box_Test",xlab="Lag",ylab="p-value")

Box.test(myresid,type="Ljung-Box")

#Prediksi
pred<-predict(fit,n.ahead=5)
ts.plot(data1,pred$pred)
data1
pred$pred
pred


#SARIMA
AirPassengers
str(AirPassengers)
ts.plot(AirPassengers)
components.ts = decompose(AirPassengers)
plot(components.ts)

acf(AirPassengers)
pacf(AirPassengers)
x=log(AirPassengers)
plot(diff(x))
data=diff(diff(x,12))
plot(data)
acf(data)
pacf(data)

library(sarima)
library(astsa)

#tmp.u <- sarima(log(AirPassengers) ~ 0 | ma(1, c(-0.3)) + sma(12,1, c(-0.1)) + i(2) + s(2) + u((1:5)/12), ss.method = "base")
#ap.arima <- arima(log(AirPassengers), order = c(0,1,1), seasonal = c(0,1,1))

model1=sarima(x,0,1,1,0,1,1,12)
model1
sarima.for(x,24,0,1,1,0,1,1,12)


