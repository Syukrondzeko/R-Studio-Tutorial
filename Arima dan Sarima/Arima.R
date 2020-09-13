data1=arima.sim(n=50,model=list(order=c(0,0,1),ma=0.1,sd=0.1))
data1=read.csv("C:/Users/HP PC/Downloads/8. Arima/data_arima.csv")
View(data1)
data1=data1$x

library(timeSeries)
data1=as.ts(data1)

str(data1)

plot.ts(data1)

library(tseries)
adf.test(data1)

acf(data1)
pacf(data1)

fit=arima(data1,c(0,0,1))
fit

myresid=fit$residuals
qqnorm(myresid)

library(FitAR)
boxresult<-LjungBoxTest(myresid)
plot(boxresult[,3],main="Ljung-Box_Test",xlab="Lag",ylab="p-value")

Box.test(myresid,type="Ljung-Box")

pred<-predict(fit,n.ahead=5)
ts.plot(data1,pred$pred)
data1
pred$pred
pred


