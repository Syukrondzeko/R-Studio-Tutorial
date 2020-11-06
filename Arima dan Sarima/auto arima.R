library(forecast)
library(readxl)

?auto.arima
Data <- read_excel("F:/Course/youtube/27.Auto.arima/Data_Auto_Arima.xlsx")
tsdata=ts(Data$WSKT)

str(Data$TLKM)
str(tsdata)

plot(tsdata)
model=auto.arima(tsdata)
model

#forecasting
prediksi=forecast(model,h=7)
prediksi

#plot hasil prediksi
plot(prediksi)
#cek besaran nilai residual
plot(prediksi$residuals)
#cek asumsi
qqnorm(prediksi$residuals)
#cek akurasi
summary(model)
accuracy(model)
