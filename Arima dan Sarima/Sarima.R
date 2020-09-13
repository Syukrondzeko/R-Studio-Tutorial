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

model1=sarima(x,0,1,1,0,1,1,12)
model1
sarima.for(x,24,0,1,1,0,1,1,12)
