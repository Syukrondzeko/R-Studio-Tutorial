residu=residuals(model)
#Uji Asumsi Normalitas
qqnorm(residu)
ks.test(residu,'pnorm',0,sd(residu))
#Uji Homoskedastisitas
library(lmtest)
bptest(model)
#Uji asumsi non autokorelasi
dwtest(model)
#uji asumsi non multikolinieritas
library(car)
vif(model)
