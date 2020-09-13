library(readxl)
Data_Mentah <- read_excel("C:/Users/HP PC/Downloads/6. VaR Portofolio Normal, T, dan Historical Simulation/Data_VaR.xlsx")
View(Data_Mentah)
Data_VaR=Data_Mentah[2:40,c(3,5,7)]
View(Data_VaR)

TLKM=Data_VaR[,1]
WSKT=Data_VaR[,2]
UNVR=Data_VaR[,3]

v1=var(TLKM)
v2=var(WSKT)
v3=var(UNVR)

v12=cov(TLKM,WSKT)
v13=cov(TLKM,UNVR)
v23=cov(WSKT,UNVR)

m=matrix(c(v1,v12,v13,v12,v2,v23,v13,v23,v3), nrow = 3, ncol = 3)
library(matlib)
mi=inv(m)
m1=matrix(c(1,1,1),nrow=3)
mh=mi%*%m1
total=colSums (mh, dims = 1)
w1=mh[1,1]/total
w2=mh[2,1]/total
w3=mh[3,1]/total

Data_VaR$rportof=w1*Data_VaR$`r(TLKM)`+w2*Data_VaR$`r(WSKT)`+w3*Data_VaR$`r(UNVR)`
View(Data_VaR)

data=Data_VaR$rportof
n=length(data)

#VaR T
m=mean(data)
s=sd(data)
z=qnorm(0.05)
t=qt(0.05,n)
hp=1
modal=1000000

VaR_T = -(modal*sqrt(hp)*t*s*sqrt((n-2)/n))
