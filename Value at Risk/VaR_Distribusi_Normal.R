library(readxl)
Data_Mentah <- read_excel("C:/Users/HP PC/Downloads/5. Var Normal, T, dan Hist Simulation/Data_VaR.xlsx")
View(Data_Mentah)
Data_VaR=Data_Mentah[2:40,c(3,5,7)]
View(Data_VaR)

RTLKM=Data_VaR[,1]
View(RTLKM)
data=RTLKM$`r(TLKM)`
n=length(data)

#VaR Z
m=mean(data)
s=sd(data)
z=qnorm(0.05)
t=qt(0.05,n)
hp=1
modal=1000000

VaR_Z = -(modal*sqrt(hp)*z*s)
