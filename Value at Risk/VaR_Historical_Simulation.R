library(readxl)
Data_Mentah <- read_excel("C:/Users/HP PC/Downloads/5. Var Normal, T, dan Hist Simulation/Data_VaR.xlsx")
View(Data_Mentah)
Data_VaR=Data_Mentah[2:40,c(3,5,7)]
View(Data_VaR)

RTLKM=Data_VaR[,1]
View(RTLKM)
data=RTLKM$`r(TLKM)`
n=length(data)

modal=1000000
data_urut=sort(data, decreasing = FALSE)
q=quantile(data,0.05)
VaR_HS=-(modal*q*sqrt(hp))
