library(readxl)
data_anova <- read_excel("C:/Users/HP PC/Downloads/1. Anova/data_anova.xlsx")
View(data_anova)
x1=data_anova[c(1:5),1]
x2=data_anova[c(1:5),2]
x3=data_anova[c(1:5),3]
x4=data_anova[c(1:5),4]
x5=data_anova[c(1:5),5]

anova<-function(x1,x2,x3,x4,x5)
{
  n1=nrow(x1)
  n2=nrow(x2)
  n3=nrow(x3)
  n4=nrow(x4)
  n5=nrow(x5)
  N=n1+n2+n3+n4+n5
  T1=sum(x1)
  T2=sum(x2)
  T3=sum(x3)
  T4=sum(x4)
  T5=sum(x5)
  G=T1+T2+T3+T4+T5
  JKP=((T1^2/n1)+(T2^2/n2)+(T3^2/n3)+(T4^2/n4)+(T5^2/n5))-(G^2/N)
  JKT=sum(x1^2,x2^2,x3^2,x4^2,x5^2)-G^2/N
  JKE=JKT-JKP
  dk1=4
  dk2=N-5
  Fhit=(JKP/dk1)/(JKE/dk2)
  Ftabel=qf(.99, df1=4, df2=20) 
  cat("JKP     : ",JKP)
  cat("\n")
  cat("JKE     : ",JKE)
  cat("\n")
  cat("JKT     : ",JKT)
  cat("\n")
  cat("F Hitung: ",Fhit)
  cat("\n")
  cat("F Tabel : ",Ftabel)
  cat("\n")
  
}
anova(x1,x2,x3,x4,x5)
