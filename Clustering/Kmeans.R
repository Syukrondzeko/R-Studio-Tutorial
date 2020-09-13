#Reading File
library(readr)
dataku<- read_csv("C:/Users/HP PC/Downloads/4. cluster/datasets_42674_74935_Mall_Customers.csv")
dataku=dataku[,c(3:5)]
View(dataku)
colnames(dataku)=c("Age","Income","Spending")
View(dataku)

fit <- kmeans(dataku, 5) # 5 cluster solution
# get cluster means
aggregate(dataku,by=list(fit$cluster),FUN=mean)
# append cluster assignment
dataku2 <- data.frame(dataku, fit$cluster)
View(dataku2)

str(dataku2)
dataku2$fit.cluster=as.factor(dataku2$fit.cluster)
library(ggplot2)
ggplot(dataku2,aes(x=Income,y=Spending,col=fit.cluster,size=Age))+geom_point()
