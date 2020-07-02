library(readr)
dataku<- read_csv("C:/Users/HP PC/Downloads/4. cluster/datasets_42674_74935_Mall_Customers.csv")
dataku=dataku[,c(3:5)]
View(dataku)
colnames(dataku)=c("Age","Income","Spending")
View(dataku)
wss <- (nrow(dataku)-1)*sum(apply(dataku,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(dataku,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# K-Means Cluster Analysis
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

#hclust
distance<-dist(dataku)

hc.c<-hclust(distance,method='complete')
plot(hc.c)
hc.a<-hclust(distance,method='average')
plot(hc.a,hang=-1)
member.a<-cutree(hc.c,5)
aggregate(dataku,list(member.a),mean)
member.a

