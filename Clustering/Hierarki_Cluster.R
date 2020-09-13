library(readr)
dataku<- read_csv("C:/Users/HP PC/Downloads/4. cluster/datasets_42674_74935_Mall_Customers.csv")
dataku=dataku[,c(3:5)]
View(dataku)
colnames(dataku)=c("Age","Income","Spending")
View(dataku)

#hclust
distance<-dist(dataku)

hc.c<-hclust(distance,method='complete')
plot(hc.c)
hc.a<-hclust(distance,method='average')
plot(hc.a,hang=-1)
member.a<-cutree(hc.c,5)
aggregate(dataku,list(member.a),mean)
member.a

