x=c(35,39,43,54,56,88,95,105,112,119)
y=c(1.73,2.45,3.31,6.83,6.99,10.44,16.36,27.47,29.06,33.96)
cor(x, y, method = "pearson")
cor.test(x, y, method="pearson")

cor.test(x, y, method="kendall")
cor.test(x, y, method="spearman")

library(ggplot2)
dataku=data.frame(x,y)
ggplot(dataku,aes(x,y))+geom_point()+geom_smooth()
