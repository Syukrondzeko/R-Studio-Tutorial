library(readr)
crime_data <- read_csv("F:/youtube/13. cluster twitter/crime_data.csv")
View(crime_data)
dataku=crime_data[,3:4]
View(dataku)

dataku=as.data.frame(dataku)
library(ppclust)
c2 <- fcm(dataku, centers=4)
as.data.frame(c2$u)[1:6,]
summary(c2)

library(factoextra)
fviz_cluster(c2, ellipse.type = "norm", repel = TRUE,
             palette = "jco", ggtheme = theme_minimal(),
             legend = "right")




