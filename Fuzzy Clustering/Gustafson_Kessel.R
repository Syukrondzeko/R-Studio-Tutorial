library(readr)
crime_data <- read_csv("F:/youtube/13. cluster twitter/crime_data.csv")
View(crime_data)
dataku=crime_data[,3:4]
View(dataku)

library(advclust)
help(fuzzy.GK)

c1=fuzzy.GK(dataku, 4, 2, 0.8)
c1
