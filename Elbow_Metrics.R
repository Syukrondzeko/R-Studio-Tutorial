wss <- (nrow(dataku)-1)*sum(apply(dataku,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(dataku,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
