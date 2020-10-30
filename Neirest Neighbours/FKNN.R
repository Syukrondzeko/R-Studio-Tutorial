library(readxl)
data_knn <- read_excel("F:/Course/youtube/28. KNN/data_knn.xlsx")
View(data_knn)

#pembagian data train dan data test
nrow(data_knn)
train=data_knn[1:17,]
test=data_knn[18,]
for(i in 1:nrow(train)){
  train$jarak[i]=sqrt((train$x1[i]-test$x1[1])^2+(train$x2[i]-test$x2[1])^2)
  train$d[i]=train$jarak[i]^(-2)
}

trainTemp=train
k1=min(train$jarak)
train=train[train$jarak!=k1,]
k2=min(train$jarak)
train=train[train$jarak!=k2,]
k3=min(train$jarak)
train=train[train$jarak!=k3,]

S1=trainTemp[trainTemp$jarak==k1,]
S2=trainTemp[trainTemp$jarak==k2,]
S3=trainTemp[trainTemp$jarak==k3,]
Prob_kelas_0=(S2$d+S3$d)/(S1$d+S2$d+S3$d)
Prob_kelas_1=S1$d/(S1$d+S2$d+S3$d)
cat("Probabilitas Kelas 0 adalah ",Prob_kelas_0)
cat("Probabilitas Kelas 1 adalah ",Prob_kelas_1)
