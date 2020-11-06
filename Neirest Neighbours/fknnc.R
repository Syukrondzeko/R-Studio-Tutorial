library(readxl)
data_knn <- read_excel("F:/Course/youtube/30. FKNNC/data_fknnc.xlsx")
#View(data_knn)

#pembagian data train dan data test
nrow(data_knn)
data_knn=as.data.frame(data_knn)
train=data_knn[1:17,]
test=data_knn[18,]
train$jarak=0
train$jarak=as.numeric(train$jarak)
train$d=0
train$d=as.numeric(train$d)
for(i in 1:nrow(train)){
  train$jarak[i]=sqrt((train$x1[i]-test$x1[1])^2+(train$x2[i]-test$x2[1])^2)
  train$d[i]=train$jarak[i]^(-2)
}


#kelas 0
train0=train[train$y==0,]
k1=min(train0$jarak)
train0=train0[train0$jarak!=k1,]
k2=min(train0$jarak)
train0=train0[train0$jarak!=k2,]

train0=train[train$y==0,]
T1=train0[train0$jarak==k1,]
T2=train0[train0$jarak==k2,]

S0=T1$d+T2$d

#kelas 1
train1=train[train$y==1,]
k3=min(train1$jarak)
train1=train1[train1$jarak!=k3,]
k4=min(train1$jarak)
train1=train1[train1$jarak!=k4,]

train1=train[train$y==1,]
T3=train1[train1$jarak==k3,]
T4=train1[train1$jarak==k4,]


S1=T3$d+T4$d

D=S0+S1
Prob_kelas_0=S0/D
Prob_kelas_1=S1/D
cat("Probabilitas Kelas 0 adalah ",Prob_kelas_0)
cat("Probabilitas Kelas 1 adalah ",Prob_kelas_1)
