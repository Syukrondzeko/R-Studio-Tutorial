library(readxl)
dataku<- read_excel("C:/Users/HP PC/Downloads/9. NB dan SVM/nb.xlsx", 
                    sheet = "Sheet2")

View(dataku)
str(dataku)
dataku$`penutup kulit`=as.factor(dataku$`penutup kulit`)
dataku$melahirkan=as.factor(dataku$melahirkan)
dataku$kelas=as.factor(dataku$kelas)
colnames(dataku)=c("hewan","kulit","melahirkan","kelas")
dataku=dataku[,2:4]
library(e1071)
library(caret)
model1<-naiveBayes(kelas~melahirkan+kulit,data=dataku)
model1
pred1<-predict(model1,dataku)
pred1
confusionMatrix(pred1,dataku$kelas)


#SVM
model2<-svm(kelas~melahirkan+kulit,data=dataku,kernel="linear",cost=1)
pred2<-predict(model2,dataku)
confusionMatrix(pred1,dataku$kelas)
SVMku<-svm(kelas~melahirkan+kulit,data=dataku,kernel="polynomial",cost=1,degree=3)
SVMku<-svm(kelas~melahirkan+kulit,data=dataku,kernel="radial",cost=1,gamma=0.005)
