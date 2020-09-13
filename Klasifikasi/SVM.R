library(readxl)
dataku<- read_excel("C:/Users/HP PC/Downloads/9. NB dan SVM/nb.xlsx", 
                    sheet = "Sheet2")

View(dataku)
str(dataku)

#SVM
model2<-svm(kelas~melahirkan+kulit,data=dataku,kernel="linear",cost=1)
pred2<-predict(model2,dataku)
confusionMatrix(pred1,dataku$kelas)
SVMku<-svm(kelas~melahirkan+kulit,data=dataku,kernel="polynomial",cost=1,degree=3)
SVMku<-svm(kelas~melahirkan+kulit,data=dataku,kernel="radial",cost=1,gamma=0.005)
