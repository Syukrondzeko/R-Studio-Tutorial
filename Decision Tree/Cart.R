library(rpart)
library(rpart.plot)
library(caret)
data(ptitanic)

str(ptitanic)
View(ptitanic)

model2<- rpart(survived ~ pclass+sex, data = ptitanic)
pred2=predict(model2,type="class")
conf.matrix2 <- table(ptitanic$survived, pred2)
conf.matrix2

confusionMatrix(pred2,ptitanic$survived)
