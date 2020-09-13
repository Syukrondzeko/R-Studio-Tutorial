library(rpart)
library(rpart.plot)
library(caret)
data(ptitanic)

str(ptitanic)
View(ptitanic)

# install.packages("CHAID", repos="http://R-Forge.R-project.org")
require(CHAID)
model1<- chaid(survived ~ pclass+sex, data = ptitanic)
print(model1)
plot(model1)
pred1 <- predict(model1)
conf.matrix1 <- table(ptitanic$survived, pred1)
conf.matrix1
confusionMatrix(pred1,ptitanic$survived)
