library(readr)
data_reglog <- read_csv("C:/Users/HP PC/Downloads/10. Reglog/data_reglog.csv")
View(data_reglog)
str(data_reglog)
data_reglog$admit=as.factor(data_reglog$admit)
model <- glm(admit ~ gre + gpa + rank, data = data_reglog, family = "binomial")
model
summary(model)
prediksi<-predict(model,data_reglog,type="response")
head(prediksi)

library(generalhoslem)
logitgof(data_reglog$admit, fitted(model))
