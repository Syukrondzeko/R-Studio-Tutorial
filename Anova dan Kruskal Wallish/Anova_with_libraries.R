library(readxl)
data_anova <- read_excel("C:/Users/HP PC/Downloads/1. Anova/data_anova.xlsx")
View(data_anova)

library(readxl)
data_anova <- read_excel("C:/Users/HP PC/Downloads/1. Anova/data_anova.xlsx", 
                         sheet = "Sheet2")
View(data_anova)
str(data_anova)
data_anova$Perlakuan=as.factor(data_anova$Perlakuan)
levels(data_anova$Perlakuan)

library(dplyr)
group_by(data_anova, Perlakuan) %>%
  summarise(
    count = n(),
    mean = mean(Nilai, na.rm = TRUE),
    sd = sd(Nilai, na.rm = TRUE)
  )

res.aov <- aov(Nilai ~ Perlakuan, data = data_anova)
# Summary of the analysis
summary(res.aov)

