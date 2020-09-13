library(readxl)
data_anova <- read_excel("C:/Users/HP PC/Downloads/1. Anova/data_anova.xlsx")
View(data_anova)

#Kruskal Wallish
kruskal.test(Nilai ~ Perlakuan, data = data_anova)
