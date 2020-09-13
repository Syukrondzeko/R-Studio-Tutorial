#see anova_with libraries.R
# Summary of the analysis
summary(res.aov)

#Uji Lanjut
TukeyHSD(res.aov)

#Homogeniity Test
plot(res.aov, 1)

library(car)
leveneTest(Nilai ~ Perlakuan, data = data_anova)

#Normality tes
plot(res.aov, 2)
