install.packages("plm")
library(plm)

data("Grunfeld", package="plm")
View(Grunfeld)
grun.ce <- plm(inv~value+capital, data = Grunfeld, model = "pooling")
grun.fe <- plm(inv~value+capital, data = Grunfeld, model = "within")
fixef(grun.fe)
grun.re <- plm(inv~value+capital, data = Grunfeld, model = "random")
ranef(grun.re)


# LM test for fixed effects versus OLS
pFtest(grun.fe, grun.ce)

# Hausman test for fixed versus random effects model
phtest(grun.re, grun.fe)

# LM test for random effects versus OLS
plmtest(grun.ce)
