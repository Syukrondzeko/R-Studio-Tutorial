library(splines2)
require(stats)
require(graphics)
women

splines::bs(women$height, knots = c(67), degree = 2)
splines2::bSpline(women$height, knots = c(67),degree = 2)
summary(fm1 <- lm(weight ~ -1+splines::bs(height, knots = c(63), degree = 2), data = women))
summary(fm1 <- lm(weight ~ -1+splines2::bSpline(height, knots = c(63),degree = 2), data = women))
