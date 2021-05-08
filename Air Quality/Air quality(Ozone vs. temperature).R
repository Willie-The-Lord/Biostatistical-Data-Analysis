install.packages("sm")
library(sm)
library(KernSmooth)
library(splines)
data(airquality)
head(airquality, 6)
data <- na.omit(airquality)

x <- data$Ozone
y <- data$Temp

plot(x, y)      
title(main = "[AirQuality] Ozone v.s. Temperature" )

#local linear regression
lines(locpoly(x, y, bandwidth=3, degree=1), col="green")

#smoothing splines
sspline <- smooth.spline(x, y, cv=TRUE)
lines(sspline, col="blue")

#NW estimator
lines(ksmooth(x, y, "normal", bandwidth = 3), col = "red")

#simple linear regression
abline(lm(y~x))

#quadratic regression
fit <-lm(y~poly(x,2, raw = T))
summary(fit)
quadratic = fit$coefficient[3]*x^2 + fit$coefficient[2]*x + fit$coefficient[1]
quadratic
par(new = TRUE)
lines(x[1:20],quadratic[1:20], col="orange")

legend("bottom", legend = c("LL", "SM", "NW", "simple linear", "quadratic"), lwd = 2, col = c("green", "blue", "red", "black", "orange"))


