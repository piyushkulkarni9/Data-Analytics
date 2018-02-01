# creating y and x variables
y <- pubexp$ee/pubexp$p
x <- pubexp$gdp/pubexp$p
x2 = x^2

data1 <- cbind(pubexp, y)
data <- cbind(data1, x)

pubexp <- data
# Running least square regression

leastsq <- lm (y ~ x, data = pubexp)
summary(leastsq)
# getting residuals
leastsq.res <- resid(leastsq)


# Plotting residuals

library(ggfortify)
library(ggplot2)
autoplot(leastsq, data=pubexp, color = 'blue')

ggplot(pubexp, aes(x = pubexp$x, y = pubexp$y)) + geom_point() + 
  stat_smooth( method = "lm", col = 'red')

# White formula

res = residuals(leastsq)
ressq = res^2
#yhat = leastsq$fitted.values
#yhat2 = yhat^2

#combining all into one dataset
#m4 = data.frame(cbind(ressq, yhat, yhat2))

m3 = data.frame(cbind(ressq, x, x2))

white <- lm(ressq ~ x + x2, data = m3)
summary(white)

# critical chai-square 
qchisq(.95, 2)

#Robust std errors
library(sandwich)
#coef

#coeftest(leastsq, vcov = vcovHC(leastsq,type = "HC1"))

SE_robust <- sqrt(diag(vcovHC(leastsq, type="HC2")))

model2 <- summary(leastsq)
SE_robust

model2$coefficients[,2] <- SE_robust

model2

qt(.975,32)

0.0731 - (2.0369*.0063)

#Weightd least squares

model <- lm(y ~ x, data = pubexp, weights = (1/pubexp$x))
summary(model)

confint(model, 'x', level = 0.95)