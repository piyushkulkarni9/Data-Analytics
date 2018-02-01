# -----------------------calculating error------------------

e <- ivreg2$y- 3 - ivreg2$x

# --- Correlation in x and e

cor(ivreg2$x, e)

line <- abline(3,1)

# --- Scatterplot ---

library(ggplot2)

ggplot(data = ivreg2, aes(x = ivreg2$x, y = ivreg2$y)) + 
            geom_point(color = 'blue') + 
                geom_abline(intercept = 3, slope = 1, color = "red") + ggtitle('y = 3 + x')


# ------ OLS Regression with different observations -------
data_10 <- ivreg2[1:10,]
model_10 <- lm(y ~ x, data = data_10)
summary(model_10)

data_20 <- ivreg2[1:20,]
model_20 <- lm(y ~ x, data = data_20)
summary(model_20)

data_100 <- ivreg2[1:100,]
model_100 <- lm(y ~ x, data = data_100)
summary(model_100)


model_500 <- lm(y ~ x , data = ivreg2)
summary(model_500)


# ---------correlation between x, z1, z2 ,e-------
dat <- cbind(ivreg2, e)
cor(dat)


# ------- IV reg  Z1--------

library(AER)
ivreg_10 <- ivreg( y ~ x | z1 , data =data_10)
summary(ivreg_10)

ivreg_20 <- ivreg( y ~ x | z1 , data =data_20)
summary(ivreg_20)


ivreg_100 <- ivreg( y ~ x | z1 , data =data_100)
summary(ivreg_100)


model <- ivreg( y ~ x | z1 , data =ivreg2)
summary(model)


# ------------IV Reg Z2--------------


library(AER)
ivregz2_10 <- ivreg( y ~ x | z2 , data =data_10)
summary(ivregz2_10)

ivregz2_20 <- ivreg( y ~ x | z2 , data =data_20)
summary(ivregz2_20)


ivregz2_100 <- ivreg( y ~ x | z2 , data =data_100)
summary(ivregz2_100)


modelz2 <- ivreg( y ~ x | z2 , data =ivreg2)
summary(modelz2)


# ------------IV Reg z1 + Z2--------------


library(AER)
ivregz1z2_10 <- ivreg( y ~ x | z1 + z2 , data =data_10)
summary(ivregz1z2_10)

ivregz1z2_20 <- ivreg( y ~ x | z1 + z2 , data =data_20)
summary(ivregz1z2_20)


ivregz1z2_100 <- ivreg( y ~ x | z1 + z2 , data =data_100)
summary(ivregz1z2_100)


modelz1z2 <- ivreg( y ~ x | z1 + z2 , data =ivreg2)
summary(modelz1z2)
