library(haven)
capm4 <- read_dta("D:/Class Notes/Fall 17 Classes/ECON/Data_sets/capm4.dta")
View(capm4)
nrow(capm4)


capm4$Disney_Y=capm4$dis - capm4$riskfree

capm4$GE_Y=capm4$ge - capm4$riskfree

capm4$GM_Y=capm4$gm - capm4$riskfree

capm4$IBM_Y=capm4$ibm - capm4$riskfree

capm4$MICROSOFT_Y=capm4$msft - capm4$riskfree

capm4$EXXON_Y=capm4$xom - capm4$riskfree

capm4$Risk_X=capm4$mkt - capm4$riskfree

regDisney <- lm(Disney_Y ~ Risk_X)
regDisney <- lm(Disney_Y ~ Risk_X, data = capm4)
summary(regDisney)

regGE <- lm(GE_Y ~ Risk_X, data = capm4)
summary(regGE)

regGM <- lm(GM_Y ~ Risk_X, data = capm4)
summary(regGM)

regIBM <- lm(IBM_Y ~ Risk_X, data = capm4)
summary(regIBM)

regMicrosoft <- lm(MICROSOFT_Y ~ Risk_X, data = capm4)
summary(regMicrosoft)

regExxon <- lm(EXXON_Y ~ Risk_X, data = capm4)
summary(regMicrosoft)

save.image("D:/Class Notes/Fall 17 Classes/ECON/Assignment 2/Q10/Rcode Q10.RData")

summary(regExxon)

plot(regMicrosoft)
abline(regMicrosoft)
abline(regMicrosoft)
abline(lm(MICROSOFT_Y ~ Risk_X, data = capm4))
plot.new()

abline(regMicrosoft)
abline(regMicrosoft)
abline(regMicrosoft)
plot (MICROSOFT_Y ~ Risk_X, data = capm4)

abline(regMicrosoft)

regExxon <- lm(EXXON_Y ~ Risk_X -1, data = capm4)
summary(regExxon)

regDisney <- lm(Disney_Y ~ Risk_X -1, data = capm4)
summary(regDisney)

regGE <- lm(GE_Y ~ Risk_X -1, data = capm4)
summary(regGE)

regGM <- lm(GM_Y ~ Risk_X -1, data = capm4)
summary(regGM)

regIBM <- lm(IBM_Y ~ Risk_X - 1, data = capm4)
summary(regIBM)

regMicrosoft <- lm(MICROSOFT_Y ~ Risk_X -1, data = capm4)
summary(regMicrosoft)

regExxon <- lm(EXXON_Y ~ Risk_X - 1, data = capm4)
summary(regExxon)

library(haven)
library(haven)
fair4 <- read_dta("D:/Class Notes/Fall 17 Classes/ECON/Data_sets/fair4.dta")
View(fair4)

fit <- lm(vote ~ growth, data = fair4, fair4$year>1915)
summary(fit)


plot (x = fair4$growth[fair4$year>1915], y = fair4$vote[fair4$year>1915], xlab="Growth", ylab="Vote", col="blue", pch = 10)

fit <- lm(vote ~ growth, data = fair4, fair4$year>1915)
summary(fit)

plot (x = fair4$growth[fair4$year>1915], y = fair4$vote[fair4$year>1915], xlab="Growth", ylab="Vote", col="blue", pch = 10)
abline(fit)

fit <- lm(vote ~ growth, data = fair4, fair4$year>1915 & fair4$year<2008)
summary(fit)

fair4[fair4$year>2007, ]

fit <- lm(vote ~ inflation, data = fair4, fair4$year>1915)
summary(fit)

plot (x = fair4$inflation[fair4$year>1915], y = fair4$vote[fair4$year>1915], xlab="Growth", ylab="Vote", col="blue", pch = 10)
plot (x = fair4$inflation[fair4$year>1915], y = fair4$vote[fair4$year>1915], xlab="Inflation", ylab="Vote", col="blue", pch = 10)
