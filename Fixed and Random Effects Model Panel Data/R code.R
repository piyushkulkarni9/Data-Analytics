library(plm)
library(haven)
library(lme4)
mexican <- read_dta("D:/Class Notes/Fall 17 Classes/ECON/Data_sets/mexican.dta")
View(mexican)


# ------ Fixed effect with client char and othe char ----------

modelfe <- plm (lnprice ~ regular + rich + alcohol + nocondom + bar + street, 
                data = mexican, index = c("id", "trans"), model = "within")
summary(modelfe)
100*(exp(.1702)-1)

# ------ Random Effects with all char ----------

modelre <- plm (lnprice ~ regular + rich + alcohol + nocondom + 
                  bar + street +
                  age + attractive + school, 
                data = mexican, index = c("id", "trans"), model = "random" )
summary(modelre)
0.138+0.276+.216
100*(exp(0.63)-1)

# H Test

phtest(modelfe, modelre)

# ------ No condom Endogenous -----

modelrenc <- pht(lnprice ~  regular + rich + alcohol + nocondom +
                   + bar + street + 
                   age + attractive + school | age + attractive + school  ,
                 data = mexican, model = "ht")

summary(modelrenc)
.17+.33+.29
100*(exp(0.79-1))
# --------------Problem 15.10 Crime Rate ----------------

library(haven)
crime <- read_dta("D:/Class Notes/Fall 17 Classes/ECON/Data_sets/crime.dta")
View(crime)

# -----OLS -------

crimereg <- lm (lcrmrte ~ lprbarr + lprbconv + lprbpris + lavgsen + lwmfg, data = crime)
summary(crimereg)
anova(crimereg)

# -----Fixed Effects ---------

crimefe <- plm(lcrmrte ~ lprbarr + lprbconv + lprbpris + lavgsen + lwmfg, data = crime, 
               index = c("county"), model = "within")

summary(crimefe)

qf(0.95,89,535)

# -----OLS with Pop density, %young male and time dummies ----
crimereg2 <- lm (lcrmrte ~ lprbarr + lprbconv + lprbpris + lavgsen + lwmfg + 
                             ldensity + lpctymle + 
                             d82 + d83 + d84 + d85 + d86 + d87, data = crime)
summary(crimereg2)
anova(crimereg2)

# ---- Fixed effects with time dummies ----------

crimefe2 <- plm(lcrmrte ~ lprbarr + lprbconv + lprbpris + lavgsen + lwmfg +
                  ldensity + lpctymle + 
                  d82 + d83 + d84 + d85 + d86 + d87, data = crime, 
               index = c("county"), model = "within")

summary(crimefe2)

qf(.95,5,527)
