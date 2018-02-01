
library(haven)
pizza4 <- read_dta("D:/Class Notes/Fall 17 Classes/ECON/Data_sets/pizza4.dta")
View(pizza4)
pizza <- pizza4
View(pizza)

ageinc <- pizza$age*pizza$income

pizza1<-cbind(pizza,ageinc)

age2inc <- ageinc * pizza$age

pizza2<-cbind(pizza1,age2inc)

age3inc <- age2inc*pizza$age

pizza3<-cbind(pizza2,age3inc)

View(pizza3)

pizzacorr <- pizza3$pizza, pizza3$income, pizza3$age, pizza3$ageinc, pizza3$age2inc, pizza3$age3inc

View(pizza3)

pizzacorr <- [pizza3$pizza, pizza3$income, pizza3$age, pizza3$ageinc, pizza3$age2inc, pizza3$age3inc]

pizzacorr = subset(pizza3, select = pizza3$pizza, pizza3$income, pizza3$age, pizza3$ageinc, pizza3$age2inc, pizza3$age3inc)

pizzacorr = pizza3[c( pizza3$pizza, pizza3$income, pizza3$age, pizza3$ageinc, pizza3$age2inc, pizza3$age3inc)


pizzacorr = pizza3[c( pizza3$pizza, pizza3$income, pizza3$age, pizza3$ageinc, pizza3$age2inc, pizza3$age3inc)]

pizzacorr = pizza3[c( 'pizza)]'
pizza, pizza3$income, pizza3$age, pizza3$ageinc, pizza3$age2inc, pizza3$age3inc)]

pizzacorr = pizza3[c( 'pizza')]

View(pizzacorr)
pizzacorr = pizza3[c( 'pizza', 'income', 'age', 'ageinc', 'age2inc', 'age3inc')]

View(pizzacorr)
cor(pizzacorr)

model<-lm (pizza~ income + age  + ageinc + age2inc, data = pizzacorr )
summary(model)

pizzacorr1 = pizza3[c( 'pizza', 'income', 'age', 'ageinc', 'age2inc')]
cor(pizzacorr1)
cor(pizzacorr)
library(haven)

cps4c_small <- read_dta("D:/Class Notes/Fall 17 Classes/ECON/Data_sets/cps4c_small.dta")
View(cps4c_small)

model <-lm(log(wage)~ educ + I(educ^2) + exper + I(exper^2) + I(educ*exper) + hrswk, data = cps4c_small)
summary(model)

model <-lm(log(wage)~ educ + I(educ^2) + exper + I(exper^2) +  hrswk, data = cps4c_small)
model <-lm(log(wage)~ educ + I(educ^2) +  hrswk, data = cps4c_small)
model <-lm(log(wage)~  exper + I(exper^2) +  hrswk, data = cps4c_small)
model <-lm(log(wage)~ educ + exper + I(exper^2)  + hrswk, data = cps4c_small)

qt(.975,993)
((254.176-222.6674)/2)/(222.6674/994)
qf(.95,2,994)
((280.5061-222.6674)/2)/(222.6674/994)
log(222.4166/1000)+(7*log(1000)/1000)

model <-lm(log(wage)~ educ + I(educ^2) + exper + I(exper^2) + I(educ*exper) + hrswk, data = cps4c_small)
summary(model)

model <-lm(log(wage)~ educ + I(educ^2) + exper + I(exper^2) +  hrswk, data = cps4c_small)
summary(model)

model <-lm(log(wage)~ educ + I(educ^2) +  hrswk, data = cps4c_small)
summary(model)

model <-lm(log(wage)~  exper + I(exper^2) +  hrswk, data = cps4c_small)
summary(model)

model <-lm(log(wage)~ educ + exper + I(exper^2)  + hrswk, data = cps4c_small)
summary(model)

qt(.975,993)
((254.176-222.6674)/2)/(222.6674/994)
qf(.95,2,994)
((280.5061-222.6674)/2)/(222.6674/994)
log(222.4166/1000)+(7*log(1000)/1000)
source('D:/Class Notes/Fall 17 Classes/ECON/Assi 5/Assi 5/R script assi 5.R')
source('D:/Class Notes/Fall 17 Classes/ECON/Assi 5/Assi 5/R script assi 5.R')
