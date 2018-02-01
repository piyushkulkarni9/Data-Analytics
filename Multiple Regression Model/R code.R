qt(0.975,497)

qt(c(-0.1834,.975), df = 467)

a <- qt(0.975,497)
-.1834+a
-.1834 - a
-.1834+ (a*0.0365)
-.1834 - (a*0.0365)
.2723 + (a*0.0723)
.2723 - (a*0.0723)
.2723 +- (a*0.0723)

a <- qt(0.975,994)
a
sqrt((.1048^2))
sqrt((.1048^2)+(64*.0019^2)+(16*-0.000189))
0.6821-(8*0.0101)+(1.9623*0.09045)
0.6821-(8*0.0101)-(1.9623*0.09045)
0.6821-(50*0.0101)-(1.9623*0.09045)
sqrt((.1048^2)+(50^2*.0019^2)+(50*-0.000189))
sqrt((.1048^2)+(50^2*.0019^2)+(100*-0.000189))
0.6821-(50*0.0101)+(1.9623*0.03329)
0.6821-(50*0.0101)-(1.9623*0.03329)
sqrt((.10486^2)+(50^2*.0019^2)+(100*-0.000189))
0.6821-(8*.0101)+(1.96*23*.09045)
0.6821-(8*.0101)+(1.9623*.09045)
0.6821-(8*.0101)-(1.9623*.09045)
sqrt((.1048^2)+(50^2*.0019^2)+(100*-0.000189))
sqrt((.1048^2)+(50^2*.0019^2)+(100*-0.000189259))
sqrt((.1048^2)+(50^2*.0019^2)-(100*+0.000189259))
06821-(50*.0101)
0.6821-(50*.0101)
.1771+(1.9623*.03329)
.1771-(1.9623*.03329)

library(haven)
cocaine <- read_dta("D:/Class Notes/Fall 17 Classes/ECON/Data_sets/cocaine.dta")
View(cocaine)

model <- lm(price ~ quant qual trend, data = cocaine)
model <- lm(price ~ quant +qual+ trend, data = cocaine)
summary(model)

qt(0.05,52)

library(haven)

br2 <- read_dta("D:/Class Notes/Fall 17 Classes/ECON/Data_sets/br2.dta")
View(br2)

model <- lm(price ~ sqft + age, data = br2)
summary(model)

confint(model, sqft, level=095)
confint(model, 'sqft', level=095)
confint(model, 'sqft', level=0.95)

qt(.95,1077)
-755-1000
-17755/140.89
1755/140.89
1000-755
245/140.89

model <- lm(price ~ sqft + age + sqft*sqft + age*age, data = br2)
summary(model)

model <- lm(price ~ sqft + age + sqft:sqft + age:age, data = br2)
summary(model)

model <- lm(price ~ sqft + age + (sqft*sqft) + (age*age), data = br2)
summary(model)

model <- lm(price ~ sqft + age + I(sqft^2) + I(age^2), data = br2)
summary(model)

model <- lm(price ~ sqft + age + (sqft^2) + (age^2), data = br2)
summary(model)

model <- lm(price ~ sqft + age + poly(sqft, 2) + poly(age,2), data = br2)
summary(model)

model <- lm(price ~ sqft + age + (sqft^2) + (age^2), data = br2)
summary(model)

model <- lm(price ~ sqft + age + I(sqft*sqft) + I(age*age), data = br2)
summary(model)

glm(formula = price ~ sqft + age + I(sqft^2) + I(age^2))

glm(formula = price ~ sqft + age + I(sqft^2) + I(age^2), family = binomial)

glm(formula = price ~ sqft + age + I(sqft^2) + I(age^2), family = binomial, data=br2)

model <- lm(price ~ sqft + age + poly(age,2) ,  data = br2)
summary(model)

model <- lm(price ~ sqft + age + age^age ,  data = br2)
model <- lm(price ~ sqft + age + I(sqft^2) + I(age^age), data = br2)
summary(model)

age2<- br2$age^2
view(age2)
View(age2)

View(br2$age)
View(age2)

sqft2<-br2$sqft^2
model <- lm(price ~ br2$sqft + br2$age + age2 + sqft2)

model <- lm(br2$price ~ br2$sqft + br2$age + age2 + sqft2)
summary(model)

glm(formula = price ~ sqft + age + I(sqft^2) + I(age^2), family = binomial, data=br2)
glm(formula = price ~ sqft + age + I(sqft^2) + I(age^2))

model <- lm(price ~ sqft + age + I(sqft*sqft) + I(age*age), data = br2)
summary(model)

.02315-55.78
2*.02315
-55.78
0.0463-.5578
max(br2$sqft)
min(br2$sqft)
(.02315*662)-55.78
(.02315**2662)-55.78
(.02315**2*662)-55.78
(.02315*2*662)-55.78
(.02315*2*7897)-55.78
(.02315*2*2300)-55.78
max(br2$age)
min(br2$age)
(2*30.16)-2798
(2*30.16*80)-2798
(2*30.16-2798
min(br2$age)
(2*30.16)-2798
(2*30.16*20)-2798
qc(.972,1075)
qt(.972,1075)
qt(.975,1075)
vcov(model)
23*10^2
23*10^-2
sqrt(40.82+((4600^2)*(9.29*10^-7))+(2*2300*2*5.87*10^-3))
sqrt(40.82+((4600^2)*(9.29*10^-7))-(2*2300*2*5.87*10^-3))
-55.78+(2*.023*2300)+(1.9621*2.544)
-55.78+(2*.023*2300)-(1.9621*2.544)
-55.78+(2*.02315*2300)-(1.9621*2.544)
-55.78+(2*.02315*2300)+(1.9621*2.544)
sqrt((9.309*10^4)+(4*400*25.7155)-(80*1.4345*10^3))
((40*30.16)+1000-2798)/139.5521
qt(.95,1075)

model <- lm(price ~ sqft+ age + I(sqft * sqft) + I(age * age) + I(sqft * age), data= br2)
summary(model)

-30.73+(2*2.218*10^-2*2300)-(.09306*20)
-30.73+(2*0.02218**2300)-(.09306*20)
-30.73+(2*0.02218*2300)-(.09306*20)
-30.73+(2*0.02218*662)-(.09306*20)
-30.73+(2*0.02218*662)-(.9306*20)
-30.73+(2*0.02218*2300)-(.9306*20)
-30.73+(2*0.02218*662)-(.9306*20)
-30.73+(2*0.02218*7897)-(.9306*20)
-420+(2*26.52*1)-(0.9306*2300)
-442+(2*26.52*1)-(0.9306*2300)
-442+(2*26.52*80)-(0.9306*2300)
-442+(2*26.52*20)-(0.9306*2300)
vcov(model)
