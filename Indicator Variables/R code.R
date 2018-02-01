library(haven)
br2 <- read_dta("D:/Class Notes/Fall 17 Classes/ECON/Data_sets/br2.dta")
View(br2)
ggplot(br2, aes(y= price)) + geom_point()

library(ggplot2)
ggplot(br2, aes(y= price)) + geom_point()
ggplot(br2, aes(y= price, x = sqq(1, length(br2$price.Length) ))) + geom_point()
ggplot(br2, aes(y= price, x = seq(1, length(br2$price.Length) ))) + geom_point()
ggplot(br2, aes(y= price, x = seq(1, length(br2$price) ))) + geom_point()
ggplot(br2, aes(y= price, x = seq(1, length(br2$price) )), shape=cyl, color=cyl, size=cyl) + geom_point()
ggplot(br2, aes(y= price, x = seq(1, length(br2$price) )), shape=cyl, color=cyl, size=cyl) + geom_point()

/* Histogram */
ggplot(br2, aes(br2$price)) + geom_histogram()

/* Histogram */
ggplot(br2, aes(br2$price)) + geom_histogram(col="red",
fill="green",
alpha = .2)

/* Histogram */
ggplot(br2, aes(br2$price))
+ geom_histogram(col="red",fill="green", alpha = .2)
+ labs(title = "Histogram of Price")
+ labs(x = "Price" , y = "Count")
ggplot(br2, aes(br2$price))
+ geom_histogram(col="red",fill="green", alpha = .2)
+ labs(title = "Histogram of Price")
ggplot(br2, aes(br2$price)) +
geom_histogram(col="red",fill="green", alpha = .2) +
labs(title = "Histogram of Price")
labs(x = "Price" , y = "Count")

/* Histogram */
ggplot(br2, aes(br2$price)) +
geom_histogram(col="red",fill="green", alpha = .2) +
labs(title = "Histogram of Price") +
labs(x = "Price" , y = "Count")
clear
clear()
clear

/* Histogram */
ggplot(br2, aes(br2$price)) +
geom_histogram(col="red",fill="green", alpha = .2) +
labs(title = "Histogram of Price") +
labs(x = "Price" , y = "Count")

library(ggplot2)
/* Histogram */
ggplot(br2, aes(br2$price)) +
geom_histogram(col="red",fill="green", alpha = .2) +
labs(title = "Histogram of Price") +
labs(x = "Price" , y = "Count")
ggplot(br2, aes(br2$price)) +
geom_histogram(col="red",fill="green", alpha = .2) +
labs(title = "Histogram of Price") +
labs(x = "Price" , y = "Count")
ggplot(br2, aes(br2$price)) +
geom_histogram(col="red",fill="green", alpha = .2) +
labs(title = "Histogram of Price") +
labs(x = "Price" , y = "Count")

source('D:/Class Notes/Fall 17 Classes/ECON/Assignment 6/Assignment 6/R code.R')
source('D:/Class Notes/Fall 17 Classes/ECON/Assignment 6/Assignment 6/R code.R')
source('D:/Class Notes/Fall 17 Classes/ECON/Assignment 6/Assignment 6/R code.R')
source('D:/Class Notes/Fall 17 Classes/ECON/Assignment 6/Assignment 6/R code.R')

summary(br2)
model <- lm(log(price/1000) ~ (sqft/100) + bedrooms + baths + age + owner + pool + traditional + fireplace + waterfront, data = br2)

model <- lm(log(price/1000) ~ I(sqft/100) + bedrooms + baths + age + owner + pool + traditional + fireplace + waterfront, data = br2)
summary(model)

source('D:/Class Notes/Fall 17 Classes/ECON/Assignment 6/Assignment 6/R code.R')

100*(exp(.1099-1))
100*(exp(.1099)-1)

model <- lm(log(price/1000) ~ I(sqft/100) + bedrooms + baths + age + owner + pool + traditional + fireplace +
waterfront, I(waterfront * traditional), data = br2)
summary(model)

model <- lm(log(price/1000) ~ I(sqft/100) + bedrooms + baths + age + owner + pool + traditional + fireplace +
waterfront + I(waterfront * traditional), data = br2)

summary(model)

anova(model)

Resstricted <- lm(log(price/1000) ~ I(sqft/100) + bedrooms + baths + age + owner + pool + fireplace +
waterfront, data = br2)
summary(Resstricted)
anova(Resstricted)

Unristricted <- lm(log(price/1000) ~ I(sqft/100) + bedrooms + baths + age + owner + pool + traditional + fireplace +
waterfront + I((sqft/100) * traditional) + I(bedrooms * traditional)+
I(baths * traditional) + I(age * traditional) + I(owner * traditional) + I(pool * traditional)+
I(fireplace * traditional) + I(waterfront * traditional), data = br2)
summary(Unristricted)
anova(Unristricted)

qf(.95,9,1062)
d <- 2500, 3, 2, 20, 1, 0, 1, 1, 0

source('D:/Class Notes/Fall 17 Classes/ECON/Assignment 6/Assignment 6/R code.R')
source('D:/Class Notes/Fall 17 Classes/ECON/Assignment 6/Assignment 6/R code.R')
source('D:/Class Notes/Fall 17 Classes/ECON/Assignment 6/Assignment 6/R code.R')
source('D:/Class Notes/Fall 17 Classes/ECON/Assignment 6/Assignment 6/R code.R')
d = data.frame(sqft = 2500)
d
d = data.frame(sqft = 2500, bedrooms = 3)
d
d = data.frame(sqft = 2500, bedrooms = 3, baths = 2, age = 20, owner = 1, pool = 0, traditional = 1, fireplace =1, waterfront =0)
d
predict(Unristricted, d)
exp(5.005)*1000

library(ggplot2)
ggplot(br2, aes(y= price, x = seq(1, length(br2$price) )), shape=cyl, color=cyl, size=cyl) + geom_point()
ggplot(br2, aes(br2$price)) +
geom_histogram(col="red",fill="green", alpha = .2) +
labs(title = "Histogram of Price") +
labs(x = "Price" , y = "Count"
)

summary(br2)
model <- lm(log(price/1000) ~ I(sqft/100) + bedrooms + baths + age + owner + pool + traditional + fireplace + waterfront, data = br2)
summary(model)

100*(exp(.1099)-1)
model <- lm(log(price/1000) ~ I(sqft/100) + bedrooms + baths + age + owner + pool + traditional + fireplace +
waterfront + I(waterfront * traditional), data = br2)
summary(model)

Resstricted <- lm(log(price/1000) ~ I(sqft/100) + bedrooms + baths + age + owner + pool + fireplace +
waterfront, data = br2)
summary(Resstricted)
anova(Resstricted)

Unristricted <- lm(log(price/1000) ~ I(sqft/100) + bedrooms + baths + age + owner + pool + traditional + fireplace +
waterfront + I((sqft/100) * traditional) + I(bedrooms * traditional)+
I(baths * traditional) + I(age * traditional) + I(owner * traditional) + I(pool * traditional)+
I(fireplace * traditional) + I(waterfront * traditional), data = br2)
summary(Unristricted)
anova(Unristricted)

qf(.95,9,1062)
predict(Unristricted, d)
exp(5.005)*1000
d = data.frame(sqft = 2500, bedrooms = 3, baths = 2, age = 20, owner = 1, pool = 0, traditional = 1, fireplace =1, waterfront =0)
predict(Unristricted, d)
exp(5.005)*1000

source('D:/Class Notes/Fall 17 Classes/ECON/Assignment 6/Assignment 6/R code.R')
library(haven)
stckton4 <- read_dta("D:/Class Notes/Fall 17 Classes/ECON/Data_sets/stckton4.dta")
View(stckton4)

# -------------Question 7.16
ggplot(br2, aes(stckton4$sprice)) +
geom_histogram(col="red",fill="green", alpha = .2) +
labs(title = "Histogram of Price") +
labs(x = "Price" , y = "Count")

# Summary stastics
library(ggplot2)
ggplot(br2, aes(stckton4$sprice)) +
geom_histogram(col="red",fill="green", alpha = .2) +
labs(title = "Histogram of Price") +
labs(x = "Price" , y = "Count")

ggplot(stckton4, aes(stckton4$sprice)) +
geom_histogram(col="red",fill="green", alpha = .2) +
labs(title = "Histogram of Price") +
labs(x = "Price" , y = "Count")

ggplot(stckton4, aes(stckton4$log(sprice))) +
geom_histogram(col="red",fill="green", alpha = .2) +
labs(title = "Histogram of Price") +
labs(x = "Price" , y = "Count")

ggplot(stckton4, aes(stckton4$(log(sprice)))) +
geom_histogram(col="red",fill="green", alpha = .2) +
labs(title = "Histogram of Price") +
labs(x = "Price" , y = "Count")

ggplot(stckton4, aes(log(sprice))) +
geom_histogram(col="red",fill="green", alpha = .2) +
labs(title = "Histogram of Price") +
labs(x = "Price" , y = "Count")

ggplot(stckton4, aes(log(sprice))) +
geom_histogram(col="red",fill="green", alpha = .2) +
labs(title = "Histogram of ln(Price)") +
labs(x = "ln(Price)" , y = "Count")

debugSource('D:/Class Notes/Fall 17 Classes/ECON/Assignment 6/Assignment 6/R code.R')
model <- lm(log(sprice) ~ livarea + beds + baths + leglot + pool + age, data = stckton4)
model <- lm(log(sprice) ~ livarea + beds + baths + lgelot + pool + age, data = stckton4)
summary(model)

model <- lm(log(sprice/1000) ~ livarea + beds + baths + lgelot + pool + age, data = stckton4)
summary(model)

exp(.2530)
unristricted <- model <- lm(log(sprice/1000) ~ livarea + beds + baths + lgelot + pool + age + I(livarea * lgelot), data = stckton4)
summary(unristricted)

Restricted <- lm(log(sprice/1000) ~ livarea + beds + baths + pool + age, data = stckton4)
summary(Restricted)
anova(Restricted)

Unrestricted <- lm(log(sprice/1000) ~ livarea + beds + baths + pool + age + lgelot +  I(lgelot * livarea) +
I(lgelot * beds ) + I(lgelot * baths)+ I(lgelot * pool) + I(lgelot * age), data = stckton4)
summary(Unrestricted)
anova(Unrestricted)

qf(.95,6,1488)

# -------------Question 7.16
ggplot(stckton4, aes(stckton4$sprice)) +
geom_histogram(col="red",fill="green", alpha = .2) +
labs(title = "Histogram of Price") +
labs(x = "Price" , y = "Count")

#With log price
ggplot(stckton4, aes(log(sprice))) +
geom_histogram(col="red",fill="green", alpha = .2) +
labs(title = "Histogram of ln(Price)") +
labs(x = "ln(Price)" , y = "Count")

model <- lm(log(sprice/1000) ~ livarea + beds + baths + lgelot + pool + age, data = stckton4)
summary(model)

exp(.2530)

# Interaction variable
unristricted <- model <- lm(log(sprice/1000) ~ livarea + beds + baths + lgelot + pool + age + I(livarea * lgelot), data = stckton4)
summary(unristricted)

# ---------Chow Test
Restricted <- lm(log(sprice/1000) ~ livarea + beds + baths + pool + age, data = stckton4)
summary(Restricted)
anova(Restricted)

# ----
Unrestricted <- lm(log(sprice/1000) ~ livarea + beds + baths + pool + age + lgelot +  I(lgelot * livarea) +
I(lgelot * beds ) + I(lgelot * baths)+ I(lgelot * pool) + I(lgelot * age), data = stckton4)
summary(Unrestricted)
anova(Unrestricted)

qf(.95,6,1488)

library(ggplot2)
ggplot(br2, aes(br2$price)) +
geom_histogram(col="red",fill="green", alpha = .2) +
labs(title = "Histogram of Price") +
labs(x = "Price" , y = "Count")

model <- lm(log(price/1000) ~ I(sqft/100) + bedrooms + baths + age + owner + pool + traditional + fireplace + waterfront, data = br2)
summary(model)

100*(exp(.1099)-1)
model <- lm(log(price/1000) ~ I(sqft/100) + bedrooms + baths + age + owner + pool + traditional + fireplace +
waterfront + I(waterfront * traditional), data = br2)
summary(model)

Resstricted <- lm(log(price/1000) ~ I(sqft/100) + bedrooms + baths + age + owner + pool + fireplace +
waterfront, data = br2)
summary(Resstricted)
anova(Resstricted)

Unristricted <- lm(log(price/1000) ~ I(sqft/100) + bedrooms + baths + age + owner + pool + traditional + fireplace +
waterfront + I((sqft/100) * traditional) + I(bedrooms * traditional)+
I(baths * traditional) + I(age * traditional) + I(owner * traditional) + I(pool * traditional)+
I(fireplace * traditional) + I(waterfront * traditional), data = br2)
summary(Unristricted)
anova(Unristricted)

qf(.95,9,1062)
d = data.frame(sqft = 2500, bedrooms = 3, baths = 2, age = 20, owner = 1, pool = 0, traditional = 1, fireplace =1, waterfront =0)
predict(Unristricted, d)
exp(5.005)*1000

debugSource('D:/Class Notes/Fall 17 Classes/ECON/Assignment 6/Assignment 6/R code.R')
debugSource('D:/Class Notes/Fall 17 Classes/ECON/Assignment 6/Assignment 6/R code.R')
debugSource('D:/Class Notes/Fall 17 Classes/ECON/Assignment 6/Assignment 6/R code.R')
debugSource('D:/Class Notes/Fall 17 Classes/ECON/Assignment 6/Assignment 6/R code.R')
debugSource('D:/Class Notes/Fall 17 Classes/ECON/Assignment 6/Assignment 6/R code.R')
exit

library(ggplot2)
library(ggplot2)
library(ggplot2)
quit()
