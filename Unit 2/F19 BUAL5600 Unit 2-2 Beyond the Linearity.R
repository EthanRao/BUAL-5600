## Unit 2: Beyond the Linearity
library(dplyr)
library(ggplot2)
## 1. Load data
data(Wage,package = "ISLR")

## Explore the data
head(Wage)
names(Wage)#to check all the columns

## Polynomial
# This syntax fits a linear model, using the lm() function, in order to predict wage using a fourth-degree polynomial 
# in age: poly(age,4). The poly() command allows us to avoid having to write out a long formula with powers of age. 
# The function returns a matrix whose columns are a basis of orthogonal polynomials, 
# which essentially means that each column is a linear combination of the variables age, age^2, age^3 and age^4.

fit = lm(wage ~ poly(age, 4), data = Wage)
coef(summary(fit))

# If we prefer, we can also use poly() to obtain age, age^2, age^3 and age^4 directly. 
# We can do this by using the raw = TRUE argument to the poly() function. 
fit2 = lm(wage~poly(age, 4, raw = TRUE), data = Wage)
coef(summary(fit2))

# Prediction
# Get min/max values of age using the range() function
agelims = Wage %>%
  select(age) %>%
  range

# Generate a sequence of age values spanning the range
age_grid = seq(from = min(agelims), to = max(agelims))

# Predict the value of the generated ages,
# returning the standard error using se = TRUE
preds = predict(fit, newdata = list(age = age_grid), se = TRUE)

# Compute error bands (2*SE)
se_bands = cbind("upper" = preds$fit+2*preds$se.fit, 
                 "lower" = preds$fit-2*preds$se.fit)

preds2 = predict(fit2, newdata = list(age = age_grid), se = TRUE)

# Calculate the difference between the two estimates, print out the first few values
head(abs(preds$fit - preds2$fit))

## Deciding on a degree
fit_1 = lm(wage~age, data = Wage)
fit_2 = lm(wage~poly(age,2), data = Wage)
fit_3 = lm(wage~poly(age,3), data = Wage)
fit_4 = lm(wage~poly(age,4), data = Wage)
fit_5 = lm(wage~poly(age,5), data = Wage)
print(anova(fit_1,fit_2,fit_3,fit_4,fit_5))

print(coef(summary(fit_5)))
(-11.983)^2

fit_1 = lm(wage~education+age, data = Wage)
fit_2 = lm(wage~education+poly(age,2), data = Wage)
fit_3 = lm(wage~education+poly(age,3), data = Wage)
print(anova(fit_1,fit_2,fit_3))

fit = glm(I(wage>250)~poly(age,4), data = Wage, family = binomial)
preds = predict(fit, newdata = list(age = age_grid), se = TRUE)

pfit = exp(preds$fit) / (1+exp(preds$fit))

se_bands_logit = cbind("upper" = preds$fit+2*preds$se.fit, 
                       "lower" = preds$fit-2*preds$se.fit)

se_bands = exp(se_bands_logit) / (1+exp(se_bands_logit))

high = Wage %>%
  filter(wage > 250)

low = Wage %>%
  filter(wage <= 250)

ggplot() +
  geom_rug(data = low, aes(x = jitter(age), y = wage), sides = "b", alpha = 0.3) +
  geom_rug(data = high, aes(x = jitter(age), y = wage), sides = "t", alpha = 0.3) +
  geom_line(aes(x = age_grid, y = pfit), color = "#0000FF") +
  geom_ribbon(aes(x = age_grid, 
                  ymin = se_bands[,"lower"], 
                  ymax = se_bands[,"upper"]), 
              alpha = 0.3) +
  xlim(agelims) +
  ylim(c(0,1)) +
  labs(title = "Degree-4 Polynomial",
       x = "Age",
       y = "P(wage > 250)")

## Step function
table(cut(Wage$age,4))
fit_step = lm(wage~cut(age,4), data = Wage)
print(coef(summary(fit)))

# Predict the value of the generated ages, returning the standard error using se = TRUE
preds = predict(fit_step, newdata = list(age = age_grid), se = TRUE)

# Compute error bands (2*SE)
se_bands = cbind("upper" = preds$fit+2*preds$se.fit, 
                 "lower" = preds$fit-2*preds$se.fit)

# Plot
ggplot() +
  geom_point(data = Wage, aes(x = age, y = wage)) +
  geom_line(aes(x = age_grid, y = preds$fit), color = "#0000FF") +
  geom_ribbon(aes(x = age_grid, 
                  ymin = se_bands[,"lower"], 
                  ymax = se_bands[,"upper"]), 
              alpha = 0.3) +
  xlim(agelims) +
  labs(title = "Step Function")
## Cubic Splines
library(splines)
cubic.model=lm(wage~bs(age,knots=c(25,40,60)),data=Wage) #3 knots, seven parameters
## do prediction for every age integer
attach(Wage)
agelims=range(age)#get the upper and under border
agelims#18,80
age.grid=seq(from=agelims[1],to=agelims[2])#18-80
pred=predict(cubic.model,newdata=list(age=age.grid),se=T)#need to return the standard error of the predict
## plot the results and calculate the confidential interval wage^±2?????
plot(age,wage,col="gray")#plot the scatter plot of age and wage
title("Cubic Splines")
lines(age.grid,pred$fit,lwd=2)#plot the fitted spline curve  #line width
lines(age.grid,pred$fit+2*pred$se,lty="dashed")#+2 sigma  #line type
lines(age.grid,pred$fit-2*pred$se,lty="dashed")#-2 sigma  #line type

dim(bs(age,knots=c(25,40,60))) #fix the number of knots is 3,df =7-1 (intercept is 1)
dim(bs(age,df=6))#fix the df directly
attr(bs(age,df=6),"knots") #get the quantiles of 4th quantiles

## Natrual splines: (total df=K) with head and tail first let us see what the ns and bs function creates
head(ns(age,df=4))
head(bs(age,df=6))
natrual.model=lm(wage~ns(age,df=4),data=Wage) #choose 4 knots as a example
pred2=predict(natrual.model,newdata=list(age=age.grid),se=T)
plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
title("Natrual Splines")
lines(age.grid, pred2$fit,col="red",lwd=2)

## Smoothing sline:Since smoothing splines use every obs point as a knot,it is no need to specify the df.
smooth.1=smooth.spline(age,wage,df=16) #specify the df, not accurate,will display warning
smooth.2=smooth.spline(age,wage,cv=TRUE)# use cross validation method
smooth.2$df  #the final df achieved by cross validation
plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
title("Smoothing Spline")
lines(smooth.1,col="red",lwd=2)
lines(smooth.2,col="blue",lwd=2)#cross validation
legend("topright",legend=c("16 DF","6.8 DF"),col=c("red","blue"),lty=1,lwd=2,cex=.8)