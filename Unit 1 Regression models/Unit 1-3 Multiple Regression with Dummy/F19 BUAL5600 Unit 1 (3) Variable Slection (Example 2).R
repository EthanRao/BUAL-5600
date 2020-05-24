## Variable Selection
## Example 2
rm(list = ls())

library(ISLR)
library(leaps)
library(glmnet)
library(pls)

# Best Subset Selection (want to predict salary variable)
# Data clean-up
Hitters <- Hitters
dim(Hitters)
sum(is.na(Hitters$Salary)) # how many NAs we have
Hitters = na.omit(Hitters) # remove variables with NA for salary
sum(is.na(Hitters)) # check if anymore NAs

# The regsubsets function (leaps library) performs best subset selection by identifying 
# the best model that contains a given number of predictors
# where best is quantified using RSS.
regfit.full = regsubsets(Salary~.,Hitters)
summary(regfit.full) # by default only reports up to 8 variable model

# fit all 19 variables (R automatically encodes qualitative variables)
# use contrasts() to see how they are coded - affects interpretation of coeffs
regfit.full=regsubsets(Salary~., data=Hitters, nvmax=19)
reg.summary=summary(regfit.full)
regnames(reg.summary) # which metrics are contained in the summary
reg.summary$rsq # view R^2 of each model fit (i.e. best 1, 2, 3 variable model etc...)

# plot results
par(mfrow=c(2,2))
# RSS
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
# adj R^2
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
m <- which.max(reg.summary$adjr2) # find max adj R^2 model 
points(m,reg.summary$adjr2[m], col="red",cex=2,pch=20) # add point to highlight max
# Cp
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
cp.min <- which.min(reg.summary$cp)
points(cp.min,reg.summary$cp[cp.min],col="red",cex=2,pch=20)
# BIC
BIC.min <- which.min(reg.summary$bic)
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
points(BIC.min,reg.summary$bic[BIC.min],col="red",cex=2,pch=20)

# shows which features are in full model with 1 to 19 variables
graphics::plot(regfit.full,scale="r2")
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="Cp")
plot(regfit.full,scale="bic")
coef(regfit.full,6)

##### 
# Forward and Backward Stepwise Selection

# forward stepwise and output
regfit.fwd = regsubsets(Salary~.,data=Hitters,nvmax=19,method="forward")
summary(regfit.fwd)
