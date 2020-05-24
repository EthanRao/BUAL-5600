# Assignmant 3
library(gpairs)
library(leaps)
# Question 2
## Part(i)
real.estate <- read.csv("real-estate.csv")


## Part (ii)
# (i)
summary(real.estate)
# All of the variable is quantitative.
# (ii)
dev.off()
gpairs(real.estate[,2:11])


## Part (iii)
# real.estate$Air<-as.factor(real.estate[,6])
# or 
# real.estate$Air<-as.factor(real.estate$Airconditioning)
## Extending the code
real.estate$Bathroom <- as.factor(real.estate$Bathroom)
real.estate$Bedroom <- as.factor(real.estate$Bedroom)
real.estate$Airconditioning <- as.factor(real.estate$Garage)
real.estate$Pool <- as.factor(real.estate$Pool)
real.estate$Quality <- as.factor(real.estate$Quality)
real.estate$AdjHighway <- as.factor(real.estate$AdjHighway)
summary(real.estate)
# Question 3
## Part(i)
null<- lm(Price~1-ID, data=real.estate)
full <- lm(Price ~ . -ID, data=real.estate)

## Part (ii)
step(null, scope=list(lower=null, upper=full), data=real.estate, direction="forward")
step(full, data=real.estate, direction="backward")
step(null, scope = list(upper=full), data=real.estate, direction="both")
bs2 <- lm(formula = Price ~ Sqft + Quality + YearBuild + Lot + Bedroom + 
     Bathroom + Airconditioning + Pool + AdjHighway, data = real.estate)
## Part (iii)
bs1=regsubsets(Price ~ . -ID, data=real.estate)
summary(bs1)

par(mar=c(1,1,1,1))
dev.off()
par(mfrow=c(2,2))
plot(bs1, scale="bic")
plot(bs1, scale="Cp")
plot(bs1, scale="adjr2")
plot(bs1, scale="r2")

## Part (iv)
## Write the best model
best1=lm(formula = Price ~ Sqft + Quality + YearBuild + Lot + 
           Bathroom + Airconditioning + AdjHighway, data = real.estate)
best2=lm(formula = Price ~ Sqft + Quality + YearBuild + Lot + Bedroom + 
           Bathroom + Airconditioning + Pool + AdjHighway, data = real.estate)

anova(best1, best2)


best=lm(formula = Price ~ Sqft + Quality + YearBuild + Lot + Bedroom + 
          Bathroom + Airconditioning + Pool + AdjHighway, data = real.estate)

## Part (vii)
plot(best)

