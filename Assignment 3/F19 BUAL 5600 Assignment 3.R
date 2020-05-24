# Assignmant 3
library(gpairs)
library(leaps)
# Question 2
## Part(i)
summary(real.estate)

## Part (ii)
dev.off()
gpairs(real.estate)
gpairs(real.estate[,2:12])

## Part (iii)
real.estate$Air<-as.factor(real.estate[,6])
# or 
real.estate$Air<-as.factor(real.estate$Airconditioning)
## Extending the code

# Question 3
## Part(i)
null<- lm(Price~1-ID, data=real.estate)
full <- lm(Price ~ . -ID, data=real.estate)

## Part (ii)
step(null, scope=list(lower=null, upper=full), data=real.estate, direction="forward")
step(full, data=real.estate, direction="backward")
step(null, scope = list(upper=full), data=real.estate, direction="both")

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
best1=lm(Price~xx, data=real.estate)
best2=lm(Price~xx, data=real.estate)

anova(best1, best2)


best=lm(Price~xx, data=real.estate)

## Part (vii)
plot(best)

