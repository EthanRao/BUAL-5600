## Assignment 5 ##

##### Question 1
library(faraway)

## Q1
data(orings)
plot(damage/6 ~ temp, orings, 
     main = "NASA Space Shuttle O-Ring Failures",
     xlim=c(25,85), ylim = c(0,1), 
     xlab="Temperature", ylab="Prob of damage",
     pch = 19, col = 4)

lmod <- lm(damage/6 ~ temp, orings)
summary(lmod)
abline(lmod)


##### fit the logit model: logistic regression
logitmod <- glm(cbind(damage,6-damage) ~ temp, family=binomial, orings)
summary(logitmod)

plot(damage/6 ~ temp, orings, xlim=c(25,85), ylim = c(0,1), xlab="Temperature", ylab="Prob of damage")
x <- seq(25,85,1)
lines(x,ilogit(11.6630-0.2162*x))


##### fit the probit model
probitmod <- glm(cbind(damage,6-damage) ~ temp, family=binomial(link=probit), orings)
summary(probitmod)
lines(x,pnorm(5.5915-0.1058*x),lty=2)

## Q2: Find odds ratio
exp(coef(logitmod))

## Q3: pedict the response a 31F for the both models
ilogit(11.6630-0.2162*31)
pnorm(5.5915-0.1058*31)

##### Question 2
head(warpbreaks)


breaksmodel1<-glm(breaks~wool+tension, warpbreaks, family=poisson)
summary(breaksmodel1)
exp(coef(breaksmodel1))

breaksmodel2<-glm(breaks~wool*tension, warpbreaks, family=poisson)
summary(breaksmodel2)

anova(breaksmodel1, breaksmodel2)

