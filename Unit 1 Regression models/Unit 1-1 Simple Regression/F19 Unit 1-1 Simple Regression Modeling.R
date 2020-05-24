---
  Title: Code for: BUAL5600 Predictive Modeling I
output: html_notebook
# Last update: September 4, 2019

# some setup
install.packages ("corrplot")
install.packages ("multcomp")

# Load the data
satData <- read.csv("http://goo.gl/UDv12g") #Online

# convert the Segment to a factor (categorical) variable
satData$Segment <- factor(satData$Segment)
head(satData)
summary(satData)

# correlation plot
library(corrplot)
corrplot.mixed(cor(satData[, -3]))

# product satisfaction by segment
aggregate(iProdSAT ~ Segment, satData, mean)

# ANOVA
sat.anova <- aov(iProdSAT ~ -1 + Segment, satData)
summary(sat.anova)

# plot the ANOVA estimates
library(multcomp)
par(mar=c(4,8,4,2))
plot(glht(sat.anova))

### END TOUR

# Advertising data

Advertising <- read.csv("~/Desktop/Advertising.csv")
View(Advertising)
head(Advertising)
pairs(Advertising [,-1])
cor (Advertising [,-1])

attach(Advertising)
plot(TV, Sales)
plot(Radio, Sales)
plot(Newspaper, Sales)

par(mfrow=c(1,3))
plot(TV, Sales)
plot(Radio, Sales)
plot(Newspaper, Sales)

# fit simple linear regression model using the function lm():
lm.fit1<-lm(Sales~TV, data=Advertising)
# print the estimates
lm.fit1
# Use the fuction summary() to get some outputs:
summary(lm.fit1)

# Create a table with fitted values and residuals:
data.frame(Advertising, fitted.value=fitted(lm.fit1), residual=resid(lm.fit1))

## Create a scatterplot with fitted line
plot(Sales~TV, data=Advertising)
abline(lm.fit1, col=2)

####### Load data 
sat.df <- read.csv("http://goo.gl/HKnl74")

#### Question 1
# Simulate satisfaction data
set.seed(08226)
nresp <- 500 # number of survey respondents
halo <- floor(rnorm(n=nresp, mean=0, sd=5))
rides <- floor(halo + rnorm(n=nresp, mean=80, sd=3)+7)
games <- floor(halo + rnorm(n=nresp, mean=70, sd=7)+10)
wait <- floor(halo + rnorm(n=nresp, mean=65, sd=10)+6)
clean <- floor(halo + rnorm(n=nresp, mean=85, sd=2)+4)
cor(rides, games)
distance <- rlnorm(n=nresp, meanlog=3, sdlog=1)
num.child <- sample(x=0:5, size=nresp, replace=TRUE, 
                    prob=c(0.3, 0.15, 0.25, 0.15, 0.1, 0.05))
weekend <- as.factor(sample(x=c("yes", "no"), size=nresp, replace=TRUE, 
                            prob=c(0.5,0.5)))
overall <- floor(halo + 0.5*rides + 0.1*games + 0.3*wait + 0.2*clean + 
                   0.03*distance + 5*(num.child==0) + 0.3*wait*(num.child>0) + 
                   rnorm(n=nresp, mean=0, sd=7) - 54)

sat.df <- data.frame(weekend, num.child, distance, rides, games, wait, clean, overall)
rm(nresp, weekend, distance, num.child, halo, rides, games, wait, clean, 
   overall)


#### Amusement park data
# Overview of linear regression
summary(sat.df)

# Question 3
# check basic data suitability
library("gpairs")

gpairs(sat.df)

# fix distance
sat.df$logdist <- log(sat.df$distance)
gpairs(sat.df)


# NOTE: if using RStudio, it can be helpful to "Clear All" plots if a plot
# appears too small or too large; 
# this is a known issue in RStudio with various packages such as corrplot
#

library(corrplot)

corrplot.mixed(cor(sat.df[ , c(2, 4:9)]), upper="ellipse")

plot(overall~rides, data=sat.df,
     xlab="Satisfaction with Rides", ylab="Overall Satisfaction")


# Fitting a model with a single predictor
lm(overall~rides, data=sat.df)

## Question 5 and 6
# Try it!: sd(m1$residuals)
m1 <- lm(overall~rides, data=sat.df)

plot(overall~rides, data=sat.df,
     xlab="Satisfaction with Rides", ylab="Overall Satisfaction")
abline(m1, col='blue')

