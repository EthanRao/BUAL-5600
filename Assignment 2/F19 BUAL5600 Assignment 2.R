library("ISLR")
library ("gpairs")
library("corrplot")

names(Carseats)
head(Carseats)

#### Question 1
str(Carseats)
summary(Carseats)


#### Question 2
# Omit the categorical variables
Carseats1<-Carseats[,c(1,2,3,4,5,6,8,9)]
summary(Carseats1)
# Pairwise Correlation Matrix
gpairs(Carseats1)
# Create correlation table after dropping the Advertising variable
cor(Carseats1[,-4])

#### Question 3-5
# Fit the multiple regression model
q3fit1<-lm(Sales~CompPrice, data=Carseats)
# Print output
summary(q3fit1)
# Examine residual plot and normal Q-Q plot
# par(mar=c(1,1,1,1)) # If you got error of the margin size
par(mfrow=c(2,2))
plot(q3fit1)

# Copy the lines from 24-28 and paste here.
# Modify the copy-pasting code
# Chance the name of predictor and model name

#### Question 6
confint(q3fit1) # Reject H0 if the confidence interval contains 0


#### Question 7
plot(q3fit1)
