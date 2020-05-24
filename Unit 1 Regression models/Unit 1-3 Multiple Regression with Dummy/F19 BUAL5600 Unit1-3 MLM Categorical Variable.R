library(ISLR)

head(Credit)
str(Credit)

attach(Credit)

#Dummy coding
dGender <- as.numeric(Gender) - 1

#Visualization
#use the plot() function to create a box plot
#what does the relationship between Gender and Credit Card Balance look like?
plot(Gender, Balance, main="Card Blance by Gender", xlab="Gender", ylab="CreditCard Balance ($10,000s)")

#Summary Statistics
#what are the mean and standard deviation of Gender?
mean(dGender)
sd(dGender)
#what is the correlation between Credit Card Balance and Gender?
cor(dGender, Balance)

#create a linear model using lm(FORMULA, DATAVAR)
#predict Credit Card Balance using Gender
lm.fit1<- lm(Balance ~ dGender)
lm.fit1<- lm(Balance ~ Gender)
#generate model summary
summary(lm.fit1)

#predict Credit Card Balance using Ethnicity
dEthnicity <- as.numeric(Ethnicity) - 1
plot(Ethnicity, Balance, main="Card Blance by Ethnicity", xlab="Ethnicity", ylab="CreditCard Balance ($10,000s)")

lm.fit2<- lm(Balance ~ dEthnicity)
lm.fit2<- lm(Balance ~ Ethnicity)
summary(lm.fit2)

# Which model is better?
lm.fit0<-lm(Balance~1) # Null Model
summary(lm.fit0)

anova(lm.fit0, lm.fit1)
anova(lm.fit0, lm.fit2)

lm.fit3<-lm(Balance~dGender+dEthnicity)
summary(lm.fit3)

##### Class Activity
head(NFL.data)
str(NFL.data)

attach(NFL.data)
#Dummy coding
dCONF <- as.numeric(CONF) - 1
plot(CONF, TOTAL, main="Total Salary by Conference", xlab="Conference", ylab="Total Salary ($1,000s)")
mean(dCONF)
sd(dCONF)
cor(dCONF, TOTAL)

fit1<-lm(TOTAL~dCONF)
summary(fit1)
