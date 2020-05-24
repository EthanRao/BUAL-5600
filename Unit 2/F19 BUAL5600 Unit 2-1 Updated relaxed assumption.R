mm<-lm(Sales~TV+Radio+Newspaper, data=Advertising)
summary(mm)

require(stats)
reg<-lm(Radio~TV, data=Advertising)
coeff=coefficients(reg)
# equation of the line : 
eq = paste0("y = ", round(coeff[2],1), "*x ", round(coeff[1],1))
# plot
plot(Advertising$Newspaper, Advertising$Radio, main=eq)
abline(reg, col="red")

reg1<-lm(Radio~Newspaper, data=Advertising)
coeff=coefficients(reg1)
# equation of the line : 
eq = paste0("y = ", round(coeff[2],1), "*x ", round(coeff[1],1))

# plot
plot(Advertising$Newspaper, Advertising$Radio, main=eq)
abline(reg, col="blue")

cor(Advertising)

mm<-lm(Sales~TV+Radio, data=Advertising)
summary(mm)

mm<-lm(Sales~TV+Radio+TV*Radio, data=Advertising)
summary(mm)

mm1<-lm(Balance~Income+Student+Income*Student, data=Credit)
summary(mm1)

# Auto Data set
names(Auto)
summary(Auto)
plot(Auto)
gpairs(Auto)

plot(Auto$mpg~Auto$cylinders) # Looks cyliners are treated as continuous (numeric)
Cylinders1<-as.factor(Auto$cylinders) # Convert the cylinders to factor
Year1<-as.factor(Auto$year)
Origin1<-as.factor(Auto$origin)

summary(Cylinders1) 
plot(Auto$mpg~Cylinders1) # looks okay; 
                          # a side-by-side boxplot is more appropriate display 
                          # for factor(Cylinder1) and continulus variable(mpg)

fit1<-lm(Auto$mpg~Cylinders1)
summary(fit1)

summary(Year1)
plot(Auto$mpg~Year1)

fit2<-lm(Auto$mpg~Year1)
summary(fit2)

plot(Auto$mpg~Auto$horsepower) # R recognizes horsepower as a factor
horsepower2<-as.numeric(Auto$horsepower) # transform the variable to continuous
plot(Auto$mpg~horsepower2) # scatterplot seems more make sense

mm2_1<-lm(Auto$mpg~horsepower2)
plot(mm2_1)
mm2_2<-lm(Auto$mpg~horsepower2+I(horsepower2^2))
summary (mm2_1)
summary (mm2_2)


mm2_3<-lm(Auto$mpg~horsepower2+I(horsepower2^2)+I(horsepower2^3)+I(horsepower2^4)+I(horsepower2^5))
summary(mm2_3)
plot(mm2_3)

AIC(mm2_1, mm2_2, mm2_3)
BIC(mm2_1, mm2_2, mm2_3)