# we will use predict "income" in the "Mroz" dataset using pls. 

install.packages("pls")
install.packages("Ecdat")

library(pls)
library(Ecdat)

data("Mroz")
str(Mroz)

# First, we must prepare our data by dividing it into a training and test set.
# We will do this by doing a 50/50 split of the data.

set.seed(777)
train<-sample(c(T,F),nrow(Mroz),rep=T) #50/50 train/test split
test<-(!train)
#50/50 train/test split
#Assignment T/F randomly, then split the sample into tro group

set.seed(777)
pls.fit<-plsr(income~.,data=Mroz,subset=train,scale=T,validation="CV")
summary(pls.fit)

# here are 17 components because there are 17 independent variables. 
# You can see that after component 3 or 4 there is little improvement in the variance explained in the dependent variable.
validationplot(pls.fit)
validationplot(pls.fit,val.type = "MSEP")
validationplot(pls.fit,val.type = "R2")

## Addition 
pls.RMSEP = RMSEP(pls.fit, estimate="CV")
plot(pls.RMSEP, main="RMSEP PLS", xlab="components")
min_comp = which.min(pls.RMSEP$val)
points(min_comp, min(pls.RMSEP$val), pch=1, col="red", cex=1.5)
min_comp

ncomp.onesigma <- selectNcomp(pls.fit, method = "onesigma", plot = TRUE)
ncomp.permut <- selectNcomp(pls.fit, method = "randomization", plot = TRUE)


B <- coef(pls.fit, ncomp = 3, intercept = TRUE)
B

set.seed(777)
pls.pred<-predict(pls.fit,Mroz[test,],ncomp=3)
mean((pls.pred-Mroz$income[test])^2)

plot(pls.fit)
plot(pls.fit, ncomp=9, asp=1, line=TRUE)
biplot(pls.fit)

