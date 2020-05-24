install.packages("tidyr")
install.packages("magrittr")

library(ISLR)
library(dplyr)
library(tidyr)
library(pls)
library(magrittr)

# Omit empty rows
Hitters <-na.omit(Hitters) 
Hitters1<-Hitters[,(1:19)]
Hitters2<-Hitters1[,-(14:15)]

### Performing PCA
pca.fit<-prcomp(Hitters2, center=TRUE, scale.=TRUE)
print(pca.fit)
plot(pca.fit)
biplot(pca.fit)
summary(pca.fit)
predict(pca.fit, newdata=tail(Hitters2,2))


### Performing PCR
set.seed(2)
pcr_fit = pcr(Salary~., data = Hitters, scale = TRUE, validation = "CV")
summary(pcr_fit)

plot(pcr_fit)
biplot(pcr_fit)

validationplot(pcr_fit)
validationplot(pcr_fit, val.type = "MSEP")
validationplot(pcr_fit, val.type = "R2")

set.seed(1)

train = Hitters %>%
  sample_frac(0.5)

test = Hitters %>%
  setdiff(train)

pcr_fit2 = pcr(Salary~., data = train, scale = TRUE, validation = "CV")

validationplot(pcr_fit2)
validationplot(pcr_fit2, val.type = "MSEP")
validationplot(pcr_fit2, val.type = "R2")


x_train = model.matrix(Salary~., train)[,-1]
x_test = model.matrix(Salary~., test)[,-1]

y_train = train %>%
  select(Salary) %>%
  unlist() %>%
  as.numeric()

y_test = test %>%
  select(Salary) %>%
  unlist() %>%
  as.numeric()

pcr_pred = predict(pcr_fit2, x_test, ncomp=7)
mean((pcr_pred-y_test)^2)


x = model.matrix(Salary~., Hitters)[,-1]

y = Hitters %>%
  select(Salary) %>%
  unlist() %>%
  as.numeric()

pcr_fit2 = pcr(y~x, scale = TRUE, ncomp = 7)
summary(pcr_fit2)
```

## Performing PLS

set.seed(1)
pls_fit = plsr(Salary~., data = train, scale = TRUE, validation = "CV")
summary(pls_fit)

validationplot(pls_fit)
validationplot(pls_fit, val.type = "MSEP")
validationplot(pls_fit, val.type = "R2")

pls_pred = predict(pls_fit, x_test, ncomp = 2)
mean((pls_pred - y_test)^2)

pls_fit2 = plsr(Salary~., data = Hitters, scale = TRUE, ncomp = 2)
summary(pls_fit2)

# MLR model
