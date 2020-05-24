install.packages("tidyr")
install.packages("magrittr")
install.packages("pls")

library(ISLR)
library(dplyr)
library(tidyr)
library(pls)
library(magrittr)
library(ggplot2)

# Question 1
# Part 1
# Omit empty rows
Hitters <-na.omit(Hitters) 

# Part 2
# 50-50 split
set.seed(1)
train = Hitters %>%
  sample_frac(0.5)
test = Hitters %>%
  setdiff(train)

# Part 3
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

## Question 2
# Exploring data
apply(x_train, 2, mean)
apply(x_train, 2, var)


### Question 3 erforming PCA

# create new data frame with centered variables
scaled_df <- apply(x_test, 2, scale)
head(scaled_df)

# Part 1: Find the appropriate number of PCs
# PVE
hitters.cov <- cov(scaled_df)
hitters.eigen <- eigen(hitters.cov)
str(hitters.eigen)

PVE <- hitters.eigen$values / sum(hitters.eigen$values)
round(PVE, 2)

# Scree plots and pca summary
pca_fit<-prcomp(x_train, center=TRUE, scale.=TRUE)
summary(pca_fit)

# means
pca_fit$center
# standard deviations
pca_fit$scale
#rotation
pca_fit$rotation
pca_fit$rotation <- -pca_fit$rotation
pca_fit$rotation

pca_fit$x <- - pca_fit$x
head(pca_fit$x)

screeplot(pca_fit,npcs = 19)
screeplot(pca_fit, npcs = 19, type = "lines")


#Part 3
# Biplot
biplot(pca_fit, scale = 0)

## Question 3
#Performing PCR
# Part 1: find the number of PCs
pcr_fit2 = pcr(Salary~., data = train, scale = TRUE, validation = "CV")

validationplot(pcr_fit2)
validationplot(pcr_fit2, val.type = "MSEP")
validationplot(pcr_fit2, val.type = "R2")

# Part 2
pcr_pred = predict(pcr_fit2, x_test, ncomp=4) ## replace 'xx' with the number of PCs
mean((pcr_pred-y_test)^2)

# Part 3
pcr_fit3 = pcr(y_train~x_train, scale = TRUE, ncomp = 4)# replace 'xx' with the number of PCs
summary(pcr_fit3)


## Question 5
# part 1: Performing PLS
set.seed(1)
pls_fit = plsr(Salary~., data = train, scale = TRUE, validation = "CV")
summary(pls_fit)

validationplot(pls_fit)
validationplot(pls_fit, val.type = "MSEP")
validationplot(pls_fit, val.type = "R2")

# Part 2
pls_pred = predict(pls_fit, x_test, ncomp = 2)
mean((pls_pred - y_test)^2)

# Part 3
pls_fit2 = plsr(Salary~., data = Hitters, scale = TRUE, ncomp = 2)
summary(pls_fit2)



http://archive.ics.uci.edu/ml/datasets.html

