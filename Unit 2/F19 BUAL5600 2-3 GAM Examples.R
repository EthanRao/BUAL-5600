library(ISLR)
library(dplyr)
library(ggplot2)

library(splines)

# Get min/max values of age using the range() function
agelims = Wage %>%
  select(age) %>%
  range

# Generate a sequence of age values spanning the range
age_grid = seq(from = min(agelims), to = max(agelims))

# Fit a regression spline using basis functions
fit = lm(wage~bs(age, knots = c(25,40,60)), data = Wage)

# Predict the value of the generated ages, 
# returning the standard error using se = TRUE
pred = predict(fit, newdata = list(age = age_grid), se = TRUE)

# Compute error bands (2*SE)
se_bands = with(pred, cbind("upper" = fit+2*se.fit, 
                            "lower" = fit-2*se.fit))

# Plot the spline and error bands
ggplot() +
  geom_point(data = Wage, aes(x = age, y = wage)) +
  geom_line(aes(x = age_grid, y = pred$fit), color = "#0000FF") + 
  geom_ribbon(aes(x = age_grid, 
                  ymin = se_bands[,"lower"], 
                  ymax = se_bands[,"upper"]), 
              alpha = 0.3) +
  xlim(agelims)

# Specifying knots directly: 6 basis functions
with(Wage, dim(bs(age, knots = c(25,40,60))))

# Specify desired degrees of freedom, select knots automatically: 
# still 6 basis functions
with(Wage, dim(bs(age, df = 6)))

# Show me where the knots were placed
with(Wage, attr(bs(age, df = 6),"knots"))

fit2 = lm(wage~ns(age, df = 4), data = Wage)
pred2 = predict(fit2, newdata = list(age = age_grid), se = TRUE)

# Compute error bands (2*SE)
se_bands2 = with(pred, cbind("upper" = fit+2*se.fit, 
                             "lower" = fit-2*se.fit))

# Plot the natural spline and error bands
ggplot() +
  geom_point(data = Wage, aes(x = age, y = wage)) +
  geom_line(aes(x = age_grid, y = pred2$fit), color = "#0000FF") + 
  geom_ribbon(aes(x = age_grid, 
                  ymin = se_bands2[,"lower"], 
                  ymax = se_bands2[,"upper"]), 
              alpha = 0.3) +
  xlim(agelims)

# Fit 2 smoothing splines
fit_smooth = with(Wage, smooth.spline(age, wage, df = 16))
fit_smooth_cv = with(Wage, smooth.spline(age, wage, cv = TRUE))

# Plot the smoothing splines
ggplot() +
  geom_point(data = Wage, aes(x = age, y = wage)) +
  geom_line(aes(x = fit_smooth$x, y = fit_smooth$y, 
                color = "16 degrees of freedom"))  +
  geom_line(aes(x = fit_smooth_cv$x, y = fit_smooth_cv$y, 
                color = "6.8 effective degrees of freedom")) +
  theme(legend.position = 'bottom')+ 
  labs(title = "Smoothing Splines", colour="")

# Fit GAMs
gam1 = lm(wage ~ ns(year, 4) + ns(age, 5) + education, data = Wage)
summary(gam1)
gam1 = gam(wage ~ ns(year, 4) + ns(age, 5) + education, data = Wage)

library(gam)
gam2 = gam(wage ~ s(year, 4) + s(age, 5) + education, data = Wage)
par(mfrow = c(1,3))
plot(gam2, se = TRUE, col = "blue")

par(mfrow = c(1,3))
plot(gam1, se = TRUE, col = "red")

gam_no_year = gam(wage ~ s(age, 5) + education, data = Wage)
gam_linear_year = gam(wage ~ year + s(age, 5) + education, data = Wage)
print(anova(gam_no_year, gam_linear_year, gam2, test = "F"))

gam_no_year = gam(wage ~ s(age, 5) + education, data = Wage)
gam_linear_year = gam(wage ~ year + s(age, 5) + education, data = Wage)
print(anova(gam_no_year, gam_linear_year, gam2, test = "F"))

summary(gam2)

preds = predict(gam_linear_year, newdata = Wage)

# Logistic GAMs
gam_logistic = gam(I(wage>250) ~ year + s(age, df = 5) + education, 
                   family = binomial, data = Wage)
par(mfrow=c(1,3))
plot(gam_logistic, se = TRUE, col = "green")

with(Wage, table(education, I(wage>250)))

college_educated = Wage %>%
  filter(education != "1. < HS Grad")

gam_logistic_subset = gam(I(wage>250) ~ year + s(age, df = 5) + education, 
                          family = binomial, data = college_educated)
par(mfrow=c(1,3))
plot(gam_logistic_subset, se = TRUE, col = "green")

#### Example 2
# Inclass activity
install.packages("mgcv")
install.packages("car")
install.packages("broom")
install.packages("DMwR")
library(mgcv)
library(car)
library(gpairs)
library(broom)
library(DMwR)

str(Prestige)
summary(Prestige)
is.na(Prestige$type)
Prestige2<-na.omit(Prestige)
gpairs(Prestige2[1:5])

train_rows = sample(1:nrow(Prestige2), size = 0.7*nrow(Prestige2))
training = Prestige2[train_rows,]
test = Prestige2[-train_rows,]

plot(x = training$income, y = training$prestige)

sp_cv = smooth.spline(x = training$income, y = training$prestige, cv =T)
sp_cv

sp2 = smooth.spline(x = training$income, y = training$prestige, df = 2)
sp4 = smooth.spline(x = training$income, y = training$prestige, df = 4)
sp10 = smooth.spline(x = training$income, y = training$prestige, df = 10)
sp20 = smooth.spline(x = training$income, y = training$prestige, df = 20)
sp50 = smooth.spline(x = training$income, y = training$prestige, df = 50)

plot(x = Prestige$income, y = Prestige$prestige, main = "Income vs Prestige")
lines(sp2, col = "blue")
lines(sp4, col = "red", lwd = 2)
lines(sp10, col = "green")
lines(sp20, col = "orange")
lines(sp50, col = "black")

require (DMwR)
predicted = predict(sp_cv, test$income)$y
DMwR::regr.eval(test$prestige, predicted)

## Loading required package: splines
require(splines)
head(ns(Prestige$income, df = 3))

gamMod = mgcv::gam(prestige ~ ns(income,3) + ns(education,4) + type, data = training)
summary(gamMod)

predicted = predict(gamMod, test)

DMwR::regr.eval(test$prestige, predicted)




model1<-gam(prestige~education+income+women+census+type, data=training)
summary(model1)

model2<-gam(prestige~education+income+census+type, data=training)
summary(model2)

model3<-gam(prestige~education+income, data=training)
summary(model3)

model4<-gam(prestige~s(education)+s(income), data=training)
summary(model4)

plot(model4, pages=1)


