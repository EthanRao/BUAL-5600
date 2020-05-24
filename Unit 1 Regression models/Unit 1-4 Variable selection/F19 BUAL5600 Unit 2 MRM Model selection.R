
## Example 1

# Example 1
library(leaps)
house = read.table("http://www.rossmanchance.com/iscam2/data/housing.txt", header = T,sep = "\t")
attach(house)
names(house)

## Descriptive Statistics
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  text(0.5, 0.5, txt)
}

summary(house)


pairs(cbind(price, sqft, bedrooms, baths), lower.panel = panel.cor, pch = 18)

## Run a linear model trying to predict price
summary(lm(price ~ sqft))

summary(lm(price ~ bedrooms))

# The p-values for both explanatory variables (sqft and bedrooms) are significant. Sqft seems more significant, and indeed, the first model has a higher \( R^2 \) - that is, a higher proportion of the variability in price is explained by sqft (42.07%) than by number of bedrooms (8.08%).
# However, it is important for us to ask whether either of the relationships actually fit the technical conditions of the linear regression model. We can see from the pairs plots that the relationships look Linear, we'll assume the variables were collected Independently, but the Normality and the Error structure we can check using residual plots.

par(mfrow = c(1, 2))
plot(lm(price ~ sqft)$fitted, lm(price ~ sqft)$resid, , xlab = "fitted w sqft", 
     ylab = "resid", pch = 18)
abline(h = 0)
plot(lm(price ~ bedrooms)$fitted, lm(price ~ bedrooms)$resid, , xlab = "fitted w bed", 
     ylab = "resid", pch = 18)
abline(h = 0)
# For both of the plots, it seems like the residuals have higher variability for positive residuals. Additionally, it seems that the variability of the residuals increases for larger fitted observations. A natural log transformation should fix both of these problems.

par(mfrow = c(1, 2))
plot(lm(log(price) ~ sqft)$fitted, lm(log(price) ~ sqft)$resid, , xlab = "fitted w sqft", 
     ylab = "resid for ln", pch = 18)
abline(h = 0)
plot(lm(log(price) ~ bedrooms)$fitted, lm(log(price) ~ bedrooms)$resid, , xlab = "fitted w bed", 
     ylab = "resid for ln", pch = 18)
abline(h = 0)
# Though no residual plot will ever look perfect, these residual plots seem to fit the technical conditions of the model better than the untransformed data.


library("leaps")
names(house)
leaps=regsubsets(price~sqft+bedrooms+baths, data=house)
summary(leaps)

par(mfrow=c(2,2))
plot(leaps, scale="r2")
plot(leaps, scale="adjr2")
plot(leaps, scale="Cp")
plot(leaps, scale="bic")

null=lm(price~1, data=house)
null

full=lm(price~., data=house)
full

step(null, scope=list(lower=null, upper=full), direction="forward")
step(full, data=Housing, direction="backward")
step(null, scope = list(upper=full), data=Housing, direction="both")
## Combining variables
# We'll stick with the transformed data. What happens when we try to predict price (log(price), here) using BOTH sqft and bedrooms?

summary(lm(log(price) ~ sqft + bedrooms))
summary(lm(log(price) ~ sqft))

## Prediction
# As with the prediction intervals we had when we had a single sample, we can now create intervals for either an average (a confidence interval) of an individual (a prediction interval).
predict(lm(log(price) ~ sqft), newdata = data.frame(sqft = 2000), interval = "confidence")
# I am 95% confident that the true average log price for a 2000 sqft home is between 12.79 log$ and 12.99 log$. Back transforming can be a little tricky.
predict(lm(log(price) ~ sqft), newdata = data.frame(sqft = 2000), interval = "prediction")
# I am 95% of homes with 2000 sqft are between 11.99 log$ and 13.99 log$. Now back transforming is easy (because there are no averages), so 95% of homes with 2000 sqft are between $161,126 and $977,301.

# Plotting the confidence bounds on a scatterplot
plot(sqft, log(price), pch = 18)
sqftlm = lm(log(price) ~ sqft)
abline(sqftlm, col = "red")
newX = seq(min(sqft), max(sqft), 1)
prd.CI = predict(sqftlm, newdata = data.frame(sqft = newX), interval = "confidence", 
                 level = 0.95)
lines(newX, prd.CI[, 2], col = "blue", lty = 2)
lines(newX, prd.CI[, 3], col = "blue", lty = 2)
prd.PI = predict(sqftlm, newdata = data.frame(sqft = newX), interval = "prediction", 
                 level = 0.95)
lines(newX, prd.PI[, 2], col = "green", lty = 3)
lines(newX, prd.PI[, 3], col = "green", lty = 3)

