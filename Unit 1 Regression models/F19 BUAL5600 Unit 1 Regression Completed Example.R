# Assume that we are fitting a multiple linear regression
# on the MTCARS data
install.packages("car")
library(car)
summary(mtcars)

require(graphics)
pairs(mtcars, main = "mtcars data", gap = 1/4)

## possibly more meaningful, e.g., for summary() or bivariate plots:
mtcars2 <- within(mtcars, {
  vs <- factor(vs, labels = c("V", "S"))
  am <- factor(am, labels = c("automatic", "manual"))
  cyl  <- ordered(cyl)
  gear <- ordered(gear)
  carb <- ordered(carb)
})
summary(mtcars2)

fit <- lm(mpg~disp+hp+wt+drat, data=mtcars2)
par(mfrow=c(2,2))
plot(fit)


# Assessing Outliers
outlierTest(fit) # Bonferonni p-value for most extreme obs
dev.off()
qqPlot(fit, main="QQ Plot") #qq plot for studentized resid 
leveragePlots(fit) # leverage plots

# Influential Observations
# added variable plots 
# also called partial-regression plots for linear and generalized linear models.
avPlots(fit)
# Cook's D plot
# identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(mtcars)-length(fit$coefficients)-2)) 
plot(fit, which=4, cook.levels=cutoff)
# Influence Plot 
influencePlot(fit, id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

summary(influence.measures(fit))
# Normality of Residuals
# qq plot for studentized resid
qqPlot(fit, main="QQ Plot")
# distribution of studentized residuals
library(MASS)
sresid <- studres(fit) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)

# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(fit)
# plot studentized residuals vs. fitted values 
spreadLevelPlot(fit)

# Evaluate Collinearity
vif(fit) # variance inflation factors 
sqrt(vif(fit)) > 2 # problem?

# Evaluate Nonlinearity
# component + residual plot 
crPlots(fit)
# Ceres plots 
ceresPlots(fit)

# Non-independence of Errors
# Test for Autocorrelated Errors
durbinWatsonTest(fit)


# Global test of model assumptions
# The gvlma( ) function in the gvlma package, 
# performs a global validation of linear model assumptions as well separate evaluations 
# of skewness, kurtosis, and heteroscedasticity.
install.packages("gvlma")
library(gvlma)
gvmodel <- gvlma(fit) 
summary(gvmodel)



library(car)
options(contrasts=c("contr.sum", "contr.poly")) # use deviation regressors (not really necessary)

Moore$fcategory <- factor(Moore$fcategory, c("low", "medium", "high")) # reorder levels
Moore$partner.status <- factor(Moore$partner.status, c("low", "high"))

moore.anova <- lm(conformity ~ fcategory*partner.status, data=Moore) # fit ANOVA model
Anova(moore.anova)

# unusual data diagnostics

influencePlot(moore.anova)
av.plots(moore.anova, ask=FALSE)
outlier.test(moore.anova)

# plot means with and without subjects 16 and 19

with(Moore, interaction.plot(fcategory, partner.status, conformity,
                             type="b", main="With Subjects 16 and 19"))
with(Moore[-c(16,19),] , interaction.plot(fcategory, partner.status, conformity,
                                          type="b", main="Removing Subjects 16 and 19"))

moore.anova.1 <- update(moore.anova, subset=-c(16, 19)) # redo analysis
Anova(moore.anova.1)

# Alternative ANCOVA (or dummy regression)

scatterplot(conformity ~ fscore|partner.status, data=Moore, smooth=FALSE, labels=1:45)

Moore$fscored <- with(Moore, fscore - mean(fscore)) # mean deviations (not strictly necessary)
moore.ancova <- lm(conformity ~ fscored*partner.status, data=Moore) # fit ANCOVA model
summary(moore.ancova)

# unusual data diagnostics

influencePlot(moore.ancova)
av.plots(moore.ancova, ask=FALSE)
outlier.test(moore.ancova)

# remove influential obs.

moore.ancova.1 <- update(moore.ancova, subset=-c(16, 19))
summary(moore.ancova.1)
moore.ancova.2 <- update(moore.ancova, subset=-c(16, 19, 23))
summary(moore.ancova.2)
moore.ancova.3 <- update(moore.ancova, subset=-c(16, 19, 23, 36))
summary(moore.ancova.3)

# fit of original and final models

library(effects)
plot(effect("fscored:partner.status", moore.ancova), multiline=TRUE)
plot(effect("fscored:partner.status", moore.ancova.3), multiline=TRUE)








library(car)
mod.1 <- lm(prestige ~ income + education + women, data=Prestige)
summary(mod.1)

# distribution of studentized residuals

par(mfrow=c(1, 3))
plot(density(rstudent(mod.1)))
qq.plot(rstudent(mod.1))
boxplot(rstudent(mod.1))

# non-constant variance

plot(fitted(mod.1), rstudent(mod.1))
abline(h=0)

spread.level.plot(mod.1)

ncv.test(mod.1)
ncv.test(mod.1, var= ~ income + education + women, data=Prestige)

# nonlinearity

cr.plots(mod.1, ask=FALSE)

mod.2 <- lm(prestige ~ log(income, 2) + education + poly(women, degree=2, raw=TRUE), data=Prestige)
summary(mod.2)

library(effects)
plot(allEffects(mod.2, default.levels=100), ask=FALSE)

cr.plots(mod.2, ask=FALSE)

box.tidwell(prestige ~ income,
            other.x = ~ education + poly(women, degree=2, raw=TRUE), data=Prestige)

mod.bt <- lm(prestige ~ income + education + poly(women, degree=2, raw=TRUE)
             + I(income*log(income)), data=Prestige)
av.plot(mod.bt, "I(income * log(income))")

# alternative view of the data

scatterplot.matrix(~ prestige + income + education + women | type, 
                   span=1, by.groups=TRUE, data=Prestige)

mod.3 <- lm(prestige ~ (income + education + women)*type, data=Prestige)
Anova(mod.3)
plot(allEffects(mod.3), ask=FALSE)

mod.4 <- lm(prestige ~ (income + women)*type + education, data=Prestige)
plot(allEffects(mod.4), ask=FALSE)




library(car)
Chile$yes <- as.factor(ifelse(Chile$vote == "Y", "yes", 
                              ifelse(Chile$vote == "N", "no", NA)))
with(Chile, table(yes, vote, exclude=NULL))
Chile$education <- factor(Chile$education, level=c("P", "S", "PS")) # reorder levels
mod.chile <- glm(yes ~ region + population + sex + age + education + income, 
                 family=binomial, data=Chile)
summary(mod.chile)
Anova(mod.chile)
library(effects)
plot(allEffects(mod.chile), ask=FALSE)

# unusual data diagnostics
influencePlot(mod.chile)
dfbs <- dfbetas(mod.chile)
nms <- colnames(dfbs)
par(mfrow=c(3,4))
for (name in nms) plot(dfbs[, name], ylab=name)

# nonlinearity
par(mfrow=c(2,2))
for (var in c("population", "age", "income")) cr.plot(mod.chile, var)

# tests of "lack of fit"
table(Chile$population)
table(Chile$income)

mod.chile.1 <- glm(yes ~ region + as.factor(population) + sex + age + education 
                   + as.factor(income), family=binomial, data=Chile)
deviance(mod.chile.1)
mod.chile.2 <- glm(yes ~ region + population + sex + age + education 
                   + as.factor(income), family=binomial, data=Chile)
deviance(mod.chile.2)
anova(mod.chile.2, mod.chile.1, test="Chisq") # test for nonlinearity of population

mod.chile.3 <- glm(yes ~ region + as.factor(population) + sex + age + education 
                   + income, family=binomial, data=Chile)
deviance(mod.chile.3)
anova(mod.chile.3, mod.chile.1, test="Chisq") # test for nonlinearity of income