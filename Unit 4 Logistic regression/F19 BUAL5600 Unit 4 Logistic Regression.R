###### Unit 5 
###### 1. Logistic Regression
exp(0) / (exp(0) + 1)  # computing logistic by hand; could use plogis()
plogis(-Inf)           # infinite dispreference = likelihood 0
plogis(2)              # moderate preference = 88% chance (e.g., of purchase)
plogis(-0.2)           # weak dispreference

log(0.5 / (1-0.5))     # indifference = 50% likelihood = 0 utility
log(0.88 / (1-0.88))   # moderate high likelihood
qlogis(0.88)           # equivalent to hand computation

### Inclass example 1: season pass data
install.packages ("vcdExtra")
install.packages("vcd")

library(vcdExtra)
library(vcd)  

# alternative code to load the data from website
pass.df <- read.csv("http://goo.gl/J8MH6A")
pass.df$Promo <- factor(pass.df$Promo, levels=c("NoBundle", "Bundle"))
summary(pass.df)

# construct the data 

pass.tab <- c(242, 639, 38, 359, 284, 27, 449, 223, 83, 278, 49, 485)
dim(pass.tab) <- c(3, 2, 2)
class(pass.tab) <- "table"
dimnames(pass.tab) <- list(Channel=c("Mail", "Park", "Email"),
                           Promo=c("Bundle", "NoBundle"),
                           Pass=c("YesPass", "NoPass") )
pass.tab


#classes and attributes
class(c(1, pi, exp(1)))
class(data.frame(1:10))

dim(pass.tab)
dimnames(pass.tab)
names(pass.tab)
str(pass.tab)

### create data frame from table
### this is not strictly required but more closely mimics common data format

pass.df <- expand.dft(pass.tab)
str(pass.df)

table(pass.df$Pass, pass.df$Promo)

# factors are alphabetized ... but that would put "NoBundle" as higher level
# so let's reorder the factors to make "Bundle" higher
pass.df$Promo <- factor(pass.df$Promo, levels=c("NoBundle", "Bundle"))
table(pass.df$Pass, pass.df$Promo)

# Logistic regression with glm()

# initial logistic regression model
pass.m1 <- glm(Pass ~ Promo, data=pass.df, family=binomial)
summary(pass.m1)

# how the coef translates to an odds ratio
plogis(0.3888)                          # outcome %
plogis(0.3888) / (1-plogis(0.3888))     # ratio of outcome % to alternative %
exp(0.3888)                             # identical


# odds ratio for sales
exp(coef(pass.m1))
# confidence intervals
exp(confint(pass.m1))

# at first it looks like the promotion is working
# but is this really the right model? check Channel
table(pass.df$Pass, pass.df$Channel)


# visualization
doubledecker(table(pass.df))


# Model 2: add the effect of channel
pass.m2 <- glm(Pass ~ Promo + Channel, data=pass.df, family=binomial)
summary(pass.m2)

# updated coefs and odds ratios
exp(coef(pass.m2))
exp(confint(pass.m2))

# Model 3: add the interaction of promotion and channel
pass.m3 <- glm(Pass ~ Promo + Channel + Promo:Channel, 
               data=pass.df, family=binomial)
summary(pass.m3)

# updated coefs and odds ratios
exp(confint(pass.m3))

## extras on visualization for logistic coefficients
# plot the coefs
install.packages("coefplot")
library(coefplot)
library(ggplot2)
coefplot(pass.m2, intercept=FALSE, outerCI=1.96, lwdOuter=1.5,
         title="Coefficients for Season Pass by Factor", ylab="Factor")

# plot the odds ratio confidence intervals
pass.ci <- data.frame(confint(pass.m2))     # coef confidence intervals
pass.ci$X50 <- coef(pass.m2)                # add the midpoint estimate

# plot odds
pass.ci$Factor <- rownames(pass.ci)           # for ggplot2 to use in its model
pass.ci

# ggplot of odds ratios
# first: a plot by factor (x=) of the midpoint (y), high (ymax) and low (ymin)
p <- ggplot(pass.ci[-1, ], 
            aes(x=Factor, y=exp(X50), ymax=exp(X97.5..), ymin=exp(X2.5..)))

# ... displaying those elements as points & error bars
p <- p + geom_point(size=4) + geom_errorbar(width=0.25)

# ... adding a vertical line at an odds ratio of 1.0 (no change)
p <- p + geom_hline(yintercept=1, linetype="dotted", size=1.5, color="red")

# now plot it with titles
p + ylab("Likehood by Factor (odds ratio, main effect)") +
  ggtitle(paste("95% CI: Card sign up odds by factor")) + coord_flip()



### Example 2: Admission data
install.packages("aod")
install.packages("caret")

library(aod)
library(ggplot2)
library(caret)
# Loading the 'Admit' data

# Description of the data
## view the first few rows of the data
head(Admit)
summary(Admit)
## two-way contingency table of categorical outcome and predictors we want
## to make sure there are not 0 cells
xtabs(~admit + rank, data = Admit)

# Modeling
Admit$rank <- factor(Admit$rank)
Admit.m1 <- glm(admit ~ gre + gpa + rank, data = Admit, family = "binomial")
summary(Admit.m1)
## CIs using profiled log-likelihood
confint(Admit.m1)
## CIs using standard errors
confint.default(Admit.m1)
## Wald Test:Testing an overall effect of rank
wald.test(b = coef(Admit.m1), Sigma = vcov(Admit.m1), Terms = 4:6)

## odds ratios only
exp(coef(Admit.m1))
## odds ratios and 95% CI
exp(cbind(OR = coef(Admit.m1), confint(Admit.m1)))

## Perdiction
newdata1 <- with(Admit, data.frame(gre = mean(gre), gpa = mean(gpa), rank = factor(1:4)))
## view data frame
newdata1

newdata1$rankP <- predict(Admit.m1, newdata = newdata1, type = "response")
newdata1

newdata2 <- with(Admit, data.frame(gre = rep(seq(from = 200, to = 800, length.out = 100),
                                             4), gpa = mean(gpa), rank = factor(rep(1:4, each = 100))))
newdata3 <- cbind(newdata2, predict(Admit.m1, newdata = newdata2, type = "link",
                                    se = TRUE))
newdata3 <- within(newdata3, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})
## view first few rows of final dataset
head(newdata3)
## Visualization
ggplot(newdata3, aes(x = gre, y = PredictedProb)) + geom_ribbon(aes(ymin = LL,
                                                                    ymax = UL, fill = rank), alpha = 0.2) + geom_line(aes(colour = rank),
                                                                                                                      size = 1)
## Calculating the difference in deviance between two models
with(Admit.m1, null.deviance - deviance)
with(Admit.m1, df.null - df.residual)
with(Admit.m1, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
logLik(Admit.m1)