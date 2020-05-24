install.packages("Amelia")
install.packages("caTools")


## Get the data
str(adult)

## Combining levels of categorical data
attach(adult)
##Workclass
table(adult$workclass)
adult$workclass <- as.character(adult$workclass)

adult$workclass[adult$workclass == "Without-pay" | 
                  adult$workclass == "Never-worked"] <- "Unemployed"

adult$workclass[adult$workclass == "State-gov" |
                  adult$workclass == "Local-gov"] <- "SL-gov"

adult$workclass[adult$workclass == "Self-emp-inc" |
                  adult$workclass == "Self-emp-not-inc"] <- "Self-employed"

table(adult$workclass)

## Marital Status
table(adult$marital.status)
adult$marital.status <- as.character(adult$marital.status)

adult$marital.status[adult$marital.status == "Married-AF-spouse" |
                       adult$marital.status == "Married-civ-spouse" |
                       adult$marital.status == "Married-spouse-absent"] <- "Married"

adult$marital.status[adult$marital.status == "Divorced" |
                       adult$marital.status == "Separated" |
                       adult$marital.status == "Widowed"] <- "Not-Married"
table(adult$marital.status)

## County
adult$native.country <- as.character(adult$native.country)

north.america <- c("Canada", "Cuba", "Dominican-Republic", "El-Salvador", "Guatemala",
                   "Haiti", "Honduras", "Jamaica", "Mexico", "Nicaragua",
                   "Outlying-US(Guam-USVI-etc)", "Puerto-Rico", "Trinadad&Tobago",
                   "United-States")
asia <- c("Cambodia", "China", "Hong", "India", "Iran", "Japan", "Laos",
          "Philippines", "Taiwan", "Thailand", "Vietnam")
south.america <- c("Columbia", "Ecuador", "Peru")
europe <- c("England", "France", "Germany", "Greece", "Holand-Netherlands",
            "Hungary", "Ireland", "Italy", "Poland", "Portugal", "Scotland",
            "Yugoslavia")
other <- c("South", "?")

adult$native.country[adult$native.country %in% north.america] <- "North America"
adult$native.country[adult$native.country %in% asia] <- "Asia"
adult$native.country[adult$native.country %in% south.america] <- "South America"
adult$native.country[adult$native.country %in% europe] <- "Europe"
adult$native.country[adult$native.country %in% other] <- "Other"

table(adult$native.country)

## Update the variables in the form of categorical variable
adult$native.country <- as.factor(adult$native.country)
adult$marital.status <- as.factor(adult$marital.status)
adult$workclass <- as.factor(adult$workclass)
str(adult)


### Missing values
table(adult$workclass)
adult[adult == "?"] <- NA
table(adult$workclass)

library("Amelia")
missmap(adult, y.at = 1, y.labels = "", col = c("yellow", "black"), legend = FALSE)
adult <- na.omit(adult)
missmap(adult, y.at = 1, y.label = "", legend = FALSE, col = c("yellow", "black"))


### Exploratory analysis
library(ggplot2)
ggplot(adult, aes(age)) + geom_histogram(aes(fill = income), color = "black",
                                         binwidth = 1)

ggplot(adult, aes(hours.per.week)) + geom_histogram()


library(data.table)
setnames(adult, "native.country", "region")

# Reorder factor levels by count
region.ordered <- reorder(adult$region, adult$region, length)
region.ordered <- factor(region.ordered, levels = rev(levels(region.ordered)))

ggplot(adult, aes(region.ordered)) + geom_bar(aes(fill = income), color = "black")


### Building the model
library(caTools)

split <- sample.split(adult$income, SplitRatio = 0.7)
train <- subset(adult, split == TRUE)
test <- subset(adult, split == FALSE)

log.model <- glm(income ~ ., family = binomial(), train)
summary(log.model)
exp(coef(log.model))


### Prediction
prediction <- predict(log.model, test, type = "response")
summary(prediction)


table(test$income, prediction >= 0.5)

# Calculate Accuracy: How close are the predicted values to the true values?
(9677 + 2056) / (9677 + 706 + 1371 + 2056)


# positive predictive value
9677 / (9677 + 706)
