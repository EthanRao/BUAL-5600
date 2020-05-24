###################################
# Code for: BUAL5600 Predictive Modeling I
#
# Last update: August 27, 2019


# Unit 0: R Crush 1 (Basic, Part 2)

#### Language structures
x <- -2:2
log(x)                     # warning, can't log() negative numbers
ifelse(x > 0, x, NA)       # replace non-positive values with NA
log(ifelse(x > 0, x, NA))  # no warning now

log(c(-1,0,1))

my.data <- matrix(runif(100), ncol=5)   # 100 random numbers in 5 columns
apply(my.data, 2, median) / 2

halfmedian <- function (x) { median(x) / 2 }
apply(my.data, 2, halfmedian)

apply(my.data, 2, function(x) { median(x) / 2 } )


# Generating data using probability distribution
x=rnorm(50)
y=x+rnorm(50,mean=50,sd=.1)

# Creating data frame
# 1. Based on Probability distribution
(a_data_frame <- data.frame(
  x = letters[1:5],
  y = rnorm(5),
  z = runif(5) > 0.5
))
class(a_data_frame)

# 2. Using Seed
x=rnorm(50)
y=x+rnorm(50,mean=50,sd=.1)
cor(x,y)
set.seed(1303)
rnorm(50)
set.seed(3)
y=rnorm(100)
mean(y)
var(y)
sqrt(var(y))
sd(y)


# 3. Including time variable
y <- rnorm(5)
names(y) <- month.name[1:5]
data.frame(
  x = letters[1:5],
  y = y,
  z = runif(5) > 0.5
)

# 4. Using Built in data and loading existing data

# Example
store.num <- factor(c(3, 14, 21, 32, 54))   # store id
store.rev <- c(543, 654, 345, 678, 234)     # store revenue, $1000
store.visits <- c(45, 78, 32, 56, 34)       # visits, 1000s
store.manager <- c("Annie", "Bert", "Carla", "Dave", "Ella")
(store.df <- data.frame(store.num, store.rev, store.visits,
                        store.manager, stringsAsFactors=F))  # F = FALSE

store.df$store.manager
mean(store.df$store.rev)
cor(store.df$store.rev, store.df$store.visits)

summary(store.df)


# loading and saving data
save(store.df, file="store-df-backup.RData")
rm(store.df)     # caution, first ensure 'save' worked
mean(store.df$store.rev)    # error
load("store-df-backup.RData")
mean(store.df$store.rev)     # works now
save(list=c("store.df","store.visits"), file="store-df-backup.RData")

store.df <- 5 # incorrectrly save the file
store.df
load("store-df-backup.RData")
store.df

# Works on Windows:
save(store.df, file="C:\\Users\\szh0117\\Desktop\\BUAL5600\\store-df-backup.RData")

# Works on Mac OSX, Linux and Windows:
save(store.df, file="~/Documents/R/store-df-backup.RData")

getwd()
setwd("C:/Users/szh0117/Desktop/BUAL5600")   # tilde is handled on UNIX-like systems
getwd()

save.image()    # saves .RData
save.image("mywork.RData")

load("mywork.RData")

list.files()

# warning: dangerous!
file.remove("mywork.RData", "store-df-backup.RData")

write.csv(store.df, row.names=FALSE)
write.csv(store.df, file="store-df.csv", row.names=FALSE)
read.csv("store-df.csv")  # "file=" is optional

store.df2 <- read.csv("store-df.csv", stringsAsFactors=FALSE)  # "file=" is optional
store.df2$store.num <- factor(store.df2$store.num)

store.df == store.df2

all.equal(store.df, store.df2)
rm(store.df2)

# missing and interesting values

my.test.scores <- c(91, NA, NA)

mean(my.test.scores)
max(my.test.scores)
mean(my.test.scores, na.rm=TRUE)
max(my.test.scores, na.rm=TRUE)

mean(na.omit(my.test.scores))
is.na(my.test.scores)
my.test.scores[!is.na(my.test.scores)]
my.test.scores <- c(91, -999, -999)
mean(my.test.scores)
my.test.scores[my.test.scores < -900] <- NA
mean(my.test.scores, na.rm=TRUE)

ls()
rm(store.num)
rm(list=c("store.rev","store.visits"))
rm(list=ls(pattern="store"))

# Warning!! putting this inside an "if (FALSE)" block on purpose
if (FALSE) {
  rm(list=ls())   # warning! deletes all objects in memory (except hidden ones)
}


# Warning!! putting this inside an "if (FALSE)" block on purpose
if (FALSE) {
  rm(list=ls())   # warning! deletes all objects in memory (except hidden ones)
}

####
#### Functions
se <- function(x) { sd(x) / sqrt(length(x)) }
se(store.df$store.visits)
mean(store.df$store.visits) + 1.96 * se(store.df$store.visits)

se(store.df$store.manager)

se <- function(x) {
  # computes standard error of the mean
  tmp.sd <- sd(x)   # standard deviation
  tmp.N  <- length(x)  # sample size
  tmp.se <- tmp.sd / sqrt(tmp.N)   # std error of the mean
  return(tmp.se)
}

# generating data set


