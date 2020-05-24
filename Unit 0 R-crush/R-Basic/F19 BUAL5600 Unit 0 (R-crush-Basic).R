
---
Title: Code for: BUAL5600 Predictive Modeling I
output: html_notebook
# Last update: August 20, 2018


# Unit 1: R Crush (1)

## ----setup, include=FALSE------------------------------------------------
install.packages("knitr")
library(knitr)
options(width=120)
opts_chunk$set(fig.path='figure/beamer-',fig.align='center',fig.show='hold',size='footnotesize',highlight=FALSE,tidy=FALSE,warning=FALSE,message=FALSE,small.mar=TRUE,fig.width=3,fig.height=3,out.width='0.5\\linewidth')
knit_hooks$set(small.mar = function(before, options, envir) {
  if (before) par(mar = c(4, 4, .1, .1),cex=.8,cex.main=.8,cex.lab=.8,cex.axis=.8)  # smaller margin on top and right
})
segmented.barchart <-
  function(x) {
    xlabel <- deparse(substitute(x))
    par(mfrow=c(1,1))
    par(mar=c(5,2,4,4))
    offset <- table(x)/length(x)/2
    fs <- c(0,cumsum(table(x)/length(x)) )[1:length(offset)]
    par(mgp = c(0.5, 1, 0))
    plot(factor(x)~factor(rep(" ",length(x))),xlab=xlabel,ylab="",axes=FALSE,col=grey(seq(.3,.9,length=length(offset))))
    axis(2)
    text(0.5,fs+offset,labels=levels(factor(x)))
    axis(4,at=fs+offset,labels=prettyNum(table(x)/length(x),drop0trailing=FALSE,digits=2,format="e"),las=1)
    par(mgp=c(3, 1, 0))
  }

## ------------------------------------------------------------------------------------------------

# Language Fundamentals
# Creating, listing and deleting the objects in memory

x<-15
x # Print the saved value

y=2
y

z<-x+2*y-3
z

x<-3
x <- 3
x< -3

x<-1
x<-10
x    # if the object already exists, its previous value is erased. 

x<-3+rnorm(1) # a normal random variate with mean zero and variance unity

(10+2)*5
10+2*5

x <- c(2, 4, 6, 8) # "c" combine, with multiple elements
x

X   # different than "x" and likely will produce an error

1:5
6:10

1:5 + 6:10
c(1, 3, 6, 10, 15) + c(0, 1, 3, 6, 10)

x <- c(2, 4, 6, 8)   # start a cheer
y <- c(1,4,3)
length(x)
length(y)

## Lengths
length(1:5)
length(c(TRUE, FALSE, NA))


sn <- c("Sheena", "leads", "Sheila", "needs")
length(sn)
nchar(sn)

x+y

# Regular sequences
xSeq <- 1:10
xSeq
xSeq[2:4]
xSeq[-5:-7]

1:10-1 # Generate sequences first (1-10), then minus 1
1:(10-1) # Generate sequences from 1 to (10-1), 9

1:5*2 # Generate sequences 1-5, then times2
1:(5*2) # Generate sequence 1-10 (=5*2)


seq(from=1, to=5, by=0.5)
seq(1,5,0.5)

seq(from=-5, to=28, by=4)

seq(length=9, from=1, to=5) # define # of elements using "length"


1:5 + 1
1 + 1:5

1:5 + 1:15

1:5 + 1:7


rep(1:5, 3)
rep(1:5, each = 3)
rep(1:5, times = 1:5)
rep(1:5, length.out = 7)


#### Others
seq.int(3, 12) #same as 3:12
seq.int(3, 12, 2)
seq.int(0.1, 0.01, -0.01)

n <- 0
1:n #not what you might expect!
seq_len(n)

pp <- c("Peter", "Piper", "picked", "a", "peck", "of", "pickled", "peppers")
for(i in seq_along(pp)) print(pp[i])


rep(c(1,2,3), each=3)
### rep(seq(from=-3, to=13, by=4), c(1, 2, 3, 2, 1))


# semicolon operator (;)
name<-"Carmen"
n1<-10
n2<-100
m<-0.5

name<-"Carmen"; n1<-10; n2<-100; m<-0.5


# Other calculation
x <- c(2, 4, 6, 8)

x2 <- c(x, x)
x2

x2 + 1
x2 * pi 
(x+cos(0.5)) * x2

length(x)
length(x2)

# basic objects

# vectors with 4 elements
xNum  <- c(1, 3.14159, 5, 7) # all numbers
xLog  <- c(TRUE, FALSE, TRUE, TRUE) # all logical
xChar <- c("foo", "bar", "boo", "far") # all characters
xMix  <- c(1, TRUE, 3, "Hello, world!") # mixed

xNum
summary(xNum)
summary(xChar)

gender <- factor(c("male", "female", "female", "male", "female"))
levels(gender)
nlevels(gender)

as.integer(gender)


## Checking and changing classes
is.character("red lorry, yellow lorry")
is.logical(FALSE)
is.list(list(a = 1, b = 2))

x <- "123.456"
as(x, "numeric")
as.numeric(x)

ls() # the function "ls" lists simply the objects in mamory,
# only the names of the objects are displayed
ls(pat="m") # the option "pattern" can be used to restrict the list of objects 
# whose names start with this character:
# option "pat" is helpful to make subset. 

x<-1
mode(x) # check the type of variable
length(x) # number of elements

A<-"BusinessAnalyticsIsFun"; compar<-TRUE; Z<-1i
mode(A); # A is character variable (letter)
mode(compar); # logical variable
mode(Z) # R and R-studio are case sensitive!!!!!


ls() # the function "ls" lists simply the objects in mamory,
# only the names of the objects are displayed
rm(x,y)
ls()
rm(list=ls())

# Other Examples
xNum  <- c(1, 3.14159, 5, 7) # all numbers
xLog  <- c(TRUE, FALSE, TRUE, TRUE) # all logical
xChar <- c("foo", "bar", "boo", "far") # all characters
xMix  <- c(1, TRUE, 3, "Hello, world!") # mixed

xMix
xNum[1] # Square Blanket indicates a location of the elements
xMix[1]
xNum[1] + 1

xMix[1] + 1    # error

as.numeric(xMix[1])+1 # character -> numeric :as.numeric()
                      # numeric -> character (categorical):as.character()

str(xNum)
str(xChar)
str(xMix)

xNum[2:4]
xSub <- xNum[2:4]
xSub

xNum
xNum[c(FALSE, TRUE, TRUE, TRUE)]

xNum[xNum > 3]

# Examining Variable
num <- runif(30)
summary(num)

fac <- factor(sample(letters[1:5], 30, replace = TRUE))
summary(fac)

bool <- sample(c(TRUE, FALSE, NA), 30, replace = TRUE)
summary(bool)

dfr <- data.frame(num, fac, bool)
head(dfr)

?matrix
x=matrix(data=c(1,2,3,4), nrow=2, ncol=2)
x=matrix(c(1,2,3,4),2,2) # 2 by 2 matrix
x=matrix(c(1,2,3,4),2,2,byrow=TRUE)

### Creating Matrices 
#1
(three_d_array <- array(
  1:24,
  dim = c(4, 3, 2),
  dimnames = list(
    c("one", "two", "three", "four"),
    c("ein", "zwei", "drei"),
    c("un", "deux")
  )
))

#2
(a_matrix <- matrix(
  1:12,
  nrow = 4, #ncol = 3 works the same
  dimnames = list(
    c("one", "two", "three", "four"),
    c("ein", "zwei", "drei")
  )
))

#3
(a_matrix <- matrix(
  1:12,
  nrow = 4, #ncol = 3 works the same
  dimnames = list(
    c("one", "two", "three", "four"),
    c("ein", "zwei", "drei")
  )
))

## or 

  matrix(
    1:12,
    nrow = 4,
    byrow = TRUE,
    dimnames = list(
      c("one", "two", "three", "four"),
      c("ein", "zwei", "drei")
    )
  )

## Rows, Columns and Dimensions
dim(three_d_array)
dim(a_matrix)

nrow(a_matrix)
ncol(a_matrix)

nrow(three_d_array)
ncol(three_d_array)

length(three_d_array)
length(a_matrix)

dim(a_matrix) <- c(6, 2)
a_matrix

  
summary(dfr)

str(num)
str(dfr)

attributes(fac)

