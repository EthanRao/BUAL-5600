###################################
# Code for: BUAL5600 Predictive Modeling I
#
# Last update: August 27, 2019


# Unit 0: R Crush 2 (Summarization)

install.packages("regclass")

## ----Frequency Table-----------------------------------------------------
TIPS <- read.csv("C:/Users/Downloads/tips.dat")
table(TIPS$Day)
table(TIPS$Smoker)

## ----Relative Frequency Table--------------------------------------------
table(TIPS$Day)/nrow(TIPS)  
table(TIPS$Smoker)/nrow(TIPS)

## ----Bar Chart,out.width='0.4\\linewidth'--------------------------------
## par(mar=c(1,1,1,1))
plot(TIPS$Day)
plot(TIPS$Smoker)

## ----Segmented Bar Chart,fig.height=6,fig.width=3,out.width='3cm',out.height='6cm'----
library(regclass) #this command won't work until you install the package
segmented.barchart(TIPS$Day)

t.ct<-table(TIPS$Day, TIPS$Smoker)
t.ct
barplot(prop.table(t.ct,2), main="TIPS", xlab="Smoker", ylab="Tips")

## ----Histogram of Bills,fig.width=4,fig.height=2,fig.out='0.33\\linewidth'----
hist(TIPS$Bill)  #default is usually fine, here bars are increments of 5

## ----Custom Histograms of Bills,fig.width=4,fig.height=2,fig.out='0.33\\linewidth'----
hist(TIPS$Bill,breaks=20) #about 20 breaks (never exact)
hist(TIPS$Bill,breaks=seq(from=0,to=100,by=7)) #custom breaks

## ----Endpoint convention example,fig.width=3,fig.height=2,fig.out='0.7\\linewidth'----
x <- c(1,1.5,2,3,3.1,3.4,3.7,3.9,3.9,3.9,4,4.8,5,5.2,5.5,5.7,5.7,7.2)
hist(x)

## ----Boxplots------------------------------------------------------------
boxplot(TIPS$Bill)
boxplot(TIPS$Tip,horizontal=TRUE) #Change orientation

## ----Logarithms for skewed distributions,out.width='0.34\\linewidth'-----
MOVIES <- read.csv("movies.dat"); hist(MOVIES$TDOMGROSS); boxplot(MOVIES$TDOMGROSS)

## ----Logarithms for skewed distributions2,out.width='0.34\\linewidth'----
hist( log10(MOVIES$TDOMGROSS) ); boxplot( log10(MOVIES$TDOMGROSS) )

## ----Look at both plots in tandem,fig.height=2,fig.width=2,out.width='0.3\\linewidth'----
data(faithful)  #loading up a default dataset
hist(faithful$eruptions)
boxplot(faithful$eruptions)

## ----Peaks example,echo=1,fig.width=4,fig.height=3,out.width='0.45\\linewidth'----
hist(TIPS$Bill,breaks=30)
rect(12,0,14,31,col="grey")
rect(16,0,18,31,col="grey")
rect(24,0,26,15,col="grey")
rect(48,0,50,3,col="red")
rect(50,0,52,1,col="red")

## ----Bimodal,fig.width=4,fig.height=3,out.width='0.45\\linewidth'--------
data(faithful)
hist(faithful$waiting)

## ----Multimodal,fig.width=4,fig.height=3,out.width='0.55\\linewidth'-----
STUDENT <- read.csv("studentsurveyf10.dat")
hist(STUDENT$X33.Distance.from.Home.to.UT,breaks=100,xlim=c(0,1000))

## ----Five number summary and frequency table,size='tiny'-----------------
summary(TIPS)
summary(TIPS$Bill)
summary(TIPS$Day)

## ----Mean, Variance, Standard Deviation----------------------------------
mean(TIPS$Bill)
sd(TIPS$Bill)
var(TIPS$Bill)
x <- sqrt( TIPS$Bill-20 ) #some of the resulting x's will be undefined
mean(x)
mean(x,na.rm=TRUE)

## ----Mean vs. Median, Standard Deviation vs. IQR-------------------------
x <- c(1,2,3,4,5,6,7,8,90)
mean(x); sd(x)
median(x); IQR(x)

## ----Median and percentiles----------------------------------------------
median(TIPS$Bill)
IQR(TIPS$Bill)
quantile(TIPS$Bill,.8)
quantile(TIPS$Bill,c(.05,.25,.9))
max(TIPS$Bill)
min(TIPS$Bill)

## ----Movies - percent opening weekend------------------------------------
perc <- MOVIES$WEEKEND/MOVIES$TDOMGROSS  #Calculating percent

## ----Adding to the dataframe---------------------------------------------
MOVIES$Percent <- perc

## ----Percent opening weekend summary,fig.width=2,out.width='4cm',fig.height=2,out.height='4cm'----
summary(MOVIES$Percent); hist(MOVIES$Percent); boxplot(MOVIES$Percent)

## ----Bump investigation,size='tiny'--------------------------------------
LOWP <- subset(MOVIES,Percent < 0.80)
HIGHP <- subset(MOVIES,Percent >= 0.80)
summary(LOWP); summary(HIGHP)

## ----Lessons learned and more investigation------------------------------
#First, find the rows where gross is between 0.7 and 1.25 million
selected.rows <- which(MOVIES$TDOMGROSS >=0.7 & MOVIES$TDOMGROSS <= 1.25)
#Look at the grosses of these movies, sorted for convenience
sort(MOVIES$TDOMGROSS[selected.rows])  

## ----Subsetting movies for closer investigation,fig.width=2,out.width='4cm',fig.height=2,out.height='4cm'----
BIGMOVIES <- subset(MOVIES,TDOMGROSS >=0.98)
summary(BIGMOVIES$Percent); hist(BIGMOVIES$Percent); boxplot(BIGMOVIES$Percent)

## ----Movies that gain momentum-------------------------------------------
BIGMOVIES$MOVIE[which(BIGMOVIES$Percent < 0.10)]
