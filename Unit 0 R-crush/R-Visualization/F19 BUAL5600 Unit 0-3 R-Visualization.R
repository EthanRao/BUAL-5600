###################################
# Code for: BUAL5600 Predictive Modeling I
#
# Last update: August 27, 2019


# Unit 0: R Crush 3 (Visualization)

## ----setup, include=FALSE------------------------------------------------
library(knitr)
options(width=120)
opts_chunk$set(fig.path='figure/beamer-',fig.align='center',fig.show='hold',size='footnotesize',highlight=FALSE,tidy=FALSE,warning=FALSE,message=FALSE,small.mar=TRUE,fig.width=3,fig.height=3,out.width='0.5\\linewidth')
knit_hooks$set(small.mar = function(before, options, envir) {
  if (before) par(mar = c(4, 4, .1, .1),cex=.8,cex.main=.8,cex.lab=.8,cex.axis=.8)  # smaller margin on top and right
})

## ----Basic syntax (one variable),fig.width=3,fig.height=3,out.width='4cm',out.height='4cm'----
x <- c(9,1,8,2,7,3,7,4,5,5,5,5,5)
plot(x)
FRIEND <- read.csv("friendpotM.dat")
plot(FRIEND$HairColor)

## ----Basic syntax (two variables),fig.width=2.5,fig.height=2.5,out.width='3.5cm',out.height='3.5cm'----
plot(Hat~Glasses,data=FRIEND)
plot(FRIEND$AttractiveScore,FRIEND$FriendScore)
plot(FRIEND$FriendScore~FRIEND$Glasses)

## ----Axis Limits,tidy=FALSE,fig.width=3,fig.height=3,out.width='4cm',out.height='4cm'----
TIPS <- read.csv("tips.dat")
plot(Tip~Bill,data=TIPS)
plot(Tip~Bill,data=TIPS,xlim=c(10,20),ylim=c(0.5,5))

## ----Axis Names,fig.width=5,fig.height=5,out.width='4cm',out.height='4cm',size='tiny'----
SELFRATE <- read.csv("selfratings.dat")
plot(PMIntelligent~PMAttractive,data=SELFRATE,
     xlab="% of UT believe more attractive than",ylab="% of UT believe more intelligent than")

## ----Axis Font Sizes,tidy=FALSE,fig.width=4,fig.height=4,out.width='4.5cm',out.height='4.5cm'----
SELFRATE <- read.csv("selfratings.dat")
plot(GPA~Gender,data=SELFRATE)
plot(GPA~Gender,data=SELFRATE,xlab="sex",ylab="grade point",cex.lab=0.7,cex.axis=1.5)

## ----Logarithmic axes,fig.width=2.5,fig.height=2.5,out.width='4cm',out.height='4cm'----
SELFRATE <- read.csv("selfratings.dat")
plot(TextsPerDay~CellPhoneContacts,data=SELFRATE)
plot(TextsPerDay~CellPhoneContacts,data=SELFRATE,log="xy")

## ----Tick marks,echo=-1,fig.width=4,fig.height=2,out.width='8cm',out.height='4cm',small.mar=FALSE----
par(mar=c(5,4,1,2))
SELFRATE <- read.csv("selfratings.dat")
plot(PMIntelligent~GPA,data=SELFRATE,xaxt="n",xlim=c(0,5),ylab="% Int")
axis(side=1,at=c(0,1.5,2.8,3.5),labels=c("Huh?","Do you even school?","Eh","Great"))
axis(side=4,at=c(30,50,70,90))

## ----You can add axes to all sides,echo=-1,fig.width=4,fig.height=2,out.width='8cm',out.height='4cm',small.mar=FALSE----
par(mar=c(4,4,3,2))
plot(PMIntelligent~GPA,data=SELFRATE,xaxt="n",yaxt="n",xlim=c(0,5),ylab="% Int")
axis(side=1,at=c(0,1.5,2.8,3.5),labels=c("Huh","Do you even school?","Eh","Great"))
axis(2)
axis(3,at=c(1,1.5,2,2.5,3,3.5,4))
axis(side=4,at=c(30,50,70,90))

## ----Giving a plot a title,echo=-1,fig.width=4,fig.height=2,out.width='8cm',out.height='4cm',small.mar=FALSE----
par(mar=c(4,4,3,2))
plot(SELFRATE$GPA,SELFRATE$PMIntelligent,main="Putting Title in while plotting")

## ----Giving a title after plotting,echo=-1,fig.width=4,fig.height=2,out.width='8cm',out.height='4cm',small.mar=FALSE----
par(mar=c(4,4,3,2))
plot(SELFRATE$GPA,SELFRATE$PMIntelligent)
title("Putting title in afterwards",cex.main=1.2)

## ----Adding points,fig.width=6,fig.height=3,out.width='8cm',out.height='4cm'----
this <- 1:20; that <- 1:20
plot(this,that,xlab="x",ylab="y",cex=2)  #adding cex=2 makes points big
points(c(1.5,2.5,3.5),c(18,11,4))
x <- c(15,16,18);  y <- c(5,5.5,8)
points(x,y)

## ----Adding lines,fig.width=6,fig.height=3,out.width='8cm',out.height='4cm'----
x <- 1:20; y <- 5+seq(2,40,length=20)
plot(y~x,cex=2)
abline(v=2) #vertical line at x=2
abline(h=30) #horizontal line at y=30
abline(5,2) #line with intercept 5 and slope 2
x <- c(15,16,18);  y <- c(40,10,20)
lines(x,y)  #connects points (15,40), (16,10), (18,20)

## ----Line type and thickness,fig.width=6,fig.height=3,out.width='8cm',out.height='4cm'----
x <- 1:20; y <- rep(10,20)
plot(y~x,cex=2)
abline(v=2,lwd=5) #thick line
abline(v=10,lty=2) #dashed line
x <- c(15,16,18);  y <- c(13,7,11)
lines(x,y,lty=3) # dotted line

## ----Adding text,fig.width=6,fig.height=3,out.width='8cm',out.height='4cm'----
plot(GPA~PMIntelligent,data=SELFRATE,ylim=c(0,5))
abline(h=4.0)
#By default, text is centered at location specified
points(60,.3,pch=20,cex=1.5,col="blue");  text(60,.3,"Freshmen") 
#Pos to the right by adding pos=4
points(30,4.8,pch=20,cex=1.5,col="blue")
text(30,4.8,"Aligned to the right!",pos=4,cex=.6,col="red") 

## ----Point shapes and sizes,fig.width=6,fig.height=3,out.width='8cm',out.height='4cm'----
plot(1:20,1:20,pch=1:20,xaxt="n",yaxt="n",xlab="",ylab="",cex=2)
text(1:10,1:10+2,1:10)
text(11:20,11:20-2,11:20)

## ----colors,tidy=FALSE,fig.width=6,fig.height=3,out.width='8cm',out.height='4cm',small.mar=TRUE,highlight=FALSE----
plot(1:20,1:20,pch=20,xaxt="n",yaxt="n",xlab="",ylab="",cex=2,
     col=c(1:8,"red","blue","pink",grey(0),grey(1),grey(.5),
           rainbow(6,start=.3,end=.5)))
text(1:8,2+1:8,1:8)

## ----Displaying more than one plot,fig.width=6,fig.height=3,out.width='7cm',out.height='3.5cm'----
par(mfrow=c(1,2)) #1 row, 2 columns
hist(SELFRATE$PMAttractive,main=""); abline(v=50)
hist(SELFRATE$PMIntelligent,main=""); abline(v=50)
par(mfrow=c(1,1))

## ----Another example,size='scriptsize',fig.width=6,fig.height=3,out.width='0.8\\linewidth'----
par(mfrow=c(2,3))
hist(SELFRATE$PMAttractive,main=""); hist(SELFRATE$PMIntelligent,main="") 
hist(SELFRATE$PMAthletic,main=""); hist(SELFRATE$PMFunnier,main="")
hist(SELFRATE$PMWeathier,main=""); hist(SELFRATE$GPA,main="") 

## ----Changing Margins,size='scriptsize',fig.width=6,fig.height=3,out.width='0.8\\linewidth',echo=1----
par(mar=c(4,4,2,1)) #A more reasonable choice
par(mfrow=c(2,3))
hist(SELFRATE$PMAttractive,cex.main=0.6); hist(SELFRATE$PMIntelligent,cex.main=0.6); 
hist(SELFRATE$PMAthletic,cex.main=0.6); hist(SELFRATE$PMFunnier,cex.main=0.6); 
hist(SELFRATE$PMWeathier,cex.main=0.6); hist(SELFRATE$GPA,cex.main=0.6); 

## ----Adding legend to a known position,fig.width=6,size='scriptsize',fig.height=3,out.width='8cm',out.height='4cm'----
TIPS$Percent <- 100*TIPS$Tip/TIPS$Bill #create tip percentage col
MALE <- subset(TIPS,Gender == "Male"); FEMALE <- subset(TIPS,Gender == "Female");
plot(MALE$Bill,MALE$Percent, xlab="Bill",ylab="Tip Percent",main="",pch=20,cex=0.8,ylim=c(0,45) ) 
points(FEMALE$Bill,FEMALE$Percent, col="red",pch=8)  
abline(lm(Percent~Bill,data=MALE)); abline(lm(Percent~Bill,data=FEMALE),col="red",lty=2)
legend(30,40,c("Male","Female"),pch=c(20,8),col=c("black","red"),lty=c(1,2))

## ----Adding legend with mouseclick,fig.width=6,size='scriptsize',fig.height=3,out.width='8cm',out.height='4cm',warning=FALSE,message=FALSE,echo=-2----
plot(1:9,xlab="",ylab="",col=c(1,1,1,2,2,2,3,3,3))
legend(2,8,legend=c("Legend","wait for it","ary"),pch=1,col=1:3)
legend(locator(1),legend=c("Legend","wait for it","ary"),pch=1,col=1:3)
#Ignore the error above, slides compile weird with locator() in code

## ----Dynamic allocation of legend,fig.width=6,size='scriptsize',fig.height=3,out.width='8cm',out.height='4cm'----
plot(1:9,15:23); abline(v=3,col="red"); abline(v=7,col="purple")
legend("topright",c("Interesting","Text"),lty=1,col=c("red","purple"))
legend("center",c("Why","Here?"),lty=2,col=c("red","purple"),cex=0.5)
