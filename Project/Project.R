attach(mtcars)
head(mtcars)

require(stats); require(graphics)
pairs(mtcars, panel = panel.smooth, main = "mtcars data", col = 3 + mtcars$am)

#comparative kernel density plots
library(sm)
par(lwd=2)
am.f <- factor(mtcars$am,levels=c(1,0),labels=c("automatic","manual"))
sm.density.compare(mpg,am,xlab="Miles Per Gallon")
title(main="MPG distribution by types of transmission") 
colfill <-c(2:(1+length(levels(am.f))))
legend(locator(1),levels(am.f),fill=colfill)

#box plots
#boxplot(mtcars$mpg,main="box plot",ylab="Miles Per Gallon")
mtcars$am.f <- factor(mtcars$am,levels=c(1,0),labels=c("automatic","manual"))

boxplot(mpg ~ am.f,data=mtcars,varwidth=T,col=c("gold","darkgreen"),
        main="MPG distribution by types of transmission",
        xlab="auto type")

#select the "best" regression model
library(MASS)
fit <- lm(mpg~.,data=mtcars)
reg1 <- stepAIC(fit,direction="backward")
summary(reg1)

fit<-lm(mpg~am,data=mtcars)
summary(fit)
#Diagnostics for linear regression
library(car)
reg1 <- lm(formula = mpg ~ wt + qsec + am, data = mtcars)

residualPlots(reg1) #residual plots

avPlots(reg1, id.n=2, id.cex=0.7)#Influential variables - Added-variable plots

qqPlot(reg1, id.n=3)

#Outliers  - Bonferonni test 
outlierTest(reg1)

influenceIndexPlot(reg1, id.n=3)
influencePlot(reg1, id.n=3)
qqPlot(reg1)
ncvTest(reg1) ##Testing for heteroskedasticity
vif(reg1) #Testing for multicolinearity

------------------
fit1<-lm(mpg~wt+qsec,data=mtcars[mtcars$am==1,]);fit2<-lm(mpg~wt+qsec,data=mtcars[mtcars$am==0,])

coefficients(fit1)
coefficients(fit2)

fit3 <- lm(mpg~wt+qsec+am,data=mtcars)
coefficients(fit3)

summary(fit1)
summary(fit2)
summary(fit3)
-------------------
fit1<-lm(mpg~wt,data=mtcars[mtcars$am==1,]);fit2<-lm(mpg~wt,data=mtcars[mtcars$am==0,])

coefficients(fit1)
coefficients(fit2)

fit3 <- lm(mpg~wt+am,data=mtcars)
coefficients(fit3)
summary(fit1)
summary(fit2)
summary(fit3)
----------------------
fit1<-lm(mpg~qsec,data=mtcars[mtcars$am==1,]);fit2<-lm(mpg~qsec,data=mtcars[mtcars$am==0,])

coefficients(fit1)
coefficients(fit2)

fit3 <- lm(mpg~qsec+am,data=mtcars)
coefficients(fit3)
summary(fit1)
summary(fit2)
summary(fit3)
----------------------
