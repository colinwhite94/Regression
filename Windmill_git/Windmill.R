# control + enter runs a line of code
#setwd("C:/Users/philaw/Box/Teaching/330/Winter2020/Lectures/1 - CarMPG/Code")
setwd("~/Desktop/1A School/1A Winter 2021/STAT330/HW1")

## Read in the Data
wind <- read.table("data.txt",header=TRUE)

summary(wind) ### gives general summary

head(wind)
wind$Weight

## Plot the data, calculate correlation and covariance
plot(wind$RSpd,wind$CSpd,xlab="RSpd",pch=19,ylab="CSpd", cex = .3) # plot(x values, y values) and ($ means grab dat field )
scatter.smooth(wind$RSpd,wind$CSpd,xlab="RSpd",ylab="CSpd", pch=19, cex = .3)

cor(wind$RSpd,wind$CSpd) #correlation
cov(wind$RSpd,wind$CSpd) #covariance

## Fit an SLR Model
wind.lm <- lm(CSpd~RSpd,data=wind) #lm is linear model
summary(wind.lm) #betta not will be 3.14123 for HW, betta 1 is 0.75573
summary(wind.lm)$sigma #Residual standard error: 2.466 is sigma hat

## Plot the Fitted Regression Line
plot(mpg$Weight,mpg$MPG,xlab="Weight",pch=19,ylab="MPG")
abline(a=mpg.lm$coef[1],b=mpg.lm$coef[2],lwd=3,col="green")
plot(mpg$Weight,mpg$MPG,xlab="Weight",pch=19,ylab="MPG")
abline(reg=mpg.lm,lwd=3,col="green")

## Make a few Predictions (by hand and with R)
weights.for.preds <- c(3000,20000)
mpg.lm$coef[1]+mpg.lm$coef[2]*weights.for.preds
predict.lm(mpg.lm,newdata=data.frame(Weight=weights.for.preds))

predict.lm(mpg.lm,newdata=data.frame(Weight=weights.for.preds),
           se.fit = TRUE)

#### Prediction w/ uncertainty for one car at those x's

predict.lm(mpg.lm,newdata=data.frame(Weight=weights.for.preds),
           interval = "prediction",level = 0.95)

#### Prediction w/ uncertainty for all cars at those x's


predict.lm(mpg.lm,newdata=data.frame(Weight=weights.for.preds),
           interval = "confidence",level = 0.95)




## Plot using ggplot2
library(ggplot2)
ggplot(mpg,aes(x=Weight,y=MPG))+geom_point()+geom_smooth(method="lm",se=FALSE)


