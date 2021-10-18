library(ggplot2)
library(wesanderson)
library(glmnet)
library(caret)
library(lmtest)
library(car)
library(plotly)


## read the data
setwd("D:/courseware/wisconsin/stat 628/assignment 2/data")
data <- read.csv("./BodyFat.csv",encoding = 'UTF-8',head = T)

set.seed(123) 

## clean the data
data = data[data$BODYFAT!=0.0,]
data = data[-39,]
index = sample(1:nrow(data), 0.7*nrow(data)) 
train = data[index,] # Create the training data 
test = data[-index,] # Create the test data

## selecting only 4 variables
## linear regression
lr_final = lm(BODYFAT ~ AGE + ADIPOSITY + CHEST, data=train)
summary(lr_final)
predictions_train = predict(lr_final, newdata = train)
sqrt(mean((train$BODYFAT - predictions_train)^2))
predictions_test = predict(lr_final, newdata = test)
sqrt(mean((test$BODYFAT - predictions_test)^2))

## Autocorrelation
dwtest(lr_final)
len <- length(lr_final$residuals)
plot(lr_final$residuals[1:(len-1)],lr_final$residuals[2:len],
     xlab="e[t-1]",ylab="e[t]",
     main="Autocorrelation Plot")   

## residual plot
plot(lr_final$fitted.values,lr_final$residuals, xlab = 'Fitted values',ylab = 'Residuals')
plot(lr_final$fitted.values,sqrt(abs(rstandard(lr_final))), xlab = 'Fitted values',ylab = 'square root of standardized residuals')

# QQ plot
plot(lr_final,which=2)

## Cook's distance
plot(lr_final,which=4)

## vif
vif(lr_final)

