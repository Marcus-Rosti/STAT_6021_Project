#Team Assignment 7
#Team 8
#kms6bn, mer3ef, doc2g, mv5vf

library(MASS)
library(car)
library(cvTools)
library(leaps)

#load data
setwd("~/Git/STAT_6021_Project/TeamAssign07")
train <- read.csv("TA07train.csv", header = T, stringsAsFactors=TRUE, colClasses="factor")
test <- read.csv("TA07predict.csv", header = T, stringsAsFactors=TRUE, colClasses="factor")

#The objective is to predict Grade based on the other variables that provide information about
#the student and family.
train$Grade <- as.numeric(train$Grade)

#create a model with all values
lm1 <- lm(Grade ~ ., data=train)
summary(lm1)

#create a partial model
lm2 <- lm(Grade ~ sex + age + famsize + Pstatus + Medu + 
            
            Fedu + Mjob + Fjob + reason + guardian + traveltime + studytime + 
            freetime + Walc, data=train)
summary(lm2)

#all vif values are small
vif(lm2)

#likelihood ratio tests
anova(lm2, lm1, test="LRT")

#stepwise modeling
step <- stepAIC(lm2, direction="backward")
step$anova

#cross validation (wont work with age  or Fedu variable)
cvFit(lm, Grade ~ sex + famsize + Pstatus + Medu + 
        Mjob + Fjob + reason + guardian + traveltime + studytime + 
        freetime + Walc, data = train, K = 5, seed = 10)

#residuals
residuals <- resid(lm2)
plot(residuals)
abline(0,0)

#there are an issue issues with normality (goes away with log(Grade))
qqnorm(residuals)
qqline(residuals)

# Predictions
predvect <- as.vector(predict(lm2, newdata=test))

#export to csv
write.table(predvect, file = "TA07preds.csv", row.names=F, col.names=F, sep=",")