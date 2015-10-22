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

################################################################################
#
# Exploration of the data
#
################################################################################
train$Grade <- as.numeric(train$Grade)

#create a model with all values
lm1 <- lm(Grade ~ ., data=train)
summary(lm1)

#collapse some variables
train$guardian <- ifelse((train$guardian == "other"),"other","parent")
train$Edu <- ifelse((train$Medu == 4 | train$Medu == 4),1,0)
train$Edu <- as.factor(train$Edu)

#create a partial model
lm2 <- lm(Grade ~ sex + age + famsize + Pstatus + Edu + Mjob + Fjob + reason + 
            guardian + traveltime + studytime + 
            freetime + Walc, data=train)
summary(lm2)

#all vif values are small
vif(lm2)

#likelihood ratio tests
anova(lm2, lm1, test="LRT")

#stepwise modeling
step <- stepAIC(lm2, direction="backward")
step$anova

#cross validation (wont work with age or Fedu variable)
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

#calculate mse
mse = function(model){
  return (mean(model$residuals^2))
}

mse(lm2)
mse(lm1)

################################################################################
#
# Final model
#
################################################################################

# transform absences and failures as well
train$absences.group <- train$absences
levels(train$absences.group) <- list(low = c(0:21),
                                     veryhigh = c(22:32))
train$failures.high <- ifelse(train$failures == 3,1,0)
train$failures.high <- as.factor(train$failures.high)

# transform the variables for test as well
test$guardian <- ifelse((test$guardian == "other"),"other","parent")
test$Edu <- ifelse((test$Medu == 4 | test$Medu == 4),1,0)
test$Edu <- as.factor(test$Edu)
test$absences.group <- test$absences
levels(test$absences.group) <- list(low = c(0:21),
                                    veryhigh = c(22:32))
test$failures.high <- ifelse(test$failures == 3,1,0)
test$failures.high <- as.factor(test$failures.high)

# after transformations
lm3 <- lm(log(Grade) ~ Edu + guardian + failures.high + absences.group, data=train)
summary(lm3)
vif(lm3)

#residuals
residuals <- resid(lm3)
plot(residuals)
abline(0,0)

#there are an issue issues with normality, but better since log(Grade)
qqnorm(residuals)
qqline(residuals)

##########################################

# Predictions
predvect <- as.vector(predict(lm3, newdata=test))
predvect <- exp(predvect)

#export to csv
write.table(predvect, file = "TA07preds.csv", row.names=F, col.names=F, sep=",")
