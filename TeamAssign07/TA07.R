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

levels(train$parent.guard) <- list(parent = c("mother","father"), other = "other")
train$Edu <- ifelse((train$Medu == 4 | train$Medu == 4),1,0)
train$Edu <- as.factor(train$Edu)
levels(train$absences.group) <- list(low = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21),
                                    veryhigh = c(22,23,24,32))

############### Don's stuff ###############
# transform guardian, Medu/Fedu, absenses
train$parent.guard <- train$guardian
levels(train$parent.guard) <- list(parent = c("mother","father"), other = "other")
train$Edu <- ifelse((train$Medu == 4 | train$Medu == 4),1,0)
train$Edu <- as.factor(train$Edu)
train$absences.group <- train$absences
levels(train$absences.group) <- list(low = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21),
                                     veryhigh = c(22,23,24,32))
train$failures.high <- ifelse(train$failures == 3,1,0)
train$failures.high <- as.factor(train$failures.high)

# after transformations
lm3 <- lm(Grade~Edu+parent.guard+failures.high+absences.group, data=train)
summary(lm3)
vif(lm3)

#likelihood ratio tests
anova(lm3, lm1, test="LRT")
##########################################

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

################################################################################
#
# Final model
#
################################################################################

# transform guardian, Medu/Fedu, absenses
train$parent.guard <- train$guardian
levels(train$parent.guard) <- list(parent = c("mother","father"), other = "other")
train$Edu <- ifelse((train$Medu == 4 | train$Medu == 4),1,0)
train$Edu <- as.factor(train$Edu)
train$absences.group <- train$absences
levels(train$absences.group) <- list(low = c(0:21),
                                     veryhigh = c(22:32))
train$failures.high <- ifelse(train$failures == 3,1,0)
train$failures.high <- as.factor(train$failures.high)

# transform the test as well
test$parent.guard <- test$guardian
levels(test$parent.guard) <- list(parent = c("mother","father"), other = "other")
test$Edu <- ifelse((test$Medu == 4 | test$Medu == 4),1,0)
test$Edu <- as.factor(test$Edu)
test$absences.group <- test$absences
levels(test$absences.group) <- list(low = c(0:21),
                                    veryhigh = c(22:32))
test$failures.high <- ifelse(test$failures == 3,1,0)
test$failures.high <- as.factor(test$failures.high)

# after transformations
lm3 <- lm(log(Grade) ~ Edu + parent.guard + failures.high + absences.group, data=train)
summary(lm3)
vif(lm3)

train$log_Grade <- log(train$Grade)

##########################################

# Predictions
predvect <- as.vector(predict(lm3, newdata=test))
predvect <- exp(predvect)

#export to csv
write.table(predvect, file = "TA07preds.csv", row.names=F, col.names=F, sep=",")
