
## Team Assignment 8
#Team 8
#kms6bn, mer3ef, doc2g, mv5vf

library(MASS)
library(car)
library(cvTools)
library(leaps)
library(mice) #multiple imputation
setwd("~/Git/STAT_6021_Project/TeamAssign08")

## Question 1: This problem requires the data in the file "parkinsons.csv".
##    Information about this data set is in "parkinsons.txt".
p <- read.csv("parkinsons.csv", header = TRUE)

#
#   (a) Find the "best" linear model with "motor_UPDRS" as the response, using
#       only variable selection.  (No transformations.)  Clearly state your model
#       and give a brief explanation for your model choice.
p$subject. <- as.factor(p$subject.)
p$sex <- as.factor(p$sex)

#create a model with all values
lm1 <- lm(motor_UPDRS ~ . -subject., data=p)
summary(lm1)

reg <- regsubsets(motor_UPDRS ~ . -subject., data=p)
summary(reg)

lm2 <- lm(motor_UPDRS ~ age + sex + test_time + Jitter.Abs. + NHR +
            Jitter.PPQ5 + Jitter.DDP + Shimmer.APQ3 + Shimmer.APQ5 + Shimmer.APQ11 + HNR +
            DFA + PPE, data=p)
summary(lm2)

#removed Shimmer because it had the highest VIF value
vif(lm2)

#p value shows that smaller model is close to the larger model in predictive power (0.04)
anova(lm2, lm1)

#   (b) Repeat part (a), this time with transformations allowed.
lm3 <- lm(motor_UPDRS ~ poly(age,13) + test_time + Jitter.Abs. +
            Jitter.PPQ5 + Jitter.DDP + Shimmer.APQ5 +
            Shimmer.APQ11 + HNR + DFA + poly(PPE,7), data=p)

cvFit(lm3, data = p, K = 10, seed = 1, y = p$motor_UPDRS)
summary(lm3)

## Question 2: This problem requires the data in the files "credit-train.csv" and
##    "credit-predict.csv".  Information about the data is contained in "credit.txt".
c <- read.csv("credit-train.csv", header = TRUE, na.strings=c("?"))
cPred <- read.csv("credit-predict.csv", header = TRUE, na.strings=c("?"))

#
#   (a) Develop a logistic regression model based on the "train" data to predict
#       the value of A16 (class attribute) from the variables A1-A15.  Note
#       that there are a few missing values!
c$A16 <- as.factor(c$A16)


# Let's subset to cross validate
sub <- sample(1:nrow(c),size=nrow(c)*(3/4))
c.train <- c[sub,]     # Select subset for cross-validation
c.valid <- c[-sub,]

#make model with all variables (except for A7 which results in fitted probabilities numerically 0 or 1 occurred error)
lm1 <- glm(A16 ~ . -A7 -A1, data=na.omit(c.train), family="binomial")
summary(lm1)

lm2 <- glm(A16 ~ A8*A14 + A6 + A9 + A11, data=na.omit(c.train), family="binomial")
summary(lm2)

# test on validation set
probs <- as.vector(predict(lm2, newdata=c.valid, type="response"))
preds <- rep(0,nrow(c.valid))  # Initialize prediction vector
preds[probs>0.5] <- 1 # p>0.5 -> 1
preds
table(preds,c.valid$A16)
(53+34)/nrow(c.valid)  #around 87% correct

#   (b) Predict the class attribute for each observation in the "predict" data,
#       then export your predictions using the code below.

#make model with all data
lm2 <- glm(A16 ~ A8*A14 + A6 + A9 + A11, data=na.omit(c), family="binomial")

mypreds <- predict(lm2, newdata=cPred, type="response")

preds <- rep(0,nrow(cPred))  # Initialize all to success=0 (pessimistic)
preds[mypreds >= 0.5] <- 1 # Change those with prob >= .5 to 1

write.table(preds, file = "TA08preds.csv", row.names=F, col.names=F, sep=",")

## Your submission should consist of "TA08.R" (all R code), "TA08.pdf" (description
## of the models developed for Question 1), and "TA08preds.csv" (predicted class
## attribute values for the prediction observation data in Question 2).
