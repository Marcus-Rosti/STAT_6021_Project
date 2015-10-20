#Team Assignment 7
#Team 8
#kms6bn, mer3ef, doc2g, mv5vf

#load data
setwd("~/Git/STAT_6021_Project/TeamAssign07")
train <- read.csv("TA07train.csv", header = T, stringsAsFactors=TRUE, colClasses="factor")
test <- read.csv("TA07predict.csv", header = T, stringsAsFactors=TRUE, colClasses="factor")

#The objective is to predict Grade based on the other variables that provide information about
#the student and family.
train$Grade <- as.numeric(train$Grade)

#create a model
lm1 <- lm(Grade ~ ., data=train)

# Predictions
predvect <- as.vector(predict(lm1, newdata=test))

#export to csv
write.table(predvect, file = "TA07preds.csv", row.names=F, col.names=F, sep=",")