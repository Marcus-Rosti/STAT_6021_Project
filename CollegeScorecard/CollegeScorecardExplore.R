#katherine schinkel
#college scorecard exploration
#10/18/2015
library(leaps)

#read in data - set NULL as NA
setwd("~/Documents/MSDS/Stat6021")
year13 <- read.csv("MERGED2013_PP.csv", na.strings=c("NULL"))

#remove private for-profit colleges (3)
#1 is public, 2 is private non-profit
year13 <- subset(year13, year13$CONTROL != 3)
year13$CONTROL <- as.factor(year13$CONTROL)

#remove columns with more than 500 na values, but keet logitude and latitude
nafind <- function(x){sum(is.na(x))}
keep <- c("LONGITUDE", "LATITUDE", "DEBT_MDN")
nacount <- apply(year13, 2, "nafind")
varWna <- which(nacount > 500)

for (i in keep) {
  if (any(colnames(year13)[varWna] == i)) {
    varWna <- varWna[-which(colnames(year13)[varWna] == i)]
  }
}

year13clean <- year13[,-varWna]

#remove ID columns
year13clean <- year13clean[,c(-1, -2, -3)]

#make categorical columns factors
uniquevalues <- lapply(year13clean, function(x) length(unique(x[!is.na(x)])))
uniquevalues <- uniquevalues[uniquevalues <= 12]
uniquevalues <- names(uniquevalues)

for (i in uniquevalues) {
  year13clean[[i]] <- as.factor(year13clean[[i]])
}

# make model, median debt is response
year13clean$DEBT_MDN <- as.numeric(year13clean$DEBT_MDN)

reg <- regsubsets(DEBT_MDN ~ ., data=year13clean[,c(8:307)])

lm1 <- lm(DEBT_MDN ~ ., na.action=na.exclude, data=year13clean[,c(9:12, 17, 25:26, 301)])
summary(lm1)

