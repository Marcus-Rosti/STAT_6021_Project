# Mike

#PREDDEG = 2, mostly associates degree
#PREDDEG = 3, mostly bachelors degree

library(leaps)
library(fmsb)

#read in data - set NULL as NA
year13 <- read.csv("MERGED2013_PP.csv", na.strings=c("NULL", "PrivacySuppressed"))

# 7804 rows
# 1729 columns
dim(year13)

# keep only 4 year colleges
year13 <- year13[year13$PREDDEG == 3,]

#remove private for-profit colleges (3)
#1 is public, 2 is private non-profit
year13 <- subset(year13, year13$CONTROL != 3)
year13$CONTROL <- as.factor(year13$CONTROL)

#remove columns with more than 500 na values, but keep longitude and latitude
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

# remove variables that have all the same values
year13clean <- year13clean[sapply(year13clean, function(x) length(unique(x)) > 1)]

# remove schools that have NA DEBT_MDN
na_debt <- which(is.na(year13clean$DEBT_MDN))
year13clean <- year13clean[-na_debt, ]

# remove & save ID columns
info <- year13clean[, c(1:10)]
year13clean <- year13clean[, c(-1:-10)]

# remove & save y variables - what is our other y variable?
y_debt <- year13clean$DEBT_MDN
year13clean$DEBT_MDN <- NULL

######################
# CONVERT TO FACTORS #
######################

# several factor variables follow a certain pattern - convert these to factors
cip_factors <- as.data.frame(apply(year13clean[,grep("CIP[0-9][0-9][A-Z]", names(year13clean), value = TRUE)], 2, as.factor))
columns <- names(cip_factors)
not_cip_factors <- year13clean[, !(colnames(year13clean) %in% columns)]

# these variables need to be converted to factors
factors <- c("HCM2", "main", "HIGHDEG", "st_fips", "region", "LOCALE", "CCBASIC", "CCUGPROF", "CCSIZSET", "HBCU", "PBI",
              "ANNHI", "TRIBAL", "AANAPII", "HSI", "NANTI", "MENONLY", "WOMENONLY", "DISTANCEONLY", "CURROPER")
other_factors <- as.data.frame(apply(not_cip_factors[, factors], 2, as.factor))

# collect variables that aren't factors
not_factors <- not_cip_factors[, !(colnames(not_cip_factors) %in% factors)]

# combine all back together
year13_features <- data.frame(cip_factors, other_factors, not_factors)

# remove variables that are only one value
# when using str() it will say these variables have 2 levels, but 1 is NA
constants <- c("TRIBAL", "poolyrs200", "poolyrs", "MENONLY", "DISTANCEONLY", "CURROPER", "HCM2", "CIP29CERT2")
year13_features <- year13_features[,!(colnames(year13_features) %in% constants)]

# 1713 schools
# 377 features

# TEST return colleges with complete data - yields only 484 colleges
#complete <- which(complete.cases(year13_features))
#y_debt <- y_debt[complete]
#year13_features_test <- year13_features[complete, ]

#####################
# MULTICOLLINEARITY #
#####################

# VIF doesn't work currently

# Test for multicollinearity
vif_func<-function(in_frame,thresh=10,trace=T,...){
  
  if(class(in_frame) != 'data.frame') in_frame<-data.frame(in_frame)
  
  #get initial vif value for all comparisons of variables
  vif_init<-NULL
  var_names <- names(in_frame)
  for(val in var_names){
    regressors <- var_names[-which(var_names == val)]
    form <- paste(regressors, collapse = '+')
    form_in <- formula(paste(val, '~', form))
    vif_init<-rbind(vif_init, c(val, VIF(lm(form_in, data = in_frame, ...))))
  }
  vif_max<-max(as.numeric(vif_init[,2]))
  
  if(vif_max < thresh){
    if(trace==T){ #print output of each iteration
      prmatrix(vif_init,collab=c('var','vif'),rowlab=rep('',nrow(vif_init)),quote=F)
      cat('\n')
      cat(paste('All variables have VIF < ', thresh,', max VIF ',round(vif_max,2), sep=''),'\n\n')
    }
    return(var_names)
  }
  else{
    
    in_dat<-in_frame
    
    #backwards selection of explanatory variables, stops when all VIF values are below 'thresh'
    while(vif_max >= thresh){
      
      vif_vals<-NULL
      var_names <- names(in_dat)
      
      for(val in var_names){
        regressors <- var_names[-which(var_names == val)]
        form <- paste(regressors, collapse = '+')
        form_in <- formula(paste(val, '~', form))
        vif_add<-VIF(lm(form_in, data = in_dat, ...))
        vif_vals<-rbind(vif_vals,c(val,vif_add))
      }
      max_row<-which(vif_vals[,2] == max(as.numeric(vif_vals[,2])))[1]
      
      vif_max<-as.numeric(vif_vals[max_row,2])
      
      if(vif_max<thresh) break
      
      if(trace==T){ #print output of each iteration
        prmatrix(vif_vals,collab=c('var','vif'),rowlab=rep('',nrow(vif_vals)),quote=F)
        cat('\n')
        cat('removed: ',vif_vals[max_row,1],vif_max,'\n\n')
        flush.console()
      }
      
      in_dat<-in_dat[,!names(in_dat) %in% vif_vals[max_row,1]]
      
    }
    
    return(names(in_dat))
    
  }
  
}

#vif <- vif_func(year13_features, thresh = 100, trace = T)
#form.in <- paste("y_debt ~", paste(vif, collapse = "+"))

################
# FORWARD STEP #
################

# add y variable back in
year13 <- data.frame(y_debt, year13_features)

# because of NA values - linear regression is only being ran on 484 schools
# forward and both yield 2 variables
null <- lm(y_debt ~ 1, data = year13, na.action = na.exclude)
full <- lm(y_debt ~ ., data = year13, na.action = na.exclude)
step2 <- step(null, scope = list(lower = null, upper = full), direction = "forward", trace = T)

# try lasso



# Katherine's section
# # make categorical columns factors
# uniquevalues <- lapply(year13clean, function(x) length(unique(x[!is.na(x)])))
# uniquevalues <- uniquevalues[uniquevalues <= 12]
# uniquevalues <- names(uniquevalues)
# 
# for (i in uniquevalues) {
#   year13clean[[i]] <- as.factor(year13clean[[i]])
# }

# make model, median debt is response
year13clean$DEBT_MDN <- as.numeric(year13clean$DEBT_MDN)

reg <- regsubsets(DEBT_MDN ~ ., data=year13clean[,c(8:307)])

lm1 <- lm(DEBT_MDN ~ ., na.action=na.exclude, data=year13clean[,c(9:12, 17, 25:26, 301)])
summary(lm1)

