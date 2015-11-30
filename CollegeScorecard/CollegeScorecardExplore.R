# Mike

#PREDDEG = 2, mostly associates degree
#PREDDEG = 3, mostly bachelors degree

library(leaps)
library(fmsb)
library(glmnet)
library(car)
library(mice)

setwd("~/Git/STAT_6021_Project/CollegeScorecard")

source("Data_Parsing.R")

test = read.csv("MERGED2013_PP.csv")

#data for debt analysis of 4 year colleges
year13clean <- cleanData("MERGED2013_PP.csv")

#data for debt analysis of 2 year colleges
year13clean2 <- cleanData2("MERGED2013_PP.csv")

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

#############################
# RIDGE, LASSO, ELASTIC NET #
#############################
features_matrix <- as.matrix(year13_features[1:377])
# NA Values don't work in glmnet. For now I made them -1, but will need case-by-case review
features_matrix[is.na(features_matrix)] <- -1

fit.lasso <- glmnet(x=features_matrix, y=y_debt, family="gaussian", alpha=1)
fit.ridge <- glmnet(x=features_matrix, y=y_debt, family="gaussian", alpha=0)
fit.elastic <- glmnet(x=features_matrix, y=y_debt, family="gaussian", alpha=.5)

#############################
#         Impute            #
#############################
year13imp <- mice(year13clean, m=10, maxit=25)

#############################
#         MODELING          #
#############################

#create models
lm1 <- lm(y_debt ~ ., data = year13clean[,c(50:60, 128)], na.action = na.exclude)
summary(lm1)

#relevel default to Virginia
year13clean <- within(year13clean, STABBR <- relevel(STABBR, ref = "VA"))
year13clean2 <- within(year13clean2, STABBR <- relevel(STABBR, ref = "VA"))

#adj R^2 of 0.8039
lm2 <- lm(y_debt ~ STABBR + CCSIZSET + UGDS_BLACK + TUITFTE + poly(ADM_RATE,3) + 
          TUITIONFEE_OUT + PCTFLOAN + CDR3 +
          NOTFIRSTGEN_RPY_3YR_RT + DEP_INC_PCT_LO + RPY_5YR_N + DEP_RPY_5YR_N +
          PAR_ED_PCT_1STGEN + PELL_RPY_3YR_RT_SUPP +
          C150_4_POOLED_SUPP,
          data=na.omit(year13clean))

summary(lm2)

#2 year colleges (removed ADM_RATE, C150_4_POOLED_SUPP)
lm3 <- lm(y_debt ~ STABBR + CCSIZSET + UGDS_BLACK + TUITFTE +
          TUITIONFEE_OUT + PCTFLOAN + CDR3 +
          NOTFIRSTGEN_RPY_3YR_RT + DEP_INC_PCT_LO + RPY_5YR_N + DEP_RPY_5YR_N +
          PAR_ED_PCT_1STGEN + PELL_RPY_3YR_RT_SUPP,
          data=na.omit(year13clean2))

summary(lm3)
