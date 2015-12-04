########################################
# STAT6021 FINAL PROJECT - Team 8      #
# College Scorecard Data               #
# Don Chesworth, Mike Voltmer,         #
# Katherine Schinkel, and Marcus Rosti #
########################################

#########################
# LIBRARIES, WD, SOURCE #
#########################
library(leaps)
library(fmsb)
library(glmnet)
library(car)
library(mice)

setwd("~/Git/STAT_6021_Project/CollegeScorecard")

source("Data_Parsing.R")

#########################
#    DATA CLEANING      #
#########################
test = read.csv("MERGED2013_PP.csv")

#data for debt analysis of 4 year colleges
year13clean <- cleanData("MERGED2013_PP.csv")

#data for debt analysis of 2 year colleges
year13clean2 <- cleanData2("MERGED2013_PP.csv")

#####################
# MULTICOLLINEARITY #
#####################
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

# Remove some columns that error on the "full" run:
year13forward <- year13clean[,!names(year13clean) %in% c("main", "PBI", "ANNHI", "WOMENONLY")]
year13first_third <- year13forward[,c(1:54,163)]
year13second_third1 <- year13forward[,c(55:81,163)]
year13second_third2 <- year13forward[,c(82:108,163)]
year13last_third <- year13forward[,c(109:162,163)]

## Run first third on 1,080 rows due to NAs. Yields INSTURL, HIGHDEG
year13first_third <- na.omit(year13first_third)
first_null <- lm(y_debt ~ 1, data = year13first_third)
first_third <- lm(y_debt ~ ., data = year13first_third)
step1 <- step(first_null, scope = list(lower = first_null, upper = first_third), direction = "forward", trace = T)

## Run first half of second third on 442 rows due to NAs. Yields:
# DEP_INC_AVG + APPL_SCH_PCT_GE3 + MD_INC_RPY_5YR_RT + 
# FIRSTGEN_RPY_7YR_RT + IND_INC_AVG + IND_INC_N + LO_INC_RPY_5YR_RT + 
# DEP_INC_N + DEP_INC_PCT_M1 + LO_INC_RPY_7YR_RT + DEP_INC_PCT_LO + 
# IND_INC_PCT_LO + IND_INC_PCT_M1 + PAR_ED_PCT_1STGEN + DEP_STAT_PCT_IND + 
# FIRSTGEN_RPY_5YR_RT + NOTFIRSTGEN_RPY_7YR_RT
year13second_third1 <- na.omit(year13second_third1)
second_null <- lm(y_debt ~ 1, data = year13second_third1)
second_third <- lm(y_debt ~ ., data = year13second_third1)
step2 <- step(second_null, scope = list(lower = second_null, upper = second_third), direction = "forward", trace = T)

# Run second half of second third on 656 rows due to NAs. Yields:
# DEP_INC_AVG + APPL_SCH_PCT_GE3 + MD_INC_RPY_5YR_RT + 
# FIRSTGEN_RPY_7YR_RT + IND_INC_AVG + IND_INC_N + LO_INC_RPY_5YR_RT + 
# DEP_INC_N + DEP_INC_PCT_M1 + LO_INC_RPY_7YR_RT + DEP_INC_PCT_LO + 
# IND_INC_PCT_LO + IND_INC_PCT_M1 + PAR_ED_PCT_1STGEN + DEP_STAT_PCT_IND + 
# FIRSTGEN_RPY_5YR_RT + NOTFIRSTGEN_RPY_7YR_RT
year13second_third2 <- na.omit(year13second_third2)
second_null <- lm(y_debt ~ 1, data = year13second_third2)
second_third <- lm(y_debt ~ ., data = year13second_third2)
step2 <- step(second_null, scope = list(lower = second_null, upper = second_third), direction = "forward", trace = T)

# Run last third on 409 rows due to NAs. Yields:
# C150_4_POOLED_SUPP + HI_INC_RPY_7YR_N + APPL_SCH_N + 
# NOPELL_RPY_3YR_RT_SUPP + NOPELL_RPY_5YR_N + PAR_ED_N + IND_RPY_3YR_RT_SUPP + 
# COMPL_RPY_3YR_RT_SUPP + DEP_RPY_3YR_RT_SUPP + PELL_RPY_3YR_RT_SUPP + 
# DEP_RPY_5YR_N + RPY_3YR_RT_SUPP + FEMALE_RPY_3YR_N + FIRSTGEN_RPY_5YR_N
year13last_third <- na.omit(year13last_third)
third_null <- lm(y_debt ~ 1, data = year13last_third)
last_third <- lm(y_debt ~ ., data = year13last_third)
step3 <- step(third_null, scope = list(lower = third_null, upper = last_third), direction = "forward", trace = T)

# Create a final data set with all yielded columns
year13step_final <- year13clean[,c("INSTURL", "HIGHDEG",
                                    "DEP_INC_AVG", "APPL_SCH_PCT_GE3", "MD_INC_RPY_5YR_RT",
                                    "FIRSTGEN_RPY_7YR_RT", "IND_INC_AVG", "IND_INC_N", "LO_INC_RPY_5YR_RT",
                                    "DEP_INC_N", "DEP_INC_PCT_M1", "LO_INC_RPY_7YR_RT", "DEP_INC_PCT_LO",
                                    "IND_INC_PCT_LO", "IND_INC_PCT_M1", "PAR_ED_PCT_1STGEN", "DEP_STAT_PCT_IND",
                                    "FIRSTGEN_RPY_5YR_RT", "NOTFIRSTGEN_RPY_7YR_RT",
                                    "DEP_INC_AVG", "APPL_SCH_PCT_GE3", "MD_INC_RPY_5YR_RT",
                                    "FIRSTGEN_RPY_7YR_RT", "IND_INC_AVG", "IND_INC_N", "LO_INC_RPY_5YR_RT",
                                    "DEP_INC_N", "DEP_INC_PCT_M1", "LO_INC_RPY_7YR_RT", "DEP_INC_PCT_LO",
                                    "IND_INC_PCT_LO", "IND_INC_PCT_M1", "PAR_ED_PCT_1STGEN", "DEP_STAT_PCT_IND",
                                    "FIRSTGEN_RPY_5YR_RT", "NOTFIRSTGEN_RPY_7YR_RT",
                                    "C150_4_POOLED_SUPP", "HI_INC_RPY_7YR_N", "APPL_SCH_N",
                                    "NOPELL_RPY_3YR_RT_SUPP", "NOPELL_RPY_5YR_N", "PAR_ED_N", "IND_RPY_3YR_RT_SUPP",
                                    "COMPL_RPY_3YR_RT_SUPP", "DEP_RPY_3YR_RT_SUPP", "PELL_RPY_3YR_RT_SUPP",
                                    "DEP_RPY_5YR_N", "RPY_3YR_RT_SUPP", "FEMALE_RPY_3YR_N", "FIRSTGEN_RPY_5YR_N", "y_debt")]

# Run final on 601 rows due to NAs. Yields:
# INSTURL + HIGHDEG + C150_4_POOLED_SUPP
year13step_final <- na.omit(year13step_final)
final_null <- lm(y_debt ~ 1, data = year13step_final)
final_full <- lm(y_debt ~ ., data = year13step_final)
step2 <- step(final_null, scope = list(lower = final_null, upper = final_full), direction = "forward", trace = T)


##################
# REGULARIZATION #
##################

# Using our final model below, attempt to refine using 
features_matrix <- year13clean[,c("CCSIZSET", "UGDS_BLACK", "TUITFTE",
                                  "TUITIONFEE_OUT", "PCTFLOAN", "CDR3",
                                  "NOTFIRSTGEN_RPY_3YR_RT", "DEP_INC_PCT_LO", "DEP_RPY_5YR_N",
                                  "PAR_ED_PCT_1STGEN", "PELL_RPY_3YR_RT_SUPP",
                                  "C150_4_POOLED_SUPP", "y_debt")]

# Remove the NA values
features_matrix <- na.omit(features_matrix)

# Separate out the debt, and turn the factors into a matrix
y_debt <- features_matrix[,13]
features_matrix <- model.matrix(~.,data=features_matrix[,1:12])

# Create the fits
fit.lasso <- glmnet(x=features_matrix, y=y_debt, family="gaussian", alpha=1, nlambda=100)
fit.ridge <- glmnet(x=features_matrix, y=y_debt, family="gaussian", alpha=0)
fit.elastic <- glmnet(x=features_matrix, y=y_debt, family="gaussian", alpha=.5)

fit.lasso
# [46,] 18 0.69010   40.970
# [47,] 20 0.69050   37.330
# [48,] 20 0.69080   34.010
# [49,] 20 0.69110   30.990
# [50,] 20 0.69130   28.240
# [51,] 20 0.69150   25.730
# [52,] 22 0.69200   23.440

coef(fit.lasso,s=25.730)


# Plot the fits
plot(fit.lasso, xvar="lambda")
plot(fit.ridge, xvar="lambda")
plot(fit.elastic, xvar="lambda")

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

#adj R^2 of 0.7187
lm <- lm(y_debt ~ STABBR + CCSIZSET + UGDS_BLACK + TUITFTE  + 
          TUITIONFEE_OUT + PCTFLOAN + CDR3 +
          NOTFIRSTGEN_RPY_3YR_RT + DEP_INC_PCT_LO + RPY_5YR_N + DEP_RPY_5YR_N +
          PAR_ED_PCT_1STGEN + PELL_RPY_3YR_RT_SUPP +
          C150_4_POOLED_SUPP,
          data=year13clean, na.action = na.omit)

summary(lm)
vif(lm)

#adj R^2 of 0.6529
lm2 <- lm(y_debt ~ STABBR + CCSIZSET + 
            TUITIONFEE_OUT + PCTFLOAN + CDR3 +
            NOTFIRSTGEN_RPY_3YR_RT + DEP_INC_PCT_LO + RPY_5YR_N +
            C150_4_POOLED_SUPP,
          data=year13clean, na.action = na.omit)

summary(lm2)
vif(lm2)

#2 year colleges (removed ADM_RATE, C150_4_POOLED_SUPP), adj R^2 of 0.7396
lm3 <- lm(y_debt ~ STABBR + CCSIZSET + UGDS_BLACK + TUITFTE +
          TUITIONFEE_OUT + PCTFLOAN + CDR3 +
          NOTFIRSTGEN_RPY_3YR_RT + DEP_INC_PCT_LO + RPY_5YR_N + DEP_RPY_5YR_N +
          PAR_ED_PCT_1STGEN + PELL_RPY_3YR_RT_SUPP,
          data=year13clean2, na.action = na.omit)

summary(lm3)
vif(lm3)

