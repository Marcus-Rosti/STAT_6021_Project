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

# Remove some columns that error on the "full" run, as well as candidate keys:
year13forward <- year13clean[,!names(year13clean) %in% c("main", "st_fips", "INSTNM", "ZIP", "INSTURL", "NPCURL", 
                                                         "HIGHDEG", "PBI", "ANNHI", "WOMENONLY", "CITY",
                                                         "LATITUDE", "LONGITUDE")]
names(year13forward)

# Subset into four groups
year13first_fourth <- year13forward[,c(1:38,154)]
year13second_fourth <- year13forward[,c(39:76,154)]
year13third_fourth <- year13forward[,c(78:114,154)]
year13last_fourth <- year13forward[,c(115:153,154)]

## Run first fourth on 1,080 rows due to NAs. Yields:
# STABBR + CCSIZSET + CONTROL + LOCALE + 
# CCBASIC + ADM_RATE + HBCU + CCUGPROF + UGDS_ASIAN + UGDS_2MOR + 
# AccredAgency + UGDS_AIAN
year13first_fourth <- na.omit(year13first_fourth)
first_null <- lm(y_debt ~ 1, data = year13first_fourth)
first_fourth <- lm(y_debt ~ ., data = year13first_fourth)
step1 <- step(first_null, scope = list(lower = first_null, upper = first_fourth), direction = "forward", trace = T)
step1$call

## Run second fourth on 394 rows due to NAs. Yields:
# COSTT4_A + PCTFLOAN + RET_FT4 + PCTPELL + 
# MD_INC_RPY_3YR_RT + PFTFTUG1_EF + CDR3 + UG25abv + TUITFTE + 
# C150_4_NRA + D200_4 + TUITIONFEE_IN + PFTFAC + FIRSTGEN_RPY_5YR_RT
year13second_fourth <- na.omit(year13second_fourth)
second_null <- lm(y_debt ~ 1, data = year13second_fourth)
second_fourth <- lm(y_debt ~ ., data = year13second_fourth)
step2 <- step(second_null, scope = list(lower = second_null, upper = second_fourth), direction = "forward", trace = T)
step2$call

# Run third fourth on 415 rows due to NAs. Yields:
# DEP_INC_AVG + APPL_SCH_PCT_GE3 + PAR_ED_PCT_MS + 
# IND_INC_AVG + IND_INC_PCT_LO + DEP_STAT_PCT_IND + DEP_RPY_3YR_N + 
# APPL_SCH_N + PAR_ED_PCT_HS + HI_INC_RPY_3YR_N + DEP_INC_N + 
# MALE_RPY_3YR_N + APPL_SCH_PCT_GE5 + DEP_INC_PCT_M1 + NOTFIRSTGEN_RPY_3YR_N + 
# NONCOM_RPY_3YR_N + MD_INC_RPY_3YR_N
year13third_fourth <- na.omit(year13third_fourth)
third_null <- lm(y_debt ~ 1, data = year13third_fourth)
third_fourth <- lm(y_debt ~ ., data = year13third_fourth)
step3 <- step(third_null, scope = list(lower = third_null, upper = third_fourth), direction = "forward", trace = T)
step3$call

# Run last third on 567 rows due to NAs. Yields:
# C150_4_POOLED_SUPP + HI_INC_RPY_7YR_N + 
# MALE_RPY_7YR_N + NOPELL_RPY_3YR_RT_SUPP + NOTFIRSTGEN_RPY_3YR_RT_SUPP + 
# PELL_RPY_3YR_RT_SUPP + RPY_3YR_RT_SUPP + COMPL_RPY_3YR_RT_SUPP + 
# DEP_RPY_7YR_N + NONCOM_RPY_7YR_N + MALE_RPY_5YR_N + NONCOM_RPY_3YR_RT_SUPP + 
# DEP_RPY_5YR_N + IND_RPY_3YR_RT_SUPP
year13last_fourth <- na.omit(year13last_fourth)
last_null <- lm(y_debt ~ 1, data = year13last_fourth)
last_fourth <- lm(y_debt ~ ., data = year13last_fourth)
step4 <- step(last_null, scope = list(lower = last_null, upper = last_fourth), direction = "forward", trace = T)
step4$call

# Create a final data set with all yielded columns
year13step_final <- year13clean[,c("STABBR", "CCSIZSET", "CONTROL", "LOCALE",
                                   "COSTT4_A", "PCTFLOAN", "RET_FT4", "PCTPELL",
                                   "DEP_INC_AVG", "APPL_SCH_PCT_GE3", "PAR_ED_PCT_MS",
                                   "IND_INC_AVG",
                                   "C150_4_POOLED_SUPP", "HI_INC_RPY_7YR_N",
                                   "MALE_RPY_7YR_N", "NOPELL_RPY_3YR_RT_SUPP",
                                   "y_debt")]

# Run final on 1029 rows due to NAs. 
year13step_final <- na.omit(year13step_final)
final_null <- lm(y_debt ~ 1, data = year13step_final)
final_full <- lm(y_debt ~ ., data = year13step_final)
final_step <- step(final_null, scope = list(lower = final_null, upper = final_full), direction = "forward", trace = T)
final_step$call
lmforward <- lm(formula = y_debt ~ DEP_INC_AVG + PCTFLOAN + RET_FT4 + APPL_SCH_PCT_GE3 + 
     STABBR + CCSIZSET + CONTROL + PCTPELL + NOPELL_RPY_3YR_RT_SUPP + 
     HI_INC_RPY_7YR_N + IND_INC_AVG + C150_4_POOLED_SUPP, data=year13clean, na.action = na.omit)
summary(lmforward)
#adj R^2 of 0.6992

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

