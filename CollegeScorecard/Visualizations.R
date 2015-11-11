source("Data_Parsing.R")
require(ggplot2)

### import the college dataframe
if(file.exists("./data/cached_scorecard.Rda")){
  # if the dataframe was cached then don't bother loading it again
  load("./data/cached_scorecard.Rda")
} else {
  # otherwise, call cleanData and cache the frame
  college_df <- cleanData("./data/CollegeScorecard_Raw_Data/MERGED2013_PP.csv")
  save(college_df,file="./data/cached_scorecard.Rda")
}

qplot(college_df$DEBT_MDN)

qplot(college_df$COSTT4_A)

plot(DEBT_MDN~COSTT4_A,data=college_df)

plot(DEBT_MDN~TUITIONFEE_IN,data=college_df)

plot(DEBT_MDN~TUITIONFEE_OUT,data=college_df)
