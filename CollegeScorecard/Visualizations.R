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

######################
# PLOT DEBT BY STATE #
######################

require("rgdal")
require("maptools")
require("ggplot2")
require("plyr")
require("RColorBrewer")
require("scales")

# read in placement data
data <- read.csv("MERGED2013_PP.csv", na.strings=c("NULL", "PrivacySuppressed"), stringsAsFactors = FALSE)

# 4 year
data_4 <- data[data$CCBASIC %in% c(15, 16, 17, 18, 19, 20, 21, 22, 23),]

# 2 year
data_2 <- data[data$CCBASIC %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 13),]

# import shape file
state <- readOGR(dsn = "us_shapefiles", layer = "tl_2010_us_state10")
state@data$id <- rownames(state@data)

# convert polygons in state to a data frame for plotting
state.df_fort <- fortify(state)

# calculate average debt - 4 year
avg.debt_4 <- aggregate(DEBT_MDN ~ STABBR, data = data_4, FUN = mean)
colnames(avg.debt_4)[1] <- "STUSPS10"
# remove "MP"
avg.debt_4 <- avg.debt_4[-26, ]

# calculate average debt - 2 year
avg.debt_2 <- aggregate(DEBT_MDN ~ STABBR, data = data_2, FUN = mean)
colnames(avg.debt_2)[1] <- "STUSPS10"
# remove "MP"
avg.debt_2 <- avg.debt_2[-26, ]

# join columns - 4 year
state.df <- join(state.df_fort, state@data, by="id")
state.df <- join(state.df, avg.debt_4, by = "STUSPS10")

state.df <- state.df[state.df$long < 0,] # delete random rows with inaccurate longitudes

# remove Puerto Rico, Alaska, and Hawaii
state.df <- state.df[state.df$NAME10 != "Puerto Rico",]
state.df <- state.df[state.df$NAME10 != "Alaska",]
state.df <- state.df[state.df$NAME10 != "Hawaii",]

# plot 4 year colleges
ggp <- ggplot(data=state.df, aes(x=long, y=lat, group=group))   # draw polygons
ggp <- ggp + geom_path(color="#d3d3d3", size = 1) # draw boundaries
ggp <- ggp + coord_map()
ggp <- ggp + scale_fill_gradient(limits=c(6010, 21141), low="#c6dbef", high = "#08306b", na.value = "white")
ggp <- ggp + geom_polygon(aes(fill = DEBT_MDN))
ggp <- ggp + ggtitle(expression(atop(bold("2013 Median Debt"), atop("4-Year Colleges", ""))))
ggp <- ggp + theme(panel.background = element_blank(),
                   axis.ticks = element_blank(),
                   axis.text = element_blank(),
                   axis.title = element_blank(), 
                   panel.grid = element_blank())

plot(ggp)

# join columns - 2 year
state.df_2 <- join(state.df_fort, state@data, by="id")
state.df_2 <- join(state.df_2, avg.debt_2, by = "STUSPS10")

state.df_2 <- state.df_2[state.df_2$long < 0,] # delete random rows with inaccurate longitudes

# remove Puerto Rico, Alaska, and Hawaii
state.df_2 <- state.df_2[state.df_2$NAME10 != "Puerto Rico",]
state.df_2 <- state.df_2[state.df_2$NAME10 != "Alaska",]
state.df_2 <- state.df_2[state.df_2$NAME10 != "Hawaii",]

# plot 2 year colleges
ggp <- ggplot(data=state.df_2, aes(x=long, y=lat, group=group))   # draw polygons
ggp <- ggp + geom_path(color="#d3d3d3", size = 1) # draw boundaries
ggp <- ggp + coord_map()
ggp <- ggp + scale_fill_gradient(limits=c(3489, 11848), low="#edf8e9", high = "#006d2c", na.value = "white")
ggp <- ggp + geom_polygon(aes(fill = DEBT_MDN))
ggp <- ggp + ggtitle(expression(atop(bold("2013 Median Debt"), atop("2-Year Colleges", ""))))
ggp <- ggp + theme(panel.background = element_blank(),
                   axis.ticks = element_blank(),
                   axis.text = element_blank(),
                   axis.title = element_blank(), 
                   panel.grid = element_blank())

plot(ggp)