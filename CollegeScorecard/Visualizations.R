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

# import shape file
state <- readOGR(dsn = "us_shapefiles", layer = "tl_2010_us_state10")
state@data$id <- rownames(state@data)

# convert polygons in state to a data frame for plotting
state.df <- fortify(state)

# calculate average salaries
avg.debt <- aggregate(DEBT_MDN ~ STABBR, data = data, FUN = mean)
colnames(avg.debt)[1] <- "STUSPS10"
# remove "MP"
avg.debt <- avg.debt[-26, ]

# join columns
state.df <- join(state.df, state@data, by="id")
state.df <- join(state.df, avg.debt, by = "STUSPS10")

state.df <- state.df[state.df$long < 0,] # delete random rows with inaccurate longitudes

# remove Puerto Rico, Alaska, and Hawaii
state.df <- state.df[state.df$NAME10 != "Puerto Rico",]
state.df <- state.df[state.df$NAME10 != "Alaska",]
state.df <- state.df[state.df$NAME10 != "Hawaii",]

# plot by count of students per state
ggp <- ggplot(data=state.df, aes(x=long, y=lat, group=group))   # draw polygons
ggp <- ggp + geom_path(color="#d3d3d3", size = 1) # draw boundaries
ggp <- ggp + coord_map()
ggp <- ggp + scale_fill_gradient(limits=c(8243, 15988), low="#c6dbef", high = "#08306b", na.value = "white")
ggp <- ggp + geom_polygon(aes(fill = DEBT_MDN))
ggp <- ggp + ggtitle(expression(atop(bold("2013 Median Debt"), atop("By State", ""))))
ggp <- ggp + theme(panel.background = element_blank(),
                   axis.ticks = element_blank(),
                   axis.text = element_blank(),
                   axis.title = element_blank(), 
                   panel.grid = element_blank())

plot(ggp)