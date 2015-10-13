#Team Assignment 6
#Team 8
#kms6bn, mer3ef, doc2g, mv5vf

#load data
setwd("~/Git/STAT_6021_Project/TeamAssign06")
trees <- read.csv("trees.csv", header = T)

#plot trees
plot(trees$x, trees$y, col = "dark green")

#save the actual basal area of the 750 x 750 forest
#this value is the same for all methods
ba_actual <- 311.906

################################################################################
#
# Matasuyama's method
#
################################################################################
#set radius
r <- 37

#set area of circular sample
a <- pi * r ^ 2

#set area of the 750 x 750 forest, and include the radius
A <- (750 + 2 * r) ^ 2

#set the probability that a tree is selected into a random sample (the same for all samples)
pi_i <- a / A

#run loop 100,000 times
loop_lim <- 10 ^ 5

#create empty vector to store basal area estimate
ba_area_estimate_values <- rep(0, loop_lim)

#keep track of time it takes to run loop
start <- proc.time()

for (i in 1:loop_lim) {
  
  #get random points for circle
  x1 <- runif(1,-r, 750 + r)
  y1 <- runif(1,-r, 750 + r)

  #get the subset of trees that are within that circle
  trees.sub <- subset(trees,(trees$x - x1) ^ 2 + (trees$y - y1) ^ 2 <= 37 ^ 2)
  
  #calculate basal area of trees in the circle
  ba_area_estimate_values[i] <- (1 / pi_i) * sum(trees.sub$ba)
}
total_time <- proc.time() - start

#calculate percentage bias
ba_area_estimate = sum(ba_area_estimate_values) / loop_lim
percent_bias <- 100 * (ba_area_estimate - ba_actual) / ba_actual

#calculate percentage root mean square error
rmse <- 100 * sqrt(var(ba_area_estimate_values)) / ba_actual

#print elapsed time, precentage bias, and percentage rmse
print(total_time[3])
print(percent_bias)
print(rmse)

################################################################################
#
# Measure pi_i method
#
################################################################################
#set radius
r <- 37

#set area of the 750 x 750 forest
A <- 750 ^ 2

#run loop 100,000 times
loop_lim <- 10 ^ 5

#create empty vector to store basal area estimate
ba_area_estimate_values <- rep(0, loop_lim)

#keep track of time it takes to run loop
start <- proc.time()

for (i in 1:loop_lim) {
  
  #get random points for circle
  x1 <- runif(1, 0, 750)
  y1 <- runif(1, 0, 750)

  #calculate overlap area
  a <- overlap.area(x1, y1, r)
  
  #set the probability that a tree is selected into a random sample
  pi_i <- a / A

  #get the subset of trees that are within that circle
  trees.sub <- subset(trees,(trees$x - x1) ^ 2 + (trees$y - y1) ^ 2 <= 37 ^ 2)

  #calculate basal area of trees in the circle
  ba_area_estimate_values[i] <- (1 / pi_i) * sum(trees.sub$ba)
}
total_time <- proc.time() - start

#calculate percentage bias
ba_area_estimate = sum(ba_area_estimate_values) / loop_lim
percent_bias <- 100 * (ba_area_estimate - ba_actual) / ba_actual

#calculate percentage root mean square error
rmse <- 100 * sqrt(var(ba_area_estimate_values)) / ba_actual

#print elapsed time, precentage bias, and percentage rmse
print(total_time[3])
print(percent_bias)
print(rmse)

################################################################################
#
# Repeated Masuyama Method
#
################################################################################
#set radius
r <- 37


################################################################################
################################################################################
################################################################################
################################################################################

# 1. A couple of examples of the proc.time function

start <- proc.time()   # A slow example
t <- 0
for (i in 1:1000000) {
  t <- t + i
}
proc.time() - start

start <- proc.time()   # A faster example
t <- sum(as.numeric(1:1000000))
proc.time() - start

# 2. A gift to your team
# "overlap.area(xt,yt,rl)" is a function that computes the intersection of
# a disk of radius rl centered at (xt,yt) with the 750-by-750 region that
# contains the stand of trees.
#
overlap.area <- function(xt,yt,rl) {
  dx <- min(xt, 750 - xt)
  dy <- min(yt, 750 - yt)
  if (dx >= rl & dy >= rl) {
    area <- pi * rl ^ 2
  } else {
    if (dx < rl & dy >= rl) {
      if (dx >= 0) {
        area <- (pi - acos(dx / rl)) * rl ^ 2 + dx * sqrt(rl ^ 2 - dx ^ 2)
      } else {
        ndx <- -dx
        area <- acos(ndx / rl) * rl ^ 2 - ndx * sqrt(rl ^ 2 - ndx ^ 2)
      }
    }
    if (dx >= rl & dy < rl) {
      if (dy >= 0) {
        area <- (pi - acos(dy / rl)) * rl ^ 2 + dy * sqrt(rl ^ 2 - dy ^ 2)
      } else {
        ndy <- -dy
        area <- acos(ndy / rl) * rl ^ 2 - ndy * sqrt(rl ^ 2 - ndy ^ 2)
      }
    }
    if (dx < rl & dy < rl & (dx ^ 2 + dy ^ 2) >= rl ^ 2) {
      if (dx >= 0 & dy >= 0) {
        area <-
          (pi - acos(dx / rl) - acos(dy / rl)) * rl ^ 2 + dx * sqrt(rl ^ 2 - dx ^
                                                                      2) + dy * sqrt(rl ^ 2 - dy ^ 2)
      }
      if (dx >= 0 & dy < 0) {
        ndy <- -dy
        area <- acos(ndy / rl) * rl ^ 2 - ndy * sqrt(rl ^ 2 - ndy ^ 2)
      }
      if (dx < 0 & dy >= 0) {
        ndx <- -dx
        area <- acos(ndx / rl) * rl ^ 2 - ndx * sqrt(rl ^ 2 - ndx ^ 2)
      }
      if (dx < 0 & dy < 0) {
        area <- 0
      }
    }
    if (dx < rl & dy < rl & (dx ^ 2 + dy ^ 2) < rl ^ 2) {
      if (dx >= 0 & dy >= 0) {
        theta <- (3 / 2) * pi - acos(dx / rl) - acos(dy / rl)
        area <-
          (theta / 2) * rl ^ 2 + 0.5 * (dx * sqrt(rl ^ 2 - dx ^ 2) + dy * sqrt(rl ^
                                                                                 2 - dy ^ 2)) + dx * dy
      }
      if (dx >= 0 & dy < 0) {
        area1 <- acos(dx / rl) * rl ^ 2 - dx * sqrt(rl ^ 2 - dx ^ 2)
        ndy <- -dy
        theta <- (3 / 2) * pi - acos(dx / rl) - acos(ndy / rl)
        area2 <-
          (theta / 2) * rl ^ 2 + 0.5 * (dx * sqrt(rl ^ 2 - dx ^ 2) + ndy * sqrt(rl ^
                                                                                  2 - ndy ^ 2)) + dx * ndy
        area <- pi * rl ^ 2 - (area1 + area2)
      }
      if (dx < 0 & dy >= 0) {
        area1 <- acos(dy / rl) * rl ^ 2 - dy * sqrt(rl ^ 2 - dy ^ 2)
        ndx <- -dx
        theta <- (3 / 2) * pi - acos(ndx / rl) - acos(dy / rl)
        area2 <-
          (theta / 2) * rl ^ 2 + 0.5 * (ndx * sqrt(rl ^ 2 - ndx ^ 2) + dy * sqrt(rl ^
                                                                                   2 - dy ^ 2)) + ndx * dy
        area <- pi * rl ^ 2 - (area1 + area2)
      }
      if (dx < 0 & dy < 0) {
        ndx <- -dx
        ndy <- -dy
        theta <- (3 / 2) * pi + asin(ndx / rl) + asin(ndy / rl)
        area <-
          pi * rl ^ 2 - ((theta / 2) * rl ^ 2 + 0.5 * (ndx * sqrt(rl ^ 2 - ndx ^
                                                                    2) + ndy * sqrt(rl ^ 2 - ndy ^ 2)) - ndx * ndy)
      }
    }
  }
  return(area)
}