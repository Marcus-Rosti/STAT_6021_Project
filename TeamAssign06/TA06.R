#Team Assignment 6
#Team 8
#kms6bn, mer3ef, doc2g, mv5vf

library(parallel)

#load data
setwd("~/Git/STAT_6021_Project/TeamAssign06")
trees <- read.csv("trees.csv", header = T)


#################################################################################
### helper functions
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
        area <-
          acos(ndx / rl) * rl ^ 2 - ndx * sqrt(rl ^ 2 - ndx ^ 2)
      }
    }
    if (dx >= rl & dy < rl) {
      if (dy >= 0) {
        area <- (pi - acos(dy / rl)) * rl ^ 2 + dy * sqrt(rl ^ 2 - dy ^ 2)
      } else {
        ndy <- -dy
        area <-
          acos(ndy / rl) * rl ^ 2 - ndy * sqrt(rl ^ 2 - ndy ^ 2)
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
        area <-
          acos(ndy / rl) * rl ^ 2 - ndy * sqrt(rl ^ 2 - ndy ^ 2)
      }
      if (dx < 0 & dy >= 0) {
        ndx <- -dx
        area <-
          acos(ndx / rl) * rl ^ 2 - ndx * sqrt(rl ^ 2 - ndx ^ 2)
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

printf <- function(...)
  cat(sprintf(...))

printStats<-function(listr,timer) {
  #calculate percentage bias
  ba_area_estimate = sum(listr) / loop_lim
  percent_bias <- 100 * (ba_area_estimate - ba_actual) / ba_actual

  #calculate percentage root mean square error
  rmse <- 100 * sqrt(var(listr)) / ba_actual

  #print elapsed time, precentage bias, and percentage rmse
  printf(
    "Total Time: %3.3f secs\nbias      : %3.3f percent\nroot mse  : %3.3f",timer[3],percent_bias,rmse
  )
}

#
#
#################################################################################

# basic analysis
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
# set fixed values
r <- 37
a <- pi * r ^ 2
A <- (750 + 2 * r) ^ 2

#set the probability that a tree is selected into a random sample (the same for all samples)
pi_i <- a / A

loop_lim <- 10 ^ 5

#create empty vector to store basal area estimate
ba_area_estimate_values <- rep(0,loop_lim)

# function that calculates the estimated basal area
parTreeSub <- function(x) {
  x1 <- runif(1,-r, 750 + r)
  y1 <- runif(1,-r, 750 + r)
  subtrees <-
    subset(trees,(trees$x - x1) ^ 2 + (trees$y - y1) ^ 2 <= 37 ^ 2)$ba
  return(sum(subtrees) / pi_i)
}

# calculations
start = proc.time()
naive_ba_est <- unlist(mcMap(parTreeSub, 1:loop_lim))
total_time2 <- proc.time() - start

#statistics
printStats(naive_ba_est,total_time2)

################################################################################
#
# Measure pi_i method
#
################################################################################
# default parameters
r <- 37
A <- 750 ^ 2
loop_lim <- 10 ^ 5

#create empty vector to store basal area estimate
ba_area_estimate_values <- rep(0, loop_lim)

#keep track of time it takes to run loop
parTreePiI <- function(x) {
  #get random points for circle
  x1 <- runif(1, 0, 750)
  y1 <- runif(1, 0, 750)

  #calculate overlap area
  a <- overlap.area(x1, y1, r)

  #set the probability that a tree is selected into a random sample
  pi_i <- a / A

  #get the subset of trees that are within that circle
  trees.sub <-
    subset(trees,(trees$x - x1) ^ 2 + (trees$y - y1) ^ 2 <= 37 ^ 2)

  #calculate basal area of trees in the circle
  return((1 / pi_i) * sum(trees.sub$ba))
}

start = proc.time()
pi_i_ba_est <- unlist(mcMap(parTreePiI, 1:loop_lim))
total_time <- proc.time() - start

printStats(pi_i_ba_est,total_time)

################################################################################
#
# Repeated Masuyama Method
#
################################################################################
#set radius
repeatedMat <- function(x, radius = 37) {
  x1 <- runif(1, 0-radius, 750+radius)
  y1 <- runif(1, 0-radius, 750+radius)

  #get the subset of trees that are within that circle
  trees.sub <-
    subset(trees,(trees$x - x1) ^ 2 + (trees$y - y1) ^ 2 <= radius ^ 2)

  #calculate overlap area
  a <- overlap.area(x1, y1, radius)
  printf("sub area : %f\n",a)
  # if the sub area isn't the whole area
  if (a != pi * radius ^ 2) {
    radius_prime = sqrt( (pi*radius ^ 2 - a)/pi)
    printf("RadiusPrime : %f\n",radius_prime)
    return( sum(trees.sub$ba) + repeatedMat(0,radius = radius_prime ))
  }

  return(sum(trees.sub$ba))
}

start = proc.time()
# set fixed values
r <- 37
a <- pi * r ^ 2
A <- (750) ^ 2

#set the probability that a tree is selected into a random sample (the same for all samples)
pi_i <- a / A

# calculate each one
repeated_masuyama_ba_est <- unlist(mcMap(repeatedMat, 1:loop_lim))/pi_i
total_time <- proc.time() - start

#statistics
printStats(repeated_masuyama_ba_est,total_time)
