#Team Assignment 6
#Team 8
#kms6bn, mer3ef, doc2g, mv5vf

#load data
setwd("~/Git/STAT_6021_Project/TeamAssign06")
trees <- read.csv("trees.csv", header=T)

#plot trees
plot(trees$x, trees$y, col="dark green")

#Masuyama's Method

#get a random point
r <- 37

x1 <- runif(1, -r, 750+r)
y1 <- runif(1, -r, 750+r)


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
  dx <- min(xt, 750-xt)
  dy <- min(yt, 750-yt)
  if (dx >= rl & dy >= rl) {
    area <- pi*rl^2
  } else {
    if (dx < rl & dy >= rl) {
      if (dx >= 0) {
        area <- (pi - acos(dx/rl))*rl^2 + dx*sqrt(rl^2 - dx^2)
      } else {
        ndx <- -dx
        area <- acos(ndx/rl)*rl^2 - ndx*sqrt(rl^2 - ndx^2)
      }
    }
    if (dx >= rl & dy < rl) {
      if (dy >= 0) {
        area <- (pi - acos(dy/rl))*rl^2 + dy*sqrt(rl^2 - dy^2)
      } else {
        ndy <- -dy
        area <- acos(ndy/rl)*rl^2 - ndy*sqrt(rl^2 - ndy^2)
      }
    }
    if (dx < rl & dy < rl & (dx^2 + dy^2) >= rl^2) {
      if (dx >= 0 & dy >= 0) {
        area <- (pi-acos(dx/rl)-acos(dy/rl))*rl^2 + dx*sqrt(rl^2-dx^2)+dy*sqrt(rl^2-dy^2)
      }
      if (dx >= 0 & dy < 0) {
        ndy <- -dy
        area <- acos(ndy/rl)*rl^2 - ndy*sqrt(rl^2 - ndy^2)
      }
      if (dx < 0 & dy >= 0) {
        ndx <- -dx
        area <- acos(ndx/rl)*rl^2 - ndx*sqrt(rl^2 - ndx^2)
      }
      if (dx < 0 & dy < 0) {
        area <- 0
      }