# Calculate angle
# Modified from https://stackoverflow.com/questions/14066933/direct-way-of-computing-clockwise-angle-between-2-vectors/16544330#16544330

#Import libraries
library(NISTunits)
library(tidyverse)
library(vegan)
library(devtools)
library("factoextra")




##########################################
#Permutation 
# Read in pre and peak datasets 
pre <- read.csv("Data/pre.csv") ; peak <- read.csv("Data/peak.csv")
pre <- na.omit(pre) ; peak <- na.omit(peak)
#new Time columns
pre$Time <- "Pre" ; peak$Time <- "Peak"
#bind together
all <- rbind(pre,peak)

angle.diff <- data_frame()
#shuffling
set.seed(42)
rows <- sample(nrow(all)) # randomize row indices
all.shuf <- all[rows, ] # shuffle rows
pre.shuf <- all.shuf[1:245,] # make random pre dataset
peak.shuf <- all.shuf[246:534,] # make random post dataset
pc.pre <- prcomp(pre.shuf[,c("Experiment_Date","SLA","Water_Content","Stomatal_Conductance","Assimilation")], scale=T)
pc.peak <- prcomp(peak.shuf[,c("Experiment_Date","SLA","Water_Content","Stomatal_Conductance","Assimilation")], scale=T)
#biplot(pc.pre, scale=0, col=c("black", "red")) Visualize
#biplot(pc.peak, scale=0, col=c("black", "red"))

# Get pre coordinates
var.pre <- get_pca_var(pc.pre) # extract data
x1.pre <- var.pre$coord[1,1] # set coordinates
y1.pre <- var.pre$coord[1,2]
x2.pre <- var.pre$coord[3,1]
y2.pre <- var.pre$coord[3,2]

#calc pre angle
dot.pre <- x1.pre*x2.pre + y1.pre*y2.pre      # dot product between [x1, y1] and [x2, y2]
det.pre <- x1.pre*y2.pre - y1.pre*x2.pre      # determinant
radian.pre <- atan2(det.pre, dot.pre)  # atan2(y, x) or atan2(sin, cos)
angle.pre <- NISTradianTOdeg(radian.pre)
angle.diff[1,1]<-angle.pre

# Get peak coordinates
var.peak <- get_pca_var(pc.peak) # extract data
x1.peak <- var.peak$coord[1,1] # set coordinates
y1.peak <- var.peak$coord[1,2]
x2.peak <- var.peak$coord[3,1]
y2.peak <- var.peak$coord[3,2]

#calc peak angle
dot.peak <- x1.peak*x2.peak + y1.peak*y2.peak      # dot product between [x1, y1] and [x2, y2]
det.peak <- x1.peak*y2.peak - y1.peak*x2.peak      # determinant
radian.peak <- atan2(det.peak, dot.peak)  # atan2(y, x) or atan2(sin, cos)
angle.peak <- NISTradianTOdeg(radian.peak)
angle.diff[1,2]<-angle.peak

colnames(angle.diff) <- c("Pre_Angle", "Peak_Angle")





#How to calculate angle in 360 deg with counterclockwise direction

x1 <- 1
y1 <- 0
x2 <- -0.5
y2 <- -0.5

dot <- x1*x2 + y1*y2      # dot product between [x1, y1] and [x2, y2]
det <- x1*y2 - y1*x2      # determinant
angle <- atan2(det, dot)  # atan2(y, x) or atan2(sin, cos)
angle
NISTradianTOdeg(angle)









