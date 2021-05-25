####################################################################################
## Calculate angle
## Modified from https://stackoverflow.com/questions/14066933/direct-way-of-computing-clockwise-angle-between-2-vectors/16544330#16544330
## Daniel Anstett
## Last Modified Feb 16, 2021
####################################################################################
#Import libraries
library(NISTunits)
library(tidyverse)
library(vegan)
library(devtools)
library("factoextra")

####################################################################################
# Read in pre and peak datasets 
all <- read.csv("Data/all.csv", header=T)
pre <- read.csv("Data/pre.csv", header=T)
peak <- read.csv("Data/peak.csv", header=T)
####################################################################################
####################################################################################
#Permutation 
angle.diff <- data_frame() # set dataframe reciving permuated angles

# row shuffling

for(i in 1:10000) {
set.seed(i)
rows <- sample(nrow(all),replace = FALSE,) # randomize row indices
all.shuf <- all[rows, ] # shuffle rows
pre.shuf <- all.shuf[1:225,] # make random pre dataset
peak.shuf <- all.shuf[226:498,] # make random post dataset
pc.pre <- prcomp(pre.shuf[,c("Experiment_Date","SLA","Water_Content","Stomatal_Conductance","Assimilation")], scale=T)
pc.peak <- prcomp(peak.shuf[,c("Experiment_Date","SLA","Water_Content","Stomatal_Conductance","Assimilation")], scale=T)


# Get pre coordinates
var.pre <- get_pca_var(pc.pre) # extract data
x1.pre <- var.pre$coord[1,1] # set coordinates
y1.pre <- var.pre$coord[1,2]
x2.pre <- var.pre$coord[3,1]
y2.pre <- var.pre$coord[3,2]
x5.pre <- var.pre$coord[5,1]
y5.pre <- var.pre$coord[5,2]

#calc pre angle between Flowering Time and Assimilation
dot.pre.5 <- x1.pre*x5.pre + y1.pre*y5.pre      # dot product between [x1, y1] and [X5, y2]
det.pre.5 <- x1.pre*y5.pre - y1.pre*x5.pre      # determinant
radian.pre.5 <- atan2(det.pre.5, dot.pre.5)  # atan2(y, x) or atan2(sin, cos)
angle.pre.5 <- NISTradianTOdeg(radian.pre.5)

#calc pre angle
dot.pre <- x1.pre*x2.pre + y1.pre*y2.pre      # dot product between [x1, y1] and [x2, y2]
det.pre <- x1.pre*y2.pre - y1.pre*x2.pre      # determinant
radian.pre <- atan2(det.pre, dot.pre)  # atan2(y, x) or atan2(sin, cos)
angle.pre <- NISTradianTOdeg(radian.pre)
if(angle.pre<0){angle.pre<-angle.pre+360} #scale in 360 deg counterclockwise
if(angle.pre.5<0){angle.pre<-360-angle.pre} #scale in 360 deg counterclockwise
angle.diff[i,1]<-angle.pre

# Get peak coordinates
var.peak <- get_pca_var(pc.peak) # extract data
x1.peak <- var.peak$coord[1,1] # set coordinates
y1.peak <- var.peak$coord[1,2]
x2.peak <- var.peak$coord[3,1]
y2.peak <- var.peak$coord[3,2]
x5.peak <- var.peak$coord[5,1]
y5.peak <- var.peak$coord[5,2]

#calc peak angle between Flowering Time and Assimilation
dot.peak.5 <- x1.peak*x5.peak + y1.peak*y5.peak      # dot product between [x1, y1] and [X5, y2]
det.peak.5 <- x1.peak*y5.peak - y1.peak*x5.peak      # determinant
radian.peak.5 <- atan2(det.peak.5, dot.peak.5)  # atan2(y, x) or atan2(sin, cos)
angle.peak.5 <- NISTradianTOdeg(radian.peak.5)


#calc peak angle
dot.peak <- x1.peak*x2.peak + y1.peak*y2.peak      # dot product between [x1, y1] and [x2, y2]
det.peak <- x1.peak*y2.peak - y1.peak*x2.peak      # determinant
radian.peak <- atan2(det.peak, dot.peak)  # atan2(y, x) or atan2(sin, cos)
angle.peak <- NISTradianTOdeg(radian.peak)
if(angle.peak<0){angle.peak<-angle.peak+360} #scale in 360 deg counterclockwise
if(angle.peak.5<0){angle.peak<-360-angle.peak} #scale in 360 deg counterclockwise
angle.diff[i,2]<-angle.peak
}

colnames(angle.diff) <- c("Pre_Angle", "Peak_Angle")
angle.diff <- angle.diff %>% mutate(Angle_Difference=Peak_Angle-Pre_Angle)



##########################################
#Angle of actual data

pc.pre <- prcomp(pre[,c("Experiment_Date","SLA","Water_Content","Stomatal_Conductance","Assimilation")], scale=T)
pc.peak <- prcomp(peak[,c("Experiment_Date","SLA","Water_Content","Stomatal_Conductance","Assimilation")], scale=T)
biplot(pc.pre, scale=0, col=c("black", "red"), xlab = "PC1 (54%)", ylab="PC2 (20%)")
biplot(pc.peak, scale=0, col=c("black", "red"), xlab = "PC1 (54%)", ylab="PC2 (20%)")



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
if(angle.pre<0){angle.pre<-angle.pre+360} #scale in 360 deg counterclockwise

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
if(angle.peak<0){angle.peak<-angle.peak+360} #scale in 360 deg counterclockwise


angle.data <- angle.peak - angle.pre
angle.data

#Histogram

hist <- ggplot(angle.diff, aes(X=Angle_Difference))+
  geom_histogram(aes(Angle_Difference))+
  geom_vline(xintercept=c(angle.data),color="#0000CC")+
  scale_y_continuous(name="Count")+
  scale_x_continuous(name="Angle Difference")+
  theme_classic()
hist<- hist + theme(
  axis.text.x = element_text(size=12, face="bold"),
  axis.text.y = element_text(size=12,face="bold"),
  axis.title.x = element_text(color="black", size=14, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=14,vjust = 2, face="bold",hjust=0.5))
hist #p= 1-(7898/10,000)= 0.21

