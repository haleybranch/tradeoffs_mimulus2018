####################################################################################
## Calculate angles for regions
## Modified from https://stackoverflow.com/questions/14066933/direct-way-of-computing-clockwise-angle-between-2-vectors/16544330#16544330
## HB
## Last Modified March 4, 2021
####################################################################################
#Import libraries
library(NISTunits)
library(tidyverse)
library(vegan)
library(devtools)
library("factoextra")
library(cowplot)

####################################################################################
# Read in pre and peak datasets 
pre.N <- read.csv("Data/pre.N.csv") ; peak.N <- read.csv("Data/peak.N.csv")
pre.C <- read.csv("Data/pre.C.csv") ; peak.C <- read.csv("Data/peak.C.csv")
pre.S <- read.csv("Data/pre.S.csv") ; peak.S <- read.csv("Data/peak.S.csv")

#bind together
all.N <- rbind(pre.N,peak.N)
all.C <- rbind(pre.C,peak.C)
all.S <- rbind(pre.S,peak.S)

# Make all blank dataframes for angle difference data
angle.diff.N <- data_frame() # set dataframe reciving permuated angles
angle.diff.C <- data_frame() # set dataframe reciving permuated angles
angle.diff.S <- data_frame() # set dataframe reciving permuated angles


####################################################################################
####################################################################################
#North Permutation 

# row shuffling
for(i in 1:10000) {
  set.seed(i)
  rows <- sample(nrow(all.N),replace = FALSE,) # randomize row indices
  all.Nshuf <- all.N[rows, ] # shuffle rows
  pre.Nshuf <- all.Nshuf[1:102,] # make random pre dataset
  peak.Nshuf <- all.Nshuf[103:208,] # make random post dataset
  pc.pre.N <- prcomp(pre.Nshuf[,c("Experiment_Date","SLA","Water_Content","Stomatal_Conductance","Assimilation")], scale=T)
  pc.peak.N <- prcomp(peak.Nshuf[,c("Experiment_Date","SLA","Water_Content","Stomatal_Conductance","Assimilation")], scale=T)
  
  
  # Get pre coordinates
  var.pre.N <- get_pca_var(pc.pre.N) # extract data
  x1.pre.N <- var.pre.N$coord[1,1] # set coordinates
  y1.pre.N <- var.pre.N$coord[1,2]
  x2.pre.N <- var.pre.N$coord[3,1]
  y2.pre.N <- var.pre.N$coord[3,2]
  x5.pre.N <- var.pre.N$coord[5,1]
  y5.pre.N <- var.pre.N$coord[5,2]
  
  #calc pre angle between Flowering Time and Assimilation
  dot.preN.5 <- x1.pre.N*x5.pre.N + y1.pre.N*y5.pre.N      # dot product between [x1, y1] and [X5, y2]
  det.preN.5 <- x1.pre.N*y5.pre.N - y1.pre.N*x5.pre.N      # determinant
  radian.preN.5 <- atan2(det.preN.5, dot.preN.5)  # atan2(y, x) or atan2(sin, cos)
  angle.preN.5 <- NISTradianTOdeg(radian.preN.5)
  
  #calc pre angle
  dot.preN <- x1.pre.N*x2.pre.N + y1.pre.N*y2.pre.N      # dot product between [x1, y1] and [x2, y2]
  det.preN <- x1.pre.N*y2.pre.N - y1.pre.N*x2.pre.N      # determinant
  radian.preN <- atan2(det.preN, dot.preN)  # atan2(y, x) or atan2(sin, cos)
  angle.preN <- NISTradianTOdeg(radian.preN)
  if(angle.preN<0){angle.preN<-angle.preN+360} #scale in 360 deg counterclockwise
  if(angle.preN.5<0){angle.preN<-360-angle.preN} #scale in 360 deg counterclockwise
  angle.diff.N[i,1]<-angle.preN
  
  # Get peak coordinates
  var.peak.N <- get_pca_var(pc.peak.N) # extract data
  x1.peak.N <- var.peak.N$coord[1,1] # set coordinates
  y1.peak.N <- var.peak.N$coord[1,2]
  x2.peak.N <- var.peak.N$coord[3,1]
  y2.peak.N <- var.peak.N$coord[3,2]
  x5.peak.N <- var.peak.N$coord[5,1]
  y5.peak.N <- var.peak.N$coord[5,2]
  
  #calc peak angle between Flowering Time and Assimilation
  dot.peakN.5 <- x1.peak.N*x5.peak.N + y1.peak.N*y5.peak.N      # dot product between [x1, y1] and [X5, y2]
  det.peakN.5 <- x1.peak.N*y5.peak.N - y1.peak.N*x5.peak.N      # determinant
  radian.peakN.5 <- atan2(det.peakN.5, dot.peakN.5)  # atan2(y, x) or atan2(sin, cos)
  angle.peakN.5 <- NISTradianTOdeg(radian.peakN.5)
  
  
  #calc peak angle
  dot.peakN <- x1.peak.N*x2.peak.N + y1.peak.N*y2.peak.N      # dot product between [x1, y1] and [x2, y2]
  det.peakN <- x1.peak.N*y2.peak.N - y1.peak.N*x2.peak.N      # determinant
  radian.peakN <- atan2(det.peakN, dot.peakN)  # atan2(y, x) or atan2(sin, cos)
  angle.peakN <- NISTradianTOdeg(radian.peakN)
  if(angle.peakN<0){angle.peakN<-angle.peakN+360} #scale in 360 deg counterclockwise
  if(angle.peakN.5<0){angle.peakN<-360-angle.peakN} #Ensure correct directionality with respect to Assimilation
  angle.diff.N[i,2]<-angle.peakN
}

colnames(angle.diff.N) <- c("Pre_Angle", "Peak_Angle")
angle.diff.N <- angle.diff.N %>% mutate(Angle_Difference=Peak_Angle-Pre_Angle)



##########################################
#Angle of actual data

pc.pre <- prcomp(pre.N[,c("Experiment_Date","SLA","Water_Content","Stomatal_Conductance","Assimilation")], scale=T)
pc.peak <- prcomp(peak.N[,c("Experiment_Date","SLA","Water_Content","Stomatal_Conductance","Assimilation")], scale=T)

#biplot(pc.pre, scale=0, col=c("black", "red"))
#biplot(pc.peak, scale=0, col=c("black", "red"))

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
if(angle.pre.5<0){angle.pre<-360-angle.pre} #Ensure correct directionality with respect to Assimilation

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
if(angle.peak.5<0){angle.peak<-360-angle.peak} #Ensure correct directionality with respect to Assimilation

angle.data <- angle.peak - angle.pre
angle.data

#Histogram

hist.N <- ggplot(angle.diff.N, aes(X=Angle_Difference))+
  geom_histogram(aes(Angle_Difference))+
  geom_vline(xintercept=c(angle.data),color="#0000CC")+
  scale_y_continuous(name="Count")+
  scale_x_continuous(name="Angle Difference")+
  theme_classic()
hist.N<- hist.N + theme(
  axis.text.x = element_text(size=12, face="bold"),
  axis.text.y = element_text(size=12,face="bold"),
  axis.title.x = element_text(color="black", size=14, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=14,vjust = 2, face="bold",hjust=0.5))
hist.N # p = 1-(9799/10,000) = 0.0201




####################################################################################
####################################################################################
#Central Permutation 

# row shuffling
for(i in 1:10000) {
  set.seed(i)
  rows <- sample(nrow(all.C),replace = FALSE,) # randomize row indices
  all.Cshuf <- all.C[rows, ] # shuffle rows
  pre.Cshuf <- all.Cshuf[1:79,] # make random pre dataset
  peak.Cshuf <- all.Cshuf[80:180,] # make random post dataset
  pc.pre.C <- prcomp(pre.Cshuf[,c("Experiment_Date","SLA","Water_Content","Stomatal_Conductance","Assimilation")], scale=T)
  pc.peak.C <- prcomp(peak.Cshuf[,c("Experiment_Date","SLA","Water_Content","Stomatal_Conductance","Assimilation")], scale=T)
  
  
  # Get pre coordinates
  var.pre.C <- get_pca_var(pc.pre.C) # extract data
  x1.pre.C <- var.pre.C$coord[1,1] # set coordinates
  y1.pre.C <- var.pre.C$coord[1,2]
  x2.pre.C <- var.pre.C$coord[3,1]
  y2.pre.C <- var.pre.C$coord[3,2]
  x5.pre.C <- var.pre.C$coord[5,1]
  y5.pre.C <- var.pre.C$coord[5,2]
  
  #calc pre angle between Flowering Time and Assimilation
  dot.preN.5 <- x1.pre.C*x5.pre.C + y1.pre.C*y5.pre.C      # dot product between [x1, y1] and [X5, y2]
  det.preN.5 <- x1.pre.C*y5.pre.C - y1.pre.C*x5.pre.C      # determinant
  radian.preN.5 <- atan2(det.preN.5, dot.preN.5)  # atan2(y, x) or atan2(sin, cos)
  angle.preN.5 <- NISTradianTOdeg(radian.preN.5)
  
  #calc pre angle
  dot.preN <- x1.pre.C*x2.pre.C + y1.pre.C*y2.pre.C      # dot product between [x1, y1] and [x2, y2]
  det.preN <- x1.pre.C*y2.pre.C - y1.pre.C*x2.pre.C      # determinant
  radian.preN <- atan2(det.preN, dot.preN)  # atan2(y, x) or atan2(sin, cos)
  angle.preN <- NISTradianTOdeg(radian.preN)
  if(angle.preN<0){angle.preN<-angle.preN+360} #scale in 360 deg counterclockwise
  if(angle.preN.5<0){angle.preN<-360-angle.preN} #scale in 360 deg counterclockwise
  angle.diff.C[i,1]<-angle.preN
  
  # Get peak coordinates
  var.peak.C <- get_pca_var(pc.peak.C) # extract data
  x1.peak.C <- var.peak.C$coord[1,1] # set coordinates
  y1.peak.C <- var.peak.C$coord[1,2]
  x2.peak.C <- var.peak.C$coord[3,1]
  y2.peak.C <- var.peak.C$coord[3,2]
  x5.peak.C <- var.peak.C$coord[5,1]
  y5.peak.C <- var.peak.C$coord[5,2]
  
  #calc peak angle between Flowering Time and Assimilation
  dot.peakN.5 <- x1.peak.C*x5.peak.C + y1.peak.C*y5.peak.C      # dot product between [x1, y1] and [X5, y2]
  det.peakN.5 <- x1.peak.C*y5.peak.C - y1.peak.C*x5.peak.C      # determinant
  radian.peakN.5 <- atan2(det.peakN.5, dot.peakN.5)  # atan2(y, x) or atan2(sin, cos)
  angle.peakN.5 <- NISTradianTOdeg(radian.peakN.5)
  
  
  #calc peak angle
  dot.peakN <- x1.peak.C*x2.peak.C + y1.peak.C*y2.peak.C      # dot product between [x1, y1] and [x2, y2]
  det.peakN <- x1.peak.C*y2.peak.C - y1.peak.C*x2.peak.C      # determinant
  radian.peakN <- atan2(det.peakN, dot.peakN)  # atan2(y, x) or atan2(sin, cos)
  angle.peakN <- NISTradianTOdeg(radian.peakN)
  if(angle.peakN<0){angle.peakN<-angle.peakN+360} #scale in 360 deg counterclockwise
  if(angle.peakN.5<0){angle.peakN<-360-angle.peakN} #Ensure correct directionality with respect to Assimilation
  angle.diff.C[i,2]<-angle.peakN
}

colnames(angle.diff.C) <- c("Pre_Angle", "Peak_Angle")
angle.diff.C <- angle.diff.C %>% mutate(Angle_Difference=Peak_Angle-Pre_Angle)



##########################################
#Angle of actual data

pc.pre <- prcomp(pre.C[,c("Experiment_Date","SLA","Water_Content","Stomatal_Conductance","Assimilation")], scale=T)
pc.peak <- prcomp(peak.C[,c("Experiment_Date","SLA","Water_Content","Stomatal_Conductance","Assimilation")], scale=T)

#biplot(pc.pre, scale=0, col=c("black", "red"))
#biplot(pc.peak, scale=0, col=c("black", "red"))

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
if(angle.pre.5<0){angle.pre<-360-angle.pre} #Ensure correct directionality with respect to Assimilation

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
if(angle.peak.5<0){angle.peak<-360-angle.peak} #Ensure correct directionality with respect to Assimilation

angle.data <- angle.peak - angle.pre
angle.data

#Histogram

hist.C <- ggplot(angle.diff.C, aes(X=Angle_Difference))+
  geom_histogram(aes(Angle_Difference))+
  geom_vline(xintercept=c(angle.data),color="#0000CC")+
  scale_y_continuous(name="Count")+
  scale_x_continuous(name="Angle Difference")+
  theme_classic()
hist.C<- hist.C + theme(
  axis.text.x = element_text(size=12, face="bold"),
  axis.text.y = element_text(size=12,face="bold"),
  axis.title.x = element_text(color="black", size=14, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=14,vjust = 2, face="bold",hjust=0.5))
hist.C # p = 1-(9541/10,000) = 0.0459





####################################################################################
####################################################################################
#Southern Permutation 

# row shuffling
for(i in 1:10000) {
  set.seed(i)
  rows <- sample(nrow(all.S),replace = FALSE,) # randomize row indices
  all.Sshuf <- all.S[rows, ] # shuffle rows
  pre.Sshuf <- all.Sshuf[1:44,] # make random pre dataset
  peak.Sshuf <- all.Sshuf[45:110,] # make random post dataset
  pc.pre.S <- prcomp(pre.Sshuf[,c("Experiment_Date","SLA","Water_Content","Stomatal_Conductance","Assimilation")], scale=T)
  pc.peak.S <- prcomp(peak.Sshuf[,c("Experiment_Date","SLA","Water_Content","Stomatal_Conductance","Assimilation")], scale=T)
  
  
  # Get pre coordinates
  var.pre.S <- get_pca_var(pc.pre.S) # extract data
  x1.pre.S <- var.pre.S$coord[1,1] # set coordinates
  y1.pre.S <- var.pre.S$coord[1,2]
  x2.pre.S <- var.pre.S$coord[3,1]
  y2.pre.S <- var.pre.S$coord[3,2]
  x5.pre.S <- var.pre.S$coord[5,1]
  y5.pre.S <- var.pre.S$coord[5,2]
  
  #calc pre angle between Flowering Time and Assimilation
  dot.preN.5 <- x1.pre.S*x5.pre.S + y1.pre.S*y5.pre.S      # dot product between [x1, y1] and [X5, y2]
  det.preN.5 <- x1.pre.S*y5.pre.S - y1.pre.S*x5.pre.S      # determinant
  radian.preN.5 <- atan2(det.preN.5, dot.preN.5)  # atan2(y, x) or atan2(sin, cos)
  angle.preN.5 <- NISTradianTOdeg(radian.preN.5)
  
  #calc pre angle
  dot.preN <- x1.pre.S*x2.pre.S + y1.pre.S*y2.pre.S      # dot product between [x1, y1] and [x2, y2]
  det.preN <- x1.pre.S*y2.pre.S - y1.pre.S*x2.pre.S      # determinant
  radian.preN <- atan2(det.preN, dot.preN)  # atan2(y, x) or atan2(sin, cos)
  angle.preN <- NISTradianTOdeg(radian.preN)
  if(angle.preN<0){angle.preN<-angle.preN+360} #scale in 360 deg counterclockwise
  if(angle.preN.5<0){angle.preN<-360-angle.preN} #scale in 360 deg counterclockwise
  angle.diff.S[i,1]<-angle.preN
  
  # Get peak coordinates
  var.peak.S <- get_pca_var(pc.peak.S) # extract data
  x1.peak.S <- var.peak.S$coord[1,1] # set coordinates
  y1.peak.S <- var.peak.S$coord[1,2]
  x2.peak.S <- var.peak.S$coord[3,1]
  y2.peak.S <- var.peak.S$coord[3,2]
  x5.peak.S <- var.peak.S$coord[5,1]
  y5.peak.S <- var.peak.S$coord[5,2]
  
  #calc peak angle between Flowering Time and Assimilation
  dot.peakN.5 <- x1.peak.S*x5.peak.S + y1.peak.S*y5.peak.S      # dot product between [x1, y1] and [X5, y2]
  det.peakN.5 <- x1.peak.S*y5.peak.S - y1.peak.S*x5.peak.S      # determinant
  radian.peakN.5 <- atan2(det.peakN.5, dot.peakN.5)  # atan2(y, x) or atan2(sin, cos)
  angle.peakN.5 <- NISTradianTOdeg(radian.peakN.5)
  
  
  #calc peak angle
  dot.peakN <- x1.peak.S*x2.peak.S + y1.peak.S*y2.peak.S      # dot product between [x1, y1] and [x2, y2]
  det.peakN <- x1.peak.S*y2.peak.S - y1.peak.S*x2.peak.S      # determinant
  radian.peakN <- atan2(det.peakN, dot.peakN)  # atan2(y, x) or atan2(sin, cos)
  angle.peakN <- NISTradianTOdeg(radian.peakN)
  if(angle.peakN<0){angle.peakN<-angle.peakN+360} #scale in 360 deg counterclockwise
  if(angle.peakN.5<0){angle.peakN<-360-angle.peakN} #Ensure correct directionality with respect to Assimilation
  angle.diff.S[i,2]<-angle.peakN
}

colnames(angle.diff.S) <- c("Pre_Angle", "Peak_Angle")
angle.diff.S <- angle.diff.S %>% mutate(Angle_Difference=Peak_Angle-Pre_Angle)



##########################################
#Angle of actual data

pc.pre <- prcomp(pre.S[,c("Experiment_Date","SLA","Water_Content","Stomatal_Conductance","Assimilation")], scale=T)
pc.peak <- prcomp(peak.S[,c("Experiment_Date","SLA","Water_Content","Stomatal_Conductance","Assimilation")], scale=T)

#biplot(pc.pre, scale=0, col=c("black", "red"))
#biplot(pc.peak, scale=0, col=c("black", "red"))

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
if(angle.pre.5<0){angle.pre<-360-angle.pre} #Ensure correct directionality with respect to Assimilation

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
if(angle.peak.5<0){angle.peak<-360-angle.peak} #Ensure correct directionality with respect to Assimilation

angle.data <- angle.peak - angle.pre
angle.data

#Histogram

hist.S <- ggplot(angle.diff.S, aes(X=Angle_Difference))+
  geom_histogram(aes(Angle_Difference))+
  geom_vline(xintercept=c(angle.data),color="#0000CC")+
  scale_y_continuous(name="Count")+
  scale_x_continuous(name="Angle Difference")+
  theme_classic()
hist.S<- hist.S + theme(
  axis.text.x = element_text(size=12, face="bold"),
  axis.text.y = element_text(size=12,face="bold"),
  axis.title.x = element_text(color="black", size=14, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=14,vjust = 2, face="bold",hjust=0.5))
hist.S # p = 1-(6900/10,000) = 0.31

############### stitch together in cowplot ####################################
#call each pdf figure 
#export at 8 X 4 inches
plot_grid(hist.N,hist.C,hist.S,ncol = 1)




