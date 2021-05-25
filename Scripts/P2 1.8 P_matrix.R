# P matrix analyses
library(tidyverse)
library(vegan)
library(devtools)
library("factoextra")
library(finalfit)
library(dplyr)
library(pander)
library(cowplot)
library(ape)


# Read in pre and peak datasets 
pre.N <- read.csv("Data/pre.N.csv")
pre.C <- read.csv("Data/pre.C.csv")
pre.S <- read.csv("Data/pre.S.csv")
peak.N <- read.csv("Data/peak.N.csv")
peak.C <- read.csv("Data/peak.C.csv")
peak.S <- read.csv("Data/peak.S.csv")
all.N <- rbind(pre.N, peak.N)
all.C <- rbind(pre.C, peak.C)
all.S <- rbind(pre.S, peak.S)

#Generate matrixes Pre North
M_pre.N <- data.frame() 


#################### PCA of PRE NORTH data ######################

pca_pre.N <- prcomp(na.omit(pre.N[,c("Experiment_Date","SLA","Water_Content",
                                     "Stomatal_Conductance","Assimilation")]), scale=T)
ind_pre.N <- get_pca_ind(pca_pre.N) # get individual data
coor_pre.N <- ind_pre.N$coord # get coordinates

#Column 1
M_pre.N[1,1] <- var(coor_pre.N[,1])
M_pre.N[2,1] <- cov(coor_pre.N[,2], coor_pre.N[,1])
M_pre.N[3,1] <- cov(coor_pre.N[,3], coor_pre.N[,1])
M_pre.N[4,1] <- cov(coor_pre.N[,4], coor_pre.N[,1])
M_pre.N[5,1] <- cov(coor_pre.N[,5], coor_pre.N[,1])

#Column 2
M_pre.N[1,2] <- cov(coor_pre.N[,1], coor_pre.N[,2])
M_pre.N[2,2] <- var(coor_pre.N[,2])
M_pre.N[3,2] <- cov(coor_pre.N[,3], coor_pre.N[,2])
M_pre.N[4,2] <- cov(coor_pre.N[,4], coor_pre.N[,2])
M_pre.N[5,2] <- cov(coor_pre.N[,5], coor_pre.N[,2])

#Column 3
M_pre.N[1,3] <- cov(coor_pre.N[,1], coor_pre.N[,3])
M_pre.N[2,3] <- cov(coor_pre.N[,2], coor_pre.N[,3])
M_pre.N[3,3] <- var(coor_pre.N[,3])
M_pre.N[4,3] <- cov(coor_pre.N[,4], coor_pre.N[,3])
M_pre.N[5,3] <- cov(coor_pre.N[,5], coor_pre.N[,3])

#Column 4
M_pre.N[1,4] <- cov(coor_pre.N[,1], coor_pre.N[,4])
M_pre.N[2,4] <- cov(coor_pre.N[,2], coor_pre.N[,4])
M_pre.N[3,4] <- cov(coor_pre.N[,3], coor_pre.N[,4])
M_pre.N[4,4] <- var(coor_pre.N[,4])
M_pre.N[5,4] <- cov(coor_pre.N[,5], coor_pre.N[,4])

#Column 5
M_pre.N[1,5] <- cov(coor_pre.N[,1], coor_pre.N[,5])
M_pre.N[2,5] <- cov(coor_pre.N[,2], coor_pre.N[,5])
M_pre.N[3,5] <- cov(coor_pre.N[,3], coor_pre.N[,5])
M_pre.N[4,5] <- cov(coor_pre.N[,4], coor_pre.N[,5])
M_pre.N[5,5] <- var(coor_pre.N[,5])

#Generate matrixes Peak North
M_peak.N <- data.frame() 


#################### PCA of Peak NORTH data ######################

pca_peak.N <- prcomp(na.omit(peak.N[,c("Experiment_Date","SLA","Water_Content",
                                     "Stomatal_Conductance","Assimilation")]), scale=T)
ind_peak.N <- get_pca_ind(pca_peak.N) # get individual data
coor_peak.N <- ind_peak.N$coord # get coordinates

#Column 1
M_peak.N[1,1] <- var(coor_peak.N[,1])
M_peak.N[2,1] <- cov(coor_peak.N[,2], coor_peak.N[,1])
M_peak.N[3,1] <- cov(coor_peak.N[,3], coor_peak.N[,1])
M_peak.N[4,1] <- cov(coor_peak.N[,4], coor_peak.N[,1])
M_peak.N[5,1] <- cov(coor_peak.N[,5], coor_peak.N[,1])

#Column 2
M_peak.N[1,2] <- cov(coor_peak.N[,1], coor_peak.N[,2])
M_peak.N[2,2] <- var(coor_peak.N[,2])
M_peak.N[3,2] <- cov(coor_peak.N[,3], coor_peak.N[,2])
M_peak.N[4,2] <- cov(coor_peak.N[,4], coor_peak.N[,2])
M_peak.N[5,2] <- cov(coor_peak.N[,5], coor_peak.N[,2])

#Column 3
M_peak.N[1,3] <- cov(coor_peak.N[,1], coor_peak.N[,3])
M_peak.N[2,3] <- cov(coor_peak.N[,2], coor_peak.N[,3])
M_peak.N[3,3] <- var(coor_peak.N[,3])
M_peak.N[4,3] <- cov(coor_peak.N[,4], coor_peak.N[,3])
M_peak.N[5,3] <- cov(coor_peak.N[,5], coor_peak.N[,3])

#Column 4
M_peak.N[1,4] <- cov(coor_peak.N[,1], coor_peak.N[,4])
M_peak.N[2,4] <- cov(coor_peak.N[,2], coor_peak.N[,4])
M_peak.N[3,4] <- cov(coor_peak.N[,3], coor_peak.N[,4])
M_peak.N[4,4] <- var(coor_peak.N[,4])
M_peak.N[5,4] <- cov(coor_peak.N[,5], coor_peak.N[,4])

#Column 5
M_peak.N[1,5] <- cov(coor_peak.N[,1], coor_peak.N[,5])
M_peak.N[2,5] <- cov(coor_peak.N[,2], coor_peak.N[,5])
M_peak.N[3,5] <- cov(coor_peak.N[,3], coor_peak.N[,5])
M_peak.N[4,5] <- cov(coor_peak.N[,4], coor_peak.N[,5])
M_peak.N[5,5] <- var(coor_peak.N[,5])


#Generate matrixes Pre Centre
M_pre.C <- data.frame() 


#################### PCA of PRE CENTRE data ######################

pca_pre.C <- prcomp(na.omit(pre.C[,c("Experiment_Date","SLA","Water_Content",
                                     "Stomatal_Conductance","Assimilation")]), scale=T)
ind_pre.C <- get_pca_ind(pca_pre.C) # get individual data
coor_pre.C <- ind_pre.C$coord # get coordinates

#Column 1
M_pre.C[1,1] <- var(coor_pre.C[,1])
M_pre.C[2,1] <- cov(coor_pre.C[,2], coor_pre.C[,1])
M_pre.C[3,1] <- cov(coor_pre.C[,3], coor_pre.C[,1])
M_pre.C[4,1] <- cov(coor_pre.C[,4], coor_pre.C[,1])
M_pre.C[5,1] <- cov(coor_pre.C[,5], coor_pre.C[,1])

#Column 2
M_pre.C[1,2] <- cov(coor_pre.C[,1], coor_pre.C[,2])
M_pre.C[2,2] <- var(coor_pre.C[,2])
M_pre.C[3,2] <- cov(coor_pre.C[,3], coor_pre.C[,2])
M_pre.C[4,2] <- cov(coor_pre.C[,4], coor_pre.C[,2])
M_pre.C[5,2] <- cov(coor_pre.C[,5], coor_pre.C[,2])

#Column 3
M_pre.C[1,3] <- cov(coor_pre.C[,1], coor_pre.C[,3])
M_pre.C[2,3] <- cov(coor_pre.C[,2], coor_pre.C[,3])
M_pre.C[3,3] <- var(coor_pre.C[,3])
M_pre.C[4,3] <- cov(coor_pre.C[,4], coor_pre.C[,3])
M_pre.C[5,3] <- cov(coor_pre.C[,5], coor_pre.C[,3])

#Column 4
M_pre.C[1,4] <- cov(coor_pre.C[,1], coor_pre.C[,4])
M_pre.C[2,4] <- cov(coor_pre.C[,2], coor_pre.C[,4])
M_pre.C[3,4] <- cov(coor_pre.C[,3], coor_pre.C[,4])
M_pre.C[4,4] <- var(coor_pre.C[,4])
M_pre.C[5,4] <- cov(coor_pre.C[,5], coor_pre.C[,4])

#Column 5
M_pre.C[1,5] <- cov(coor_pre.C[,1], coor_pre.C[,5])
M_pre.C[2,5] <- cov(coor_pre.C[,2], coor_pre.C[,5])
M_pre.C[3,5] <- cov(coor_pre.C[,3], coor_pre.C[,5])
M_pre.C[4,5] <- cov(coor_pre.C[,4], coor_pre.C[,5])
M_pre.C[5,5] <- var(coor_pre.C[,5])

#Generate matrixes Peak Centre
M_peak.C <- data.frame() 


#################### PCA of Peak Centre data ######################

pca_peak.C <- prcomp(na.omit(peak.C[,c("Experiment_Date","SLA","Water_Content",
                                       "Stomatal_Conductance","Assimilation")]), scale=T)
ind_peak.C <- get_pca_ind(pca_peak.C) # get individual data
coor_peak.C <- ind_peak.C$coord # get coordinates

#Column 1
M_peak.C[1,1] <- var(coor_peak.C[,1])
M_peak.C[2,1] <- cov(coor_peak.C[,2], coor_peak.C[,1])
M_peak.C[3,1] <- cov(coor_peak.C[,3], coor_peak.C[,1])
M_peak.C[4,1] <- cov(coor_peak.C[,4], coor_peak.C[,1])
M_peak.C[5,1] <- cov(coor_peak.C[,5], coor_peak.C[,1])

#Column 2
M_peak.C[1,2] <- cov(coor_peak.C[,1], coor_peak.C[,2])
M_peak.C[2,2] <- var(coor_peak.C[,2])
M_peak.C[3,2] <- cov(coor_peak.C[,3], coor_peak.C[,2])
M_peak.C[4,2] <- cov(coor_peak.C[,4], coor_peak.C[,2])
M_peak.C[5,2] <- cov(coor_peak.C[,5], coor_peak.C[,2])

#Column 3
M_peak.C[1,3] <- cov(coor_peak.C[,1], coor_peak.C[,3])
M_peak.C[2,3] <- cov(coor_peak.C[,2], coor_peak.C[,3])
M_peak.C[3,3] <- var(coor_peak.C[,3])
M_peak.C[4,3] <- cov(coor_peak.C[,4], coor_peak.C[,3])
M_peak.C[5,3] <- cov(coor_peak.C[,5], coor_peak.C[,3])

#Column 4
M_peak.C[1,4] <- cov(coor_peak.C[,1], coor_peak.C[,4])
M_peak.C[2,4] <- cov(coor_peak.C[,2], coor_peak.C[,4])
M_peak.C[3,4] <- cov(coor_peak.C[,3], coor_peak.C[,4])
M_peak.C[4,4] <- var(coor_peak.C[,4])
M_peak.C[5,4] <- cov(coor_peak.C[,5], coor_peak.C[,4])

#Column 5
M_peak.C[1,5] <- cov(coor_peak.C[,1], coor_peak.C[,5])
M_peak.C[2,5] <- cov(coor_peak.C[,2], coor_peak.C[,5])
M_peak.C[3,5] <- cov(coor_peak.C[,3], coor_peak.C[,5])
M_peak.C[4,5] <- cov(coor_peak.C[,4], coor_peak.C[,5])
M_peak.C[5,5] <- var(coor_peak.C[,5])


#Generate matrixes Pre South
M_pre.S <- data.frame() 

mantel.test(M_pre.N, M_peak.N, nperm = 10000, graph = FALSE,
            alternative = "two.sided",  ...)
#################### PCA of PRE SOUTH data ######################

pca_pre.S <- prcomp(na.omit(pre.S[,c("Experiment_Date","SLA","Water_Content",
                                     "Stomatal_Conductance","Assimilation")]), scale=T)
ind_pre.S <- get_pca_ind(pca_pre.S) # get individual data
coor_pre.S <- ind_pre.S$coord # get coordinates

#Column 1
M_pre.S[1,1] <- var(coor_pre.S[,1])
M_pre.S[2,1] <- cov(coor_pre.S[,2], coor_pre.S[,1])
M_pre.S[3,1] <- cov(coor_pre.S[,3], coor_pre.S[,1])
M_pre.S[4,1] <- cov(coor_pre.S[,4], coor_pre.S[,1])
M_pre.S[5,1] <- cov(coor_pre.S[,5], coor_pre.S[,1])

#Column 2
M_pre.S[1,2] <- cov(coor_pre.S[,1], coor_pre.S[,2])
M_pre.S[2,2] <- var(coor_pre.S[,2])
M_pre.S[3,2] <- cov(coor_pre.S[,3], coor_pre.S[,2])
M_pre.S[4,2] <- cov(coor_pre.S[,4], coor_pre.S[,2])
M_pre.S[5,2] <- cov(coor_pre.S[,5], coor_pre.S[,2])

#Column 3
M_pre.S[1,3] <- cov(coor_pre.S[,1], coor_pre.S[,3])
M_pre.S[2,3] <- cov(coor_pre.S[,2], coor_pre.S[,3])
M_pre.S[3,3] <- var(coor_pre.S[,3])
M_pre.S[4,3] <- cov(coor_pre.S[,4], coor_pre.S[,3])
M_pre.S[5,3] <- cov(coor_pre.S[,5], coor_pre.S[,3])

#Column 4
M_pre.S[1,4] <- cov(coor_pre.S[,1], coor_pre.S[,4])
M_pre.S[2,4] <- cov(coor_pre.S[,2], coor_pre.S[,4])
M_pre.S[3,4] <- cov(coor_pre.S[,3], coor_pre.S[,4])
M_pre.S[4,4] <- var(coor_pre.S[,4])
M_pre.S[5,4] <- cov(coor_pre.S[,5], coor_pre.S[,4])

#Column 5
M_pre.S[1,5] <- cov(coor_pre.S[,1], coor_pre.S[,5])
M_pre.S[2,5] <- cov(coor_pre.S[,2], coor_pre.S[,5])
M_pre.S[3,5] <- cov(coor_pre.S[,3], coor_pre.S[,5])
M_pre.S[4,5] <- cov(coor_pre.S[,4], coor_pre.S[,5])
M_pre.S[5,5] <- var(coor_pre.S[,5])

#Generate matrixes Peak North
M_peak.S <- data.frame() 


#################### PCA of Peak SOUTH data ######################

pca_peak.S <- prcomp(na.omit(peak.S[,c("Experiment_Date","SLA","Water_Content",
                                       "Stomatal_Conductance","Assimilation")]), scale=T)
ind_peak.S <- get_pca_ind(pca_peak.S) # get individual data
coor_peak.S <- ind_peak.S$coord # get coordinates

#Column 1
M_peak.S[1,1] <- var(coor_peak.S[,1])
M_peak.S[2,1] <- cov(coor_peak.S[,2], coor_peak.S[,1])
M_peak.S[3,1] <- cov(coor_peak.S[,3], coor_peak.S[,1])
M_peak.S[4,1] <- cov(coor_peak.S[,4], coor_peak.S[,1])
M_peak.S[5,1] <- cov(coor_peak.S[,5], coor_peak.S[,1])

#Column 2
M_peak.S[1,2] <- cov(coor_peak.S[,1], coor_peak.S[,2])
M_peak.S[2,2] <- var(coor_peak.S[,2])
M_peak.S[3,2] <- cov(coor_peak.S[,3], coor_peak.S[,2])
M_peak.S[4,2] <- cov(coor_peak.S[,4], coor_peak.S[,2])
M_peak.S[5,2] <- cov(coor_peak.S[,5], coor_peak.S[,2])

#Column 3
M_peak.S[1,3] <- cov(coor_peak.S[,1], coor_peak.S[,3])
M_peak.S[2,3] <- cov(coor_peak.S[,2], coor_peak.S[,3])
M_peak.S[3,3] <- var(coor_peak.S[,3])
M_peak.S[4,3] <- cov(coor_peak.S[,4], coor_peak.S[,3])
M_peak.S[5,3] <- cov(coor_peak.S[,5], coor_peak.S[,3])

#Column 4
M_peak.S[1,4] <- cov(coor_peak.S[,1], coor_peak.S[,4])
M_peak.S[2,4] <- cov(coor_peak.S[,2], coor_peak.S[,4])
M_peak.S[3,4] <- cov(coor_peak.S[,3], coor_peak.S[,4])
M_peak.S[4,4] <- var(coor_peak.S[,4])
M_peak.S[5,4] <- cov(coor_peak.S[,5], coor_peak.S[,4])

#Column 5
M_peak.S[1,5] <- cov(coor_peak.S[,1], coor_peak.S[,5])
M_peak.S[2,5] <- cov(coor_peak.S[,2], coor_peak.S[,5])
M_peak.S[3,5] <- cov(coor_peak.S[,3], coor_peak.S[,5])
M_peak.S[4,5] <- cov(coor_peak.S[,4], coor_peak.S[,5])
M_peak.S[5,5] <- var(coor_peak.S[,5])

mantel(M_pre.N, M_peak.N, method="spearman", permutations=10000)
mantel(M_pre.C, M_peak.C, method="spearman", permutations=10000, na.rm=TRUE)
mantel(M_pre.S, M_peak.S, method="spearman", permutations=10000, na.rm=TRUE)
mantel.test(M_pre.N, M_peak.N, nperm = 10000, graph = TRUE,
            alternative = "two.sided")

bartlett.test(M_pre.N, M_peak.N)
bartlett.test(M_pre.C, M_peak.C)
bartlett.test(M_pre.S, M_peak.S)
library(ade4)











