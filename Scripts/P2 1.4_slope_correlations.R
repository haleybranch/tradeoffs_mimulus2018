# Trait correlations with the raw data, comparing pre and post drought years 

#read in packages 
library(tidyverse)
library(Hmisc)


slopes_cor <- read.csv("Data/slopesAll.csv")
#slopes_cor$Block <- as.factor(slopes_cor$Block) ; slopes_cor$Family <- as.factor(slopes_cor$Family) # prep factors



#Remove unnecessary columns 
slopes_all <- select(slopes_cor, FloweringDate, SLA, WaterContent, Assimilation, Stomatal.Conductance)
#change format to matrix
slopes_all <- as.matrix(slopes_all)

#running matrix correlation 
all_corr <- rcorr(slopes_all)

#Save as dataframe
all_corr_r <- all_corr$r ; all_corr_p <- all_corr$P

#write.csv(all_corr_r, "Tables/slopes_corr_r.csv")
#write.csv(all_corr_p, "Tables/slopes_corr_p.csv")
r <- read.csv("Tables/slopes_corr_r.csv")
p <- read.csv("Tables/slopes_corr_p.csv")

