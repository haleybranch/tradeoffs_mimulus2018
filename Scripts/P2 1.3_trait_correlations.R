# Trait correlations with the raw data, comparing pre and post drought years 

#read in packages 
library(tidyverse)
library(Hmisc)

pre <- read.csv("Data/pre.csv", header = T)
peak <- read.csv("Data/peak.csv", header = T)

#Remove unnecessary columns 
pre <- select(pre, Experiment_Date, SLA, Water_Content, Assimilation, Stomatal_Conductance)
peak <-select(peak, Experiment_Date, SLA, Water_Content, Assimilation, Stomatal_Conductance)
#change format to matrix
pre <- as.matrix(pre)
peak <- as.matrix(peak)
#running matrix correlation 
pre_r <- rcorr(pre)
peak_r <- rcorr(peak)

#Save as dataframe
pre_corr<-pre_r$r
peak_corr<-peak_r$r
pre_p<-pre_r$P
peak_p<-peak_r$P

#Write Table
#write.csv(pre_corr, "Tables/pre_r.csv")
#write.csv(pre_p, "Tables/pre_p.csv")

#write.csv(peak_corr, "Tables/peak_r.csv")
#write.csv(peak_p, "Tables/peak_p.csv")





