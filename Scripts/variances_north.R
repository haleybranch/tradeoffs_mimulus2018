##########################################################################################################
## Difference Variance of residual of regression lines 
## Author Haley Branch & Daniel Anstett
## Example using north only
##
## Last Modified May 23, 2021
##########################################################################################################
#read in packages
library(tidyverse)
library(ggplot2)
library(ggforce)
library(car)
library(cowplot)
library(dplyr)
##########################################################################################################
##########################################################################################################
# Read in data, all
all <- read.csv("Data/all.csv")
all <- mutate_if(all, is.character, stringr::str_replace_all, pattern = "Pre", replacement = "1")
all <- mutate_if(all, is.character, stringr::str_replace_all, pattern = "Peak", replacement = "2")
all$Period <- as.factor(all$Period)

#Select Region
North <- all %>% filter(Region == "1.North") #subset of data
Centre <- all %>% filter(Region == "2.Center") #subset of data
South <- all %>% filter(Region == "3.South") #subset of data

#Separate periods
df1_N <- filter(North, Period=="1")
df2_N <- filter(North, Period=="2")
df1_C <- filter(Centre, Period=="1")
df2_C <- filter(Centre, Period=="2")
df1_S <- filter(South, Period=="1")
df2_S <- filter(South, Period=="2")
df1_A <- filter(all, Period=="1")
df2_A <- filter(all, Period=="2")

##########################################################################################################
#Example stdev difference calc without loop

# generate models for DF * SLA
mod1 <- lm(df1_N$Experiment_Date~df1_N$SLA)
mod2 <- lm(df2_N$Experiment_Date~df2_N$SLA)

#Get residuals
mod1_res <- residuals(mod1)
mod2_res <- residuals(mod2)

#calc stdev
stdev_1 <- sd(mod1_res)
stdev_2 <- sd(mod2_res)
stdev_diff <- stdev_2 - stdev_1

# generate permutation dataframe
perm.df.sla <- data.frame()
for(i in 1:10000) {
  set.seed(i)
  rows <- sample(nrow(North),replace = FALSE,) # randomize row indices
  all.Nshuf <- North[rows, ] # shuffle rows
  df.pre.N <- all.Nshuf[1:102,] # make random pre dataset
  df.peak.N <- all.Nshuf[103:208,] # make random post dataset
  
  # generate models for DF * SLA
  df.mod.pre <- lm(df.pre.N$Experiment_Date~df.pre.N$SLA)
  df.mod.peak <- lm(df.peak.N$Experiment_Date~df.peak.N$SLA)
  
  #Get residuals
  df_res_1 <- residuals(df.mod.pre)
  df_res_2 <- residuals(df.mod.peak)
  
  #calc stdev
  stdev_1_df <- sd(df_res_1)
  stdev_2_df <- sd(df_res_2)
  stdev_diff <- stdev_2_df - stdev_1_df
  
  perm.df.sla[i,1] <- stdev_diff
}

# plot 
hist.DF.SLA <- ggplot(perm.df.sla, aes(X=V1))+
  geom_histogram(aes(V1))+
  geom_vline(xintercept=c(stdev_diff),color="#0000CC")+
  scale_y_continuous(name="Count")+
  scale_x_continuous(name="Stdev Difference")+
  theme_classic()
hist.DF.SLA <- hist.DF.SLA  + theme(
  axis.text.x = element_text(size=12, face="bold"),
  axis.text.y = element_text(size=12,face="bold"),
  axis.title.x = element_text(color="black", size=14, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=14,vjust = 2, face="bold",hjust=0.5))
hist1.N <- hist.DF.SLA  # not significant 

# find percentile rank for angle 
ex_l <- length(perm.df.sla[perm.df.sla >= stdev_diff])
per_pval2 <- ex_l/dim(perm.df.sla)[1] 
per_pval2
# not significant
