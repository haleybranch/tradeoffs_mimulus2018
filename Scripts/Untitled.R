# Regression lines for North

#read in packages
library(tidyverse)
library(ggplot2)
library(ggforce)
library(car)
library(cowplot)
library(dplyr)
library(NISTunits)

# Read in data
all <- read.csv("Data/all.csv")
#place <- read.csv("Data/placements.csv")
North <- all %>% filter(Region == "1.North") #subset of data
North <- mutate_if(North, is.character, stringr::str_replace_all, pattern = "Pre", replacement = "1")
North <- mutate_if(North, is.character, stringr::str_replace_all, pattern = "Peak", replacement = "2")
North$Period <- as.factor(North$Period)

df1 <- filter(North, Period=="1")

df2 <- filter(North, Period=="2")

# generate models for DF * SLA
mod1 <- lm(df1$Experiment_Date~df1$SLA)
summary(mod1)
mod2 <- lm(df2$Experiment_Date~df2$SLA)
summary(mod2)

# 
# angle between the lines
l <- (mod2$coefficients[2]-mod1$coefficients[2])/1-(mod2$coefficients[2]*mod1$coefficients[2])
angle.l <- atan(l)
angle.l <- NISTradianTOdeg(angle.l)
angle.l

# shuffle the data into a new dataframe
perm.df.sla <- data.frame()

# row shuffling
for(i in 1:10000) {
  set.seed(i)
  rows <- sample(nrow(North),replace = FALSE,) # randomize row indices
  all.Nshuf <- North[rows, ] # shuffle rows
  df.pre.N <- all.Nshuf[1:102,] # make random pre dataset
  df.peak.N <- all.Nshuf[103:208,] # make random post dataset
  
  # generate models for DF * SLA
  df.mod.pre <- lm(df.pre.N$Experiment_Date~df.pre.N$SLA)
  df.mod.peak <- lm(df.peak.N$Experiment_Date~df.peak.N$SLA)
  
  # angle between the lines
  angle.df <- (df.mod.peak$coefficients[2]-df.mod.pre$coefficients[2])/1-(df.mod.peak$coefficients[2]*df.mod.pre$coefficients[2])
  angle.df.tan <- atan(angle.df)
  angle.df.true <- NISTradianTOdeg(angle.df.tan)
  
  perm.df.sla[i,1] <- angle.df.true
}

# plot 
hist.DF.SLA <- ggplot(perm.df.sla, aes(X=V1))+
  geom_histogram(aes(V1))+
  geom_vline(xintercept=c(angle.l),color="#0000CC")+
  scale_y_continuous(name="Count")+
  scale_x_continuous(name="Angle")+
  theme_classic()
hist.DF.SLA <- hist.DF.SLA  + theme(
  axis.text.x = element_text(size=12, face="bold"),
  axis.text.y = element_text(size=12,face="bold"),
  axis.title.x = element_text(color="black", size=14, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=14,vjust = 2, face="bold",hjust=0.5))
hist1.N <- hist.DF.SLA  # not significant 

# find percentile rank for angle 
ex_l <- length(perm.df.sla[perm.df.sla >= angle.l])
per_pval2 <- ex_l/dim(perm.df.sla)[1] 
per_pval2
# not significant p = 0.1799