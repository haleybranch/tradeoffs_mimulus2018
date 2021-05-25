##########################################################################################################
## Regression lines pre peak bi-variate comparisons 
## Author Haley Branch & Daniel Anstett
## Using functions. Across regions
##
## Last Modified May 17, 2021
##########################################################################################################
#read in packages
library(tidyverse)
library(ggplot2)
library(ggforce)
library(car)
library(cowplot)
library(dplyr)
library(NISTunits)
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

#Input dataframe for p-values
pvalue_angle<- read.csv("Data/pvalue_angle_setup.csv", header=TRUE)

##########################################################################################################
#Set up functions
######################################################
#1.angle between two lines, calculate "real_angle"
angle_lines <- function(var1,var2, var3, var4) {
  # generate models for var1 ~ var2
  mod1 <- lm(var1~var2,data=df1_N) 
  mod2 <- lm(var3~var4,data=df2_N)
  # angle between the lines
  l <- (mod2$coefficients[2]-mod1$coefficients[2])/(1-(mod2$coefficients[2]*mod1$coefficients[2]))
  l <- unname(l)
  angle.l <- atan(l)
  angle.l <- NISTradianTOdeg(angle.l)
  return(angle.l)
}
#e.g.
#real_angle_df_sla <- angle_lines(df1_N$Experiment_Date,df1_N$SLA,
#                                df2_N$Experiment_Date,df2_N$SLA) #this is "real_angle"
######################################################
#2.permutation, calculate "angle_df"
permute_angle <- function(region,pre_size,peak_size) {
  # shuffle the data into a new dataframe
  perm.shuf <- data.frame()
  # row shuffling
  for(i in 1:10000) {
    set.seed(i)
    rows <- sample(nrow(region),replace = FALSE,) # randomize row indices
    all.shuf <- region[rows, ] # shuffle rows
    pre.shuf <- all.shuf[1:pre_size,] # make random pre dataset
    peak.shuf <- all.shuf[pre_size+1:pre_size+peak_size,] # make random post dataset
    
    # generate models for DF * SLA
    mod.pre <- lm(pre.shuf[,1]~pre.shuf[,2])
    mod.peak <- lm(peak.shuf[,1]~peak.shuf[,2])
    
    # angle between the lines
    angle.shuf <- (mod.peak$coefficients[2]-mod.pre$coefficients[2])/(1-(mod.peak$coefficients[2]*mod.pre$coefficients[2]))
    angle.shuf <- unname(angle.shuf)
    angle.shuf.tan <- atan(angle.shuf)
    angle.shuf.true <- NISTradianTOdeg(angle.shuf.tan)
    
    perm.shuf[i,1] <- angle.shuf.true
  }
  return(perm.shuf)
}
#e.g.
#North_shuf <- North %>% select(Experiment_Date,SLA) # make sure to replace with correct variables
#perm.df.sla <- permute_angle(North_shuf,length(df1_N),length(df2_N)) #this is "angle_df
######################################################
#3. make angle plot
plot_angle <- function(angle_df,real_angle){
  # plot 
  hist.1 <- ggplot(angle_df, aes(X=V1))+
    geom_histogram(aes(V1))+
    geom_vline(xintercept=c(real_angle),color="#0000CC")+
    scale_y_continuous(name="Count")+
    scale_x_continuous(name="Angle")+
    theme_classic()
  hist.1 <- hist.1  + theme(
    axis.text.x = element_text(size=12, face="bold"),
    axis.text.y = element_text(size=12,face="bold"),
    axis.title.x = element_text(color="black", size=14, vjust = 0.5, face="bold"),
    axis.title.y = element_text(color="black", size=14,vjust = 2, face="bold",hjust=0.5))
  return(hist.1)
  }
#e.g.
#plot_angle(perm.df.sla,real_angle_df_sla)
######################################################
#4. find permutation p-value
angle_p <- function(angle_df,real_angle){
  ex_l <- length(angle_df[angle_df >= real_angle])
  per_pval <- ex_l/dim(angle_df)[1] 
  return(per_pval)
}
#e.g. 
#angle_p(perm.df.sla,angle_df_sla)
##########################################################################################################
#highly commented example
#Run for North
#df & SLA, 

#1. Use angle_lines function to find "real_angle"
real_angle_df_sla <- angle_lines(df1_N$Experiment_Date,df1_N$SLA,
                                df2_N$Experiment_Date,df2_N$SLA) #this is "real_angle"

#2A. Filter out only two wanted comarisons varaibles where var2 predicts var1
North_shuf <- North %>% select(Experiment_Date,SLA) # make sure to replace with correct variables
#2B. Use permute_angle function to calculate "angle_df" (permutation angle dataframe)
perm.df.sla <- permute_angle(North_shuf,length(df1_N),length(df2_N)) #this is "angle_df

#3 Use "plot_angle" to produce graph with inputs from #1 and #2B.
plot_angle(perm.df.sla,real_angle_df_sla)

#4 Use angle_p to calculate permutation p-value using inputs from #1 and #2B.
angle_p(perm.df.sla,real_angle_df_sla)

##########################################################################################################
##########################################################################################################
#North
#1 find real angle
real_angle_1.N<- angle_lines(df1_N$Experiment_Date,df1_N$SLA,
                                 df2_N$Experiment_Date,df2_N$SLA)
real_angle_2.N <- angle_lines(df1_N$Experiment_Date,df1_N$Water_Content,
                                 df2_N$Experiment_Date,df2_N$Water_Content)
real_angle_3.N <- angle_lines(df1_N$Experiment_Date,df1_N$Assimilation,
                                 df2_N$Experiment_Date,df2_N$Assimilation)
real_angle_4.N <- angle_lines(df1_N$Experiment_Date,df1_N$Stomatal_Conductance,
                                 df2_N$Experiment_Date,df2_N$Stomatal_Conductance)
real_angle_5.N <- angle_lines(df1_N$SLA,df1_N$Assimilation,
                                 df2_N$SLA,df2_N$Assimilation)

real_angle_6.N <- angle_lines(df1_N$Stomatal_Conductance,df1_N$SLA,
                                 df2_N$Stomatal_Conductance,df2_N$SLA)
real_angle_7.N <- angle_lines(df1_N$Water_Content,df1_N$SLA,
                                 df2_N$Water_Content,df2_N$SLA)
real_angle_8.N <- angle_lines(df1_N$Water_Content,df1_N$Assimilation,
                                 df2_N$Water_Content,df2_N$Assimilation)
real_angle_9.N <- angle_lines(df1_N$Water_Content,df1_N$Stomatal_Conductance,
                                 df2_N$Water_Content,df2_N$Stomatal_Conductance)
real_angle_10.N <- angle_lines(df1_N$Stomatal_Conductance,df1_N$Assimilation,
                                 df2_N$Stomatal_Conductance,df2_N$Assimilation)

#2A. Filter out only two wanted comparisons, where var2 predicts var1
North_shuf_1 <- North %>% select(Experiment_Date,SLA)
North_shuf_2 <- North %>% select(Experiment_Date,Water_Content)
North_shuf_3 <- North %>% select(Experiment_Date,Assimilation)
North_shuf_4 <- North %>% select(Experiment_Date,Stomatal_Conductance)
North_shuf_5 <- North %>% select(SLA,Assimilation)

North_shuf_6 <- North %>% select(Stomatal_Conductance,SLA)
North_shuf_7 <- North %>% select(Water_Content,SLA)
North_shuf_8 <- North %>% select(Water_Content,Assimilation)
North_shuf_9 <- North %>% select(Water_Content,Stomatal_Conductance)
North_shuf_10 <- North %>% select(Stomatal_Conductance,Assimilation)

#2B. Use permute_angle function to calculate "angle_df" (permutation angle dataframe)
perm.1.N <- permute_angle(North_shuf_1,length(df1_N),length(df2_N))
perm.2.N <- permute_angle(North_shuf_2,length(df1_N),length(df2_N))
perm.3.N <- permute_angle(North_shuf_3,length(df1_N),length(df2_N))
perm.4.N <- permute_angle(North_shuf_4,length(df1_N),length(df2_N))
perm.5.N <- permute_angle(North_shuf_5,length(df1_N),length(df2_N))
perm.6.N <- permute_angle(North_shuf_6,length(df1_N),length(df2_N))
perm.7.N <- permute_angle(North_shuf_7,length(df1_N),length(df2_N))
perm.8.N <- permute_angle(North_shuf_8,length(df1_N),length(df2_N))
perm.9.N <- permute_angle(North_shuf_9,length(df1_N),length(df2_N))
perm.10.N <- permute_angle(North_shuf_10,length(df1_N),length(df2_N))

#3 Use "plot_angle" to produce graph with inputs from #1 and #2B.
plot_1.N <- plot_angle(perm.1.N,real_angle_1.N)
plot_2.N <- plot_angle(perm.2.N,real_angle_2.N)
plot_3.N <- plot_angle(perm.3.N,real_angle_3.N)
plot_4.N <- plot_angle(perm.4.N,real_angle_4.N)
plot_5.N <- plot_angle(perm.5.N,real_angle_5.N)
plot_6.N <- plot_angle(perm.6.N,real_angle_6.N)
plot_7.N <- plot_angle(perm.7.N,real_angle_7.N)
plot_8.N <- plot_angle(perm.8.N,real_angle_8.N)
plot_9.N <- plot_angle(perm.9.N,real_angle_9.N)
plot_10.N <- plot_angle(perm.10.N,real_angle_10.N)
## Cowplot export at 10 X 6 inches
plot_grid(plot_1.N,plot_2.N,plot_3.N,plot_4.N,plot_5.N,
          plot_6.N,plot_7.N,plot_8.N,plot_9.N,plot_10.N,ncol = 2)

#4 Use angle_p to calculate permutation p-value using inputs from #1 and #2B.
pvalue_angle[1,4] <- angle_p(perm.1.N,real_angle_1.N)
pvalue_angle[2,4] <- angle_p(perm.2.N,real_angle_2.N)
pvalue_angle[3,4] <- angle_p(perm.3.N,real_angle_3.N)
pvalue_angle[4,4] <- angle_p(perm.4.N,real_angle_4.N)
pvalue_angle[5,4] <- angle_p(perm.5.N,real_angle_5.N)
pvalue_angle[6,4] <- angle_p(perm.6.N,real_angle_6.N)
pvalue_angle[7,4] <- angle_p(perm.7.N,real_angle_7.N)
pvalue_angle[8,4] <- angle_p(perm.8.N,real_angle_8.N)
pvalue_angle[9,4] <- angle_p(perm.9.N,real_angle_9.N)
pvalue_angle[10,4] <- angle_p(perm.10.N,real_angle_10.N)



##########################################################################################################
##########################################################################################################
#Centre
#1 find real angle
real_angle_1.C<- angle_lines(df1_C$Experiment_Date,df1_C$SLA,
                             df2_C$Experiment_Date,df2_C$SLA)
real_angle_2.C <- angle_lines(df1_C$Experiment_Date,df1_C$Water_Content,
                              df2_C$Experiment_Date,df2_C$Water_Content)
real_angle_3.C <- angle_lines(df1_C$Experiment_Date,df1_C$Assimilation,
                              df2_C$Experiment_Date,df2_C$Assimilation)
real_angle_4.C <- angle_lines(df1_C$Experiment_Date,df1_C$Stomatal_Conductance,
                              df2_C$Experiment_Date,df2_C$Stomatal_Conductance)
real_angle_5.C <- angle_lines(df1_C$SLA,df1_C$Assimilation,
                              df2_C$SLA,df2_C$Assimilation)

real_angle_6.C <- angle_lines(df1_C$Stomatal_Conductance,df1_C$SLA,
                              df2_C$Stomatal_Conductance,df2_C$SLA)
real_angle_7.C <- angle_lines(df1_C$Water_Content,df1_C$SLA,
                              df2_C$Water_Content,df2_C$SLA)
real_angle_8.C <- angle_lines(df1_C$Water_Content,df1_C$Assimilation,
                              df2_C$Water_Content,df2_C$Assimilation)
real_angle_9.C <- angle_lines(df1_C$Water_Content,df1_C$Stomatal_Conductance,
                              df2_C$Water_Content,df2_C$Stomatal_Conductance)
real_angle_10.C <- angle_lines(df1_C$Stomatal_Conductance,df1_C$Assimilation,
                               df2_C$Stomatal_Conductance,df2_C$Assimilation)

#2A. Filter out only two wanted comparisons, where var2 predicts var1
Centre_shuf_1 <- Centre %>% select(Experiment_Date,SLA)
Centre_shuf_2 <- Centre %>% select(Experiment_Date,Water_Content)
Centre_shuf_3 <- Centre %>% select(Experiment_Date,Assimilation)
Centre_shuf_4 <- Centre %>% select(Experiment_Date,Stomatal_Conductance)
Centre_shuf_5 <- Centre %>% select(SLA,Assimilation)

Centre_shuf_6 <- Centre %>% select(Stomatal_Conductance,SLA)
Centre_shuf_7 <- Centre %>% select(Water_Content,SLA)
Centre_shuf_8 <- Centre %>% select(Water_Content,Assimilation)
Centre_shuf_9 <- Centre %>% select(Water_Content,Stomatal_Conductance)
Centre_shuf_10 <- Centre %>% select(Stomatal_Conductance,Assimilation)

#2B. Use permute_angle function to calculate "angle_df" (permutation angle dataframe)
perm.1.C <- permute_angle(Centre_shuf_1,length(df1_C),length(df2_C))
perm.2.C <- permute_angle(Centre_shuf_2,length(df1_C),length(df2_C))
perm.3.C <- permute_angle(Centre_shuf_3,length(df1_C),length(df2_C))
perm.4.C <- permute_angle(Centre_shuf_4,length(df1_C),length(df2_C))
perm.5.C <- permute_angle(Centre_shuf_5,length(df1_C),length(df2_C))
perm.6.C <- permute_angle(Centre_shuf_6,length(df1_C),length(df2_C))
perm.7.C <- permute_angle(Centre_shuf_7,length(df1_C),length(df2_C))
perm.8.C <- permute_angle(Centre_shuf_8,length(df1_C),length(df2_C))
perm.9.C <- permute_angle(Centre_shuf_9,length(df1_C),length(df2_C))
perm.10.C <- permute_angle(Centre_shuf_10,length(df1_C),length(df2_C))

#3 Use "plot_angle" to produce graph with inputs from #1 and #2B.
plot_1.C <- plot_angle(perm.1.C,real_angle_1.C)
plot_2.C <- plot_angle(perm.2.C,real_angle_2.C)
plot_3.C <- plot_angle(perm.3.C,real_angle_3.C)
plot_4.C <- plot_angle(perm.4.C,real_angle_4.C)
plot_5.C <- plot_angle(perm.5.C,real_angle_5.C)
plot_6.C <- plot_angle(perm.6.C,real_angle_6.C)
plot_7.C <- plot_angle(perm.7.C,real_angle_7.C)
plot_8.C <- plot_angle(perm.8.C,real_angle_8.C)
plot_9.C <- plot_angle(perm.9.C,real_angle_9.C)
plot_10.C <- plot_angle(perm.10.C,real_angle_10.C)
## Cowplot export at 10 X 6 inches
plot_grid(plot_1.C,plot_2.C,plot_3.C,plot_4.C,plot_5.C,
          plot_6.C,plot_7.C,plot_8.C,plot_9.C,plot_10.C,ncol = 2)

#4 Use angle_p to calculate permutation p-value using inputs from #1 and #2B.
pvalue_angle[1,5] <- angle_p(perm.1.C,real_angle_1.C)
pvalue_angle[2,5] <- angle_p(perm.2.C,real_angle_2.C)
pvalue_angle[3,5] <- angle_p(perm.3.C,real_angle_3.C)
pvalue_angle[4,5] <- angle_p(perm.4.C,real_angle_4.C)
pvalue_angle[5,5] <- angle_p(perm.5.C,real_angle_5.C)
pvalue_angle[6,5] <- angle_p(perm.6.C,real_angle_6.C)
pvalue_angle[7,5] <- angle_p(perm.7.C,real_angle_7.C)
pvalue_angle[8,5] <- angle_p(perm.8.C,real_angle_8.C)
pvalue_angle[9,5] <- angle_p(perm.9.C,real_angle_9.C)
pvalue_angle[10,5] <- angle_p(perm.10.C,real_angle_10.C)





##########################################################################################################
##########################################################################################################
#South
#1 find real angle
real_angle_1.S<- angle_lines(df1_S$Experiment_Date,df1_S$SLA,
                             df2_S$Experiment_Date,df2_S$SLA)
real_angle_2.S <- angle_lines(df1_S$Experiment_Date,df1_S$Water_Content,
                              df2_S$Experiment_Date,df2_S$Water_Content)
real_angle_3.S <- angle_lines(df1_S$Experiment_Date,df1_S$Assimilation,
                              df2_S$Experiment_Date,df2_S$Assimilation)
real_angle_4.S <- angle_lines(df1_S$Experiment_Date,df1_S$Stomatal_Conductance,
                              df2_S$Experiment_Date,df2_S$Stomatal_Conductance)
real_angle_5.S <- angle_lines(df1_S$SLA,df1_S$Assimilation,
                              df2_S$SLA,df2_S$Assimilation)

real_angle_6.S <- angle_lines(df1_S$Stomatal_Conductance,df1_S$SLA,
                              df2_S$Stomatal_Conductance,df2_S$SLA)
real_angle_7.S <- angle_lines(df1_S$Water_Content,df1_S$SLA,
                              df2_S$Water_Content,df2_S$SLA)
real_angle_8.S <- angle_lines(df1_S$Water_Content,df1_S$Assimilation,
                              df2_S$Water_Content,df2_S$Assimilation)
real_angle_9.S <- angle_lines(df1_S$Water_Content,df1_S$Stomatal_Conductance,
                              df2_S$Water_Content,df2_S$Stomatal_Conductance)
real_angle_10.S <- angle_lines(df1_S$Stomatal_Conductance,df1_S$Assimilation,
                               df2_S$Stomatal_Conductance,df2_S$Assimilation)

#2A. Filter out only two wanted comparisons, where var2 predicts var1
shuf_1.S <- South %>% select(Experiment_Date,SLA)
shuf_2.S <- South %>% select(Experiment_Date,Water_Content)
shuf_3.S <- South %>% select(Experiment_Date,Assimilation)
shuf_4.S <- South %>% select(Experiment_Date,Stomatal_Conductance)
shuf_5.S <- South %>% select(SLA,Assimilation)

shuf_6.S <- South %>% select(Stomatal_Conductance,SLA)
shuf_7.S <- South %>% select(Water_Content,SLA)
shuf_8.S <- South %>% select(Water_Content,Assimilation)
shuf_9.S <- South %>% select(Water_Content,Stomatal_Conductance)
shuf_10.S <- South %>% select(Stomatal_Conductance,Assimilation)

#2B. Use permute_angle function to calculate "angle_df" (permutation angle dataframe)
perm.1.S <- permute_angle(shuf_1.S,length(df1_S),length(df2_S))
perm.2.S <- permute_angle(shuf_2.S,length(df1_S),length(df2_S))
perm.3.S <- permute_angle(shuf_3.S,length(df1_S),length(df2_S))
perm.4.S <- permute_angle(shuf_4.S,length(df1_S),length(df2_S))
perm.5.S <- permute_angle(shuf_5.S,length(df1_S),length(df2_S))
perm.6.S <- permute_angle(shuf_6.S,length(df1_S),length(df2_S))
perm.7.S <- permute_angle(shuf_7.S,length(df1_S),length(df2_S))
perm.8.S <- permute_angle(shuf_8.S,length(df1_S),length(df2_S))
perm.9.S <- permute_angle(shuf_9.S,length(df1_S),length(df2_S))
perm.10.S <- permute_angle(shuf_10.S,length(df1_S),length(df2_S))

#3 Use "plot_angle" to produce graph with inputs from #1 and #2B.
plot_1.S <- plot_angle(perm.1.S,real_angle_1.S)
plot_2.S <- plot_angle(perm.2.S,real_angle_2.S)
plot_3.S <- plot_angle(perm.3.S,real_angle_3.S)
plot_4.S <- plot_angle(perm.4.S,real_angle_4.S)
plot_5.S <- plot_angle(perm.5.S,real_angle_5.S)
plot_6.S <- plot_angle(perm.6.S,real_angle_6.S)
plot_7.S <- plot_angle(perm.7.S,real_angle_7.S)
plot_8.S <- plot_angle(perm.8.S,real_angle_8.S)
plot_9.S <- plot_angle(perm.9.S,real_angle_9.S)
plot_10.S <- plot_angle(perm.10.S,real_angle_10.S)
## Cowplot export at 10 X 6 inches
plot_grid(plot_1.S,plot_2.S,plot_3.S,plot_4.S,plot_5.S,
          plot_6.S,plot_7.S,plot_8.S,plot_9.S,plot_10.S,ncol = 2)

#4 Use angle_p to calculate permutation p-value using inputs from #1 and #2B.
pvalue_angle[1,6] <- angle_p(perm.1.S,real_angle_1.S)
pvalue_angle[2,6] <- angle_p(perm.2.S,real_angle_2.S)
pvalue_angle[3,6] <- angle_p(perm.3.S,real_angle_3.S)
pvalue_angle[4,6] <- angle_p(perm.4.S,real_angle_4.S)
pvalue_angle[5,6] <- angle_p(perm.5.S,real_angle_5.S)
pvalue_angle[6,6] <- angle_p(perm.6.S,real_angle_6.S)
pvalue_angle[7,6] <- angle_p(perm.7.S,real_angle_7.S)
pvalue_angle[8,6] <- angle_p(perm.8.S,real_angle_8.S)
pvalue_angle[9,6] <- angle_p(perm.9.S,real_angle_9.S)
pvalue_angle[10,6] <- angle_p(perm.10.S,real_angle_10.S)



##########################################################################################################
##########################################################################################################
#All
#1 find real angle
real_angle_1.A<- angle_lines(df1_A$Experiment_Date,df1_A$SLA,
                             df2_A$Experiment_Date,df2_A$SLA)
real_angle_2.A <- angle_lines(df1_A$Experiment_Date,df1_A$Water_Content,
                              df2_A$Experiment_Date,df2_A$Water_Content)
real_angle_3.A <- angle_lines(df1_A$Experiment_Date,df1_A$Assimilation,
                              df2_A$Experiment_Date,df2_A$Assimilation)
real_angle_4.A <- angle_lines(df1_A$Experiment_Date,df1_A$Stomatal_Conductance,
                              df2_A$Experiment_Date,df2_A$Stomatal_Conductance)
real_angle_5.A <- angle_lines(df1_A$SLA,df1_A$Assimilation,
                              df2_A$SLA,df2_A$Assimilation)

real_angle_6.A <- angle_lines(df1_A$Stomatal_Conductance,df1_A$SLA,
                              df2_A$Stomatal_Conductance,df2_A$SLA)
real_angle_7.A <- angle_lines(df1_A$Water_Content,df1_A$SLA,
                              df2_A$Water_Content,df2_A$SLA)
real_angle_8.A <- angle_lines(df1_A$Water_Content,df1_A$Assimilation,
                              df2_A$Water_Content,df2_A$Assimilation)
real_angle_9.A <- angle_lines(df1_A$Water_Content,df1_A$Stomatal_Conductance,
                              df2_A$Water_Content,df2_A$Stomatal_Conductance)
real_angle_10.A <- angle_lines(df1_A$Stomatal_Conductance,df1_A$Assimilation,
                               df2_A$Stomatal_Conductance,df2_A$Assimilation)

#2A. Filter out only two wanted comparisons, where var2 predicts var1
shuf_1.A <- all %>% select(Experiment_Date,SLA)
shuf_2.A <- all %>% select(Experiment_Date,Water_Content)
shuf_3.A <- all %>% select(Experiment_Date,Assimilation)
shuf_4.A <- all %>% select(Experiment_Date,Stomatal_Conductance)
shuf_5.A <- all %>% select(SLA,Assimilation)

shuf_6.A <- all %>% select(Stomatal_Conductance,SLA)
shuf_7.A <- all %>% select(Water_Content,SLA)
shuf_8.A <- all %>% select(Water_Content,Assimilation)
shuf_9.A <- all %>% select(Water_Content,Stomatal_Conductance)
shuf_10.A <- all %>% select(Stomatal_Conductance,Assimilation)

#2B. Use permute_angle function to calculate "angle_df" (permutation angle dataframe)
perm.1.A <- permute_angle(shuf_1.A,length(df1_A),length(df2_A))
perm.2.A <- permute_angle(shuf_2.A,length(df1_A),length(df2_A))
perm.3.A <- permute_angle(shuf_3.A,length(df1_A),length(df2_A))
perm.4.A <- permute_angle(shuf_4.A,length(df1_A),length(df2_A))
perm.5.A <- permute_angle(shuf_5.A,length(df1_A),length(df2_A))
perm.6.A <- permute_angle(shuf_6.A,length(df1_A),length(df2_A))
perm.7.A <- permute_angle(shuf_7.A,length(df1_A),length(df2_A))
perm.8.A <- permute_angle(shuf_8.A,length(df1_A),length(df2_A))
perm.9.A <- permute_angle(shuf_9.A,length(df1_A),length(df2_A))
perm.10.A <- permute_angle(shuf_10.A,length(df1_A),length(df2_A))

#3 Use "plot_angle" to produce graph with inputs from #1 and #2B.
plot_1.A <- plot_angle(perm.1.A,real_angle_1.A)
plot_2.A <- plot_angle(perm.2.A,real_angle_2.A)
plot_3.A <- plot_angle(perm.3.A,real_angle_3.A)
plot_4.A <- plot_angle(perm.4.A,real_angle_4.A)
plot_5.A <- plot_angle(perm.5.A,real_angle_5.A)
plot_6.A <- plot_angle(perm.6.A,real_angle_6.A)
plot_7.A <- plot_angle(perm.7.A,real_angle_7.A)
plot_8.A <- plot_angle(perm.8.A,real_angle_8.A)
plot_9.A <- plot_angle(perm.9.A,real_angle_9.A)
plot_10.A <- plot_angle(perm.10.A,real_angle_10.A)
## Cowplot export at 10 X 6 inches
plot_grid(plot_1.A,plot_2.A,plot_3.A,plot_4.A,plot_5.A,
          plot_6.A,plot_7.A,plot_8.A,plot_9.A,plot_10.A,ncol = 2)

#4 Use angle_p to calculate permutation p-value using inputs from #1 and #2B.
pvalue_angle[1,7] <- angle_p(perm.1.A,real_angle_1.A)
pvalue_angle[2,7] <- angle_p(perm.2.A,real_angle_2.A)
pvalue_angle[3,7] <- angle_p(perm.3.A,real_angle_3.A)
pvalue_angle[4,7] <- angle_p(perm.4.A,real_angle_4.A)
pvalue_angle[5,7] <- angle_p(perm.5.A,real_angle_5.A)
pvalue_angle[6,7] <- angle_p(perm.6.A,real_angle_6.A)
pvalue_angle[7,7] <- angle_p(perm.7.A,real_angle_7.A)
pvalue_angle[8,7] <- angle_p(perm.8.A,real_angle_8.A)
pvalue_angle[9,7] <- angle_p(perm.9.A,real_angle_9.A)
pvalue_angle[10,7] <- angle_p(perm.10.A,real_angle_10.A)


write_csv(pvalue_angle, "pvalue_angle.csv")

