##########################################################################################################
## ## Difference Variance of residual of regression lines 
## Author Haley Branch & Daniel Anstett
## Using functions. Across regions
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

#Input dataframe for p-values
pvalue_sd<- read.csv("Data/pvalue_angle_setup.csv", header=TRUE)
##########################################################################################################
#Set up functions
######################################################
#1.angle between two lines, calculate "stdev_diff"
sd_diff <- function(var1,var2, var3, var4) {
  # generate models for var1 ~ var2
  mod1 <- lm(var1~var2,data=df1_N) 
  mod2 <- lm(var3~var4,data=df2_N)
  #Get residuals
  mod1_res <- residuals(mod1)
  mod2_res <- residuals(mod2)
  #calc stdev
  stdev_1 <- sd(mod1_res)
  stdev_2 <- sd(mod2_res)
  stdev_diff <- stdev_2 - stdev_1
  return(stdev_diff)
}
######################################################
#2.permutation, calculate "angle_df"
permute_sd <- function(region,pre_size,peak_size) {
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
    #Get residuals
    mod1_res <- residuals(mod.pre)
    mod2_res <- residuals(mod.peak)
    #calc stdev
    stdev_1 <- sd(mod1_res)
    stdev_2 <- sd(mod2_res)
    stdev_diff <- stdev_2 - stdev_1
    perm.shuf[i,1] <- stdev_diff
  }
    return(perm.shuf )
}
######################################################
#3. make angle plot
plot_sd <- function(sd_df,real_sd){
  # plot 
  hist.1 <- ggplot(sd_df, aes(X=V1))+
    geom_histogram(aes(V1))+
    geom_vline(xintercept=c(real_sd),color="#0000CC")+
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
######################################################
#4. find permutation p-value
sd_p <- function(sd_df,real_sd){
  ex_l <- length(sd_df[sd_df >= real_sd])
  per_pval <- ex_l/dim(sd_df)[1] 
  return(per_pval)
}
##########################################################################################################
#highly commented example
#Run for North
#df & SLA, 

#1. Use angle_lines function to find "stdev_diff"
eg_diff <- sd_diff(df1_N$Experiment_Date,df1_N$SLA,
                                 df2_N$Experiment_Date,df2_N$SLA)

#2A. Filter out only two wanted comarisons varaibles where var2 predicts var1
North_shuf <- North %>% select(Experiment_Date,SLA) # make sure to replace with correct variables
#2B. Use permute_angle function to calculate "angle_df" (permutation angle dataframe)
perm.eg <- permute_sd (North_shuf,length(df1_N),length(df2_N))

#3 Use "plot_angle" to produce graph with inputs from #1 and #2B.
plot_sd(perm.eg,eg_diff)

#4 Use angle_p to calculate permutation p-value using inputs from #1 and #2B.
sd_p(perm.eg,eg_diff)

##########################################################################################################
##########################################################################################################
#North
#1 find real sd
real_sd_1.N<- sd_diff(df1_N$Experiment_Date,df1_N$SLA,
                             df2_N$Experiment_Date,df2_N$SLA)
real_sd_2.N <- sd_diff(df1_N$Experiment_Date,df1_N$Water_Content,
                              df2_N$Experiment_Date,df2_N$Water_Content)
real_sd_3.N <- sd_diff(df1_N$Experiment_Date,df1_N$Assimilation,
                              df2_N$Experiment_Date,df2_N$Assimilation)
real_sd_4.N <- sd_diff(df1_N$Experiment_Date,df1_N$Stomatal_Conductance,
                              df2_N$Experiment_Date,df2_N$Stomatal_Conductance)
real_sd_5.N <- sd_diff(df1_N$SLA,df1_N$Assimilation,
                              df2_N$SLA,df2_N$Assimilation)

real_sd_6.N <- sd_diff(df1_N$Stomatal_Conductance,df1_N$SLA,
                              df2_N$Stomatal_Conductance,df2_N$SLA)
real_sd_7.N <- sd_diff(df1_N$Water_Content,df1_N$SLA,
                              df2_N$Water_Content,df2_N$SLA)
real_sd_8.N <- sd_diff(df1_N$Water_Content,df1_N$Assimilation,
                              df2_N$Water_Content,df2_N$Assimilation)
real_sd_9.N <- sd_diff(df1_N$Water_Content,df1_N$Stomatal_Conductance,
                              df2_N$Water_Content,df2_N$Stomatal_Conductance)
real_sd_10.N <- sd_diff(df1_N$Stomatal_Conductance,df1_N$Assimilation,
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

#2B. Use permute_sd function to calculate "sd_df" (permutation sd dataframe)
perm.1.N <- permute_sd(North_shuf_1,length(df1_N),length(df2_N))
perm.2.N <- permute_sd(North_shuf_2,length(df1_N),length(df2_N))
perm.3.N <- permute_sd(North_shuf_3,length(df1_N),length(df2_N))
perm.4.N <- permute_sd(North_shuf_4,length(df1_N),length(df2_N))
perm.5.N <- permute_sd(North_shuf_5,length(df1_N),length(df2_N))
perm.6.N <- permute_sd(North_shuf_6,length(df1_N),length(df2_N))
perm.7.N <- permute_sd(North_shuf_7,length(df1_N),length(df2_N))
perm.8.N <- permute_sd(North_shuf_8,length(df1_N),length(df2_N))
perm.9.N <- permute_sd(North_shuf_9,length(df1_N),length(df2_N))
perm.10.N <- permute_sd(North_shuf_10,length(df1_N),length(df2_N))

#3 Use "plot_sd" to produce graph with inputs from #1 and #2B.
plot_1.N <- plot_sd(perm.1.N,real_sd_1.N)
plot_2.N <- plot_sd(perm.2.N,real_sd_2.N)
plot_3.N <- plot_sd(perm.3.N,real_sd_3.N)
plot_4.N <- plot_sd(perm.4.N,real_sd_4.N)
plot_5.N <- plot_sd(perm.5.N,real_sd_5.N)
plot_6.N <- plot_sd(perm.6.N,real_sd_6.N)
plot_7.N <- plot_sd(perm.7.N,real_sd_7.N)
plot_8.N <- plot_sd(perm.8.N,real_sd_8.N)
plot_9.N <- plot_sd(perm.9.N,real_sd_9.N)
plot_10.N <- plot_sd(perm.10.N,real_sd_10.N)
## Cowplot export at 10 X 6 inches
plot_grid(plot_1.N,plot_2.N,plot_3.N,plot_4.N,plot_5.N,
          plot_6.N,plot_7.N,plot_8.N,plot_9.N,plot_10.N,ncol = 2)

#4 Use sd_p to calculate permutation p-value using inputs from #1 and #2B.
pvalue_sd[1,4] <- sd_p(perm.1.N,real_sd_1.N)
pvalue_sd[2,4] <- sd_p(perm.2.N,real_sd_2.N)
pvalue_sd[3,4] <- sd_p(perm.3.N,real_sd_3.N)
pvalue_sd[4,4] <- sd_p(perm.4.N,real_sd_4.N)
pvalue_sd[5,4] <- sd_p(perm.5.N,real_sd_5.N)
pvalue_sd[6,4] <- sd_p(perm.6.N,real_sd_6.N)
pvalue_sd[7,4] <- sd_p(perm.7.N,real_sd_7.N)
pvalue_sd[8,4] <- sd_p(perm.8.N,real_sd_8.N)
pvalue_sd[9,4] <- sd_p(perm.9.N,real_sd_9.N)
pvalue_sd[10,4] <- sd_p(perm.10.N,real_sd_10.N)


##########################################################################################################
##########################################################################################################
#Centre
#1 find real sd
real_sd_1.C<- sd_diff(df1_C$Experiment_Date,df1_C$SLA,
                             df2_C$Experiment_Date,df2_C$SLA)
real_sd_2.C <- sd_diff(df1_C$Experiment_Date,df1_C$Water_Content,
                              df2_C$Experiment_Date,df2_C$Water_Content)
real_sd_3.C <- sd_diff(df1_C$Experiment_Date,df1_C$Assimilation,
                              df2_C$Experiment_Date,df2_C$Assimilation)
real_sd_4.C <- sd_diff(df1_C$Experiment_Date,df1_C$Stomatal_Conductance,
                              df2_C$Experiment_Date,df2_C$Stomatal_Conductance)
real_sd_5.C <- sd_diff(df1_C$SLA,df1_C$Assimilation,
                              df2_C$SLA,df2_C$Assimilation)

real_sd_6.C <- sd_diff(df1_C$Stomatal_Conductance,df1_C$SLA,
                              df2_C$Stomatal_Conductance,df2_C$SLA)
real_sd_7.C <- sd_diff(df1_C$Water_Content,df1_C$SLA,
                              df2_C$Water_Content,df2_C$SLA)
real_sd_8.C <- sd_diff(df1_C$Water_Content,df1_C$Assimilation,
                              df2_C$Water_Content,df2_C$Assimilation)
real_sd_9.C <- sd_diff(df1_C$Water_Content,df1_C$Stomatal_Conductance,
                              df2_C$Water_Content,df2_C$Stomatal_Conductance)
real_sd_10.C <- sd_diff(df1_C$Stomatal_Conductance,df1_C$Assimilation,
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

#2B. Use permute_sd function to calculate "sd_df" (permutation sd dataframe)
perm.1.C <- permute_sd(Centre_shuf_1,length(df1_C),length(df2_C))
perm.2.C <- permute_sd(Centre_shuf_2,length(df1_C),length(df2_C))
perm.3.C <- permute_sd(Centre_shuf_3,length(df1_C),length(df2_C))
perm.4.C <- permute_sd(Centre_shuf_4,length(df1_C),length(df2_C))
perm.5.C <- permute_sd(Centre_shuf_5,length(df1_C),length(df2_C))
perm.6.C <- permute_sd(Centre_shuf_6,length(df1_C),length(df2_C))
perm.7.C <- permute_sd(Centre_shuf_7,length(df1_C),length(df2_C))
perm.8.C <- permute_sd(Centre_shuf_8,length(df1_C),length(df2_C))
perm.9.C <- permute_sd(Centre_shuf_9,length(df1_C),length(df2_C))
perm.10.C <- permute_sd(Centre_shuf_10,length(df1_C),length(df2_C))

#3 Use "plot_sd" to produce graph with inputs from #1 and #2B.
plot_1.C <- plot_sd(perm.1.C,real_sd_1.C)
plot_2.C <- plot_sd(perm.2.C,real_sd_2.C)
plot_3.C <- plot_sd(perm.3.C,real_sd_3.C)
plot_4.C <- plot_sd(perm.4.C,real_sd_4.C)
plot_5.C <- plot_sd(perm.5.C,real_sd_5.C)
plot_6.C <- plot_sd(perm.6.C,real_sd_6.C)
plot_7.C <- plot_sd(perm.7.C,real_sd_7.C)
plot_8.C <- plot_sd(perm.8.C,real_sd_8.C)
plot_9.C <- plot_sd(perm.9.C,real_sd_9.C)
plot_10.C <- plot_sd(perm.10.C,real_sd_10.C)
## Cowplot export at 10 X 6 inches
plot_grid(plot_1.C,plot_2.C,plot_3.C,plot_4.C,plot_5.C,
          plot_6.C,plot_7.C,plot_8.C,plot_9.C,plot_10.C,ncol = 2)

#4 Use sd_p to calculate permutation p-value using inputs from #1 and #2B.
pvalue_sd[1,5] <- sd_p(perm.1.C,real_sd_1.C)
pvalue_sd[2,5] <- sd_p(perm.2.C,real_sd_2.C)
pvalue_sd[3,5] <- sd_p(perm.3.C,real_sd_3.C)
pvalue_sd[4,5] <- sd_p(perm.4.C,real_sd_4.C)
pvalue_sd[5,5] <- sd_p(perm.5.C,real_sd_5.C)
pvalue_sd[6,5] <- sd_p(perm.6.C,real_sd_6.C)
pvalue_sd[7,5] <- sd_p(perm.7.C,real_sd_7.C)
pvalue_sd[8,5] <- sd_p(perm.8.C,real_sd_8.C)
pvalue_sd[9,5] <- sd_p(perm.9.C,real_sd_9.C)
pvalue_sd[10,5] <- sd_p(perm.10.C,real_sd_10.C)





##########################################################################################################
##########################################################################################################
#South
#1 find real sd
real_sd_1.S<- sd_diff(df1_S$Experiment_Date,df1_S$SLA,
                             df2_S$Experiment_Date,df2_S$SLA)
real_sd_2.S <- sd_diff(df1_S$Experiment_Date,df1_S$Water_Content,
                              df2_S$Experiment_Date,df2_S$Water_Content)
real_sd_3.S <- sd_diff(df1_S$Experiment_Date,df1_S$Assimilation,
                              df2_S$Experiment_Date,df2_S$Assimilation)
real_sd_4.S <- sd_diff(df1_S$Experiment_Date,df1_S$Stomatal_Conductance,
                              df2_S$Experiment_Date,df2_S$Stomatal_Conductance)
real_sd_5.S <- sd_diff(df1_S$SLA,df1_S$Assimilation,
                              df2_S$SLA,df2_S$Assimilation)

real_sd_6.S <- sd_diff(df1_S$Stomatal_Conductance,df1_S$SLA,
                              df2_S$Stomatal_Conductance,df2_S$SLA)
real_sd_7.S <- sd_diff(df1_S$Water_Content,df1_S$SLA,
                              df2_S$Water_Content,df2_S$SLA)
real_sd_8.S <- sd_diff(df1_S$Water_Content,df1_S$Assimilation,
                              df2_S$Water_Content,df2_S$Assimilation)
real_sd_9.S <- sd_diff(df1_S$Water_Content,df1_S$Stomatal_Conductance,
                              df2_S$Water_Content,df2_S$Stomatal_Conductance)
real_sd_10.S <- sd_diff(df1_S$Stomatal_Conductance,df1_S$Assimilation,
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

#2B. Use permute_sd function to calculate "sd_df" (permutation sd dataframe)
perm.1.S <- permute_sd(shuf_1.S,length(df1_S),length(df2_S))
perm.2.S <- permute_sd(shuf_2.S,length(df1_S),length(df2_S))
perm.3.S <- permute_sd(shuf_3.S,length(df1_S),length(df2_S))
perm.4.S <- permute_sd(shuf_4.S,length(df1_S),length(df2_S))
perm.5.S <- permute_sd(shuf_5.S,length(df1_S),length(df2_S))
perm.6.S <- permute_sd(shuf_6.S,length(df1_S),length(df2_S))
perm.7.S <- permute_sd(shuf_7.S,length(df1_S),length(df2_S))
perm.8.S <- permute_sd(shuf_8.S,length(df1_S),length(df2_S))
perm.9.S <- permute_sd(shuf_9.S,length(df1_S),length(df2_S))
perm.10.S <- permute_sd(shuf_10.S,length(df1_S),length(df2_S))

#3 Use "plot_sd" to produce graph with inputs from #1 and #2B.
plot_1.S <- plot_sd(perm.1.S,real_sd_1.S)
plot_2.S <- plot_sd(perm.2.S,real_sd_2.S)
plot_3.S <- plot_sd(perm.3.S,real_sd_3.S)
plot_4.S <- plot_sd(perm.4.S,real_sd_4.S)
plot_5.S <- plot_sd(perm.5.S,real_sd_5.S)
plot_6.S <- plot_sd(perm.6.S,real_sd_6.S)
plot_7.S <- plot_sd(perm.7.S,real_sd_7.S)
plot_8.S <- plot_sd(perm.8.S,real_sd_8.S)
plot_9.S <- plot_sd(perm.9.S,real_sd_9.S)
plot_10.S <- plot_sd(perm.10.S,real_sd_10.S)
## Cowplot export at 10 X 6 inches
plot_grid(plot_1.S,plot_2.S,plot_3.S,plot_4.S,plot_5.S,
          plot_6.S,plot_7.S,plot_8.S,plot_9.S,plot_10.S,ncol = 2)

#4 Use sd_p to calculate permutation p-value using inputs from #1 and #2B.
pvalue_sd[1,6] <- sd_p(perm.1.S,real_sd_1.S)
pvalue_sd[2,6] <- sd_p(perm.2.S,real_sd_2.S)
pvalue_sd[3,6] <- sd_p(perm.3.S,real_sd_3.S)
pvalue_sd[4,6] <- sd_p(perm.4.S,real_sd_4.S)
pvalue_sd[5,6] <- sd_p(perm.5.S,real_sd_5.S)
pvalue_sd[6,6] <- sd_p(perm.6.S,real_sd_6.S)
pvalue_sd[7,6] <- sd_p(perm.7.S,real_sd_7.S)
pvalue_sd[8,6] <- sd_p(perm.8.S,real_sd_8.S)
pvalue_sd[9,6] <- sd_p(perm.9.S,real_sd_9.S)
pvalue_sd[10,6] <- sd_p(perm.10.S,real_sd_10.S)



##########################################################################################################
##########################################################################################################
#All
#1 find real sd
real_sd_1.A<- sd_diff(df1_A$Experiment_Date,df1_A$SLA,
                             df2_A$Experiment_Date,df2_A$SLA)
real_sd_2.A <- sd_diff(df1_A$Experiment_Date,df1_A$Water_Content,
                              df2_A$Experiment_Date,df2_A$Water_Content)
real_sd_3.A <- sd_diff(df1_A$Experiment_Date,df1_A$Assimilation,
                              df2_A$Experiment_Date,df2_A$Assimilation)
real_sd_4.A <- sd_diff(df1_A$Experiment_Date,df1_A$Stomatal_Conductance,
                              df2_A$Experiment_Date,df2_A$Stomatal_Conductance)
real_sd_5.A <- sd_diff(df1_A$SLA,df1_A$Assimilation,
                              df2_A$SLA,df2_A$Assimilation)

real_sd_6.A <- sd_diff(df1_A$Stomatal_Conductance,df1_A$SLA,
                              df2_A$Stomatal_Conductance,df2_A$SLA)
real_sd_7.A <- sd_diff(df1_A$Water_Content,df1_A$SLA,
                              df2_A$Water_Content,df2_A$SLA)
real_sd_8.A <- sd_diff(df1_A$Water_Content,df1_A$Assimilation,
                              df2_A$Water_Content,df2_A$Assimilation)
real_sd_9.A <- sd_diff(df1_A$Water_Content,df1_A$Stomatal_Conductance,
                              df2_A$Water_Content,df2_A$Stomatal_Conductance)
real_sd_10.A <- sd_diff(df1_A$Stomatal_Conductance,df1_A$Assimilation,
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

#2B. Use permute_sd function to calculate "sd_df" (permutation sd dataframe)
perm.1.A <- permute_sd(shuf_1.A,length(df1_A),length(df2_A))
perm.2.A <- permute_sd(shuf_2.A,length(df1_A),length(df2_A))
perm.3.A <- permute_sd(shuf_3.A,length(df1_A),length(df2_A))
perm.4.A <- permute_sd(shuf_4.A,length(df1_A),length(df2_A))
perm.5.A <- permute_sd(shuf_5.A,length(df1_A),length(df2_A))
perm.6.A <- permute_sd(shuf_6.A,length(df1_A),length(df2_A))
perm.7.A <- permute_sd(shuf_7.A,length(df1_A),length(df2_A))
perm.8.A <- permute_sd(shuf_8.A,length(df1_A),length(df2_A))
perm.9.A <- permute_sd(shuf_9.A,length(df1_A),length(df2_A))
perm.10.A <- permute_sd(shuf_10.A,length(df1_A),length(df2_A))

#3 Use "plot_sd" to produce graph with inputs from #1 and #2B.
plot_1.A <- plot_sd(perm.1.A,real_sd_1.A)
plot_2.A <- plot_sd(perm.2.A,real_sd_2.A)
plot_3.A <- plot_sd(perm.3.A,real_sd_3.A)
plot_4.A <- plot_sd(perm.4.A,real_sd_4.A)
plot_5.A <- plot_sd(perm.5.A,real_sd_5.A)
plot_6.A <- plot_sd(perm.6.A,real_sd_6.A)
plot_7.A <- plot_sd(perm.7.A,real_sd_7.A)
plot_8.A <- plot_sd(perm.8.A,real_sd_8.A)
plot_9.A <- plot_sd(perm.9.A,real_sd_9.A)
plot_10.A <- plot_sd(perm.10.A,real_sd_10.A)
## Cowplot export at 10 X 6 inches
plot_grid(plot_1.A,plot_2.A,plot_3.A,plot_4.A,plot_5.A,
          plot_6.A,plot_7.A,plot_8.A,plot_9.A,plot_10.A,ncol = 2)

#4 Use sd_p to calculate permutation p-value using inputs from #1 and #2B.
pvalue_sd[1,7] <- sd_p(perm.1.A,real_sd_1.A)
pvalue_sd[2,7] <- sd_p(perm.2.A,real_sd_2.A)
pvalue_sd[3,7] <- sd_p(perm.3.A,real_sd_3.A)
pvalue_sd[4,7] <- sd_p(perm.4.A,real_sd_4.A)
pvalue_sd[5,7] <- sd_p(perm.5.A,real_sd_5.A)
pvalue_sd[6,7] <- sd_p(perm.6.A,real_sd_6.A)
pvalue_sd[7,7] <- sd_p(perm.7.A,real_sd_7.A)
pvalue_sd[8,7] <- sd_p(perm.8.A,real_sd_8.A)
pvalue_sd[9,7] <- sd_p(perm.9.A,real_sd_9.A)
pvalue_sd[10,7] <- sd_p(perm.10.A,real_sd_10.A)


write_csv(pvalue_sd, "pvalue_sd.csv")



