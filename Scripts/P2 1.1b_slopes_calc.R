#################
# Slopes calculation
#################
library(tidyverse)
#library(lsmeans)
#library(car)
#library(maptools)
library(visreg)
#library(ggeffects)
#library(nlme)
library(lme4)
#library(lmerTest)
#library(lmtest)
library(glmmTMB)

all<- read.csv("Data/all.csv", header=T) #Imports main dataset
all<- all %>%
   separate(ID_Year, c("ID", "Year"), "_")
all$Year <- as.numeric(all$Year)
#################### Slopes ####################
slopes.rapid<-distinct(all, Region, Site.Lat) #sets up site and site lat for slopes data frame

### Flowering Time ###
fullmod.exp <- lmer(Experiment_Date ~ Site.Lat*Year + (1|Family) + (1|BlockDrought), 
                    control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=all)
vis_flower<-visreg(fullmod.exp, xvar="Year", by="Site.Lat") # plot only Drought treatment
fit_flower<-vis_flower$fit #put points representing line of best fit into new variable
flower_pop<-unique(fit_flower$Site.Lat) # sets up a vector with each site code as one entry

#For each population run lm on points on line of fit, then extract slope
for (i in 1:11){
  fit_flower_tmp<-fit_flower %>% filter(Site.Lat==flower_pop[i]) 
  lm_flower<-lm(visregFit~Year, data=fit_flower_tmp) # take a lm of residuals
  summary_flower<-summary(lm_flower) #get summary of lm
  slopes.rapid[i,3]<-summary_flower$coefficients[2,1] #extract slope
}
colnames(slopes.rapid)[3]<-"FloweringDate" # label new variable

#WaterContent
fullmod.wc <- lmer(Water_Content~ Site.Lat*Year + (1|Family) + (1|BlockDrought), 
                    control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=all)
vis_wc<-visreg(fullmod.wc, xvar="Year", by="Site.Lat") # plot only Drought treatment
fit_wc<-vis_wc$fit #put points representing line of best fit into new variable

for (i in 1:11){
  fit_wc_tmp<-fit_wc %>% filter(Site.Lat==flower_pop[i])
  lm_wc<-lm(visregFit~Year, data=fit_wc_tmp)
  summary_wc<-summary(lm_wc)
  slopes.rapid[i,4]<-summary_wc$coefficients[2,1]
}
colnames(slopes.rapid)[4]<-"WaterContent"

#write.csv(slopes.rapid,'Data/slopesDFWC.csv') #Export file

#SLA
fullmod.sla <- lmer(SLA~ Site.Lat*Year + (1|Family) + (1|BlockDrought), 
                   control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=all)
vis_sla<-visreg(fullmod.sla, xvar="Year", by="Site.Lat") # plot only Drought treatment
fit_sla<-vis_sla$fit #put points representing line of best fit into new variable

for (i in 1:11){
  fit_sla_tmp<-fit_sla %>% filter(Site.Lat==flower_pop[i])
  lm_sla<-lm(visregFit~Year, data=fit_sla_tmp)
  summary_sla<-summary(lm_sla)
  slopes.rapid[i,5]<-summary_sla$coefficients[2,1]
}
colnames(slopes.rapid)[5]<-"SLA"

#Assimilation
fullmod.A <- lmer(Assimilation~ Site.Lat*Year + (1|Family) + (1|BlockDrought), 
                   control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=all)
vis_A<-visreg(fullmod.A, xvar="Year", by="Site.Lat") # plot only Drought treatment
fit_A<-vis_A$fit #put points representing line of best fit into new variable

for (i in 1:11){
  fit_A_tmp<-fit_A %>% filter(Site.Lat==flower_pop[i])
  lm_A<-lm(visregFit~Year, data=fit_A_tmp)
  summary_A<-summary(lm_A)
  slopes.rapid[i,6]<-summary_A$coefficients[2,1]
}
colnames(slopes.rapid)[6]<-"Assimilation"

#Stomatal Conductance
fullmod.gs <- lmer(Stomatal_Conductance ~ Site.Lat*Year + (1|Family) + (1|BlockDrought), 
                   control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=all)
vis_gs<-visreg(fullmod.gs, xvar="Year", by="Site.Lat") # plot only Drought treatment
fit_gs<-vis_gs$fit #put points representing line of best fit into new variable

for (i in 1:11){
  fit_gs_tmp<-fit_gs %>% filter(Site.Lat==flower_pop[i])
  lm_gs<-lm(visregFit~Year, data=fit_gs_tmp)
  summary_gs<-summary(lm_gs)
  slopes.rapid[i,7]<-summary_gs$coefficients[2,1]
}
colnames(slopes.rapid)[7]<-"Stomatal Conductance"


#write.csv(slopes.rapid,'Data/slopesAll.csv') #Export file


