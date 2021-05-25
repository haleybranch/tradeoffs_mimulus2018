#################
# Put reaion/year means into PCA coordinate system
#################
library(tidyverse)
library(ggfortify)
library(tidyverse)

y7 <- read.csv("Data/y7.csv", header=T) #Imports main dataset
y7 <- y7 %>% mutate(Region = ifelse(Latitude >= 40, "1.North", ifelse((Latitude >35) & (Latitude <40), "2.Center","3.South"))) #Add in region
y7 <- y7 %>% mutate(Region.year = paste(Region, Year, sep="_")) #Make Region.year variable

region.means <- y7 %>% group_by(Region.year, Region, Year) %>% 
  summarise_at(c("Experiment_Date", "Water_Content", "SLA", "Stomatal_Conductance", "Assimilation", "Biomass"), 
               mean, na.rm=TRUE) 
region.means <-ungroup(region.means)

#Assess correlation among response variables
pc1 <- prcomp(na.omit(y7[,c("Experiment_Date","Water_Content","SLA","Stomatal_Conductance","Assimilation")]), scale=T)

#convert wet means into pc axes
region.redu <- region.means %>% select("Experiment_Date","Water_Content","SLA","Stomatal_Conductance","Assimilation") #select data from means
pc.mean.scale <- scale(region.redu , pc1$center, pc1$scale) %*% pc1$rotation #scale regional means by PC axes
pc.means<-region.means %>% select("Region.year","Region","Year")
pc.means<-cbind(pc.means,pc.mean.scale)
write.csv(pc.means,'Data/pc.means.region.csv') #Export file


# Establish what sites are represented
site.year.means <- y7 %>% group_by(ID_Year,Site, Year, Latitude, Longitude,Region) %>% 
  summarise_at(c("Experiment_Date", "Water_Content", "SLA", "Stomatal_Conductance", "Assimilation", "Biomass"), mean, na.rm=TRUE)
site.year.means <-ungroup(site.year.means)
site.year.means <- site.year.means %>% select(ID_Year,Site,Year,Region)
#write.csv(site.year.means,'Data/site.year.available.csv') #Export file