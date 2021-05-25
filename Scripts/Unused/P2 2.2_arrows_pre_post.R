#################
# Put region/year means into PCA coordinate system
#################
library(tidyverse)
library(ggfortify)
library(tidyverse)
library(ggrepel)
library(cowplot)

# Read in pre and peak datasets 
all <- read.csv("Data/all.csv")
all <- all %>% mutate(Region.period = paste(Region, Period, sep="_")) #Make Region.period variable

region.means <- all %>% group_by(Region.period, Region,Period) %>% 
  summarise_at(c("Experiment_Date", "Water_Content", "SLA", "Stomatal_Conductance", "Assimilation"), 
               mean, na.rm=TRUE) 
region.means <-ungroup(region.means)

#Get % variation explained by PC axes
pc1 <- prcomp(na.omit(all[,c("Experiment_Date","Water_Content","SLA","Stomatal_Conductance","Assimilation")]), scale=T)
summary(pc1)

#convert wet means into pc axes
region.redu <- region.means %>% select("Experiment_Date","Water_Content","SLA",
                                       "Stomatal_Conductance","Assimilation") #select data from means
pc.mean.scale <- scale(region.redu , pc1$center, pc1$scale) %*% pc1$rotation #scale regional means by PC axes
pc.means<-region.means %>% select("Region.period","Region","Period")
pc.means<-cbind(pc.means,pc.mean.scale)

#PCA Means Graph
pc_Labs<-c("1.North"="A (North)", "2.Center"="B (Center)", "3.South"="C (South)") #Set up headers

pc.plot.mean <- ggplot(pc.means, aes(PC1,PC2, color=Region, label=Period))+
  geom_point(size=1)+
  geom_path(arrow = arrow(angle = 15, type = "open"), aes(color=Region))+
  scale_x_continuous(name="PC1 (55%)") +
  scale_y_continuous(name="PC2 (20%)") +
  scale_color_manual(values= c("1.North"="#3399FF", "2.Center"="#FFCC00", "3.South"="#FF3333")) +  
  theme_classic() +  
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0),
        axis.text.y = element_text(size=14,face="bold"),
        axis.title.x = element_text(color="black", size=16, vjust = 0, face="bold"),
        axis.title.y = element_text(color="black", size=16,vjust = 0, face="bold"))
pc.plot.mean




#pc.plot.mean <- pc.plot.mean + theme(legend.text = element_text(size = 12, face = "bold"))
pc.plot.mean + facet_wrap( ~ Region, labeller = labeller(Site.Lat=pc_Labs), ncol=4)+ 
  theme(strip.background = element_blank(), strip.text.x=element_text(size=12,face="bold",hjust=0.05,vjust=-1.7))
