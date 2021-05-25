####################################################################################################
# PCA of all regions
####################################################################################################
#Import libraries
library(tidyverse)
library(vegan)
library(devtools)
library("factoextra")
library(ggfortify)
#library(ggrepel)
library(cowplot)

####################################################################################################
#Read in data
all <- read.csv("Data/all.csv")
all <- all %>% mutate(Region.period = paste(Region, Period, sep="_")) #Make Region.period variable

####################################################################################################
#Set up principle component

#PCA of all data
pc1_all <- prcomp(na.omit(all[,c("Experiment_Date","SLA","Water_Content",
                                 "Stomatal_Conductance","Assimilation")]), scale=T)

summary(pc1_all)

#biplot(pc1_all, scale=0, col=c("black", "red"), xlab = "PC1 (54%)", ylab="PC2 (20%)")

#Extract coodinates and variable loadings
ind_all <- get_pca(pc1_all, "ind")
all.ind <- ind_all$coord
all.ind <- as.data.frame(all.ind)

var_all <- get_pca(pc1_all, "var")
all.var <- var_all$coord
all.var <- as.data.frame(all.var)

all.na <- na.omit(all)
all.pc <- cbind(all.na, all.ind)

####################################################################################################
#Set up of regional means in PC space

#Get Region means
region.means <- all %>% group_by(Region.period, Region,Period) %>% 
  summarise_at(c("Experiment_Date", "Water_Content", "SLA", "Stomatal_Conductance", "Assimilation"), 
               mean, na.rm=TRUE) 
region.means <-ungroup(region.means)

#Get % variation explained by PC axes
pc1 <- prcomp(na.omit(all[,c("Experiment_Date","Water_Content","SLA","Stomatal_Conductance","Assimilation")]), scale=T)
summary(pc1)

#Convert region means into PC scaling
region.redu <- region.means %>% select("Experiment_Date","Water_Content","SLA",
                                       "Stomatal_Conductance","Assimilation") #select data from means
pc.mean.scale <- scale(region.redu , pc1$center, pc1$scale) %*% pc1$rotation #scale regional means by PC axes
pc.means<-region.means %>% select("Region.period","Region","Period")
pc.means<-cbind(pc.means,pc.mean.scale)

####################################################################################################
####################################################################################################
#Make Fig 2A (region mean arrows on all PCA with no points)

  
pc.plot.mean <- ggplot(pc.means, aes(PC1,PC2, color=Region))+
  geom_point(size=2.5)+
  geom_path(arrow = arrow(angle = 15, type = "open"), aes(color=Region), size=0.9)+
  scale_x_continuous(name="PC1 (54.8%)") +
  scale_y_continuous(name="PC2 (20.38%)") +
  scale_color_manual(values= c("1.North"="#3399FF", "2.Center"="#FFCC00", "3.South"="#FF3333")) +
  theme_classic() +  
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0),
        axis.text.y = element_text(size=14,face="bold"),
        axis.title.x = element_text(color="black", size=16, vjust = 0, face="bold"),
        axis.title.y = element_text(color="black", size=16,vjust = 0, face="bold"))
pc.plot.mean<-pc.plot.mean +
  geom_segment(aes(x=0, y=0, xend=all.var[1,1], yend=all.var[1,2]), color="black")+ #flowering time
  geom_segment(aes(x=0, y=0, xend=all.var[2,1], yend=all.var[2,2]), color="black") + #SLA
  geom_segment(aes(x=0, y=0, xend=all.var[3,1], yend=all.var[3,2]), color="black") + #water content
  geom_segment(aes(x=0, y=0, xend=all.var[4,1], yend=all.var[4,2]), color="black") + #stomatal conductance
  geom_segment(aes(x=0, y=0, xend=all.var[5,1], yend=all.var[5,2]), color="black")  # assimilation 
pc.plot.mean <-pc.plot.mean +  
  geom_text(size = 4, x=all.var[1,1]-0.02, y=all.var[1,2]-0.02, label="DF", color="black")+
  geom_text(size = 4, x=all.var[2,1]-0.02, y=all.var[2,2]-0.02, label="SLA", color="black")+
  geom_text(size = 4, x=all.var[3,1]+0.02, y=all.var[3,2]+0.02, label="WC", color="black")+
  geom_text(size = 4, x=all.var[4,1]-0.02, y=all.var[4,2]+0.02, label="gs", color="black")+
  geom_text(size = 4, x=all.var[5,1]-0.02, y=all.var[5,2]+0.02, label="A", color="black")
pc.plot.mean




####################################################################################################
####################################################################################################
##Make Fig 2A and B

pre <- read.csv("Data/pre.csv")
peak <- read.csv("Data/peak.csv")
pc_pre <- prcomp(na.omit(pre[,c("Experiment_Date","SLA","Water_Content",
                                 "Stomatal_Conductance","Assimilation")]), scale=T)

summary(pc_pre)
pc_peak <- prcomp(na.omit(peak[,c("Experiment_Date","SLA","Water_Content",
                                "Stomatal_Conductance","Assimilation")]), scale=T)
summary(pc_peak)

pre_plot <- autoplot(pc_pre, data = pre, scale=0,
              loadings = TRUE, loadings.colour = 'black',
              loadings.label = F, loadings.label.size = 3)
pre_plot <- pre_plot +
  xlim(-4, 4)+
  ylim(-4, 4)+
  geom_text(size=4.5, x=-3.6, y=4, label="Pre", color="red")+
  geom_text(size = 4, x=-2.3, y=-0.9, label="DF", color="red")+
  geom_text(size = 4, x=-2.5, y=-2.5, label="SLA", color="red")+
  geom_text(size = 4, x=2.6, y=2, label="WC", color="red")+
  geom_text(size = 4, x=-2.3, y=3, label="gs", color="red")+
  geom_text(size = 4, x=-2.4, y=2.6, label="A", color="red")+
  theme_classic()
ggsave("pre_plot.pdf", width = 7, height = 7, units = "in")


pc_peak$rotation[,1]<-pc_peak$rotation[,1]*(-1)
pc_peak$x[,1] <- pc_peak$x[,1]*(-1)

peak_plot <- autoplot(pc_peak, data = peak, scale=0,
                     loadings = TRUE, loadings.colour = 'black',
                     loadings.label = F, loadings.label.size = 3)
peak_plot <- peak_plot +
  xlim(-4, 4)+
  ylim(-4, 4)+
  geom_text(size=4.5, x=-3.6, y=4, label="Peak", color="red")+
  geom_text(size = 4, x=-1.8, y=-2.7, label="DF", color="red")+
  geom_text(size = 4, x=-2.7, y=-2.3, label="SLA", color="red")+
  geom_text(size = 4, x=3, y=1.2, label="WC", color="red")+
  geom_text(size = 4, x=-2.3, y=3, label="gs", color="red")+
  geom_text(size = 4, x=-2.6, y=2.7, label="A", color="red")+
  theme_classic()
ggsave("peak_plot.pdf", width = 7, height = 7, units = "in")
plot_grid(pre_plot,peak_plot,ncol = 2)




