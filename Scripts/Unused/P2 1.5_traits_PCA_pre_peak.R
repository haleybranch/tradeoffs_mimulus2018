# PCA 
library(tidyverse)
library(vegan)
library(devtools)
library("factoextra")

# Read in pre and peak datasets 
pre <- read.csv("Data/pre.csv")
peak <- read.csv("Data/peak.csv")

#bind together
all <- read.csv("/Data/all.csv")

#PCA of all data
pc1_all <- prcomp(na.omit(all[,c("Experiment_Date","SLA","Water_Content",
                                 "Stomatal_Conductance","Assimilation")]), scale=T)
summary(pc1_all)

#biplot(pc1_all, scale=0, col=c("black", "red"), xlab = "PC1 (54%)", ylab="PC2 (21%)")

ind_all <- get_pca(pc1_all, "ind")
all.ind <- ind_all$coord
all.ind <- as.data.frame(all.ind)

var_all <- get_pca(pc1_all, "var")
all.var <- var_all$coord
all.var <- as.data.frame(all.var)

all.na <- na.omit(all)
all.pc <- cbind(all.na, all.ind)

#ggplot of pc values
b <- ggplot(data=all.pc, aes(Dim.1, Dim.2))+
  geom_point(aes(colour=Period))+
  xlab("PC1 (54%)")+
  ylab("PC2 (22%)")+
  scale_color_manual(values= c("Peak"="#FF3333", "Pre"="#3399FF"))+
  theme_classic()

b +
  geom_segment(aes(x=0, y=0, xend = -3, yend = -2.20086205))+ #flowering time
  geom_segment(aes(x=0, y=0, xend=-3, yend=-1.836009314)) + #SLA
  geom_segment(aes(x=0, y=0, xend=4, yend=1.17845849)) + #water content
  geom_segment(aes(x=0, y=0, xend=-3, yend=2.284833241)) + #stomatal conductance
  geom_segment(aes(x=0, y=0, xend=-3, yend=2.151326351)) + # assimilation 
  geom_text()

#biplots 
#Assess PCA relationships among response variables
#Pre
pc1_pre <- prcomp(na.omit(pre[,c("Experiment_Date","SLA","Water_Content",
                            "Stomatal_Conductance","Assimilation")]), scale=T)
summary(pc1_pre)

screeplot(pc1_pre, npcs=5, type="lines") #this would go into supplemental, may need to test significance of these angles 
biplot(pc1_pre, scale=0, col=c("black", "red"), xlab = "PC1 (54%)", ylab="PC2 (21%)")

# Extract the results for variables and individuals

ind <- get_pca(pc1_pre, "ind")
pre.ind <- ind$coord
pre.ind <- as.data.frame(pre.ind)

var <- get_pca(pc1_pre, "var")
pre.var <- var$coord
pre.var <- as.data.frame(pre.var)


#remove NA values from the pre dataset 
pre.na <- na.omit(pre)

#stitch pre.ind to pre.na
pre.pc <- cbind(pre.na, pre.ind)

#ggplot of pc values
b <- ggplot(data=pre.pc, aes(Dim.1, Dim.2))+
       geom_point(aes(colour=ID_Year))+
  xlab("PC1 (54%)")+
  ylab("PC2 (22%)")+
  scale_color_manual(values= c("S02_2011"="#FF3333", "S07_2011"="#FF3333",
                              "S08_2011"="#FFCC00","S10_2011"="#FFCC00","S11_2011"="#FF3333",
                              "S15_2010"= "#3399FF", "S16_2010"="#3399FF", "S17_2011"="#3399FF", "S18_2010"="#FFCC00",
                              "S29_2010"="#FFCC00", "S32_2010"="#FFCC00", "S36_2011"="#3399FF"))+
  theme_classic()

b + 
geom_segment(aes(x=0, y=0, xend=-4.44853, yend=-2, colour = "Flowering Time"))+
  geom_segment(aes(x=0, y=0, xend=-4.666497843, yend=-3, colour = "SLA"))+
 geom_segment(aes(x=0, y=0, xend=4.877790341, yend=2, colour = "Water Content"))+
geom_segment(aes(x=0, y=0, xend=-2.474314573, yend=2, colour = "Stomatal Conductance")) +
geom_segment(aes(x=0, y=0, xend=-3.398216279, yend=2.5, colour = "Assimilation"))

  

#Peak
pc1_peak <- prcomp(na.omit(peak[,c("Experiment_Date","SLA","Water_Content",
                                 "Stomatal_Conductance","Assimilation")]), scale=T)
summary(pc1_peak)

screeplot(pc1_peak, npcs=5, type="lines") #this would go into supplemental, may need to test significance of these angles 
biplot(pc1_peak, scale=0, col=c("black", "red"), xlab = "PC1 (53%)", ylab="PC2 (21%)")


# Extract the results for variables and individuals

ind2 <- get_pca(pc1_peak, "ind")
peak.ind <- ind2$coord
peak.ind <- as.data.frame(peak.ind)

var2 <- get_pca(pc1_peak, "var")
peak.var <- var2$coord
peak.var <- as.data.frame(peak.var)


#remove NA values from the pre dataset 
peak.na <- na.omit(peak)

#stitch pre.ind to pre.na
peak.pc <- cbind(peak.na, peak.ind)

d <- ggplot(data=peak.pc, aes(Dim.1, Dim.2))+
  geom_point(aes(colour=ID_Year))+
  xlab("PC1 (53%)")+
  ylab("PC2 (21%)")+
  scale_color_manual(values= c("S02_2014"="#FF3333", "S07_2014"="#FF3333",
                               "S08_2013"="#FFCC00","S10_2014"="#FFCC00","S11_2016"="#FF3333",
                               "S15_2015"= "#3399FF", "S16_2016"="#3399FF", "S17_2015"="#3399FF", "S18_2014"="#FFCC00",
                               "S29_2015"="#FFCC00", "S32_2014"="#FFCC00", "S36_2015"="#3399FF"))+
  theme_classic()

d + 
  geom_segment(aes(x=0, y=0, xend=-2.660433795, yend=-3, colour = "Flowering Time"))+
  geom_segment(aes(x=0, y=0, xend=-4.457441504, yend=-2.5, colour = "SLA"))+
  geom_segment(aes(x=0, y=0, xend=5.15969818, yend=1, colour = "Water Content"))+
  geom_segment(aes(x=0, y=0, xend=-2.887602693, yend=2, colour = "Stomatal Conductance")) +
  geom_segment(aes(x=0, y=0, xend=-2.948203031, yend=2, colour = "Assimilation"))


#ggsave("pdsi_1980.pdf", width = 12, height = 7, units = "in")

# with full dataset 

# PCA 

# Read in pre and peak datasets 
full <- read.csv("Data/y7.csv")

#biplots 
#Assess PCA relationships among response variables
#Pre
pc1_all <- prcomp(na.omit(full[,c("Experiment_Date","SLA","Water_Content",
                                 "Stomatal_Conductance","Assimilation")]), scale=T)
summary(pc1_all)

screeplot(pc1_all, npcs=5, type="lines") #this would go into supplemental, may need to test significance of these angles 
biplot(pc1_all, scale=0, col=c("black", "red"), xlab = "PC1 (52%)", ylab="PC2 (21%)")


ggplot(data=pc1_all, aes(Time))+
  geom_point(aes(colour=Time))+
  xlab("PC1 (54%)")+
  ylab("PC2 (22%)")+
  scale_color_manual(values= c("Peak"=))+
  theme_classic()

