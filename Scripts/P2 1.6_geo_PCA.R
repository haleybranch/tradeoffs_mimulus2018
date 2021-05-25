# PCA of each region
library(tidyverse)
library(vegan)
library(devtools)
library("factoextra")
library(finalfit)
library(dplyr)
library(pander)
library(cowplot)

# Read in pre and peak datasets 
pre.N <- read.csv("Data/pre.N.csv")
pre.C <- read.csv("Data/pre.C.csv")
pre.S <- read.csv("Data/pre.S.csv")
peak.N <- read.csv("Data/peak.N.csv")
peak.C <- read.csv("Data/peak.C.csv")
peak.S <- read.csv("Data/peak.S.csv")
all.N <- rbind(pre.N, peak.N)
all.C <- rbind(pre.C, peak.C)
all.S <- rbind(pre.S, peak.S)

#################### PCA of PRE NORTH data ######################
pca_pre.N <- prcomp(na.omit(pre.N[,c("Experiment_Date","SLA","Water_Content",
                                 "Stomatal_Conductance","Assimilation")]), scale=T)
summary(pca_pre.N)
pca_pre.N$rotation[,2]
pca_pre.N$rotation[,2]<-pca_pre.N$rotation[,2]*(-1)
pca_pre.N$x[,2] <- pca_pre.N$x[,2]*(-1)

h <- autoplot(pca_pre.N, data = pre.N,scale=0,
              loadings = TRUE, loadings.colour = 'black',
              loadings.label = F, loadings.label.size = 3)
b1<- h +
  xlim(-4, 4)+
  ylim(-4,4)+
  #scale_color_manual(values= c("Peak"="#FF3333", "Pre"="#3399FF"))+
  geom_text(size=4.5, x=-2.6, y=4, label="North Pre", color="red")+
  geom_text(size = 4, x=-2.3, y=-0.03, label="DF", color="red")+
  geom_text(size = 4, x=-2.3, y=-2.8, label="SLA", color="red")+
  geom_text(size = 4, x=2.5, y=1.8, label="WC", color="red")+
  geom_text(size = 4, x=-1.8, y=2.8, label="gs", color="red")+
  geom_text(size = 4, x=-2.3, y=2.3, label="A", color="red")+
  theme_classic()

ggsave("b1.pdf", width = 7, height = 7, units = "in")

######### to extract the e.vectors for Pre North 

#indpre_N <- get_pca(pca_pre.N, "ind")
#Npre.ind <- indpre_N$coord
#Npre.ind <- as.data.frame(Npre.ind)
#Npre.pc <- cbind(pre.N, Npre.ind)
#Npre.pc$Dim.1 <- (Npre.pc$Dim.1)*-1
#Npre.pc$Dim.2 <- (Npre.pc$Dim.2)*-1

#varpre_N <- get_pca(pca_pre.N, "var")
#Npre.var <- varpre_N$coord
#Npre.var <- as.data.frame(Npre.var)
#Npre.var$Dim.1 <- (Npre.var$Dim.1)*-1
#Npre.var$Dim.2 <- (Npre.var$Dim.2)*-1


#ggplot of Pre North pc values
#b1 <- ggplot(data=Npre.pc, aes(Dim.1, Dim.2))+
 # geom_point()+
  #xlab("PC1 (55%)")+
  #ylab("PC2 (22%)")+
  #xlim(-4, 4)+
  #ylim(-4,4)+
  #scale_color_manual(values= c("Peak"="#FF3333", "Pre"="#3399FF"))+
  #theme_classic()


############# PCA of PEAK NORTH data #########################
pca_peak.N <- prcomp(na.omit(peak.N[,c("Experiment_Date","SLA","Water_Content",
                                       "Stomatal_Conductance","Assimilation")]), scale=T)
summary(pca_peak.N)

######### to extract the e.vectors for Peak North 

#indpeak_N <- get_pca(pca_peak.N, "ind")
#Npeak.ind <- indpeak_N$coord
#Npeak.ind <- as.data.frame(Npeak.ind)

#varpeak_N <- get_pca(pca_peak.N, "var")
#Npeak.var <- varpeak_N$coord
#Npeak.var <- as.data.frame(Npeak.var)


#Npeak.na <- na.omit(peak.N)
#Npeak.pc <- cbind(Npeak.na, Npeak.ind)

#Calculate extension of y component of each variable loading
#Nfty <- (Npeak.var[1,2]/Npeak.var[1,1])*(-3)
#Nslay <- (Npeak.var[2,2]/Npeak.var[2,1])*(-3)
#Nwcy <- (Npeak.var[3,2]/Npeak.var[3,1])*(3)
#Ngsy <- (Npeak.var[4,2]/Npeak.var[4,1])*(-2.5)
#Nay <- (Npeak.var[5,2]/Npeak.var[5,1])*(-2.5)

#ggplot of Peak North pc values

b2 <- autoplot(pca_peak.N, data = peak.N,scale=0,
              loadings = TRUE, loadings.colour = 'black',
              loadings.label = F, loadings.label.size = 3)
b2<- b2 +
  xlim(-4, 4)+
  ylim(-4,4)+
  #scale_color_manual(values= c("Peak"="#FF3333", "Pre"="#3399FF"))+
  geom_text(size=4.5, x=-2.6, y=4, label="North Peak", color="red")+
  geom_text(size = 4, x=-1.5, y=-2, label="DF", color="red")+
  geom_text(size = 4, x=-2.3, y=-1.5, label="SLA", color="red")+
  geom_text(size = 4, x=2.2, y=1.1, label="WC", color="red")+
  geom_text(size = 4, x=-1.9, y=2.4, label="gs", color="red")+
  geom_text(size = 4, x=-1.7, y=2.5, label="A", color="red")+
  theme_classic()

ggsave("b2.pdf", width = 7, height = 7, units = "in")

############# #PCA of PRE CENTRE data ########################
pca_pre.C <- prcomp(na.omit(pre.C[,c("Experiment_Date","SLA","Water_Content",
                                     "Stomatal_Conductance","Assimilation")]), scale=T)
summary(pca_pre.C)
pca_pre.C$rotation[,2]
pca_pre.C$rotation[,2]<-pca_pre.C$rotation[,2]*(-1)
pca_pre.C$x[,2] <- pca_pre.C$x[,2]*(-1)

######### to extract the e.vectors for Pre Centre 

#indpre_C <- get_pca(pca_pre.C, "ind")
#Cpre.ind <- indpre_C$coord
#Cpre.ind <- as.data.frame(Cpre.ind)
#Npre.pc$Dim.1 <- (Npre.pc$Dim.1)*-1
#Cpre.ind$Dim.2 <- (Cpre.ind$Dim.2)*-1

#varpre_C <- get_pca(pca_pre.C, "var")
#Cpre.var <- varpre_C$coord
#Cpre.var <- as.data.frame(Cpre.var)
#Npre.var$Dim.1 <- (Npre.var$Dim.1)*-1
#Cpre.var$Dim.2 <- (Cpre.var$Dim.2)*-1


#Cpre.na <- na.omit(pre.C)
#Cpre.pc <- cbind(Cpre.na, Cpre.ind)

#Cprefty <- (Cpre.var[1,2]/Cpre.var[1,1])*(-4)
#Cpreslay <- (Cpre.var[2,2]/Cpre.var[2,1])*(-3)
#Cprewcy <- (Cpre.var[3,2]/Cpre.var[3,1])*(3)
#Cpregsy <- (Cpre.var[4,2]/Cpre.var[4,1])*(-2.5)
#Cpreay <- (Cpre.var[5,2]/Cpre.var[5,1])*(-2.5)

#ggplot of Peak North pc values

b3 <- autoplot(pca_pre.C, data = pre.C,scale=0,
               loadings = TRUE, loadings.colour = 'black',
               loadings.label = F, loadings.label.size = 3)
b3<- b3 +
  xlim(-4, 4)+
  ylim(-4,4)+
  #scale_color_manual(values= c("Peak"="#FF3333", "Pre"="#3399FF"))+
  geom_text(size=4.5, x=-2.6, y=4, label="Centre Pre", color="red")+
  geom_text(size = 4, x=-2, y=-1.2, label="DF", color="red")+
  geom_text(size = 4, x=-2, y=-1.8, label="SLA", color="red")+
  geom_text(size = 4, x=2.2, y=2.1, label="WC", color="red")+
  geom_text(size = 4, x=-2, y=2.8, label="gs", color="red")+
  geom_text(size = 4, x=-2.2, y=2.3, label="A", color="red")+
  theme_classic()

ggsave("b3.pdf", width = 7, height = 7, units = "in")

################### PCA of PEAK CENTRE data ######################
pca_peak.C <- prcomp(na.omit(peak.C[,c("Experiment_Date","SLA","Water_Content",
                                       "Stomatal_Conductance","Assimilation")]), scale=T)
summary(pca_peak.C)
pca_peak.C$rotation[,2]<-pca_peak.C$rotation[,2]*(-1)
pca_peak.C$x[,2] <- pca_peak.C$x[,2]*(-1)
pca_peak.C$rotation[,1]<-pca_peak.C$rotation[,1]*(-1)
pca_peak.C$x[,1] <- pca_peak.C$x[,1]*(-1)



#ggplot of Peak North pc values

b4 <- autoplot(pca_peak.C, data = peak.C,scale=0,
               loadings = TRUE, loadings.colour = 'black',
               loadings.label = F, loadings.label.size = 3)

b4 <- b4 + 
  xlim(-4,4)+
  ylim(-4,4)+
  geom_text(size=4.5, x=-2.4, y=4, label="Centre Peak", color="red")+
  geom_text(size = 4, x=-1.6, y=1.7, label="DF", color="red")+
  geom_text(size = 4, x=-2.3, y=-3.1, label="SLA", color="red")+
  geom_text(size = 4, x=2.6, y=2.1, label="WC", color="red")+
  geom_text(size = 4, x=-2.3, y=2.6, label="gs", color="red")+
  geom_text(size = 4, x=-2.4, y=1.8, label="A", color="red")+
  theme_classic()
b4
ggsave("b4.pdf", width = 7, height = 7, units = "in")

################## PCA of PRE SOUTH data ######################
pca_pre.S <- prcomp(na.omit(pre.S[,c("Experiment_Date","SLA","Water_Content",
                                     "Stomatal_Conductance","Assimilation")]), scale=T)
summary(pca_pre.S)
pca_pre.S$rotation[,2]<-pca_pre.S$rotation[,2]*(-1)
pca_pre.S$x[,2] <- pca_pre.S$x[,2]*(-1)

#ggplot of pre South pc values

b5 <- autoplot(pca_pre.S, data = pre.S,scale=0,
               loadings = TRUE, loadings.colour = 'black',
               loadings.label = F, loadings.label.size = 3)

b5 <- b5 + 
  xlim(-4,4)+
  ylim(-4,4)+
  geom_text(size=4.5, x=-2.4, y=4, label="South Pre", color="red")+
  geom_text(size = 4, x=-1.6, y=-2.1, label="DF", color="red")+
  geom_text(size = 4, x=-2.1, y=-1.8, label="SLA", color="red")+
  geom_text(size = 4, x=2.4, y=0.8, label="WC", color="red")+
  geom_text(size = 4, x=-1.8, y=2.6, label="gs", color="red")+
  geom_text(size = 4, x=-2, y=2.3, label="A", color="red")+
  theme_classic()
b5
ggsave("b5.pdf", width = 7, height = 7, units = "in")


################## PCA of PEAK SOUTH data ####################
pca_peak.S <- prcomp(na.omit(peak.S[,c("Experiment_Date","SLA","Water_Content",
                                       "Stomatal_Conductance","Assimilation")]), scale=T)
summary(pca_peak.S)

#ggplot of Peak North pc values
pca_peak.S$rotation[,1]<-pca_peak.S$rotation[,1]*(-1)
pca_peak.S$x[,1] <- pca_peak.S$x[,1]*(-1)

#ggplot of pre South pc values

b6 <- autoplot(pca_peak.S, data = peak.S,scale=0,
               loadings = TRUE, loadings.colour = 'black',
               loadings.label = F, loadings.label.size = 3)

b6 <- b6 + 
  xlim(-4,4)+
  ylim(-4,4)+
  geom_text(size=4.5, x=-2.4, y=4, label="South Peak", color="red")+
  geom_text(size = 4, x=-1.6, y=-3, label="DF", color="red")+
  geom_text(size = 4, x=-3.4, y=-3, label="SLA", color="red")+
  geom_text(size = 4, x=3.6, y=1.2, label="WC", color="red")+
  geom_text(size = 4, x=-3, y=3.4, label="gs", color="red")+
  geom_text(size = 4, x=-3, y=3, label="A", color="red")+
  theme_classic()
b6
ggsave("b6.pdf", width = 7, height = 7, units = "in")


############### stitch together in cowplot ####################################
#call each pdf figure 
plot_grid(b1,b2,b3,b4,b5,b6,ncol = 2)

