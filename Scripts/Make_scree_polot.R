#Make an nice scree plot
all <- read.csv("Data/all.csv", header=T) #Imports main datase
pc1 <- prcomp(na.omit(all[,c("Experiment_Date","Water_Content","SLA","Stomatal_Conductance","Assimilation")]), scale=T)
pc1_summary<-summary(pc1)
pc1_summary
biplot(pc1, scale=0, col=c("black", "red"), xlab = "PC1 (46%)", ylab="PC2 (21%)")
screeplot((pc1), type = "lines")

pc1_scree<-pc1_summary$importance[2,]
plot_scree<-data.frame()
plot_scree[1,1]<-"PC1"
plot_scree[2,1]<-"PC2"
plot_scree[3,1]<-"PC3"
plot_scree[4,1]<-"PC4"
plot_scree[5,1]<-"PC5"
plot_scree[1,2]<-pc1_scree[1]
plot_scree[2,2]<-pc1_scree[2]
plot_scree[3,2]<-pc1_scree[3]
plot_scree[4,2]<-pc1_scree[4]
plot_scree[5,2]<-pc1_scree[5]
colnames(plot_scree)<-c("PC_Axis", "Variance_Explained")
#Make Scree Plot
scree_bar<-ggplot(plot_scree, aes(x=PC_Axis, y=Variance_Explained))+ geom_bar(stat = "identity")+
  scale_y_continuous(name="Variance Explained")+ scale_x_discrete(name="PC_Axis")+theme_classic() 
scree_bar + theme(axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0),
                  axis.text.y = element_text(size=12,face="bold"),
                  axis.title.x = element_text(color="black", size=12, vjust = 0, face="bold"),
                  axis.title.y = element_text(color="black", size=12, face="bold",vjust = 1.5,hjust=0.45))

# Pre all and peak all
pre <- read.csv("Data/pre.csv", header=T) #Imports main datase
pc2 <- prcomp(na.omit(pre[,c("Experiment_Date","Water_Content","SLA","Stomatal_Conductance","Assimilation")]), scale=T)
pc2_summary<-summary(pc2)
pc2_summary
biplot(pc2, scale=0, col=c("black", "red"), xlab = "PC1 (56%)", ylab="PC2 (21%)")
screeplot((pc2), type = "lines")

peak <- read.csv("Data/peak.csv", header=T) #Imports main datase
pc3 <- prcomp(na.omit(peak[,c("Experiment_Date","Water_Content","SLA","Stomatal_Conductance","Assimilation")]), scale=T)
pc3_summary<-summary(pc3)
pc3_summary
biplot(pc3, scale=0, col=c("black", "red"), xlab = "PC1 (53%)", ylab="PC2 (21%)")
screeplot((pc3), type = "lines")

# Regional North
N.pre <- read.csv("Data/pre.N.csv", header=T) #Imports main datase
pc4 <- prcomp(na.omit(N.pre[,c("Experiment_Date","Water_Content","SLA","Stomatal_Conductance","Assimilation")]), scale=T)
pc4_summary<-summary(pc4)
pc4_summary
biplot(pc4, scale=0, col=c("black", "red"), xlab = "PC1 (46%)", ylab="PC2 (21%)")
screeplot((pc4), type = "lines")

N.peak <- read.csv("Data/peak.N.csv", header=T) #Imports main datase
pc5 <- prcomp(na.omit(N.peak[,c("Experiment_Date","Water_Content","SLA","Stomatal_Conductance","Assimilation")]), scale=T)
pc5_summary<-summary(pc5)
pc5_summary
biplot(pc5, scale=0, col=c("black", "red"), xlab = "PC1 (56%)", ylab="PC2 (23%)")
screeplot((pc5), type = "lines")

# Regional Centre
C.pre <- read.csv("Data/pre.C.csv", header=T) #Imports main datase
pc6 <- prcomp(na.omit(C.pre[,c("Experiment_Date","Water_Content","SLA","Stomatal_Conductance","Assimilation")]), scale=T)
pc6_summary<-summary(pc6)
pc6_summary
biplot(pc6, scale=0, col=c("black", "red"), xlab = "PC1 (60%)", ylab="PC2 (21%)")
screeplot((pc6), type = "lines")

C.peak <- read.csv("Data/peak.C.csv", header=T) #Imports main datase
pc7 <- prcomp(na.omit(C.peak[,c("Experiment_Date","Water_Content","SLA","Stomatal_Conductance","Assimilation")]), scale=T)
pc7_summary<-summary(pc7)
pc7_summary
biplot(pc7, scale=0, col=c("black", "red"), xlab = "PC1 (57%)", ylab="PC2 (17%)")
screeplot((pc7), type = "lines")
## ***** PC2 has an eigenvalue of less than 1********

# Regional South
S.pre <- read.csv("Data/pre.S.csv", header=T) #Imports main datase
pc8 <- prcomp(na.omit(S.pre[,c("Experiment_Date","Water_Content","SLA","Stomatal_Conductance","Assimilation")]), scale=T)
pc8_summary<-summary(pc8)
pc8_summary
biplot(pc8, scale=0, col=c("black", "red"), xlab = "PC1 (58%)", ylab="PC2 (21%)")
screeplot((pc8), type = "lines")

S.peak <- read.csv("Data/peak.S.csv", header=T) #Imports main datase
pc9 <- prcomp(na.omit(S.peak[,c("Experiment_Date","Water_Content","SLA","Stomatal_Conductance","Assimilation")]), scale=T)
pc9_summary<-summary(pc9)
pc9_summary
biplot(pc9, scale=0, col=c("black", "red"), xlab = "PC1 (48%)", ylab="PC2 (26%)")
screeplot((pc9), type = "lines")
