##### visualization of the raw data pairwise scatterplots 
#read in packages
library(tidyverse)
library(ggplot2)
library(ggforce)
library(car)
library(cowplot)

# Read in data
all <- read.csv("Data/all.csv")
#place <- read.csv("Data/placements.csv")
South <- all %>% filter(Region == "3.South") #subset of data
South <- mutate_if(South, is.character, stringr::str_replace_all, pattern = "Pre", replacement = "1")
South <- mutate_if(South, is.character, stringr::str_replace_all, pattern = "Peak", replacement = "2")
South$Period <- as.factor(South$Period)

#calculate ellipses
elli_Period <- dataEllipse(South$Experiment_Date, South$Water_Content, South$Period, group.labels=South$Period,
                           weights, log="", levels=0.95, center.pch=19, 
                           center.cex=1.5, draw=TRUE, segments=51, 
                           robust=FALSE, xlab="Date of Flowering", ylab="Water Content", 
                           lwd=2, fill=FALSE, fill.alpha=0.3, grid=TRUE, id=FALSE) 
df1 <- data.frame(elli_Period$'1')
df1[,3] <- "Pre"

df2 <- data.frame(elli_Period$`2`)
df2[,3] <- "Peak"

df <- data.frame(rbind(df1,df2))


### DF by WC
df.wc <- ggplot(South, aes(Experiment_Date,Water_Content))+
  geom_point() +
  theme_classic() +  
  geom_path(data=df, aes(x=x, y=y,colour=V3), size=1) +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0),
        axis.text.y = element_text(size=14,face="bold"),
        axis.title.x = element_text(color="black", size=16, vjust = 0, face="bold"),
        axis.title.y = element_text(color="black", size=16,vjust = 0, face="bold"))
df.wc 

#### DF by Ass
elli2_Period <- dataEllipse(South$Experiment_Date, South$Assimilation, South$Period, group.labels=South$Period,
                            weights, log="", levels=0.95, center.pch=19, 
                            center.cex=1.5, draw=FALSE, segments=51, 
                            robust=FALSE, xlab="Date of Flowering", ylab="Assimilation", 
                            lwd=2, fill=FALSE, fill.alpha=0.3, grid=TRUE, id=FALSE) 

dfA <- data.frame(elli2_Period$'1')
dfA[,3] <- "Pre"

dfB <- data.frame(elli2_Period$`2`)
dfB[,3] <- "Peak"

df2 <- data.frame(rbind(dfA,dfB))

df.ass <- ggplot(South, aes(Experiment_Date,Assimilation))+
  geom_point() +
  theme_classic() +  
  geom_path(data=df2, aes(x=x, y=y,colour=V3), size=1) +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0),
        axis.text.y = element_text(size=14,face="bold"),
        axis.title.x = element_text(color="black", size=16, vjust = 0, face="bold"),
        axis.title.y = element_text(color="black", size=16,vjust = 0, face="bold"))
df.ass 

#### DF by gs
elli3_Period <- dataEllipse(South$Experiment_Date, South$Stomatal_Conductance, South$Period, group.labels=South$Period,
                            weights, log="", levels=0.95, center.pch=19, 
                            center.cex=1.5, draw=FALSE, segments=51, 
                            robust=FALSE, xlab="Date of Flowering", ylab="Stomatal Conductance", 
                            lwd=2, fill=FALSE, fill.alpha=0.3, grid=TRUE, id=FALSE) 

dfA <- data.frame(elli3_Period$'1')
dfA[,3] <- "Pre"

dfB <- data.frame(elli3_Period$`2`)
dfB[,3] <- "Peak"

df3 <- data.frame(rbind(dfA,dfB))

df.gs <- ggplot(South, aes(Experiment_Date,Stomatal_Conductance))+
  geom_point() +
  theme_classic() +  
  geom_path(data=df3, aes(x=x, y=y,colour=V3), size=1) +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0),
        axis.text.y = element_text(size=14,face="bold"),
        axis.title.x = element_text(color="black", size=16, vjust = 0, face="bold"),
        axis.title.y = element_text(color="black", size=16,vjust = 0, face="bold"))
df.gs 

#### DF by SLA
elli4_Period <- dataEllipse(South$Experiment_Date, South$SLA, South$Period, group.labels=South$Period,
                            weights, log="", levels=0.95, center.pch=19, 
                            center.cex=1.5, draw=FALSE, segments=51, 
                            robust=FALSE, xlab="Date of Flowering", ylab="SLA", 
                            lwd=2, fill=FALSE, fill.alpha=0.3, grid=TRUE, id=FALSE) 

dfA <- data.frame(elli4_Period$'1')
dfA[,3] <- "Pre"

dfB <- data.frame(elli4_Period$`2`)
dfB[,3] <- "Peak"

df4 <- data.frame(rbind(dfA,dfB))

df.sla <- ggplot(South, aes(Experiment_Date,SLA))+
  geom_point() +
  theme_classic() +  
  geom_path(data=df4, aes(x=x, y=y,colour=V3), size=1) +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0),
        axis.text.y = element_text(size=14,face="bold"),
        axis.title.x = element_text(color="black", size=16, vjust = 0, face="bold"),
        axis.title.y = element_text(color="black", size=16,vjust = 0, face="bold"))
df.sla

#### WC by Ass
elli5_Period <- dataEllipse(South$Water_Content, South$Assimilation, South$Period, group.labels=South$Period,
                            weights, log="", levels=0.95, center.pch=19, 
                            center.cex=1.5, draw=FALSE, segments=51, 
                            robust=FALSE, xlab="Water Content", ylab="Assimilation", 
                            lwd=2, fill=FALSE, fill.alpha=0.3, grid=TRUE, id=FALSE) 

dfA <- data.frame(elli5_Period$'1')
dfA[,3] <- "Pre"

dfB <- data.frame(elli5_Period$`2`)
dfB[,3] <- "Peak"

df5 <- data.frame(rbind(dfA,dfB))

wc.ass <- ggplot(South, aes(Water_Content,Assimilation))+
  geom_point() +
  theme_classic() +  
  geom_path(data=df5, aes(x=x, y=y,colour=V3), size=1) +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0),
        axis.text.y = element_text(size=14,face="bold"),
        axis.title.x = element_text(color="black", size=16, vjust = 0, face="bold"),
        axis.title.y = element_text(color="black", size=16,vjust = 0, face="bold"))
wc.ass 

#### WC by gs
elli6_Period <- dataEllipse(South$Water_Content, South$Stomatal_Conductance, South$Period, group.labels=South$Period,
                            weights, log="", levels=0.95, center.pch=19, 
                            center.cex=1.5, draw=FALSE, segments=51, 
                            robust=FALSE, xlab="Water Content", ylab="Stomatal Conductance", 
                            lwd=2, fill=FALSE, fill.alpha=0.3, grid=TRUE, id=FALSE) 

dfA <- data.frame(elli6_Period$'1')
dfA[,3] <- "Pre"

dfB <- data.frame(elli6_Period$`2`)
dfB[,3] <- "Peak"

df6 <- data.frame(rbind(dfA,dfB))

wc.gs <- ggplot(South, aes(Water_Content,Stomatal_Conductance))+
  geom_point() +
  theme_classic() +  
  geom_path(data=df6, aes(x=x, y=y,colour=V3), size=1) +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0),
        axis.text.y = element_text(size=14,face="bold"),
        axis.title.x = element_text(color="black", size=16, vjust = 0, face="bold"),
        axis.title.y = element_text(color="black", size=16,vjust = 0, face="bold"))
wc.gs 

#### WC by SLA
elli7_Period <- dataEllipse(South$Water_Content, South$SLA, South$Period, group.labels=South$Period,
                            weights, log="", levels=0.95, center.pch=19, 
                            center.cex=1.5, draw=FALSE, segments=51, 
                            robust=FALSE, xlab="Water Content", ylab="SLA", 
                            lwd=2, fill=FALSE, fill.alpha=0.3, grid=TRUE, id=FALSE) 

dfA <- data.frame(elli7_Period$'1')
dfA[,3] <- "Pre"

dfB <- data.frame(elli7_Period$`2`)
dfB[,3] <- "Peak"

df7 <- data.frame(rbind(dfA,dfB))

wc.sla <- ggplot(South, aes(Water_Content,SLA))+
  geom_point() +
  theme_classic() +  
  geom_path(data=df7, aes(x=x, y=y,colour=V3), size=1) +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0),
        axis.text.y = element_text(size=14,face="bold"),
        axis.title.x = element_text(color="black", size=16, vjust = 0, face="bold"),
        axis.title.y = element_text(color="black", size=16,vjust = 0, face="bold"))
wc.sla 

#### SLA by Ass
elli8_Period <- dataEllipse(South$SLA, South$Assimilation, South$Period, group.labels=South$Period,
                            weights, log="", levels=0.95, center.pch=19, 
                            center.cex=1.5, draw=FALSE, segments=51, 
                            robust=FALSE, xlab="SLA", ylab="Assimilation", 
                            lwd=2, fill=FALSE, fill.alpha=0.3, grid=TRUE, id=FALSE) 

dfA <- data.frame(elli8_Period$'1')
dfA[,3] <- "Pre"

dfB <- data.frame(elli8_Period$`2`)
dfB[,3] <- "Peak"

df8 <- data.frame(rbind(dfA,dfB))

sla.ass <- ggplot(South, aes(SLA,Assimilation))+
  geom_point() +
  theme_classic() +  
  geom_path(data=df8, aes(x=x, y=y,colour=V3), size=1) +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0),
        axis.text.y = element_text(size=14,face="bold"),
        axis.title.x = element_text(color="black", size=16, vjust = 0, face="bold"),
        axis.title.y = element_text(color="black", size=16,vjust = 0, face="bold"))
sla.ass 


#### gs by sla
elli9_Period <- dataEllipse(South$Stomatal_Conductance, South$SLA, South$Period, group.labels=South$Period,
                            weights, log="", levels=0.95, center.pch=19, 
                            center.cex=1.5, draw=FALSE, segments=51, 
                            robust=FALSE, xlab="Stomatal Conductance", ylab="SLA", 
                            lwd=2, fill=FALSE, fill.alpha=0.3, grid=TRUE, id=FALSE) 

dfA <- data.frame(elli9_Period$'1')
dfA[,3] <- "Pre"

dfB <- data.frame(elli9_Period$`2`)
dfB[,3] <- "Peak"

df9 <- data.frame(rbind(dfA,dfB))

gs.sla <- ggplot(South, aes(Stomatal_Conductance,SLA))+
  geom_point() +
  theme_classic() +  
  geom_path(data=df9, aes(x=x, y=y,colour=V3), size=1) +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0),
        axis.text.y = element_text(size=14,face="bold"),
        axis.title.x = element_text(color="black", size=16, vjust = 0, face="bold"),
        axis.title.y = element_text(color="black", size=16,vjust = 0, face="bold"))
gs.sla 



### gs by ASS
elli10_Period <- dataEllipse(South$Stomatal_Conductance, South$Assimilation, South$Period, group.labels=South$Period,
                             weights, log="", levels=0.95, center.pch=19, 
                             center.cex=1.5, draw=FALSE, segments=51, 
                             robust=FALSE, xlab="Stomatal Conductance", ylab="SLA", 
                             lwd=2, fill=FALSE, fill.alpha=0.3, grid=TRUE, id=FALSE) 

dfA <- data.frame(elli10_Period$'1')
dfA[,3] <- "Pre"

dfB <- data.frame(elli10_Period$`2`)
dfB[,3] <- "Peak"

df10 <- data.frame(rbind(dfA,dfB))

gs.ass <- ggplot(South, aes(Stomatal_Conductance,Assimilation))+
  geom_point() +
  theme_classic() +  
  geom_path(data=df10, aes(x=x, y=y,colour=V3), size=1) +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0),
        axis.text.y = element_text(size=14,face="bold"),
        axis.title.x = element_text(color="black", size=16, vjust = 0, face="bold"),
        axis.title.y = element_text(color="black", size=16,vjust = 0, face="bold"))
gs.ass

# cowplot export 10 by 16
plot_grid(df.sla, df.wc, df.ass, df.gs, sla.ass, gs.sla,
          wc.sla, wc.ass, wc.gs, gs.ass,  ncol=2)



