##### visualization of the raw data pairwise scatterplots 
#read in packages
library(tidyverse)
library(ggplot2)
library(ggforce)
# Read in data
all <- read.csv("Data/all.csv")
place <- read.csv("Data/placements.csv")
all <- all %>% mutate(Region.period = paste(Region, Period, sep="_")) #Make Region.period variable
str(all)
all <- mutate_if(all, is.character, stringr::str_replace_all, pattern = "1.North_Pre", replacement = "1")
all <- mutate_if(all, is.character, stringr::str_replace_all, pattern = "1.North_Peak", replacement = "2")
all <- mutate_if(all, is.character, stringr::str_replace_all, pattern = "2.Center_Pre", replacement = "3")
all <- mutate_if(all, is.character, stringr::str_replace_all, pattern = "2.Center_Peak", replacement = "4")
all <- mutate_if(all, is.character, stringr::str_replace_all, pattern = "3.South_Pre", replacement = "5")
all <- mutate_if(all, is.character, stringr::str_replace_all, pattern = "3.South_Peak", replacement = "6")
all$Region.period <- as.factor(all$Region.period)

library(car)
elli <- dataEllipse(all$Experiment_Date, all$Water_Content, all$Region.period, group.labels=all$Region.period,
            weights, log="", levels=0.95, center.pch=19, 
            center.cex=1.5, draw=TRUE, segments=51, 
            robust=FALSE, xlab="Date of Flowering", ylab="Water Content", 
            lwd=2, fill=FALSE, fill.alpha=0.3, grid=TRUE, id=FALSE) 

df <- data.frame(rbind(elli$'1', elli$'2', elli$`3`, elli$`4`, elli$`5`, elli$`6`))

df <- cbind(df, place)

#drawing
library(ggplot2)
p <- ggplot(data=df, aes(x=x, y=y,colour=Place)) + geom_point(size=1.5, alpha=.6) +
  geom_path(data=df, aes(x=x, y=y,colour=Place), size=1)
p
### DF by WC
df.wc <- ggplot(all, aes(Experiment_Date,Water_Content))+
  geom_point() +
  #stat_ellipse(aes(x=Experiment_Date, y=Water_Content,color=Region.period),type = "norm") +
  theme_classic() +  
  geom_path(data=df, aes(x=x, y=y,colour=Place), size=1) +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0),
        axis.text.y = element_text(size=14,face="bold"),
        axis.title.x = element_text(color="black", size=16, vjust = 0, face="bold"),
        axis.title.y = element_text(color="black", size=16,vjust = 0, face="bold"))
df.wc 

#### DF by Ass
elli2 <- dataEllipse(all$Experiment_Date, all$Assimilation, all$Region.period, group.labels=all$Region.period,
                    weights, log="", levels=0.95, center.pch=19, 
                    center.cex=1.5, draw=FALSE, segments=51, 
                    robust=FALSE, xlab="Date of Flowering", ylab="Assimilation", 
                    lwd=2, fill=FALSE, fill.alpha=0.3, grid=TRUE, id=FALSE) 

df2 <- data.frame(rbind(elli2$'1', elli2$'2', elli2$`3`, elli2$`4`, elli2$`5`, elli2$`6`))

df2 <- cbind(df2, place)

df.ass <- ggplot(all, aes(Experiment_Date,Assimilation))+
  geom_point() +
  theme_classic() +  
  geom_path(data=df2, aes(x=x, y=y,colour=Place), size=1) +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0),
        axis.text.y = element_text(size=14,face="bold"),
        axis.title.x = element_text(color="black", size=16, vjust = 0, face="bold"),
        axis.title.y = element_text(color="black", size=16,vjust = 0, face="bold"))
df.ass 

#### DF by gs
elli3 <- dataEllipse(all$Experiment_Date, all$Stomatal_Conductance, all$Region.period, group.labels=all$Region.period,
                     weights, log="", levels=0.95, center.pch=19, 
                     center.cex=1.5, draw=FALSE, segments=51, 
                     robust=FALSE, xlab="Date of Flowering", ylab="Stomatal Conductance", 
                     lwd=2, fill=FALSE, fill.alpha=0.3, grid=TRUE, id=FALSE) 

df3 <- data.frame(rbind(elli3$'1', elli3$'2', elli3$`3`, elli3$`4`, elli3$`5`, elli3$`6`))

df3 <- cbind(df3, place)

df.gs <- ggplot(all, aes(Experiment_Date,Stomatal_Conductance))+
  geom_point() +
  theme_classic() +  
  geom_path(data=df3, aes(x=x, y=y,colour=Place), size=1) +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0),
        axis.text.y = element_text(size=14,face="bold"),
        axis.title.x = element_text(color="black", size=16, vjust = 0, face="bold"),
        axis.title.y = element_text(color="black", size=16,vjust = 0, face="bold"))
df.gs 

#### DF by SLA
elli4 <- dataEllipse(all$Experiment_Date, all$SLA, all$Region.period, group.labels=all$Region.period,
                     weights, log="", levels=0.95, center.pch=19, 
                     center.cex=1.5, draw=FALSE, segments=51, 
                     robust=FALSE, xlab="Date of Flowering", ylab="SLA", 
                     lwd=2, fill=FALSE, fill.alpha=0.3, grid=TRUE, id=FALSE) 

df4 <- data.frame(rbind(elli4$'1', elli4$'2', elli4$`3`, elli4$`4`, elli4$`5`, elli4$`6`))

df4 <- cbind(df4, place)

df.sla <- ggplot(all, aes(Experiment_Date,SLA))+
  geom_point() +
  theme_classic() +  
  geom_path(data=df4, aes(x=x, y=y,colour=Place), size=1) +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0),
        axis.text.y = element_text(size=14,face="bold"),
        axis.title.x = element_text(color="black", size=16, vjust = 0, face="bold"),
        axis.title.y = element_text(color="black", size=16,vjust = 0, face="bold"))
df.sla

#### WC by Ass
elli5 <- dataEllipse(all$Water_Content, all$Assimilation, all$Region.period, group.labels=all$Region.period,
                     weights, log="", levels=0.95, center.pch=19, 
                     center.cex=1.5, draw=FALSE, segments=51, 
                     robust=FALSE, xlab="Water Content", ylab="Assimilation", 
                     lwd=2, fill=FALSE, fill.alpha=0.3, grid=TRUE, id=FALSE) 

df5 <- data.frame(rbind(elli5$'1', elli5$'2', elli5$`3`, elli5$`4`, elli5$`5`, elli5$`6`))

df5 <- cbind(df5, place)

wc.ass <- ggplot(all, aes(Water_Content,Assimilation))+
  geom_point() +
  theme_classic() +  
  geom_path(data=df5, aes(x=x, y=y,colour=Place), size=1) +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0),
        axis.text.y = element_text(size=14,face="bold"),
        axis.title.x = element_text(color="black", size=16, vjust = 0, face="bold"),
        axis.title.y = element_text(color="black", size=16,vjust = 0, face="bold"))
wc.ass 

#### WC by gs
elli6 <- dataEllipse(all$Water_Content, all$Stomatal_Conductance, all$Region.period, group.labels=all$Region.period,
                     weights, log="", levels=0.95, center.pch=19, 
                     center.cex=1.5, draw=FALSE, segments=51, 
                     robust=FALSE, xlab="Water Content", ylab="Stomatal Conductance", 
                     lwd=2, fill=FALSE, fill.alpha=0.3, grid=TRUE, id=FALSE) 

df6 <- data.frame(rbind(elli6$'1', elli6$'2', elli6$`3`, elli6$`4`, elli6$`5`, elli6$`6`))

df6 <- cbind(df6, place)

wc.gs <- ggplot(all, aes(Water_Content,Stomatal_Conductance))+
  geom_point() +
  theme_classic() +  
  geom_path(data=df6, aes(x=x, y=y,colour=Place), size=1) +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0),
        axis.text.y = element_text(size=14,face="bold"),
        axis.title.x = element_text(color="black", size=16, vjust = 0, face="bold"),
        axis.title.y = element_text(color="black", size=16,vjust = 0, face="bold"))
wc.gs 

#### WC by SLA
elli7 <- dataEllipse(all$Water_Content, all$SLA, all$Region.period, group.labels=all$Region.period,
                     weights, log="", levels=0.95, center.pch=19, 
                     center.cex=1.5, draw=FALSE, segments=51, 
                     robust=FALSE, xlab="Water Content", ylab="SLA", 
                     lwd=2, fill=FALSE, fill.alpha=0.3, grid=TRUE, id=FALSE) 

df7 <- data.frame(rbind(elli7$'1', elli7$'2', elli7$`3`, elli7$`4`, elli7$`5`, elli7$`6`))

df7 <- cbind(df7, place)

wc.sla <- ggplot(all, aes(Water_Content,SLA))+
  geom_point() +
  theme_classic() +  
  geom_path(data=df7, aes(x=x, y=y,colour=Place), size=1) +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0),
        axis.text.y = element_text(size=14,face="bold"),
        axis.title.x = element_text(color="black", size=16, vjust = 0, face="bold"),
        axis.title.y = element_text(color="black", size=16,vjust = 0, face="bold"))
wc.sla 

#### SLA by Ass
elli9 <- dataEllipse(all$SLA, all$Assimilation, all$Region.period, group.labels=all$Region.period,
                     weights, log="", levels=0.95, center.pch=19, 
                     center.cex=1.5, draw=FALSE, segments=51, 
                     robust=FALSE, xlab="SLA", ylab="Assimilation", 
                     lwd=2, fill=FALSE, fill.alpha=0.3, grid=TRUE, id=FALSE) 

df9 <- data.frame(rbind(elli9$'1', elli9$'2', elli9$`3`, elli9$`4`, elli9$`5`, elli9$`6`))

df9 <- cbind(df9, place)

sla.ass <- ggplot(all, aes(SLA,Assimilation))+
  geom_point() +
  theme_classic() +  
  geom_path(data=df9, aes(x=x, y=y,colour=Place), size=1) +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0),
        axis.text.y = element_text(size=14,face="bold"),
        axis.title.x = element_text(color="black", size=16, vjust = 0, face="bold"),
        axis.title.y = element_text(color="black", size=16,vjust = 0, face="bold"))
sla.ass 


#### gs by sla
elli10 <- dataEllipse(all$Stomatal_Conductance, all$SLA, all$Region.period, group.labels=all$Region.period,
                     weights, log="", levels=0.95, center.pch=19, 
                     center.cex=1.5, draw=FALSE, segments=51, 
                     robust=FALSE, xlab="Stomatal Conductance", ylab="SLA", 
                     lwd=2, fill=FALSE, fill.alpha=0.3, grid=TRUE, id=FALSE) 

df10 <- data.frame(rbind(elli10$'1', elli10$'2', elli10$`3`, elli10$`4`, elli10$`5`, elli10$`6`))

df10 <- cbind(df10, place)

gs.sla <- ggplot(all, aes(Stomatal_Conductance,SLA))+
  geom_point() +
  theme_classic() +  
  geom_path(data=df10, aes(x=x, y=y,colour=Place), size=1) +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0),
        axis.text.y = element_text(size=14,face="bold"),
        axis.title.x = element_text(color="black", size=16, vjust = 0, face="bold"),
        axis.title.y = element_text(color="black", size=16,vjust = 0, face="bold"))
gs.sla 








##### First attempts below

### DF by Assimilation
df.ass <- ggplot(all, aes(Experiment_Date,Assimilation))+
  geom_point() +
  stat_ellipse(aes(x=Experiment_Date, y=Assimilation,color=Region.period),type = "norm") +
  # scale_color_manual(values= c("1.North"="#3399FF", "2.Center"="#FFCC00", "3.South"="#FF3333")) +
  theme_classic() +  
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0),
        axis.text.y = element_text(size=14,face="bold"),
        axis.title.x = element_text(color="black", size=16, vjust = 0, face="bold"),
        axis.title.y = element_text(color="black", size=16,vjust = 0, face="bold"))
df.ass

### DF by gs 
df.gs <- ggplot(all, aes(Experiment_Date,Stomatal_Conductance))+
  geom_point() +
  stat_ellipse(aes(x=Experiment_Date, y=Stomatal_Conductance,color=Region.period),type = "norm") +
  # scale_color_manual(values= c("1.North"="#3399FF", "2.Center"="#FFCC00", "3.South"="#FF3333")) +
  theme_classic() +  
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0),
        axis.text.y = element_text(size=14,face="bold"),
        axis.title.x = element_text(color="black", size=16, vjust = 0, face="bold"),
        axis.title.y = element_text(color="black", size=16,vjust = 0, face="bold"))
df.gs

### DF by SLA
df.sla <- ggplot(all, aes(Experiment_Date,SLA))+
  geom_point() +
  stat_ellipse(aes(x=Experiment_Date, y=SLA,color=Region.period),type = "norm") +
  # scale_color_manual(values= c("1.North"="#3399FF", "2.Center"="#FFCC00", "3.South"="#FF3333")) +
  theme_classic() +  
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0),
        axis.text.y = element_text(size=14,face="bold"),
        axis.title.x = element_text(color="black", size=16, vjust = 0, face="bold"),
        axis.title.y = element_text(color="black", size=16,vjust = 0, face="bold"))
df.sla

### WC by Assimilation
ass.wc <- ggplot(all, aes(Assimilation,Water_Content))+
  geom_point() +
  stat_ellipse(aes(x=Assimilation, y=Water_Content,color=Region.period),type = "norm") +
  # scale_color_manual(values= c("1.North"="#3399FF", "2.Center"="#FFCC00", "3.South"="#FF3333")) +
  theme_classic() +  
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0),
        axis.text.y = element_text(size=14,face="bold"),
        axis.title.x = element_text(color="black", size=16, vjust = 0, face="bold"),
        axis.title.y = element_text(color="black", size=16,vjust = 0, face="bold"))
ass.wc
### WC by gs
gs.wc <- ggplot(all, aes(Stomatal_Conductance,Water_Content))+
  geom_point() +
  stat_ellipse(aes(x=Stomatal_Conductance, y=Water_Content,color=Region.period),type = "norm") +
  # scale_color_manual(values= c("1.North"="#3399FF", "2.Center"="#FFCC00", "3.South"="#FF3333")) +
  theme_classic() +  
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0),
        axis.text.y = element_text(size=14,face="bold"),
        axis.title.x = element_text(color="black", size=16, vjust = 0, face="bold"),
        axis.title.y = element_text(color="black", size=16,vjust = 0, face="bold"))
gs.wc

### WC by SLA
SLA.wc <- ggplot(all, aes(SLA,Water_Content))+
  geom_point() +
  stat_ellipse(aes(x=SLA, y=Water_Content,color=Region.period),type = "norm") +
  # scale_color_manual(values= c("1.North"="#3399FF", "2.Center"="#FFCC00", "3.South"="#FF3333")) +
  theme_classic() +  
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0),
        axis.text.y = element_text(size=14,face="bold"),
        axis.title.x = element_text(color="black", size=16, vjust = 0, face="bold"),
        axis.title.y = element_text(color="black", size=16,vjust = 0, face="bold"))
SLA.wc

### Assimilation by gs
ass.gs <- ggplot(all, aes(Assimilation,Stomatal_Conductance))+
  geom_point() +
  stat_ellipse(aes(x=Assimilation, y=Stomatal_Conductance,color=Region.period),type = "norm") +
  # scale_color_manual(values= c("1.North"="#3399FF", "2.Center"="#FFCC00", "3.South"="#FF3333")) +
  theme_classic() +  
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0),
        axis.text.y = element_text(size=14,face="bold"),
        axis.title.x = element_text(color="black", size=16, vjust = 0, face="bold"),
        axis.title.y = element_text(color="black", size=16,vjust = 0, face="bold"))
ass.gs
### Assimilation by SLA
ass.SLA <- ggplot(all, aes(Assimilation,SLA))+
  geom_point() +
  stat_ellipse(aes(x=Assimilation, y=SLA,color=Region.period),type = "norm") +
  # scale_color_manual(values= c("1.North"="#3399FF", "2.Center"="#FFCC00", "3.South"="#FF3333")) +
  theme_classic() +  
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0),
        axis.text.y = element_text(size=14,face="bold"),
        axis.title.x = element_text(color="black", size=16, vjust = 0, face="bold"),
        axis.title.y = element_text(color="black", size=16,vjust = 0, face="bold"))
ass.SLA

### gs by SLA 
SLA.gs <- ggplot(all, aes(SLA,Stomatal_Conductance))+
  geom_point() +
  stat_ellipse(aes(x=SLA, y=Stomatal_Conductance,color=Region.period),type = "norm") +
  # scale_color_manual(values= c("1.North"="#3399FF", "2.Center"="#FFCC00", "3.South"="#FF3333")) +
  theme_classic() +  
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0),
        axis.text.y = element_text(size=14,face="bold"),
        axis.title.x = element_text(color="black", size=16, vjust = 0, face="bold"),
        axis.title.y = element_text(color="black", size=16,vjust = 0, face="bold"))
SLA.gs
