##### visualization of the raw data pairwise scatterplots 
#read in packages
library(tidyverse)
library(ggplot2)
library(ggforce)
library(car)
library(cowplot)

# Read in data
all <- read.csv("Data/all.csv")
all <- all %>%
  separate(BlockDrought, c("Block", "Treatment"), "_")
#place <- read.csv("Data/placements.csv")
Center <- all %>% filter(Region == "2.Center") #subset of data
Center <- mutate_if(Center, is.character, stringr::str_replace_all, pattern = "Pre", replacement = "1")
Center <- mutate_if(Center, is.character, stringr::str_replace_all, pattern = "Peak", replacement = "2")
Center$Period <- as.factor(Center$Period)

Center.preW <- filter(Center, Period=="1")
Center.peakW  <- filter(Center, Period=="2")
Center.preD <- filter(Center, Period=="1")
Center.peakD  <- filter(Center, Period=="2")



##### Calculate the means 
SLA_mean_n1 <- 221.6480
DF_mean_n1 <- 101.27039
A_mean_n1 <- 11.703539
wc_mean_n1 <- 0.2467564
gs_mean_n1 <-  0.4673319
  
SLA_mean_n2 <- 181.5637
DF_mean_n2 <- 100.71453
A_mean_n2 <- 9.007431
wc_mean_n2 <- 0.2855723
gs_mean_n2<- 0.3262141

SLA_mean_n3 <- 230.9971
DF_mean_n3<- 95.96158
A_mean_n3 <- 12.234403
wc_mean_n3 <- 0.2265951
gs_mean_n3 <- 0.4178903

SLA_mean_n4 <- 181.3223
DF_mean_n4 <- 96.39451
A_mean_n4 <- 8.861988
wc_mean_n4 <- 0.2733646
gs_mean_n4<- 0.2330421
  
#calculate ellipses
elli_Period <- dataEllipse(Center$Experiment_Date, Center$Water_Content, Center$Period, group.labels=Center$Period,
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
df.wc <- ggplot(Center, aes(Experiment_Date,Water_Content))+
  geom_point() +
  theme_classic() +  
  geom_path(data=df, aes(x=x, y=y,colour=V3), size=1) +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0),
        axis.text.y = element_text(size=14,face="bold"),
        axis.title.x = element_text(color="black", size=16, vjust = 0, face="bold"),
        axis.title.y = element_text(color="black", size=16,vjust = 0, face="bold"))
df.wc <- df.wc +  geom_point(aes(x=DF_mean_n1, y=wc_mean_n1), size=3, colour="turquoise")#pre wet
df.wc <- df.wc +  geom_point(aes(x=DF_mean_n2, y=wc_mean_n2), size=3, colour="lightcoral") #peak wet
df.wc <- df.wc +  geom_point(aes(x=DF_mean_n3, y=wc_mean_n3), size=3, colour="turquoise4")#pre dry
df.wc <- df.wc +  geom_point(aes(x=DF_mean_n4, y=wc_mean_n4), size=3, colour="firebrick1")#peak dry

df.wc 

#### DF by Ass
elli2_Period <- dataEllipse(Center$Experiment_Date, Center$Assimilation, Center$Period, group.labels=Center$Period,
                            weights, log="", levels=0.95, center.pch=19, 
                            center.cex=1.5, draw=FALSE, segments=51, 
                            robust=FALSE, xlab="Date of Flowering", ylab="Assimilation", 
                            lwd=2, fill=FALSE, fill.alpha=0.3, grid=TRUE, id=FALSE) 

dfA <- data.frame(elli2_Period$'1')
dfA[,3] <- "Pre"

dfB <- data.frame(elli2_Period$`2`)
dfB[,3] <- "Peak"

df2 <- data.frame(rbind(dfA,dfB))

df.ass <- ggplot(Center, aes(Experiment_Date,Assimilation))+
  geom_point() +
  theme_classic() +  
  geom_path(data=df2, aes(x=x, y=y,colour=V3), size=1) +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0),
        axis.text.y = element_text(size=14,face="bold"),
        axis.title.x = element_text(color="black", size=16, vjust = 0, face="bold"),
        axis.title.y = element_text(color="black", size=16,vjust = 0, face="bold"))
df.ass <- df.ass +  geom_point(aes(x=DF_mean_n1, y=A_mean_n1), size=3, colour="turquoise")#pre wet
df.ass <- df.ass +  geom_point(aes(x=DF_mean_n2, y=A_mean_n2), size=3, colour="lightcoral") #peak wet
df.ass <- df.ass +  geom_point(aes(x=DF_mean_n3, y=A_mean_n3), size=3, colour="turquoise4")#pre dry
df.ass <- df.ass +  geom_point(aes(x=DF_mean_n4, y=A_mean_n4), size=3, colour="firebrick1")#peak dry


df.ass 

#### DF by gs
elli3_Period <- dataEllipse(Center$Experiment_Date, Center$Stomatal_Conductance, Center$Period, group.labels=Center$Period,
                            weights, log="", levels=0.95, center.pch=19, 
                            center.cex=1.5, draw=FALSE, segments=51, 
                            robust=FALSE, xlab="Date of Flowering", ylab="Stomatal Conductance", 
                            lwd=2, fill=FALSE, fill.alpha=0.3, grid=TRUE, id=FALSE) 

dfA <- data.frame(elli3_Period$'1')
dfA[,3] <- "Pre"

dfB <- data.frame(elli3_Period$`2`)
dfB[,3] <- "Peak"

df3 <- data.frame(rbind(dfA,dfB))

df.gs <- ggplot(Center, aes(Experiment_Date,Stomatal_Conductance))+
  geom_point() +
  theme_classic() +  
  geom_path(data=df3, aes(x=x, y=y,colour=V3), size=1) +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0),
        axis.text.y = element_text(size=14,face="bold"),
        axis.title.x = element_text(color="black", size=16, vjust = 0, face="bold"),
        axis.title.y = element_text(color="black", size=16,vjust = 0, face="bold"))
df.gs <- df.gs +  geom_point(aes(x=DF_mean_n1, y=gs_mean_n1), size=3, colour="turquoise")#pre wet
df.gs <- df.gs +  geom_point(aes(x=DF_mean_n2, y=gs_mean_n2), size=3, colour="lightcoral") #peak wet
df.gs <- df.gs +  geom_point(aes(x=DF_mean_n3, y=gs_mean_n3), size=3, colour="turquoise4")#pre dry
df.gs <- df.gs +  geom_point(aes(x=DF_mean_n4, y=gs_mean_n4), size=3, colour="firebrick1")#peak dry

df.gs 

#### DF by SLA
elli4_Period <- dataEllipse(Center$Experiment_Date, Center$SLA, Center$Period, group.labels=Center$Period,
                            weights, log="", levels=0.95, center.pch=19, 
                            center.cex=1.5, draw=FALSE, segments=51, 
                            robust=FALSE, xlab="Date of Flowering", ylab="SLA", 
                            lwd=2, fill=FALSE, fill.alpha=0.3, grid=TRUE, id=FALSE) 

dfA <- data.frame(elli4_Period$'1')
dfA[,3] <- "Pre"

dfB <- data.frame(elli4_Period$`2`)
dfB[,3] <- "Peak"

df4 <- data.frame(rbind(dfA,dfB))

df.sla <- ggplot(Center, aes(Experiment_Date,SLA))+
  geom_point() +
  theme_classic() +  
  geom_path(data=df4, aes(x=x, y=y,colour=V3), size=1) +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0),
        axis.text.y = element_text(size=14,face="bold"),
        axis.title.x = element_text(color="black", size=16, vjust = 0, face="bold"),
        axis.title.y = element_text(color="black", size=16,vjust = 0, face="bold"))
df.sla <- df.sla +  geom_point(aes(x=DF_mean_n1, y=SLA_mean_n1), size=3, colour="turquoise")#pre wet
df.sla <- df.sla +  geom_point(aes(x=DF_mean_n2, y=SLA_mean_n2), size=3, colour="lightcoral") #peak wet
df.sla <- df.sla +  geom_point(aes(x=DF_mean_n3, y=SLA_mean_n3), size=3, colour="turquoise4")#pre dry
df.sla <- df.sla +  geom_point(aes(x=DF_mean_n4, y=SLA_mean_n4), size=3, colour="firebrick1")#peak dry


df.sla

#### WC by Ass
elli5_Period <- dataEllipse(Center$Water_Content, Center$Assimilation, Center$Period, group.labels=Center$Period,
                            weights, log="", levels=0.95, center.pch=19, 
                            center.cex=1.5, draw=FALSE, segments=51, 
                            robust=FALSE, xlab="Water Content", ylab="Assimilation", 
                            lwd=2, fill=FALSE, fill.alpha=0.3, grid=TRUE, id=FALSE) 

dfA <- data.frame(elli5_Period$'1')
dfA[,3] <- "Pre"

dfB <- data.frame(elli5_Period$`2`)
dfB[,3] <- "Peak"

df5 <- data.frame(rbind(dfA,dfB))

wc.ass <- ggplot(Center, aes(Water_Content,Assimilation))+
  geom_point() +
  theme_classic() +  
  geom_path(data=df5, aes(x=x, y=y,colour=V3), size=1) +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0),
        axis.text.y = element_text(size=14,face="bold"),
        axis.title.x = element_text(color="black", size=16, vjust = 0, face="bold"),
        axis.title.y = element_text(color="black", size=16,vjust = 0, face="bold"))
wc.ass <- wc.ass +  geom_point(aes(x=wc_mean_n1, y=A_mean_n1), size=3, colour="turquoise")#pre wet
wc.ass <- wc.ass +  geom_point(aes(x=wc_mean_n2, y=A_mean_n2), size=3, colour="lightcoral") #peak wet
wc.ass <- wc.ass +  geom_point(aes(x=wc_mean_n3, y=A_mean_n3), size=3, colour="turquoise4")#pre dry
wc.ass <- wc.ass +  geom_point(aes(x=wc_mean_n4, y=A_mean_n4), size=3, colour="firebrick1")#peak dry


wc.ass 

#### WC by gs
elli6_Period <- dataEllipse(Center$Water_Content, Center$Stomatal_Conductance, Center$Period, group.labels=Center$Period,
                            weights, log="", levels=0.95, center.pch=19, 
                            center.cex=1.5, draw=FALSE, segments=51, 
                            robust=FALSE, xlab="Water Content", ylab="Stomatal Conductance", 
                            lwd=2, fill=FALSE, fill.alpha=0.3, grid=TRUE, id=FALSE) 

dfA <- data.frame(elli6_Period$'1')
dfA[,3] <- "Pre"

dfB <- data.frame(elli6_Period$`2`)
dfB[,3] <- "Peak"

df6 <- data.frame(rbind(dfA,dfB))

wc.gs <- ggplot(Center, aes(Water_Content,Stomatal_Conductance))+
  geom_point() +
  theme_classic() +  
  geom_path(data=df6, aes(x=x, y=y,colour=V3), size=1) +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0),
        axis.text.y = element_text(size=14,face="bold"),
        axis.title.x = element_text(color="black", size=16, vjust = 0, face="bold"),
        axis.title.y = element_text(color="black", size=16,vjust = 0, face="bold"))
wc.gs <- wc.gs +  geom_point(aes(x=wc_mean_n1, y=gs_mean_n1), size=3, colour="turquoise")#pre wet
wc.gs <- wc.gs +  geom_point(aes(x=wc_mean_n2, y=gs_mean_n2), size=3, colour="lightcoral") #peak wet
wc.gs <- wc.gs +  geom_point(aes(x=wc_mean_n3, y=gs_mean_n3), size=3, colour="turquoise4")#pre dry
wc.gs <- wc.gs +  geom_point(aes(x=wc_mean_n4, y=gs_mean_n4), size=3, colour="firebrick1")#peak dry

wc.gs 

#### WC by SLA
elli7_Period <- dataEllipse(Center$Water_Content, Center$SLA, Center$Period, group.labels=Center$Period,
                            weights, log="", levels=0.95, center.pch=19, 
                            center.cex=1.5, draw=FALSE, segments=51, 
                            robust=FALSE, xlab="Water Content", ylab="SLA", 
                            lwd=2, fill=FALSE, fill.alpha=0.3, grid=TRUE, id=FALSE) 

dfA <- data.frame(elli7_Period$'1')
dfA[,3] <- "Pre"

dfB <- data.frame(elli7_Period$`2`)
dfB[,3] <- "Peak"

df7 <- data.frame(rbind(dfA,dfB))

wc.sla <- ggplot(Center, aes(Water_Content,SLA))+
  geom_point() +
  theme_classic() +  
  geom_path(data=df7, aes(x=x, y=y,colour=V3), size=1) +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0),
        axis.text.y = element_text(size=14,face="bold"),
        axis.title.x = element_text(color="black", size=16, vjust = 0, face="bold"),
        axis.title.y = element_text(color="black", size=16,vjust = 0, face="bold"))
wc.sla <- wc.sla +  geom_point(aes(x=wc_mean_n1, y=SLA_mean_n1), size=3, colour="turquoise")#pre wet
wc.sla <- wc.sla +  geom_point(aes(x=wc_mean_n2, y=SLA_mean_n2), size=3, colour="lightcoral") #peak wet
wc.sla <- wc.sla +  geom_point(aes(x=wc_mean_n3, y=SLA_mean_n3), size=3, colour="turquoise4")#pre dry
wc.sla <- wc.sla +  geom_point(aes(x=wc_mean_n4, y=SLA_mean_n4), size=3, colour="firebrick1")#peak dry


wc.sla 

#### SLA by Ass
elli8_Period <- dataEllipse(Center$SLA, Center$Assimilation, Center$Period, group.labels=Center$Period,
                            weights, log="", levels=0.95, center.pch=19, 
                            center.cex=1.5, draw=FALSE, segments=51, 
                            robust=FALSE, xlab="SLA", ylab="Assimilation", 
                            lwd=2, fill=FALSE, fill.alpha=0.3, grid=TRUE, id=FALSE) 

dfA <- data.frame(elli8_Period$'1')
dfA[,3] <- "Pre"

dfB <- data.frame(elli8_Period$`2`)
dfB[,3] <- "Peak"

df8 <- data.frame(rbind(dfA,dfB))

sla.ass <- ggplot(Center, aes(SLA,Assimilation))+
  geom_point() +
  theme_classic() +  
  geom_path(data=df8, aes(x=x, y=y,colour=V3), size=1) +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0),
        axis.text.y = element_text(size=14,face="bold"),
        axis.title.x = element_text(color="black", size=16, vjust = 0, face="bold"),
        axis.title.y = element_text(color="black", size=16,vjust = 0, face="bold"))
sla.ass <- sla.ass +  geom_point(aes(x=SLA_mean_n1, y=A_mean_n1), size=3, colour="turquoise")#pre wet
sla.ass <- sla.ass +  geom_point(aes(x=SLA_mean_n2, y=A_mean_n2), size=3, colour="lightcoral") #peak wet
sla.ass <- sla.ass +  geom_point(aes(x=SLA_mean_n3, y=A_mean_n3), size=3, colour="turquoise4")#pre dry
sla.ass <- sla.ass +  geom_point(aes(x=SLA_mean_n4, y=A_mean_n4), size=3, colour="firebrick1")#peak dry

sla.ass 


#### gs by sla
elli9_Period <- dataEllipse(Center$Stomatal_Conductance, Center$SLA, Center$Period, group.labels=Center$Period,
                            weights, log="", levels=0.95, center.pch=19, 
                            center.cex=1.5, draw=FALSE, segments=51, 
                            robust=FALSE, xlab="Stomatal Conductance", ylab="SLA", 
                            lwd=2, fill=FALSE, fill.alpha=0.3, grid=TRUE, id=FALSE) 

dfA <- data.frame(elli9_Period$'1')
dfA[,3] <- "Pre"

dfB <- data.frame(elli9_Period$`2`)
dfB[,3] <- "Peak"

df9 <- data.frame(rbind(dfA,dfB))

gs.sla <- ggplot(Center, aes(Stomatal_Conductance,SLA))+
  geom_point() +
  theme_classic() +  
  geom_path(data=df9, aes(x=x, y=y,colour=V3), size=1) +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0),
        axis.text.y = element_text(size=14,face="bold"),
        axis.title.x = element_text(color="black", size=16, vjust = 0, face="bold"),
        axis.title.y = element_text(color="black", size=16,vjust = 0, face="bold"))
gs.sla <- gs.sla +  geom_point(aes(x=gs_mean_n1, y=SLA_mean_n1), size=3, colour="turquoise")#pre wet
gs.sla <- gs.sla +  geom_point(aes(x=gs_mean_n2, y=SLA_mean_n2), size=3, colour="lightcoral") #peak wet
gs.sla <- gs.sla +  geom_point(aes(x=gs_mean_n3, y=SLA_mean_n3), size=3, colour="turquoise4")#pre dry
gs.sla <- gs.sla +  geom_point(aes(x=gs_mean_n4, y=SLA_mean_n4), size=3, colour="firebrick1")#peak dry


gs.sla 



### gs by ASS
elli10_Period <- dataEllipse(Center$Stomatal_Conductance, Center$Assimilation, Center$Period, group.labels=Center$Period,
                             weights, log="", levels=0.95, center.pch=19, 
                             center.cex=1.5, draw=FALSE, segments=51, 
                             robust=FALSE, xlab="Stomatal Conductance", ylab="SLA", 
                             lwd=2, fill=FALSE, fill.alpha=0.3, grid=TRUE, id=FALSE) 

dfA <- data.frame(elli10_Period$'1')
dfA[,3] <- "Pre"

dfB <- data.frame(elli10_Period$`2`)
dfB[,3] <- "Peak"

df10 <- data.frame(rbind(dfA,dfB))

gs.ass <- ggplot(Center, aes(Stomatal_Conductance,Assimilation))+
  geom_point() +
  theme_classic() +  
  geom_path(data=df10, aes(x=x, y=y,colour=V3), size=1) +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0),
        axis.text.y = element_text(size=14,face="bold"),
        axis.title.x = element_text(color="black", size=16, vjust = 0, face="bold"),
        axis.title.y = element_text(color="black", size=16,vjust = 0, face="bold"))
gs.ass <- gs.ass +  geom_point(aes(x=gs_mean_n1, y=A_mean_n1), size=3, colour="turquoise")#pre wet
gs.ass <- gs.ass +  geom_point(aes(x=gs_mean_n2, y=A_mean_n2), size=3, colour="lightcoral") #peak wet
gs.ass <- gs.ass +  geom_point(aes(x=gs_mean_n3, y=A_mean_n3), size=3, colour="turquoise4")#pre dry
gs.ass <- gs.ass +  geom_point(aes(x=gs_mean_n4, y=A_mean_n4), size=3, colour="firebrick1")#peak dry


gs.ass

# cowplot export 10 by 16
plot_grid(df.sla, df.wc, df.ass, df.gs, sla.ass, gs.sla,
          wc.sla, wc.ass, wc.gs, gs.ass,  ncol=2)



