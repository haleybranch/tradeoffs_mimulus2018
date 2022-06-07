#Correlations comparing the pre vs peak for each group (N, C, S)
library(tidyverse)
library(Hmisc)

pre.N <- read.csv("Data/pre.N.csv")
pre.C <- read.csv("Data/pre.C.csv")
pre.S <- read.csv("Data/pre.S.csv")
peak.N <- read.csv("Data/peak.N.csv")
peak.C <- read.csv("Data/peak.C.csv")
peak.S <- read.csv("Data/peak.S.csv")

#grab variables for correlation
pre.N <- select(pre.N, Experiment_Date, SLA, Water_Content, Assimilation, Stomatal_Conductance)
pre.C <- select(pre.C, Experiment_Date, SLA, Water_Content, Assimilation, Stomatal_Conductance)
pre.S <- select(pre.S, Experiment_Date, SLA, Water_Content, Assimilation, Stomatal_Conductance)
peak.N <- select(peak.N, Experiment_Date, SLA, Water_Content, Assimilation, Stomatal_Conductance)
peak.C <- select(peak.C, Experiment_Date, SLA, Water_Content, Assimilation, Stomatal_Conductance)
peak.S <- select(peak.S, Experiment_Date, SLA, Water_Content, Assimilation, Stomatal_Conductance)

#bind for graphing
pre.N.dat <- pre.N %>% 
  mutate(Region="N",
         Time="Pre")
pre.C.dat <- pre.C %>% 
  mutate(Region="C",
         Time="Pre")
pre.S.dat <- pre.S %>% 
  mutate(Region="S",
         Time="Pre")
peak.N.dat <- peak.N %>% 
  mutate(Region="N",
         Time="Peak")
peak.C.dat <- peak.C %>% 
  mutate(Region="C",
         Time="Peak")
peak.S.dat <- peak.S %>% 
  mutate(Region="S",
         Time="Peak")

pre.dat <- rbind(pre.N.dat, pre.C.dat, pre.S.dat)
peak.dat <- rbind(peak.N.dat, peak.C.dat, peak.S.dat)
all.dat <- rbind(pre.dat, peak.dat)

# summaries for plotting
pre.dat.means <- pre.dat %>% 
  group_by(Region) %>% 
  summarise_each(funs(mean(., na.rm=T), n = sum(!is.na(.)), se = sd(., na.rm=T)/sqrt(sum(!is.na(.)))), Experiment_Date:Stomatal_Conductance)
peak.dat.means <- peak.dat %>% 
  group_by(Region) %>% 
  summarise_each(funs(mean(., na.rm=T), n = sum(!is.na(.)), se = sd(., na.rm=T)/sqrt(sum(!is.na(.)))), Experiment_Date:Stomatal_Conductance)
all.dat.means <- all.dat %>% 
  group_by(Region, Time) %>% 
  summarise_each(funs(mean(., na.rm=T), n = sum(!is.na(.)), se = sd(., na.rm=T)/sqrt(sum(!is.na(.)))), Experiment_Date:Stomatal_Conductance)

#set as matrix
pre.N <- as.matrix(pre.N)
pre.C <- as.matrix(pre.C)
pre.S <- as.matrix(pre.S)
peak.N <- as.matrix(peak.N)
peak.C <- as.matrix(peak.C)
peak.S <- as.matrix(peak.S)

#name and run each correlation
pre.N <- rcorr(pre.N)
pre.C <- rcorr(pre.C)
pre.S <- rcorr(pre.S)
peak.N <- rcorr(peak.N)
peak.C <- rcorr(peak.C)
peak.S <- rcorr(peak.S)



#Save as dataframe
preN <- pre.N$r ; preN_p <- pre.N$P
preC <- pre.C$r ; preC_p <- pre.C$P
preS <- pre.S$r ; preS_p <- pre.S$P

peakN <- peak.N$r ; peakN_p <- peak.N$P
peakC <- peak.C$r ; peakC_p <- peak.C$P
peakS <- peak.S$r ; peakS_p <- peak.S$P

mantel.test(preN, peakN, nperm = 10000, graph = FALSE,
            alternative = "two.sided")
mantel.test(preC, peakC, nperm = 10000, graph = FALSE,
            alternative = "two.sided")
mantel.test(preS, peakS, nperm = 10000, graph = FALSE,
            alternative = "two.sided")


#Write Table
#write.csv(preN, "Tables/preN_r.csv")
#write.csv(preC, "Tables/preC_r.csv")
#write.csv(preS, "Tables/preS_r.csv")
#write.csv(peakN, "Tables/peakN_r.csv")
#write.csv(peakC, "Tables/peakC_r.csv")
#write.csv(peakS, "Tables/peakS_r.csv")

#write.csv(preN_p, "Tables/preN_p.csv")
#write.csv(preC_p, "Tables/preC_p.csv")
#write.csv(preS_p, "Tables/preS_p.csv")
#write.csv(peakN_p, "Tables/peakN_p.csv")
#write.csv(peakC_p, "Tables/peakC_p.csv")
#write.csv(peakS_p, "Tables/peakS_p.csv")



SLAWC.pre <- ggplot(data=pre.dat, aes(x=SLA, y=Water_Content, color=Region)) +
  geom_point(alpha=0.2) +
  geom_point(data=pre.dat.means, aes(x=SLA_mean, y=Water_Content_mean), size = 3, shape = 17) + 
  geom_segment(data=pre.dat.means, aes(x=SLA_mean, y=Water_Content_mean, xend=SLA_mean+SLA_se, yend=Water_Content_mean)) + 
  geom_segment(data=pre.dat.means, aes(x=SLA_mean, y=Water_Content_mean, xend=SLA_mean-SLA_se, yend=Water_Content_mean)) + 
  geom_segment(data=pre.dat.means, aes(x=SLA_mean, y=Water_Content_mean, xend=SLA_mean, yend=Water_Content_mean+Water_Content_se)) + 
  geom_segment(data=pre.dat.means, aes(x=SLA_mean, y=Water_Content_mean, xend=SLA_mean, yend=Water_Content_mean-Water_Content_se)) + 
  stat_ellipse() +
  scale_color_manual(values=c("goldenrod1","blue","red")) +
  xlab("Specific leaf area") +
  ylab("Water content (%)") +
  theme_classic()

SLAFT.pre <- ggplot(pre.dat, aes(SLA, Experiment_Date, color=Region)) +
  geom_point(alpha=0.2) +
  geom_point(data=pre.dat.means, aes(x=SLA_mean, y=Experiment_Date_mean), size = 3, shape = 17) + 
  geom_segment(data=pre.dat.means, aes(x=SLA_mean, y=Experiment_Date_mean, xend=SLA_mean+SLA_se, yend=Experiment_Date_mean)) + 
  geom_segment(data=pre.dat.means, aes(x=SLA_mean, y=Experiment_Date_mean, xend=SLA_mean-SLA_se, yend=Experiment_Date_mean)) + 
  geom_segment(data=pre.dat.means, aes(x=SLA_mean, y=Experiment_Date_mean, xend=SLA_mean, yend=Experiment_Date_mean+Experiment_Date_se)) + 
  geom_segment(data=pre.dat.means, aes(x=SLA_mean, y=Experiment_Date_mean, xend=SLA_mean, yend=Experiment_Date_mean-Experiment_Date_se)) + 
  stat_ellipse() +
  scale_color_manual(values=c("goldenrod1","blue","red")) +
  xlab("Specific leaf area") +
  ylab("Date of first flower") +
  theme_classic()

SLAA.pre <- ggplot(pre.dat, aes(SLA, Assimilation, color=Region)) +
  geom_point(alpha=0.2) +
  geom_point(data=pre.dat.means, aes(x=SLA_mean, y=Assimilation_mean), size = 3, shape = 17) + 
  geom_segment(data=pre.dat.means, aes(x=SLA_mean, y=Assimilation_mean, xend=SLA_mean+SLA_se, yend=Assimilation_mean)) + 
  geom_segment(data=pre.dat.means, aes(x=SLA_mean, y=Assimilation_mean, xend=SLA_mean-SLA_se, yend=Assimilation_mean)) + 
  geom_segment(data=pre.dat.means, aes(x=SLA_mean, y=Assimilation_mean, xend=SLA_mean, yend=Assimilation_mean+Assimilation_se)) + 
  geom_segment(data=pre.dat.means, aes(x=SLA_mean, y=Assimilation_mean, xend=SLA_mean, yend=Assimilation_mean-Assimilation_se)) + 
  stat_ellipse() +
  scale_color_manual(values=c("goldenrod1","blue","red")) +
  xlab("Specific leaf area") +
  ylab("Carbon assimilation rate") +
  theme_classic()

SLAg.pre <- ggplot(pre.dat, aes(SLA, Stomatal_Conductance, color=Region)) +
  geom_point(alpha=0.2) +
  geom_point(data=pre.dat.means, aes(x=SLA_mean, y=Stomatal_Conductance_mean), size = 3, shape = 17) + 
  geom_segment(data=pre.dat.means, aes(x=SLA_mean, y=Stomatal_Conductance_mean, xend=SLA_mean+SLA_se, yend=Stomatal_Conductance_mean)) + 
  geom_segment(data=pre.dat.means, aes(x=SLA_mean, y=Stomatal_Conductance_mean, xend=SLA_mean-SLA_se, yend=Stomatal_Conductance_mean)) + 
  geom_segment(data=pre.dat.means, aes(x=SLA_mean, y=Stomatal_Conductance_mean, xend=SLA_mean, yend=Stomatal_Conductance_mean+Stomatal_Conductance_se)) + 
  geom_segment(data=pre.dat.means, aes(x=SLA_mean, y=Stomatal_Conductance_mean, xend=SLA_mean, yend=Stomatal_Conductance_mean-Stomatal_Conductance_se)) + 
  stat_ellipse() +
  scale_color_manual(values=c("goldenrod1","blue","red")) +
  xlab("Specific leaf area") +
  ylab("Stomatal conductance") +
  theme_classic()

WCFT.pre <- ggplot(pre.dat, aes(Water_Content, Experiment_Date, color=Region)) +
  geom_point(alpha=0.2) +
  geom_point(data=pre.dat.means, aes(x=Water_Content_mean, y=Experiment_Date_mean), size = 3, shape = 17) + 
  geom_segment(data=pre.dat.means, aes(x=Water_Content_mean, y=Experiment_Date_mean, xend=Water_Content_mean+Water_Content_se, yend=Experiment_Date_mean)) + 
  geom_segment(data=pre.dat.means, aes(x=Water_Content_mean, y=Experiment_Date_mean, xend=Water_Content_mean-Water_Content_se, yend=Experiment_Date_mean)) + 
  geom_segment(data=pre.dat.means, aes(x=Water_Content_mean, y=Experiment_Date_mean, xend=Water_Content_mean, yend=Experiment_Date_mean+Experiment_Date_se)) + 
  geom_segment(data=pre.dat.means, aes(x=Water_Content_mean, y=Experiment_Date_mean, xend=Water_Content_mean, yend=Experiment_Date_mean-Experiment_Date_se)) + 
  stat_ellipse() +
  scale_color_manual(values=c("goldenrod1","blue","red")) +
  xlab("Water content (%") +
  ylab("Date of first flower") +
  theme_classic()

WCA.pre <- ggplot(pre.dat, aes(Water_Content, Assimilation, color=Region)) +
  geom_point(alpha=0.2) +
  geom_point(data=pre.dat.means, aes(x=Water_Content_mean, y=Assimilation_mean), size = 3, shape = 17) + 
  geom_segment(data=pre.dat.means, aes(x=Water_Content_mean, y=Assimilation_mean, xend=Water_Content_mean+Water_Content_se, yend=Assimilation_mean)) + 
  geom_segment(data=pre.dat.means, aes(x=Water_Content_mean, y=Assimilation_mean, xend=Water_Content_mean-Water_Content_se, yend=Assimilation_mean)) + 
  geom_segment(data=pre.dat.means, aes(x=Water_Content_mean, y=Assimilation_mean, xend=Water_Content_mean, yend=Assimilation_mean+Assimilation_se)) + 
  geom_segment(data=pre.dat.means, aes(x=Water_Content_mean, y=Assimilation_mean, xend=Water_Content_mean, yend=Assimilation_mean-Assimilation_se)) + 
  stat_ellipse() +
  scale_color_manual(values=c("goldenrod1","blue","red")) +
  xlab("Water content (%)") +
  ylab("Carbon assimilation rate") +
  theme_classic()

WCg.pre <- ggplot(pre.dat, aes(Water_Content, Stomatal_Conductance, color=Region)) +
  geom_point(alpha=0.2) +
  geom_point(data=pre.dat.means, aes(x=Water_Content_mean, y=Stomatal_Conductance_mean), size = 3, shape = 17) + 
  geom_segment(data=pre.dat.means, aes(x=Water_Content_mean, y=Stomatal_Conductance_mean, xend=Water_Content_mean+Water_Content_se, yend=Stomatal_Conductance_mean)) + 
  geom_segment(data=pre.dat.means, aes(x=Water_Content_mean, y=Stomatal_Conductance_mean, xend=Water_Content_mean-Water_Content_se, yend=Stomatal_Conductance_mean)) + 
  geom_segment(data=pre.dat.means, aes(x=Water_Content_mean, y=Stomatal_Conductance_mean, xend=Water_Content_mean, yend=Stomatal_Conductance_mean+Stomatal_Conductance_se)) + 
  geom_segment(data=pre.dat.means, aes(x=Water_Content_mean, y=Stomatal_Conductance_mean, xend=Water_Content_mean, yend=Stomatal_Conductance_mean-Stomatal_Conductance_se)) + 
  stat_ellipse() +
  scale_color_manual(values=c("goldenrod1","blue","red")) +
  xlab("Water content (%)") +
  ylab("Stomatal conductance") +
  theme_classic()

FTA.pre <- ggplot(pre.dat, aes(Experiment_Date, Assimilation, color=Region)) +
  geom_point(alpha=0.2) +
  geom_point(data=pre.dat.means, aes(x=Experiment_Date_mean, y=Assimilation_mean), size = 3, shape = 17) + 
  geom_segment(data=pre.dat.means, aes(x=Experiment_Date_mean, y=Assimilation_mean, xend=Experiment_Date_mean+Experiment_Date_se, yend=Assimilation_mean)) + 
  geom_segment(data=pre.dat.means, aes(x=Experiment_Date_mean, y=Assimilation_mean, xend=Experiment_Date_mean-Experiment_Date_se, yend=Assimilation_mean)) + 
  geom_segment(data=pre.dat.means, aes(x=Experiment_Date_mean, y=Assimilation_mean, xend=Experiment_Date_mean, yend=Assimilation_mean+Assimilation_se)) + 
  geom_segment(data=pre.dat.means, aes(x=Experiment_Date_mean, y=Assimilation_mean, xend=Experiment_Date_mean, yend=Assimilation_mean-Assimilation_se)) + 
  stat_ellipse() +
  scale_color_manual(values=c("goldenrod1","blue","red")) +
  xlab("Date of first flower") +
  ylab("Carbon assimilation rate") +
  theme_classic()

FTg.pre <- ggplot(pre.dat, aes(Experiment_Date, Stomatal_Conductance, color=Region)) +
  geom_point(alpha=0.2) +
  geom_point(data=pre.dat.means, aes(x=Experiment_Date_mean, y=Stomatal_Conductance_mean), size = 3, shape = 17) + 
  geom_segment(data=pre.dat.means, aes(x=Experiment_Date_mean, y=Stomatal_Conductance_mean, xend=Experiment_Date_mean+Experiment_Date_se, yend=Stomatal_Conductance_mean)) + 
  geom_segment(data=pre.dat.means, aes(x=Experiment_Date_mean, y=Stomatal_Conductance_mean, xend=Experiment_Date_mean-Experiment_Date_se, yend=Stomatal_Conductance_mean)) + 
  geom_segment(data=pre.dat.means, aes(x=Experiment_Date_mean, y=Stomatal_Conductance_mean, xend=Experiment_Date_mean, yend=Stomatal_Conductance_mean+Stomatal_Conductance_se)) + 
  geom_segment(data=pre.dat.means, aes(x=Experiment_Date_mean, y=Stomatal_Conductance_mean, xend=Experiment_Date_mean, yend=Stomatal_Conductance_mean-Stomatal_Conductance_se)) + 
  stat_ellipse() +
  scale_color_manual(values=c("goldenrod1","blue","red")) +
  xlab("Date of first flower") +
  ylab("Stomatal conductance") +
  theme_classic()

Ag.pre <- ggplot(pre.dat, aes(Assimilation, Stomatal_Conductance, color=Region)) +
  geom_point(alpha=0.2) +
  geom_point(data=pre.dat.means, aes(x=Assimilation_mean, y=Stomatal_Conductance_mean), size = 3, shape = 17) + 
  geom_segment(data=pre.dat.means, aes(x=Assimilation_mean, y=Stomatal_Conductance_mean, xend=Assimilation_mean+Assimilation_se, yend=Stomatal_Conductance_mean)) + 
  geom_segment(data=pre.dat.means, aes(x=Assimilation_mean, y=Stomatal_Conductance_mean, xend=Assimilation_mean-Assimilation_se, yend=Stomatal_Conductance_mean)) + 
  geom_segment(data=pre.dat.means, aes(x=Assimilation_mean, y=Stomatal_Conductance_mean, xend=Assimilation_mean, yend=Stomatal_Conductance_mean+Stomatal_Conductance_se)) + 
  geom_segment(data=pre.dat.means, aes(x=Assimilation_mean, y=Stomatal_Conductance_mean, xend=Assimilation_mean, yend=Stomatal_Conductance_mean-Stomatal_Conductance_se)) + 
  stat_ellipse() +
  scale_color_manual(values=c("goldenrod1","blue","red")) +
  xlab("Carbon assimilation rate") +
  ylab("Stomatal conductance") +
  theme_classic()

multi.pre <- SLAWC.pre + plot_spacer() + plot_spacer() + plot_spacer() +
  SLAFT.pre + WCFT.pre + plot_spacer() + plot_spacer() +
  SLAA.pre + WCA.pre + FTA.pre + plot_spacer() +
  SLAg.pre + WCg.pre + FTg.pre + Ag.pre + 
  plot_layout(ncol=4, nrow=4, guides='collect') 

TukeyHSD(aov(SLA ~ Region, data=pre.dat)) #not different
TukeyHSD(aov(Water_Content ~ Region, data=pre.dat)) #centre different
TukeyHSD(aov(Experiment_Date ~ Region, data=pre.dat)) #south almost different
TukeyHSD(aov(Assimilation ~ Region, data=pre.dat)) #not different
TukeyHSD(aov(Stomatal_Conductance ~ Region, data=pre.dat)) #not different


SLAWC.peak <- ggplot(data=peak.dat, aes(x=SLA, y=Water_Content, color=Region)) +
  geom_point(alpha=0.2) +
  geom_point(data=peak.dat.means, aes(x=SLA_mean, y=Water_Content_mean), size = 3, shape = 17) + 
  geom_segment(data=peak.dat.means, aes(x=SLA_mean, y=Water_Content_mean, xend=SLA_mean+SLA_se, yend=Water_Content_mean)) + 
  geom_segment(data=peak.dat.means, aes(x=SLA_mean, y=Water_Content_mean, xend=SLA_mean-SLA_se, yend=Water_Content_mean)) + 
  geom_segment(data=peak.dat.means, aes(x=SLA_mean, y=Water_Content_mean, xend=SLA_mean, yend=Water_Content_mean+Water_Content_se)) + 
  geom_segment(data=peak.dat.means, aes(x=SLA_mean, y=Water_Content_mean, xend=SLA_mean, yend=Water_Content_mean-Water_Content_se)) + 
  stat_ellipse() +
  scale_color_manual(values=c("goldenrod1","blue","red")) +
  xlab("Specific leaf area") +
  ylab("Water content (%)") +
  theme_classic()

SLAFT.peak <- ggplot(peak.dat, aes(SLA, Experiment_Date, color=Region)) +
  geom_point(alpha=0.2) +
  geom_point(data=peak.dat.means, aes(x=SLA_mean, y=Experiment_Date_mean), size = 3, shape = 17) + 
  geom_segment(data=peak.dat.means, aes(x=SLA_mean, y=Experiment_Date_mean, xend=SLA_mean+SLA_se, yend=Experiment_Date_mean)) + 
  geom_segment(data=peak.dat.means, aes(x=SLA_mean, y=Experiment_Date_mean, xend=SLA_mean-SLA_se, yend=Experiment_Date_mean)) + 
  geom_segment(data=peak.dat.means, aes(x=SLA_mean, y=Experiment_Date_mean, xend=SLA_mean, yend=Experiment_Date_mean+Experiment_Date_se)) + 
  geom_segment(data=peak.dat.means, aes(x=SLA_mean, y=Experiment_Date_mean, xend=SLA_mean, yend=Experiment_Date_mean-Experiment_Date_se)) + 
  stat_ellipse() +
  scale_color_manual(values=c("goldenrod1","blue","red")) +
  xlab("Specific leaf area") +
  ylab("Date of first flower") +
  theme_classic()

SLAA.peak <- ggplot(peak.dat, aes(SLA, Assimilation, color=Region)) +
  geom_point(alpha=0.2) +
  geom_point(data=peak.dat.means, aes(x=SLA_mean, y=Assimilation_mean), size = 3, shape = 17) + 
  geom_segment(data=peak.dat.means, aes(x=SLA_mean, y=Assimilation_mean, xend=SLA_mean+SLA_se, yend=Assimilation_mean)) + 
  geom_segment(data=peak.dat.means, aes(x=SLA_mean, y=Assimilation_mean, xend=SLA_mean-SLA_se, yend=Assimilation_mean)) + 
  geom_segment(data=peak.dat.means, aes(x=SLA_mean, y=Assimilation_mean, xend=SLA_mean, yend=Assimilation_mean+Assimilation_se)) + 
  geom_segment(data=peak.dat.means, aes(x=SLA_mean, y=Assimilation_mean, xend=SLA_mean, yend=Assimilation_mean-Assimilation_se)) + 
  stat_ellipse() +
  scale_color_manual(values=c("goldenrod1","blue","red")) +
  xlab("Specific leaf area") +
  ylab("Carbon assimilation rate") +
  theme_classic()

SLAg.peak <- ggplot(peak.dat, aes(SLA, Stomatal_Conductance, color=Region)) +
  geom_point(alpha=0.2) +
  geom_point(data=peak.dat.means, aes(x=SLA_mean, y=Stomatal_Conductance_mean), size = 3, shape = 17) + 
  geom_segment(data=peak.dat.means, aes(x=SLA_mean, y=Stomatal_Conductance_mean, xend=SLA_mean+SLA_se, yend=Stomatal_Conductance_mean)) + 
  geom_segment(data=peak.dat.means, aes(x=SLA_mean, y=Stomatal_Conductance_mean, xend=SLA_mean-SLA_se, yend=Stomatal_Conductance_mean)) + 
  geom_segment(data=peak.dat.means, aes(x=SLA_mean, y=Stomatal_Conductance_mean, xend=SLA_mean, yend=Stomatal_Conductance_mean+Stomatal_Conductance_se)) + 
  geom_segment(data=peak.dat.means, aes(x=SLA_mean, y=Stomatal_Conductance_mean, xend=SLA_mean, yend=Stomatal_Conductance_mean-Stomatal_Conductance_se)) + 
  stat_ellipse() +
  scale_color_manual(values=c("goldenrod1","blue","red")) +
  xlab("Specific leaf area") +
  ylab("Stomatal conductance") +
  theme_classic()

WCFT.peak <- ggplot(peak.dat, aes(Water_Content, Experiment_Date, color=Region)) +
  geom_point(alpha=0.2) +
  geom_point(data=peak.dat.means, aes(x=Water_Content_mean, y=Experiment_Date_mean), size = 3, shape = 17) + 
  geom_segment(data=peak.dat.means, aes(x=Water_Content_mean, y=Experiment_Date_mean, xend=Water_Content_mean+Water_Content_se, yend=Experiment_Date_mean)) + 
  geom_segment(data=peak.dat.means, aes(x=Water_Content_mean, y=Experiment_Date_mean, xend=Water_Content_mean-Water_Content_se, yend=Experiment_Date_mean)) + 
  geom_segment(data=peak.dat.means, aes(x=Water_Content_mean, y=Experiment_Date_mean, xend=Water_Content_mean, yend=Experiment_Date_mean+Experiment_Date_se)) + 
  geom_segment(data=peak.dat.means, aes(x=Water_Content_mean, y=Experiment_Date_mean, xend=Water_Content_mean, yend=Experiment_Date_mean-Experiment_Date_se)) + 
  stat_ellipse() +
  scale_color_manual(values=c("goldenrod1","blue","red")) +
  xlab("Water content (%") +
  ylab("Date of first flower") +
  theme_classic()

WCA.peak <- ggplot(peak.dat, aes(Water_Content, Assimilation, color=Region)) +
  geom_point(alpha=0.2) +
  geom_point(data=peak.dat.means, aes(x=Water_Content_mean, y=Assimilation_mean), size = 3, shape = 17) + 
  geom_segment(data=peak.dat.means, aes(x=Water_Content_mean, y=Assimilation_mean, xend=Water_Content_mean+Water_Content_se, yend=Assimilation_mean)) + 
  geom_segment(data=peak.dat.means, aes(x=Water_Content_mean, y=Assimilation_mean, xend=Water_Content_mean-Water_Content_se, yend=Assimilation_mean)) + 
  geom_segment(data=peak.dat.means, aes(x=Water_Content_mean, y=Assimilation_mean, xend=Water_Content_mean, yend=Assimilation_mean+Assimilation_se)) + 
  geom_segment(data=peak.dat.means, aes(x=Water_Content_mean, y=Assimilation_mean, xend=Water_Content_mean, yend=Assimilation_mean-Assimilation_se)) + 
  stat_ellipse() +
  scale_color_manual(values=c("goldenrod1","blue","red")) +
  xlab("Water content (%)") +
  ylab("Carbon assimilation rate") +
  theme_classic()

WCg.peak <- ggplot(peak.dat, aes(Water_Content, Stomatal_Conductance, color=Region)) +
  geom_point(alpha=0.2) +
  geom_point(data=peak.dat.means, aes(x=Water_Content_mean, y=Stomatal_Conductance_mean), size = 3, shape = 17) + 
  geom_segment(data=peak.dat.means, aes(x=Water_Content_mean, y=Stomatal_Conductance_mean, xend=Water_Content_mean+Water_Content_se, yend=Stomatal_Conductance_mean)) + 
  geom_segment(data=peak.dat.means, aes(x=Water_Content_mean, y=Stomatal_Conductance_mean, xend=Water_Content_mean-Water_Content_se, yend=Stomatal_Conductance_mean)) + 
  geom_segment(data=peak.dat.means, aes(x=Water_Content_mean, y=Stomatal_Conductance_mean, xend=Water_Content_mean, yend=Stomatal_Conductance_mean+Stomatal_Conductance_se)) + 
  geom_segment(data=peak.dat.means, aes(x=Water_Content_mean, y=Stomatal_Conductance_mean, xend=Water_Content_mean, yend=Stomatal_Conductance_mean-Stomatal_Conductance_se)) + 
  stat_ellipse() +
  scale_color_manual(values=c("goldenrod1","blue","red")) +
  xlab("Water content (%)") +
  ylab("Stomatal conductance") +
  theme_classic()

FTA.peak <- ggplot(peak.dat, aes(Experiment_Date, Assimilation, color=Region)) +
  geom_point(alpha=0.2) +
  geom_point(data=peak.dat.means, aes(x=Experiment_Date_mean, y=Assimilation_mean), size = 3, shape = 17) + 
  geom_segment(data=peak.dat.means, aes(x=Experiment_Date_mean, y=Assimilation_mean, xend=Experiment_Date_mean+Experiment_Date_se, yend=Assimilation_mean)) + 
  geom_segment(data=peak.dat.means, aes(x=Experiment_Date_mean, y=Assimilation_mean, xend=Experiment_Date_mean-Experiment_Date_se, yend=Assimilation_mean)) + 
  geom_segment(data=peak.dat.means, aes(x=Experiment_Date_mean, y=Assimilation_mean, xend=Experiment_Date_mean, yend=Assimilation_mean+Assimilation_se)) + 
  geom_segment(data=peak.dat.means, aes(x=Experiment_Date_mean, y=Assimilation_mean, xend=Experiment_Date_mean, yend=Assimilation_mean-Assimilation_se)) + 
  stat_ellipse() +
  scale_color_manual(values=c("goldenrod1","blue","red")) +
  xlab("Date of first flower") +
  ylab("Carbon assimilation rate") +
  theme_classic()

FTg.peak <- ggplot(peak.dat, aes(Experiment_Date, Stomatal_Conductance, color=Region)) +
  geom_point(alpha=0.2) +
  geom_point(data=peak.dat.means, aes(x=Experiment_Date_mean, y=Stomatal_Conductance_mean), size = 3, shape = 17) + 
  geom_segment(data=peak.dat.means, aes(x=Experiment_Date_mean, y=Stomatal_Conductance_mean, xend=Experiment_Date_mean+Experiment_Date_se, yend=Stomatal_Conductance_mean)) + 
  geom_segment(data=peak.dat.means, aes(x=Experiment_Date_mean, y=Stomatal_Conductance_mean, xend=Experiment_Date_mean-Experiment_Date_se, yend=Stomatal_Conductance_mean)) + 
  geom_segment(data=peak.dat.means, aes(x=Experiment_Date_mean, y=Stomatal_Conductance_mean, xend=Experiment_Date_mean, yend=Stomatal_Conductance_mean+Stomatal_Conductance_se)) + 
  geom_segment(data=peak.dat.means, aes(x=Experiment_Date_mean, y=Stomatal_Conductance_mean, xend=Experiment_Date_mean, yend=Stomatal_Conductance_mean-Stomatal_Conductance_se)) + 
  stat_ellipse() +
  scale_color_manual(values=c("goldenrod1","blue","red")) +
  xlab("Date of first flower") +
  ylab("Stomatal conductance") +
  theme_classic()

Ag.peak <- ggplot(peak.dat, aes(Assimilation, Stomatal_Conductance, color=Region)) +
  geom_point(alpha=0.2) +
  geom_point(data=peak.dat.means, aes(x=Assimilation_mean, y=Stomatal_Conductance_mean), size = 3, shape = 17) + 
  geom_segment(data=peak.dat.means, aes(x=Assimilation_mean, y=Stomatal_Conductance_mean, xend=Assimilation_mean+Assimilation_se, yend=Stomatal_Conductance_mean)) + 
  geom_segment(data=peak.dat.means, aes(x=Assimilation_mean, y=Stomatal_Conductance_mean, xend=Assimilation_mean-Assimilation_se, yend=Stomatal_Conductance_mean)) + 
  geom_segment(data=peak.dat.means, aes(x=Assimilation_mean, y=Stomatal_Conductance_mean, xend=Assimilation_mean, yend=Stomatal_Conductance_mean+Stomatal_Conductance_se)) + 
  geom_segment(data=peak.dat.means, aes(x=Assimilation_mean, y=Stomatal_Conductance_mean, xend=Assimilation_mean, yend=Stomatal_Conductance_mean-Stomatal_Conductance_se)) + 
  stat_ellipse() +
  scale_color_manual(values=c("goldenrod1","blue","red")) +
  xlab("Carbon assimilation rate") +
  ylab("Stomatal conductance") +
  theme_classic()

multi.peak <- SLAWC.peak + plot_spacer() + plot_spacer() + plot_spacer() +
  SLAFT.peak + WCFT.peak + plot_spacer() + plot_spacer() +
  SLAA.peak + WCA.peak + FTA.peak + plot_spacer() +
  SLAg.peak + WCg.peak + FTg.peak + Ag.peak + 
  plot_layout(ncol=4, nrow=4, guides='collect') 



SLAWC.N <- ggplot(data=subset(all.dat, Region=="N"), aes(x=SLA, y=Water_Content, color=Time)) +
  geom_point(alpha=0.2) +
  geom_point(data=subset(all.dat.means, Region=="N"), aes(x=SLA_mean, y=Water_Content_mean), size = 3, shape = 17) + 
  geom_segment(data=subset(all.dat.means, Region=="N"), aes(x=SLA_mean, y=Water_Content_mean, xend=SLA_mean+SLA_se, yend=Water_Content_mean)) + 
  geom_segment(data=subset(all.dat.means, Region=="N"), aes(x=SLA_mean, y=Water_Content_mean, xend=SLA_mean-SLA_se, yend=Water_Content_mean)) + 
  geom_segment(data=subset(all.dat.means, Region=="N"), aes(x=SLA_mean, y=Water_Content_mean, xend=SLA_mean, yend=Water_Content_mean+Water_Content_se)) + 
  geom_segment(data=subset(all.dat.means, Region=="N"), aes(x=SLA_mean, y=Water_Content_mean, xend=SLA_mean, yend=Water_Content_mean-Water_Content_se)) + 
  stat_ellipse() +
  #scale_color_manual(values=c("goldenrod1","blue","red")) +
  xlab("Specific leaf area") +
  ylab("Water content (%)") +
  theme_classic()

SLAWC.S <- ggplot(data=subset(all.dat, Region=="S"), aes(x=SLA, y=Water_Content, color=Time)) +
  geom_point(alpha=0.2) +
  geom_point(data=subset(all.dat.means, Region=="S"), aes(x=SLA_mean, y=Water_Content_mean), size = 3, shape = 17) + 
  geom_segment(data=subset(all.dat.means, Region=="S"), aes(x=SLA_mean, y=Water_Content_mean, xend=SLA_mean+SLA_se, yend=Water_Content_mean)) + 
  geom_segment(data=subset(all.dat.means, Region=="S"), aes(x=SLA_mean, y=Water_Content_mean, xend=SLA_mean-SLA_se, yend=Water_Content_mean)) + 
  geom_segment(data=subset(all.dat.means, Region=="S"), aes(x=SLA_mean, y=Water_Content_mean, xend=SLA_mean, yend=Water_Content_mean+Water_Content_se)) + 
  geom_segment(data=subset(all.dat.means, Region=="S"), aes(x=SLA_mean, y=Water_Content_mean, xend=SLA_mean, yend=Water_Content_mean-Water_Content_se)) + 
  stat_ellipse() +
  #scale_color_manual(values=c("goldenrod1","blue","red")) +
  xlab("Specific leaf area") +
  ylab("Water content (%)") +
  theme_classic()

SLAFT.N <- ggplot(subset(all.dat, Region=="N"), aes(SLA, Experiment_Date, color=Time)) +
  geom_point(alpha=0.2) +
  geom_point(data=subset(all.dat.means, Region=="N"), aes(x=SLA_mean, y=Experiment_Date_mean), size = 3, shape = 17) + 
  geom_segment(data=subset(all.dat.means, Region=="N"), aes(x=SLA_mean, y=Experiment_Date_mean, xend=SLA_mean+SLA_se, yend=Experiment_Date_mean)) + 
  geom_segment(data=subset(all.dat.means, Region=="N"), aes(x=SLA_mean, y=Experiment_Date_mean, xend=SLA_mean-SLA_se, yend=Experiment_Date_mean)) + 
  geom_segment(data=subset(all.dat.means, Region=="N"), aes(x=SLA_mean, y=Experiment_Date_mean, xend=SLA_mean, yend=Experiment_Date_mean+Experiment_Date_se)) + 
  geom_segment(data=subset(all.dat.means, Region=="N"), aes(x=SLA_mean, y=Experiment_Date_mean, xend=SLA_mean, yend=Experiment_Date_mean-Experiment_Date_se)) + 
  stat_ellipse() +
  #scale_color_manual(values=c("goldenrod1","blue","red")) +
  xlab("Specific leaf area") +
  ylab("Date of first flower") +
  theme_classic()

SLAFT.S <- ggplot(subset(all.dat, Region=="S"), aes(SLA, Experiment_Date, color=Time)) +
  geom_point(alpha=0.2) +
  geom_point(data=subset(all.dat.means, Region=="S"), aes(x=SLA_mean, y=Experiment_Date_mean), size = 3, shape = 17) + 
  geom_segment(data=subset(all.dat.means, Region=="S"), aes(x=SLA_mean, y=Experiment_Date_mean, xend=SLA_mean+SLA_se, yend=Experiment_Date_mean)) + 
  geom_segment(data=subset(all.dat.means, Region=="S"), aes(x=SLA_mean, y=Experiment_Date_mean, xend=SLA_mean-SLA_se, yend=Experiment_Date_mean)) + 
  geom_segment(data=subset(all.dat.means, Region=="S"), aes(x=SLA_mean, y=Experiment_Date_mean, xend=SLA_mean, yend=Experiment_Date_mean+Experiment_Date_se)) + 
  geom_segment(data=subset(all.dat.means, Region=="S"), aes(x=SLA_mean, y=Experiment_Date_mean, xend=SLA_mean, yend=Experiment_Date_mean-Experiment_Date_se)) + 
  stat_ellipse() +
  #scale_color_manual(values=c("goldenrod1","blue","red")) +
  xlab("Specific leaf area") +
  ylab("Date of first flower") +
  theme_classic()

SLAA.N <- ggplot(subset(all.dat, Region=="N"), aes(SLA, Assimilation, color=Time)) +
  geom_point(alpha=0.2) +
  geom_point(data=subset(all.dat.means, Region=="N"), aes(x=SLA_mean, y=Assimilation_mean), size = 3, shape = 17) + 
  geom_segment(data=subset(all.dat.means, Region=="N"), aes(x=SLA_mean, y=Assimilation_mean, xend=SLA_mean+SLA_se, yend=Assimilation_mean)) + 
  geom_segment(data=subset(all.dat.means, Region=="N"), aes(x=SLA_mean, y=Assimilation_mean, xend=SLA_mean-SLA_se, yend=Assimilation_mean)) + 
  geom_segment(data=subset(all.dat.means, Region=="N"), aes(x=SLA_mean, y=Assimilation_mean, xend=SLA_mean, yend=Assimilation_mean+Assimilation_se)) + 
  geom_segment(data=subset(all.dat.means, Region=="N"), aes(x=SLA_mean, y=Assimilation_mean, xend=SLA_mean, yend=Assimilation_mean-Assimilation_se)) + 
  stat_ellipse() +
  #scale_color_manual(values=c("goldenrod1","blue","red")) +
  xlab("Specific leaf area") +
  ylab("Carbon assimilation rate") +
  theme_classic()

SLAA.S <- ggplot(subset(all.dat, Region=="S"), aes(SLA, Assimilation, color=Time)) +
  geom_point(alpha=0.2) +
  geom_point(data=subset(all.dat.means, Region=="S"), aes(x=SLA_mean, y=Assimilation_mean), size = 3, shape = 17) + 
  geom_segment(data=subset(all.dat.means, Region=="S"), aes(x=SLA_mean, y=Assimilation_mean, xend=SLA_mean+SLA_se, yend=Assimilation_mean)) + 
  geom_segment(data=subset(all.dat.means, Region=="S"), aes(x=SLA_mean, y=Assimilation_mean, xend=SLA_mean-SLA_se, yend=Assimilation_mean)) + 
  geom_segment(data=subset(all.dat.means, Region=="S"), aes(x=SLA_mean, y=Assimilation_mean, xend=SLA_mean, yend=Assimilation_mean+Assimilation_se)) + 
  geom_segment(data=subset(all.dat.means, Region=="S"), aes(x=SLA_mean, y=Assimilation_mean, xend=SLA_mean, yend=Assimilation_mean-Assimilation_se)) + 
  stat_ellipse() +
  #scale_color_manual(values=c("goldenrod1","blue","red")) +
  xlab("Specific leaf area") +
  ylab("Carbon assimilation rate") +
  theme_classic()

SLAg.N <- ggplot(subset(all.dat, Region=="N"), aes(SLA, Stomatal_Conductance, color=Time)) +
  geom_point(alpha=0.2) +
  geom_point(data=subset(all.dat.means, Region=="N"), aes(x=SLA_mean, y=Stomatal_Conductance_mean), size = 3, shape = 17) + 
  geom_segment(data=subset(all.dat.means, Region=="N"), aes(x=SLA_mean, y=Stomatal_Conductance_mean, xend=SLA_mean+SLA_se, yend=Stomatal_Conductance_mean)) + 
  geom_segment(data=subset(all.dat.means, Region=="N"), aes(x=SLA_mean, y=Stomatal_Conductance_mean, xend=SLA_mean-SLA_se, yend=Stomatal_Conductance_mean)) + 
  geom_segment(data=subset(all.dat.means, Region=="N"), aes(x=SLA_mean, y=Stomatal_Conductance_mean, xend=SLA_mean, yend=Stomatal_Conductance_mean+Stomatal_Conductance_se)) + 
  geom_segment(data=subset(all.dat.means, Region=="N"), aes(x=SLA_mean, y=Stomatal_Conductance_mean, xend=SLA_mean, yend=Stomatal_Conductance_mean-Stomatal_Conductance_se)) + 
  stat_ellipse() +
  #scale_color_manual(values=c("goldenrod1","blue","red")) +
  xlab("Specific leaf area") +
  ylab("Stomatal conductance") +
  theme_classic()

SLAg.S <- ggplot(subset(all.dat, Region=="S"), aes(SLA, Stomatal_Conductance, color=Time)) +
  geom_point(alpha=0.2) +
  geom_point(data=subset(all.dat.means, Region=="S"), aes(x=SLA_mean, y=Stomatal_Conductance_mean), size = 3, shape = 17) + 
  geom_segment(data=subset(all.dat.means, Region=="S"), aes(x=SLA_mean, y=Stomatal_Conductance_mean, xend=SLA_mean+SLA_se, yend=Stomatal_Conductance_mean)) + 
  geom_segment(data=subset(all.dat.means, Region=="S"), aes(x=SLA_mean, y=Stomatal_Conductance_mean, xend=SLA_mean-SLA_se, yend=Stomatal_Conductance_mean)) + 
  geom_segment(data=subset(all.dat.means, Region=="S"), aes(x=SLA_mean, y=Stomatal_Conductance_mean, xend=SLA_mean, yend=Stomatal_Conductance_mean+Stomatal_Conductance_se)) + 
  geom_segment(data=subset(all.dat.means, Region=="S"), aes(x=SLA_mean, y=Stomatal_Conductance_mean, xend=SLA_mean, yend=Stomatal_Conductance_mean-Stomatal_Conductance_se)) + 
  stat_ellipse() +
  #scale_color_manual(values=c("goldenrod1","blue","red")) +
  xlab("Specific leaf area") +
  ylab("Stomatal conductance") +
  theme_classic()

WCFT.N <- ggplot(subset(all.dat, Region=="N"), aes(Water_Content, Experiment_Date, color=Time)) +
  geom_point(alpha=0.2) +
  geom_point(data=subset(all.dat.means, Region=="N"), aes(x=Water_Content_mean, y=Experiment_Date_mean), size = 3, shape = 17) + 
  geom_segment(data=subset(all.dat.means, Region=="N"), aes(x=Water_Content_mean, y=Experiment_Date_mean, xend=Water_Content_mean+Water_Content_se, yend=Experiment_Date_mean)) + 
  geom_segment(data=subset(all.dat.means, Region=="N"), aes(x=Water_Content_mean, y=Experiment_Date_mean, xend=Water_Content_mean-Water_Content_se, yend=Experiment_Date_mean)) + 
  geom_segment(data=subset(all.dat.means, Region=="N"), aes(x=Water_Content_mean, y=Experiment_Date_mean, xend=Water_Content_mean, yend=Experiment_Date_mean+Experiment_Date_se)) + 
  geom_segment(data=subset(all.dat.means, Region=="N"), aes(x=Water_Content_mean, y=Experiment_Date_mean, xend=Water_Content_mean, yend=Experiment_Date_mean-Experiment_Date_se)) + 
  stat_ellipse() +
  #scale_color_manual(values=c("goldenrod1","blue","red")) +
  xlab("Water content (%") +
  ylab("Date of first flower") +
  theme_classic()

WCFT.S <- ggplot(subset(all.dat, Region=="S"), aes(Water_Content, Experiment_Date, color=Time)) +
  geom_point(alpha=0.2) +
  geom_point(data=subset(all.dat.means, Region=="S"), aes(x=Water_Content_mean, y=Experiment_Date_mean), size = 3, shape = 17) + 
  geom_segment(data=subset(all.dat.means, Region=="S"), aes(x=Water_Content_mean, y=Experiment_Date_mean, xend=Water_Content_mean+Water_Content_se, yend=Experiment_Date_mean)) + 
  geom_segment(data=subset(all.dat.means, Region=="S"), aes(x=Water_Content_mean, y=Experiment_Date_mean, xend=Water_Content_mean-Water_Content_se, yend=Experiment_Date_mean)) + 
  geom_segment(data=subset(all.dat.means, Region=="S"), aes(x=Water_Content_mean, y=Experiment_Date_mean, xend=Water_Content_mean, yend=Experiment_Date_mean+Experiment_Date_se)) + 
  geom_segment(data=subset(all.dat.means, Region=="S"), aes(x=Water_Content_mean, y=Experiment_Date_mean, xend=Water_Content_mean, yend=Experiment_Date_mean-Experiment_Date_se)) + 
  stat_ellipse() +
  #scale_color_manual(values=c("goldenrod1","blue","red")) +
  xlab("Water content (%") +
  ylab("Date of first flower") +
  theme_classic()

WCA.N <- ggplot(subset(all.dat, Region=="N"), aes(Water_Content, Assimilation, color=Time)) +
  geom_point(alpha=0.2) +
  geom_point(data=subset(all.dat.means, Region=="N"), aes(x=Water_Content_mean, y=Assimilation_mean), size = 3, shape = 17) + 
  geom_segment(data=subset(all.dat.means, Region=="N"), aes(x=Water_Content_mean, y=Assimilation_mean, xend=Water_Content_mean+Water_Content_se, yend=Assimilation_mean)) + 
  geom_segment(data=subset(all.dat.means, Region=="N"), aes(x=Water_Content_mean, y=Assimilation_mean, xend=Water_Content_mean-Water_Content_se, yend=Assimilation_mean)) + 
  geom_segment(data=subset(all.dat.means, Region=="N"), aes(x=Water_Content_mean, y=Assimilation_mean, xend=Water_Content_mean, yend=Assimilation_mean+Assimilation_se)) + 
  geom_segment(data=subset(all.dat.means, Region=="N"), aes(x=Water_Content_mean, y=Assimilation_mean, xend=Water_Content_mean, yend=Assimilation_mean-Assimilation_se)) + 
  stat_ellipse() +
  #scale_color_manual(values=c("goldenrod1","blue","red")) +
  xlab("Water content (%)") +
  ylab("Carbon assimilation rate") +
  theme_classic()

WCA.S <- ggplot(subset(all.dat, Region=="S"), aes(Water_Content, Assimilation, color=Time)) +
  geom_point(alpha=0.2) +
  geom_point(data=subset(all.dat.means, Region=="S"), aes(x=Water_Content_mean, y=Assimilation_mean), size = 3, shape = 17) + 
  geom_segment(data=subset(all.dat.means, Region=="S"), aes(x=Water_Content_mean, y=Assimilation_mean, xend=Water_Content_mean+Water_Content_se, yend=Assimilation_mean)) + 
  geom_segment(data=subset(all.dat.means, Region=="S"), aes(x=Water_Content_mean, y=Assimilation_mean, xend=Water_Content_mean-Water_Content_se, yend=Assimilation_mean)) + 
  geom_segment(data=subset(all.dat.means, Region=="S"), aes(x=Water_Content_mean, y=Assimilation_mean, xend=Water_Content_mean, yend=Assimilation_mean+Assimilation_se)) + 
  geom_segment(data=subset(all.dat.means, Region=="S"), aes(x=Water_Content_mean, y=Assimilation_mean, xend=Water_Content_mean, yend=Assimilation_mean-Assimilation_se)) + 
  stat_ellipse() +
  #scale_color_manual(values=c("goldenrod1","blue","red")) +
  xlab("Water content (%)") +
  ylab("Carbon assimilation rate") +
  theme_classic()

WCg.N <- ggplot(subset(all.dat, Region=="N"), aes(Water_Content, Stomatal_Conductance, color=Time)) +
  geom_point(alpha=0.2) +
  geom_point(data=subset(all.dat.means, Region=="N"), aes(x=Water_Content_mean, y=Stomatal_Conductance_mean), size = 3, shape = 17) + 
  geom_segment(data=subset(all.dat.means, Region=="N"), aes(x=Water_Content_mean, y=Stomatal_Conductance_mean, xend=Water_Content_mean+Water_Content_se, yend=Stomatal_Conductance_mean)) + 
  geom_segment(data=subset(all.dat.means, Region=="N"), aes(x=Water_Content_mean, y=Stomatal_Conductance_mean, xend=Water_Content_mean-Water_Content_se, yend=Stomatal_Conductance_mean)) + 
  geom_segment(data=subset(all.dat.means, Region=="N"), aes(x=Water_Content_mean, y=Stomatal_Conductance_mean, xend=Water_Content_mean, yend=Stomatal_Conductance_mean+Stomatal_Conductance_se)) + 
  geom_segment(data=subset(all.dat.means, Region=="N"), aes(x=Water_Content_mean, y=Stomatal_Conductance_mean, xend=Water_Content_mean, yend=Stomatal_Conductance_mean-Stomatal_Conductance_se)) + 
  stat_ellipse() +
  #scale_color_manual(values=c("goldenrod1","blue","red")) +
  xlab("Water content (%)") +
  ylab("Stomatal conductance") +
  theme_classic()

WCg.S <- ggplot(subset(all.dat, Region=="S"), aes(Water_Content, Stomatal_Conductance, color=Time)) +
  geom_point(alpha=0.2) +
  geom_point(data=subset(all.dat.means, Region=="S"), aes(x=Water_Content_mean, y=Stomatal_Conductance_mean), size = 3, shape = 17) + 
  geom_segment(data=subset(all.dat.means, Region=="S"), aes(x=Water_Content_mean, y=Stomatal_Conductance_mean, xend=Water_Content_mean+Water_Content_se, yend=Stomatal_Conductance_mean)) + 
  geom_segment(data=subset(all.dat.means, Region=="S"), aes(x=Water_Content_mean, y=Stomatal_Conductance_mean, xend=Water_Content_mean-Water_Content_se, yend=Stomatal_Conductance_mean)) + 
  geom_segment(data=subset(all.dat.means, Region=="S"), aes(x=Water_Content_mean, y=Stomatal_Conductance_mean, xend=Water_Content_mean, yend=Stomatal_Conductance_mean+Stomatal_Conductance_se)) + 
  geom_segment(data=subset(all.dat.means, Region=="S"), aes(x=Water_Content_mean, y=Stomatal_Conductance_mean, xend=Water_Content_mean, yend=Stomatal_Conductance_mean-Stomatal_Conductance_se)) + 
  stat_ellipse() +
  #scale_color_manual(values=c("goldenrod1","blue","red")) +
  xlab("Water content (%)") +
  ylab("Stomatal conductance") +
  theme_classic()

FTA.N <- ggplot(subset(all.dat, Region=="N"), aes(Experiment_Date, Assimilation, color=Time)) +
  geom_point(alpha=0.2) +
  geom_point(data=subset(all.dat.means, Region=="N"), aes(x=Experiment_Date_mean, y=Assimilation_mean), size = 3, shape = 17) + 
  geom_segment(data=subset(all.dat.means, Region=="N"), aes(x=Experiment_Date_mean, y=Assimilation_mean, xend=Experiment_Date_mean+Experiment_Date_se, yend=Assimilation_mean)) + 
  geom_segment(data=subset(all.dat.means, Region=="N"), aes(x=Experiment_Date_mean, y=Assimilation_mean, xend=Experiment_Date_mean-Experiment_Date_se, yend=Assimilation_mean)) + 
  geom_segment(data=subset(all.dat.means, Region=="N"), aes(x=Experiment_Date_mean, y=Assimilation_mean, xend=Experiment_Date_mean, yend=Assimilation_mean+Assimilation_se)) + 
  geom_segment(data=subset(all.dat.means, Region=="N"), aes(x=Experiment_Date_mean, y=Assimilation_mean, xend=Experiment_Date_mean, yend=Assimilation_mean-Assimilation_se)) + 
  stat_ellipse() +
  #scale_color_manual(values=c("goldenrod1","blue","red")) +
  xlab("Date of first flower") +
  ylab("Carbon assimilation rate") +
  theme_classic()

FTA.S <- ggplot(subset(all.dat, Region=="S"), aes(Experiment_Date, Assimilation, color=Time)) +
  geom_point(alpha=0.2) +
  geom_point(data=subset(all.dat.means, Region=="S"), aes(x=Experiment_Date_mean, y=Assimilation_mean), size = 3, shape = 17) + 
  geom_segment(data=subset(all.dat.means, Region=="S"), aes(x=Experiment_Date_mean, y=Assimilation_mean, xend=Experiment_Date_mean+Experiment_Date_se, yend=Assimilation_mean)) + 
  geom_segment(data=subset(all.dat.means, Region=="S"), aes(x=Experiment_Date_mean, y=Assimilation_mean, xend=Experiment_Date_mean-Experiment_Date_se, yend=Assimilation_mean)) + 
  geom_segment(data=subset(all.dat.means, Region=="S"), aes(x=Experiment_Date_mean, y=Assimilation_mean, xend=Experiment_Date_mean, yend=Assimilation_mean+Assimilation_se)) + 
  geom_segment(data=subset(all.dat.means, Region=="S"), aes(x=Experiment_Date_mean, y=Assimilation_mean, xend=Experiment_Date_mean, yend=Assimilation_mean-Assimilation_se)) + 
  stat_ellipse() +
  #scale_color_manual(values=c("goldenrod1","blue","red")) +
  xlab("Date of first flower") +
  ylab("Carbon assimilation rate") +
  theme_classic()

FTg.N <- ggplot(subset(all.dat, Region=="N"), aes(Experiment_Date, Stomatal_Conductance, color=Time)) +
  geom_point(alpha=0.2) +
  geom_point(data=subset(all.dat.means, Region=="N"), aes(x=Experiment_Date_mean, y=Stomatal_Conductance_mean), size = 3, shape = 17) + 
  geom_segment(data=subset(all.dat.means, Region=="N"), aes(x=Experiment_Date_mean, y=Stomatal_Conductance_mean, xend=Experiment_Date_mean+Experiment_Date_se, yend=Stomatal_Conductance_mean)) + 
  geom_segment(data=subset(all.dat.means, Region=="N"), aes(x=Experiment_Date_mean, y=Stomatal_Conductance_mean, xend=Experiment_Date_mean-Experiment_Date_se, yend=Stomatal_Conductance_mean)) + 
  geom_segment(data=subset(all.dat.means, Region=="N"), aes(x=Experiment_Date_mean, y=Stomatal_Conductance_mean, xend=Experiment_Date_mean, yend=Stomatal_Conductance_mean+Stomatal_Conductance_se)) + 
  geom_segment(data=subset(all.dat.means, Region=="N"), aes(x=Experiment_Date_mean, y=Stomatal_Conductance_mean, xend=Experiment_Date_mean, yend=Stomatal_Conductance_mean-Stomatal_Conductance_se)) + 
  stat_ellipse() +
  #scale_color_manual(values=c("goldenrod1","blue","red")) +
  xlab("Date of first flower") +
  ylab("Stomatal conductance") +
  theme_classic()

FTg.S <- ggplot(subset(all.dat, Region=="S"), aes(Experiment_Date, Stomatal_Conductance, color=Time)) +
  geom_point(alpha=0.2) +
  geom_point(data=subset(all.dat.means, Region=="S"), aes(x=Experiment_Date_mean, y=Stomatal_Conductance_mean), size = 3, shape = 17) + 
  geom_segment(data=subset(all.dat.means, Region=="S"), aes(x=Experiment_Date_mean, y=Stomatal_Conductance_mean, xend=Experiment_Date_mean+Experiment_Date_se, yend=Stomatal_Conductance_mean)) + 
  geom_segment(data=subset(all.dat.means, Region=="S"), aes(x=Experiment_Date_mean, y=Stomatal_Conductance_mean, xend=Experiment_Date_mean-Experiment_Date_se, yend=Stomatal_Conductance_mean)) + 
  geom_segment(data=subset(all.dat.means, Region=="S"), aes(x=Experiment_Date_mean, y=Stomatal_Conductance_mean, xend=Experiment_Date_mean, yend=Stomatal_Conductance_mean+Stomatal_Conductance_se)) + 
  geom_segment(data=subset(all.dat.means, Region=="S"), aes(x=Experiment_Date_mean, y=Stomatal_Conductance_mean, xend=Experiment_Date_mean, yend=Stomatal_Conductance_mean-Stomatal_Conductance_se)) + 
  stat_ellipse() +
  #scale_color_manual(values=c("goldenrod1","blue","red")) +
  xlab("Date of first flower") +
  ylab("Stomatal conductance") +
  theme_classic()

Ag.N <- ggplot(subset(all.dat, Region=="N"), aes(Assimilation, Stomatal_Conductance, color=Time)) +
  geom_point(alpha=0.2) +
  geom_point(data=subset(all.dat.means, Region=="N"), aes(x=Assimilation_mean, y=Stomatal_Conductance_mean), size = 3, shape = 17) + 
  geom_segment(data=subset(all.dat.means, Region=="N"), aes(x=Assimilation_mean, y=Stomatal_Conductance_mean, xend=Assimilation_mean+Assimilation_se, yend=Stomatal_Conductance_mean)) + 
  geom_segment(data=subset(all.dat.means, Region=="N"), aes(x=Assimilation_mean, y=Stomatal_Conductance_mean, xend=Assimilation_mean-Assimilation_se, yend=Stomatal_Conductance_mean)) + 
  geom_segment(data=subset(all.dat.means, Region=="N"), aes(x=Assimilation_mean, y=Stomatal_Conductance_mean, xend=Assimilation_mean, yend=Stomatal_Conductance_mean+Stomatal_Conductance_se)) + 
  geom_segment(data=subset(all.dat.means, Region=="N"), aes(x=Assimilation_mean, y=Stomatal_Conductance_mean, xend=Assimilation_mean, yend=Stomatal_Conductance_mean-Stomatal_Conductance_se)) + 
  stat_ellipse() +
  #scale_color_manual(values=c("goldenrod1","blue","red")) +
  xlab("Carbon assimilation rate") +
  ylab("Stomatal conductance") +
  theme_classic()

Ag.S <- ggplot(subset(all.dat, Region=="S"), aes(Assimilation, Stomatal_Conductance, color=Time)) +
  geom_point(alpha=0.2) +
  geom_point(data=subset(all.dat.means, Region=="S"), aes(x=Assimilation_mean, y=Stomatal_Conductance_mean), size = 3, shape = 17) + 
  geom_segment(data=subset(all.dat.means, Region=="S"), aes(x=Assimilation_mean, y=Stomatal_Conductance_mean, xend=Assimilation_mean+Assimilation_se, yend=Stomatal_Conductance_mean)) + 
  geom_segment(data=subset(all.dat.means, Region=="S"), aes(x=Assimilation_mean, y=Stomatal_Conductance_mean, xend=Assimilation_mean-Assimilation_se, yend=Stomatal_Conductance_mean)) + 
  geom_segment(data=subset(all.dat.means, Region=="S"), aes(x=Assimilation_mean, y=Stomatal_Conductance_mean, xend=Assimilation_mean, yend=Stomatal_Conductance_mean+Stomatal_Conductance_se)) + 
  geom_segment(data=subset(all.dat.means, Region=="S"), aes(x=Assimilation_mean, y=Stomatal_Conductance_mean, xend=Assimilation_mean, yend=Stomatal_Conductance_mean-Stomatal_Conductance_se)) + 
  stat_ellipse() +
  #scale_color_manual(values=c("goldenrod1","blue","red")) +
  xlab("Carbon assimilation rate") +
  ylab("Stomatal conductance") +
  theme_classic()

multi.N <- SLAWC.N + plot_spacer() + plot_spacer() + plot_spacer() +
  SLAFT.N + WCFT.N + plot_spacer() + plot_spacer() +
  SLAA.N + WCA.N + FTA.N + plot_spacer() +
  SLAg.N + WCg.N + FTg.N + Ag.N + 
  plot_layout(ncol=4, nrow=4, guides='collect') 

multi.S <- SLAWC.S + plot_spacer() + plot_spacer() + plot_spacer() +
  SLAFT.S + WCFT.S + plot_spacer() + plot_spacer() +
  SLAA.S + WCA.S + FTA.S + plot_spacer() +
  SLAg.S + WCg.S + FTg.S + Ag.S + 
  plot_layout(ncol=4, nrow=4, guides='collect') 


