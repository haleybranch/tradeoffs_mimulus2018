# Trait correlations with the raw data, comparing pre and post drought years 

#read in packages 
library(tidyverse)
library(Hmisc)
library(cocor)
library(patchwork)

pre <- read.csv("Data/pre.csv", header = T)
peak <- read.csv("Data/peak.csv", header = T)

#Remove unnecessary columns 
pre <- select(pre, Experiment_Date, SLA, Water_Content, Assimilation, Stomatal_Conductance)
peak <-select(peak, Experiment_Date, SLA, Water_Content, Assimilation, Stomatal_Conductance)

#Stack for visualizations below
pre$Time <- "Pre"
peak$Time <- "Peak"
dat <- rbind(pre, peak)

#Summaries for plotting
dat.means <- dat %>% 
  group_by(Time) %>% 
  summarise_each(funs(mean(., na.rm=T), n = sum(!is.na(.)), se = sd(., na.rm=T)/sqrt(sum(!is.na(.)))), Experiment_Date:Stomatal_Conductance)


#Change format to matrix
pre <- pre %>% select(-Time)
pre <- as.matrix(pre)
peak <- peak %>% select(-Time)
peak <- as.matrix(peak)

#running matrix correlation 
pre_r <- rcorr(pre)
peak_r <- rcorr(peak)

#Save as dataframe
pre_corr<-pre_r$r
peak_corr<-peak_r$r
pre_p<-pre_r$P
peak_p<-peak_r$P

#Write Table
#write.csv(pre_corr, "Tables/pre_r.csv")
#write.csv(pre_p, "Tables/pre_p.csv")

#write.csv(peak_corr, "Tables/peak_r.csv")
#write.csv(peak_p, "Tables/peak_p.csv")

# test whether pairwise correlations differ pre vs peak
cocor(~ SLA + Water_Content | SLA + Water_Content, data=list(pre, peak))
cocor(~ SLA + Experiment_Date | SLA + Experiment_Date, data=list(pre, peak))
cocor(~ SLA + Assimilation | SLA + Assimilation, data=list(pre, peak))
cocor(~ SLA + Stomatal_Conductance | SLA + Stomatal_Conductance, data=list(pre, peak))
cocor(~ Water_Content + Experiment_Date | Water_Content + Experiment_Date, data=list(pre, peak))
cocor(~ Water_Content + Assimilation | Water_Content + Assimilation, data=list(pre, peak))
cocor(~ Water_Content + Stomatal_Conductance | Water_Content + Stomatal_Conductance, data=list(pre, peak))
cocor(~ Experiment_Date + Assimilation | Experiment_Date + Assimilation, data=list(pre, peak))
cocor(~ Experiment_Date + Stomatal_Conductance | Experiment_Date + Stomatal_Conductance, data=list(pre, peak))
cocor(~ Assimilation + Stomatal_Conductance | Assimilation + Stomatal_Conductance, data=list(pre, peak))


SLAWC <- ggplot(dat, aes(SLA, Water_Content, color=Time)) +
  geom_point(alpha=0.3) +
  geom_point(data=dat.means, aes(x=SLA_mean, y=Water_Content_mean), size = 3, shape = 17) + 
  geom_segment(data=dat.means, aes(x=SLA_mean, y=Water_Content_mean, xend=SLA_mean+SLA_se, yend=Water_Content_mean)) + 
  geom_segment(data=dat.means, aes(x=SLA_mean, y=Water_Content_mean, xend=SLA_mean-SLA_se, yend=Water_Content_mean)) + 
  geom_segment(data=dat.means, aes(x=SLA_mean, y=Water_Content_mean, xend=SLA_mean, yend=Water_Content_mean+Water_Content_se)) + 
  geom_segment(data=dat.means, aes(x=SLA_mean, y=Water_Content_mean, xend=SLA_mean, yend=Water_Content_mean-Water_Content_se)) + 
  stat_ellipse() +
  xlab("Specific leaf area") +
  ylab("Water content (%)") +
  theme_classic()

SLAFT <- ggplot(dat, aes(SLA, Experiment_Date, color=Time)) +
  geom_point(alpha=0.3) +
  geom_point(data=dat.means, aes(x=SLA_mean, y=Experiment_Date_mean), size = 3, shape = 17) + 
  geom_segment(data=dat.means, aes(x=SLA_mean, y=Experiment_Date_mean, xend=SLA_mean+SLA_se, yend=Experiment_Date_mean)) + 
  geom_segment(data=dat.means, aes(x=SLA_mean, y=Experiment_Date_mean, xend=SLA_mean-SLA_se, yend=Experiment_Date_mean)) + 
  geom_segment(data=dat.means, aes(x=SLA_mean, y=Experiment_Date_mean, xend=SLA_mean, yend=Experiment_Date_mean+Experiment_Date_se)) + 
  geom_segment(data=dat.means, aes(x=SLA_mean, y=Experiment_Date_mean, xend=SLA_mean, yend=Experiment_Date_mean-Experiment_Date_se)) + 
  stat_ellipse() +
  xlab("Specific leaf area") +
  ylab("Date of first flower") +
  theme_classic()

SLAA <- ggplot(dat, aes(SLA, Assimilation, color=Time)) +
  geom_point(alpha=0.3) +
  geom_point(data=dat.means, aes(x=SLA_mean, y=Assimilation_mean), size = 3, shape = 17) + 
  geom_segment(data=dat.means, aes(x=SLA_mean, y=Assimilation_mean, xend=SLA_mean+SLA_se, yend=Assimilation_mean)) + 
  geom_segment(data=dat.means, aes(x=SLA_mean, y=Assimilation_mean, xend=SLA_mean-SLA_se, yend=Assimilation_mean)) + 
  geom_segment(data=dat.means, aes(x=SLA_mean, y=Assimilation_mean, xend=SLA_mean, yend=Assimilation_mean+Assimilation_se)) + 
  geom_segment(data=dat.means, aes(x=SLA_mean, y=Assimilation_mean, xend=SLA_mean, yend=Assimilation_mean-Assimilation_se)) + 
  stat_ellipse() +
  xlab("Specific leaf area") +
  ylab("Carbon assimilation rate") +
  theme_classic()

SLAg <- ggplot(dat, aes(SLA, Stomatal_Conductance, color=Time)) +
  geom_point(alpha=0.3) +
  geom_point(data=dat.means, aes(x=SLA_mean, y=Stomatal_Conductance_mean), size = 3, shape = 17) + 
  geom_segment(data=dat.means, aes(x=SLA_mean, y=Stomatal_Conductance_mean, xend=SLA_mean+SLA_se, yend=Stomatal_Conductance_mean)) + 
  geom_segment(data=dat.means, aes(x=SLA_mean, y=Stomatal_Conductance_mean, xend=SLA_mean-SLA_se, yend=Stomatal_Conductance_mean)) + 
  geom_segment(data=dat.means, aes(x=SLA_mean, y=Stomatal_Conductance_mean, xend=SLA_mean, yend=Stomatal_Conductance_mean+Stomatal_Conductance_se)) + 
  geom_segment(data=dat.means, aes(x=SLA_mean, y=Stomatal_Conductance_mean, xend=SLA_mean, yend=Stomatal_Conductance_mean-Stomatal_Conductance_se)) + 
  stat_ellipse() +
  xlab("Specific leaf area") +
  ylab("Stomatal conductance") +
  theme_classic()

WCFT <- ggplot(dat, aes(Water_Content, Experiment_Date, color=Time)) +
  geom_point(alpha=0.3) +
  geom_point(data=dat.means, aes(x=Water_Content_mean, y=Experiment_Date_mean), size = 3, shape = 17) + 
  geom_segment(data=dat.means, aes(x=Water_Content_mean, y=Experiment_Date_mean, xend=Water_Content_mean+Water_Content_se, yend=Experiment_Date_mean)) + 
  geom_segment(data=dat.means, aes(x=Water_Content_mean, y=Experiment_Date_mean, xend=Water_Content_mean-Water_Content_se, yend=Experiment_Date_mean)) + 
  geom_segment(data=dat.means, aes(x=Water_Content_mean, y=Experiment_Date_mean, xend=Water_Content_mean, yend=Experiment_Date_mean+Experiment_Date_se)) + 
  geom_segment(data=dat.means, aes(x=Water_Content_mean, y=Experiment_Date_mean, xend=Water_Content_mean, yend=Experiment_Date_mean-Experiment_Date_se)) + 
  stat_ellipse() +
  xlab("Water content (%") +
  ylab("Date of first flower") +
  theme_classic()

WCA <- ggplot(dat, aes(Water_Content, Assimilation, color=Time)) +
  geom_point(alpha=0.3) +
  geom_point(data=dat.means, aes(x=Water_Content_mean, y=Assimilation_mean), size = 3, shape = 17) + 
  geom_segment(data=dat.means, aes(x=Water_Content_mean, y=Assimilation_mean, xend=Water_Content_mean+Water_Content_se, yend=Assimilation_mean)) + 
  geom_segment(data=dat.means, aes(x=Water_Content_mean, y=Assimilation_mean, xend=Water_Content_mean-Water_Content_se, yend=Assimilation_mean)) + 
  geom_segment(data=dat.means, aes(x=Water_Content_mean, y=Assimilation_mean, xend=Water_Content_mean, yend=Assimilation_mean+Assimilation_se)) + 
  geom_segment(data=dat.means, aes(x=Water_Content_mean, y=Assimilation_mean, xend=Water_Content_mean, yend=Assimilation_mean-Assimilation_se)) + 
  stat_ellipse() +
  xlab("Water content (%)") +
  ylab("Carbon assimilation rate") +
  theme_classic()

WCg <- ggplot(dat, aes(Water_Content, Stomatal_Conductance, color=Time)) +
  geom_point(alpha=0.3) +
  geom_point(data=dat.means, aes(x=Water_Content_mean, y=Stomatal_Conductance_mean), size = 3, shape = 17) + 
  geom_segment(data=dat.means, aes(x=Water_Content_mean, y=Stomatal_Conductance_mean, xend=Water_Content_mean+Water_Content_se, yend=Stomatal_Conductance_mean)) + 
  geom_segment(data=dat.means, aes(x=Water_Content_mean, y=Stomatal_Conductance_mean, xend=Water_Content_mean-Water_Content_se, yend=Stomatal_Conductance_mean)) + 
  geom_segment(data=dat.means, aes(x=Water_Content_mean, y=Stomatal_Conductance_mean, xend=Water_Content_mean, yend=Stomatal_Conductance_mean+Stomatal_Conductance_se)) + 
  geom_segment(data=dat.means, aes(x=Water_Content_mean, y=Stomatal_Conductance_mean, xend=Water_Content_mean, yend=Stomatal_Conductance_mean-Stomatal_Conductance_se)) + 
  stat_ellipse() +
  xlab("Water content (%)") +
  ylab("Stomatal conductance") +
  theme_classic()

FTA <- ggplot(dat, aes(Experiment_Date, Assimilation, color=Time)) +
  geom_point(alpha=0.3) +
  geom_point(data=dat.means, aes(x=Experiment_Date_mean, y=Assimilation_mean), size = 3, shape = 17) + 
  geom_segment(data=dat.means, aes(x=Experiment_Date_mean, y=Assimilation_mean, xend=Experiment_Date_mean+Experiment_Date_se, yend=Assimilation_mean)) + 
  geom_segment(data=dat.means, aes(x=Experiment_Date_mean, y=Assimilation_mean, xend=Experiment_Date_mean-Experiment_Date_se, yend=Assimilation_mean)) + 
  geom_segment(data=dat.means, aes(x=Experiment_Date_mean, y=Assimilation_mean, xend=Experiment_Date_mean, yend=Assimilation_mean+Assimilation_se)) + 
  geom_segment(data=dat.means, aes(x=Experiment_Date_mean, y=Assimilation_mean, xend=Experiment_Date_mean, yend=Assimilation_mean-Assimilation_se)) + 
  stat_ellipse() +
  xlab("Date of first flower") +
  ylab("Carbon assimilation rate") +
  theme_classic()

FTg <- ggplot(dat, aes(Experiment_Date, Stomatal_Conductance, color=Time)) +
  geom_point(alpha=0.3) +
  geom_point(data=dat.means, aes(x=Experiment_Date_mean, y=Stomatal_Conductance_mean), size = 3, shape = 17) + 
  geom_segment(data=dat.means, aes(x=Experiment_Date_mean, y=Stomatal_Conductance_mean, xend=Experiment_Date_mean+Experiment_Date_se, yend=Stomatal_Conductance_mean)) + 
  geom_segment(data=dat.means, aes(x=Experiment_Date_mean, y=Stomatal_Conductance_mean, xend=Experiment_Date_mean-Experiment_Date_se, yend=Stomatal_Conductance_mean)) + 
  geom_segment(data=dat.means, aes(x=Experiment_Date_mean, y=Stomatal_Conductance_mean, xend=Experiment_Date_mean, yend=Stomatal_Conductance_mean+Stomatal_Conductance_se)) + 
  geom_segment(data=dat.means, aes(x=Experiment_Date_mean, y=Stomatal_Conductance_mean, xend=Experiment_Date_mean, yend=Stomatal_Conductance_mean-Stomatal_Conductance_se)) + 
  stat_ellipse() +
  xlab("Date of first flower") +
  ylab("Stomatal conductance") +
  theme_classic()

Ag <- ggplot(dat, aes(Assimilation, Stomatal_Conductance, color=Time)) +
  geom_point(alpha=0.3) +
  geom_point(data=dat.means, aes(x=Assimilation_mean, y=Stomatal_Conductance_mean), size = 3, shape = 17) + 
  geom_segment(data=dat.means, aes(x=Assimilation_mean, y=Stomatal_Conductance_mean, xend=Assimilation_mean+Assimilation_se, yend=Stomatal_Conductance_mean)) + 
  geom_segment(data=dat.means, aes(x=Assimilation_mean, y=Stomatal_Conductance_mean, xend=Assimilation_mean-Assimilation_se, yend=Stomatal_Conductance_mean)) + 
  geom_segment(data=dat.means, aes(x=Assimilation_mean, y=Stomatal_Conductance_mean, xend=Assimilation_mean, yend=Stomatal_Conductance_mean+Stomatal_Conductance_se)) + 
  geom_segment(data=dat.means, aes(x=Assimilation_mean, y=Stomatal_Conductance_mean, xend=Assimilation_mean, yend=Stomatal_Conductance_mean-Stomatal_Conductance_se)) + 
  stat_ellipse() +
  xlab("Carbon assimilation rate") +
  ylab("Stomatal conductance") +
  theme_classic()

multi <- SLAWC + plot_spacer() + plot_spacer() + plot_spacer() +
          SLAFT + WCFT + plot_spacer() + plot_spacer() +
          SLAA + WCA + FTA + plot_spacer() +
          SLAg + WCg + FTg + Ag + 
  plot_layout(ncol=4, nrow=4, guides='collect') 
