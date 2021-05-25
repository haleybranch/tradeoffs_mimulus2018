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





