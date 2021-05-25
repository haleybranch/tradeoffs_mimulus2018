#################
# Reduce data to chosen/site year combinations
#################
library(tidyverse)

y3 <- read.csv("Data/y3.csv", header=T) #Imports main dataset
y3$Block <- as.factor(y3$Block) ; y3$Family <- as.factor(y3$Family) # prep factors

#Remove site year comibnations that don't follow the climate pattern we are testing
y8 <- filter(y3, ID_Year!="S02_2010" & ID_Year!="S07_2010" & ID_Year!="S07_2015" & ID_Year!="S07_2016")  #S02, S07
y8 <- filter(y8, ID_Year!="S08_2014" & ID_Year!="S10_2010" & ID_Year!="S10_2015" & ID_Year!="S10_2016") #S08, S10
y8 <- filter(y8, ID_Year!="S15_2016" & ID_Year!= "S17_2016" & ID_Year!="S18_2016" & ID_Year!="S29_2016") #S15, S17, S18, S29
y8 <- filter(y8, ID_Year!="S32_2015" & ID_Year!="S32_2016" & ID_Year!="S36_2016") #S32, S36

y8 <- y8 %>% mutate(Region = ifelse(Latitude >= 40, "1.North", ifelse((Latitude >35) & (Latitude <40), "2.Center","3.South"))) #Add in region

write.csv(y8,'Data/y8.csv') #Export file


# Set up Pre and Peak dataframes for all 11 sites
trait_cor <- y8
trait_cor$Block <- as.factor(trait_cor$Block) ; trait_cor$Family <- as.factor(trait_cor$Family) # prep factors

# select out key traits 
trait_cor <- select(trait_cor, ID_Year, Region, Site.Lat, Drought, Experiment_Date, SLA, Water_Content, Assimilation, Stomatal_Conductance)

#grab just the pre-drought years for all 11 sites
pre <- filter(trait_cor, ID_Year=="S02_2011" | ID_Year=="S07_2011" | ID_Year=="S11_2011"| ID_Year== "S10_2011" | ID_Year== "S08_2011" 
              | ID_Year=="S32_2010" | ID_Year== "S29_2010" | ID_Year== "S18_2010" | ID_Year== "S17_2011" | ID_Year== "S16_2010" | ID_Year== "S36_2011" | ID_Year== "S15_2010") 
#Add pre column
pre[,10] <- "Pre"
colnames(pre)[10]<-"Period"

#Remove Na's from Pre
pre <- na.omit(pre)

#Export pre
write.csv(pre,'Data/pre8.csv') #Export file

#grab just the peak-drought years for all 11 sites 
peak <- filter(trait_cor, ID_Year=="S02_2014" | ID_Year=="S07_2014" | ID_Year=="S11_2016"| ID_Year== "S10_2014" | ID_Year== "S08_2013" 
               | ID_Year=="S32_2014" | ID_Year== "S29_2015" | ID_Year== "S18_2014" | ID_Year== "S17_2015" | ID_Year== "S16_2016" | ID_Year== "S36_2015" | ID_Year== "S15_2015")
#Add peak column
peak[,10] <- "Peak"
colnames(peak)[10]<-"Period"

#Remove Na's from Peak
peak <- na.omit(peak)

#Export peak
write.csv(peak,'Data/peak8.csv') #Export file

#Make all dataset
all <- rbind(pre,peak)
write.csv(all,'Data/all8.csv') #Export file


### Create Regional datasets 


pre.N <- filter(pre, ID_Year== "S17_2011" | ID_Year== "S16_2010" | ID_Year== "S36_2011" | ID_Year== "S15_2010") 
pre.C <- filter(pre, ID_Year== "S10_2011" | ID_Year== "S08_2011"  | ID_Year=="S32_2010" | ID_Year== "S29_2010" | ID_Year== "S18_2010" )
pre.S <- filter(pre, ID_Year=="S02_2011" | ID_Year=="S07_2011") 
#write.csv(pre.N, 'Data/pre.N8.csv') #Export file)
#write.csv(pre.C, 'Data/pre.C8.csv') #Export file)
#write.csv(pre.S, 'Data/pre.S8.csv') #Export file)


peak.N <- filter(peak, ID_Year== "S17_2015" | ID_Year== "S16_2016" | ID_Year== "S36_2015" | ID_Year== "S15_2015")
peak.C <- filter(peak, ID_Year== "S10_2014" | ID_Year== "S08_2013" | ID_Year=="S32_2014" | ID_Year== "S29_2015" | ID_Year== "S18_2014")
peak.S <- filter(peak, ID_Year=="S02_2014" | ID_Year=="S07_2014")
#write.csv(peak.N, 'Data/peak.N8.csv') #Export file)
#write.csv(peak.C, 'Data/peak.C8.csv') #Export file)
#write.csv(peak.S, 'Data/peak.S8.csv') #Export file)



