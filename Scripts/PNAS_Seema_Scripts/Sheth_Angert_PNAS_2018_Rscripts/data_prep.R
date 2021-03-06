#### PROJECT: Mimulus cardinalis demography 2010-2014
#### PURPOSE: 1) Convert certain columns to factors
############# 2) Omit data that should not have been recorded as new recruits
############# 3) Save object of all remaining data that should go towards total fruit count and seed set per site
############# 4) Remove data corresponding to plants that did not represent single unique individuals
#### AUTHOR: Seema Sheth
#### DATE LAST MODIFIED: 20171110

#read in data
data=read.csv("Data/Mcard_demog_data_2010-2013.csv") #17660 rows of data

#*******************************************************************************
#### 1) Convert certain columns to factors and sort in order of descending latitude
#*******************************************************************************

# convert Site, Year and ID columns to factors
data$Site=factor(data$Site)
data$Year=factor(data$Year)
data$ID=factor(data$ID)
data$NotARecruit=factor(data$NotARecruit)
data$NotAnIndividual=factor(data$NotAnIndividual)

# sort data by latitude
data=data[order(-data$Latitude),]

#*******************************************************************************
#### 2) Omit data that should not have been recorded as new recruits (based on Amy Angert's cleaning in July 2016)
#*******************************************************************************

# Remove plants that should not have been recorded as new recruits
#### NOTE: these are plants that A. Angert noted as "wrong, definitely exclude (reasons include new plot, site not visited in prior year, ID within prior years' ranges, coordinates well outside of prior year's search)"
data=subset(data,NotARecruit!=1|is.na(NotARecruit))
unique(data$NotARecruit)
length(data$Site) # 16971 rows; NOTE: there are 8 rows in which NotAnIndividual=1 & NotARecruit=1

#*******************************************************************************
#### 3) Save object of all remaining data that should go towards total fruit count and seed set per site and year
#*******************************************************************************

# Obtain total fruit and seed counts for each indivdiual at each site in each year, including monster plants
site_fruit_count_data=subset(data,select=c(Site,Year,Region,Fec1,SeedCt)) 
length(site_fruit_count_data$Site) # 16971 rows

#*******************************************************************************
#### 4) Remove data corresponding to plants that did not represent single unique individuals based on Amy Angert's cleaning in July 2016
#*******************************************************************************

# Remove monster plants where individuals were not distinguished
#### NOTE: these are plants that A. Angert noted as "not ok, definitely exclude from survival, growth, and fecundity but ok for seed input denominator for recruitment (history of lumping/splitting/relumping; redundant IDs)"
data=subset(data,NotAnIndividual!=1|is.na(NotAnIndividual))
unique(data$NotAnIndividual)
length(data$Site) # 16910 rows
data=subset(data,select=-c(NotAnIndividual,NotARecruit,Reasoning,Reasoning.1))
