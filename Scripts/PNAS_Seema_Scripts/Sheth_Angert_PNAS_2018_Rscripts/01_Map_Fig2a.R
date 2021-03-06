#### PROJECT: Mimulus cardinalis demography 2010-2014
#### PURPOSE: Create map of M. cardinalis study populations (Fig. 2a)
#### Note: GIS files are too large to provide in public repository
#### AUTHOR: Seema Sheth
#### DATE LAST MODIFIED: 20171127

# remove objects and clear workspace
rm(list = ls(all=TRUE))

#load packages
library(sp)
library(maps)
library(mapdata)
library(rgdal)
library(raster)
library(maptools)
library(rgeos)
library(rworldmap)
library(rworldxtra)
library(RColorBrewer)
library(plyr)
library(dplyr)

# set working directory
setwd("/Users/ssheth/Google Drive/demography_PNAS_November2017")

#*************************************
# Plot map of demography populations, Fig. 2a
#*************************************

#read in M. cardinalis demography data and extract lat/lon for each population
data=read.csv("Data/Mcard_demog_data_2010-2013.csv") %>% select(Latitude,Longitude) %>% unique() %>% arrange(-Latitude)

#read in all occurrence data from occupancy MS + Baja records and merge into one data frame
localities=read.csv("data/raw_data/all.records.aug.31.csv")
localities=subset(localities,DATASET=="herb"&PRESABS==1)
localities=subset(localities,select=c("Latitude","Longitude"))
baja=read.csv("data/raw_data/Baja.csv")
baja=subset(baja,select=c("Lat","Lon"))
baja=baja[1:14,] # Remove AZ and Cedros Island localities
colnames(baja)=c("Latitude","Longitude")
localities=rbind(localities,baja)

#create numeric codes for sites
data$Site.code=rev(seq(1,32,1))
lat_cols=colorRampPalette(brewer.pal(11,"Spectral"))
data$Col=lat_cols(32)[as.numeric(cut(data$Latitude,breaks = 32))]

#define projections
prj.wgs = "+proj=longlat +ellps=WGS84"
prj.aea = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0"

#convert to spatial data
coordinates(data) <- ~Longitude+Latitude
class(data)
coordinates(localities)<- ~Longitude+Latitude

#project demography site locations
proj4string(data) = CRS(prj.wgs)
dat.aea = spTransform(data, CRS=CRS(prj.aea))

#project occurrence site locations
proj4string(localities) = CRS(prj.wgs)
localities.aea = spTransform(localities, CRS=CRS(prj.aea))

# crop to buffered bounding box
ymin = min(localities$Latitude) - 1.5
ymax = max(localities$Latitude) + 1.5
xmin = min(data$Longitude) - 1.5 # latitude labels look nicer bounding by demography sites vs. all locality data
xmax = max(data$Longitude) + 1.5 # latitude labels look nicer bounding by demography sites vs. all locality data
e = extent(xmin, xmax, ymin, ymax)

# see 'help' for this function, you  need to set the directory to one folder level above where the destination file is saved 
ecoreg = readOGR(dsn="data/raw_data/ecoregions.shp", layer="us_eco_l3_no_st") 

# project to match sites
ecoreg.wgs = spTransform(ecoreg, CRS(prj.wgs))
ecoreg.aea = spTransform(ecoreg, CRS(prj.aea))

# crop polygon to buffered bounding box
bbox = as(e, "SpatialPolygons")
proj4string(bbox) = CRS(prj.wgs)
bbox.aea = spTransform(bbox, CRS=CRS(prj.aea))
ecoreg.crop.aea <- crop(ecoreg.aea, bbox.aea) 
ecoreg.crop.wgs <- crop(ecoreg.wgs, bbox) 

## world map polygons
data(countriesLow)
data(countriesHigh)
countriesLow.aea = spTransform(countriesLow, CRS=CRS(prj.aea))
countriesHigh.aea = spTransform(countriesHigh, CRS=CRS(prj.aea))

# North American polygon
northAmerica.aea=subset(countriesLow.aea,continent=="North America"|GEO3=="Meso-America")

## state polygons
sta = readOGR("data/raw_data/gz_2010_us_040_00_500k/gz_2010_us_040_00_500k.shp")
projection(sta) = CRS(prj.wgs)
sta.aea = spTransform(sta, CRS=CRS(prj.aea))

## gridlines
# create unprojected gridlines
grd.wgs = gridlines(ecoreg.crop.wgs, ndiscr=200)

# project gridlines
grd.aea = spTransform(grd.wgs, CRS=CRS(prj.aea))

# Make base plotting frame with gridlines
frame.aea <- crop(countriesHigh.aea, bbox.aea); 
frame.wgs = spTransform(frame.aea, CRS=CRS(prj.wgs))
frame.grd = gridlines(frame.wgs, ndiscr=100)
frame.grd.aea = spTransform(frame.grd, CRS=CRS(prj.aea))

# prepare labels for gridlines in unprojected space
gridat <- gridat(frame.grd, side="EN")
#project labels for gridlines
gridat.aea=spTransform(gridat, CRS=CRS(prj.aea), side="EN")

# slim down labels to fit on plots
lab.all = parse(text=as.character(gridat.aea$labels))
lab.cull = lab.all[c(2,4,6,7,8,9)]
coord.all = coordinates(gridat.aea)
coord.cull = coord.all[c(2,4,6,7,8,9),]

# plot North America map that will be inserted as inset in Fig. 2a
# print to pdf
pdf(file="Figures/Fig2a_map_inset.pdf",width=11,height=8.5)
plot(northAmerica.aea,col="white")
plot(frame.aea,add=TRUE,col="lightgrey")
dev.off()

# print to pdf; Fig. 2a
pdf(file="Figures/Fig2a.pdf",width=11,height=8.5)

par(mfrow=c(1,3),las=1,bty="l",xpd=NA,cex.lab=2.3,mar=c(10,5,4,4.5)+.1)
plot(frame.aea, col="lightgrey",xlim=c(-2004864,-1852484))
	sta.aea2 <- crop(sta.aea,frame.aea); plot(sta.aea2, col="lightgrey",add=TRUE)
	grd.aea2 <- crop(frame.grd.aea,frame.aea); plot(grd.aea2, add=T, lty="dashed", col="darkgrey", lwd=1)
	points(localities.aea,pch=21,col="black",bg="white",cex=2)
	plot(sta.aea2,add=T,border="black",lwd=0.2)
	points(dat.aea, pch=21, col="black", bg=adjustcolor(data$Col,alpha=0.75),cex=4) # this weights color by latitude
	text(coord.cull, labels=lab.cull, offset=0.5, pos=c(3,3,4,4,4,4),col="black",cex=2) #this works for single panels, but not multi-panel
	mtext("A",side=3,cex=1.5,adj=-0.18)
dev.off()















