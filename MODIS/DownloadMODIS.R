############################################################################################
# The Main script to extract MODIS data products
# MOD09GQ, Daily Tile Surface Reflectance Bands 1-2 product with a ground resolution of 250m
############################################################################################
library("MODIS")
library("bitops")
library("RCurl")
library("rgeos")
library("rgdal")
library("mapdata")
library("maptools")
library("ptw")
library("geostatsp")

source(file="getMOD09GQ.R")
# source(file="MD09G_Points_Plot.R")

#############################################
## Setting up environment variables for MODIS
#############################################
user  <-"uniovi"

Sys.setenv(MRT_HOME=paste0("/Users/",user,"/MRT"))
Sys.setenv(MRT_DATA_DIR=paste0("/Users/",user,"/MRT/data"))
# Sys.setenv(PATH=paste0("$PATH:/Users/",user,"/MRT/bin:/Library/Frameworks/GDAL.framework/Programs"))
dataDir = paste0("/Users/",user,"/spatialData/")

# MODIS Options setup
MODISoptions(gdalPath="/usr/bin", localArcPath = "data/MODIS_ARC",
             outDirPath = "/home/uniovi/MODIS_ARC/PROCESSED", 
             MODISserverOrder = c("LAADS", "LPDAAC"))

# MODISoptions()[grep("MODIS", names(options()), value=T)]

# Checking MODIS Tools 
MODIS:::checkTools()

##############################
# Defining Target Data Points
##############################
  
# 

lat1 <- -31.3997222
lat2 <- -27.6608333

#lat2 <- -27.0177778
#lat1 <- -31.9830556

#lon2 <- -057.0002778
#lon1 <- -058.9830556 

lon1 <- -058.9830556
lon2 <- -057.0002778

lat <- c(lat1, lat2)
lon <- c(lon1, lon2)

coords <- as.data.frame(cbind(lon, lat))
points <- coords

coordinates(points) <- ~lon + lat
crs(points) <- CRS("+proj=longlat +datum=WGS84")

crs="+proj=utm +zone=21 +south"
points <- spTransform(points, CRS(crs))

############################################
# Map representation of the target zone 
# (Not required. Only for testing)
############################################
library(ggmap)
library(ggplot2)

# map <- get_map(location = c(lon = mean(lon), lat = mean(lat)), zoom = 7,
#                maptype = "hybrid", source = "google")
# 
# plotMap <- ggmap(map)+
#   scale_x_continuous(limits = lon, expand = c(0, 0)) +
#   scale_y_continuous(limits = lat, expand = c(0, 0))
# 
# plotMap

#################################################
# Defining the bounding box of the target points
#################################################

# ULx Upper Left Corner x
# ULy Upper Left Corner y
# ULx <- lon1
# ULy <- lat1

ULx <- extent(points)[1]
ULy <- extent(points)[3]


# URx Upper Rigth Corner x
# URy Upper Rigth Corner y
# URx <- lon2
# URy <- lat1

URx <- extent(points)[2]
URy <- extent(points)[3]

# DRx Down Right Corner x
# DRy Down Right Corner y
# DRx <- lon2
# DRy <- lat2

DRx <- extent(points)[2]
DRy <- extent(points)[4]

# DLx Down Left Corner x
# DLy Down Left Corner y
# DLx <- lon1
# DLy <- lat2

DLx <- extent(points)[1]
DLy <- extent(points)[4]

df  <- data.frame(parcel=1, ULx=ULx, ULy=ULy, URx=URx, URy=URy, DRx=DRx, DRy=DRy, DLx=DLx, DLy=DLy)
names(df)  <- c("parcel","ULx", "ULy","URx", "URy","DRx", "DRy","DLx", "DLy")


##################################
# Setup of the date interval  
##################################
inicio = '2007/04/17'
#inicio = '2005/01/01'
#fin = '2017/11/30'
fin = '2007/04/17'

#################################
# Retrieves dataproduct MOD09GQ
# MOD09GQ, Daily Tile Surface Reflectance Bands 1-2 product with a ground resolution of 250m
#################################
getMOD09GQ(targetPoints=df, startDate=as.Date(inicio), endDate=as.Date(fin), CRSString=crs)

# Save the id of the sectors of each parcel in a CSV file and plot the parcels to a PNG file 
# plotParcelSectorsMD09G(dataProduct="MOD09GQ", NumberOfParcels=1, coordinatesUTM=df)

