library(pracma)
library(raster)
library(proj4)
library(sp)
5236
#Coordinates
northing <- 407083.21
easting <- 6578458.58

filePath = '/media/jgm/Toshiba\ HDD/SatelliteData/data/'
filePath = '/media/jgm/TOSHIBA EXT/SatelliteData/data/'
file = 'MOD09GQ_2007103.csv'
mydata <- read.table(paste(filePath,file, sep = ""),header=TRUE,sep=",")
mydata$'(x-northing)²' <- (mydata$x-northing)**2
mydata$'(y-easting)²' <- (mydata$y-easting)**2
mydata$'DISTANCE' <- sqrt(mydata$`(x-northing)²`+mydata$`(y-easting)²`)
minDistance <- min(mydata[,10], na.rm = T)
options(digits = 15)
print(minDistance)
print(mydata[which(mydata$'DISTANCE'==minDistance),4:7])



