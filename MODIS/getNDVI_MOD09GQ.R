#################################################################
# Function to calculate the NDVI and NDVI scaled from 
# reflectivity bands 1 and 2 of MOD09GQ dataproduct
#
# Parameters:
# band1: a list of reflectivity values from MOD09GQ band 1
# band2: a list of reflectivity values from MOD09GQ band 2
#
# Return a data frame with NDVI and NDVI.SCALED [0..255] values
################################################################
getNDVI_MOD09GQ  <- function(band1, band2){
  
  ndvi  <-  (band2 - band1) / (band2 + band1)
  ndvi.scaled  <- (ndvi+1)*127
  
  df  <- data.frame(ndvi=ndvi, ndvi.scaled=ndvi.scaled)
  names(df)  <- c("NDVI","NDVI.SCALED")
  
  return(df)
}