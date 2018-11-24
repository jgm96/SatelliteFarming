##################################################################################### 
##################################################################################### 
#' Process and extracts MOD09GQ MODIS data product for a particular extent and date interval
#'
#' Daily Tile Surface Reflectance Bands 1-2 product with a ground resolution of 250m
#
#' https://lpdaac.usgs.gov/products/modis_products_table/mod09gq
#' http://modis-sr.ltdri.org/products/MOD09_UserGuide_v1_3.pdf
#' 
#' The function requires access to GDAL commands. For instance via the following command providing the
#' right path to GDAL in your system:  
#' Sys.setenv(PATH="$PATH:/Library/Frameworks/GDAL.framework/Programs")
#'
#' @param targetPoints JSON with the coordinates of the bounding box for the target zone: 
#'        ULx X coordinate of the upper-left corner 
#'        ULy Y coordinate of the upper-left corner     
#'        URx X coordinate of the upper-right corner      
#'        URy Y coordinate of the upper-right corner   
#'        LRx X coordinate of the lower-right corner    
#'        LRy Y coordinate of the lower-right corner   
#'        LLx X coordinate of the lower-left corner    
#'        LLy Y coordinate of the lower-left corner
#'
#'@param CRSString Coordinate reference system. CRS string of the targetPoints. 
#'                 Default = "+proj=utm +zone=29"
#'                 
#'@param startDate A string with the start date. Default=Sys.Date()-2
#'                 Format: yyyy/mm/dd
#'                 
#'@param endDate A string with the end date. Default = Sys.Date()-2 
#'                 Format: yyyy/mm/dd
#'                 
#'                 
#'@return JSON with the reflectivity values: x, y, date, refl_b01, refl_b02, QC_250m
#'
#'
####################################################################################


getMOD09GQ  <- function(targetPoints
                        , startDate=Sys.Date()-2
                        , endDate=Sys.Date()-2
                        , CRSString="+proj=utm +zone=29"){
  
  # require(rjson)
  # require(MODIS)
  # require(bitops)
  #require(RCurl)
  # require(rgdal)
  # require(mapdata)
  # require(maptools)
  # require(snow)
  # require(ptw)
  
  dataProduct  <-  "MOD09GQ"
  
  # Initialization of variables to store the values in the dataframe
  coordinates  <- NULL
  dateList    <- NULL
  refb01  <- NULL
  refb02  <- NULL
  QC_250m <- NULL
  
    # For testing
    #startDate=inicio
    # endDate=fin
  
  df.sur_refl <- data.frame(x="", y="", date="", sur_refl_b01="", sur_refl_b02="", QC_250m="")
  dateInterval  <- transDate(begin=startDate, end=endDate)
  
  # Converting the JSON BoundingBox to a dataframe
  #targetPoints.df  <- data.frame(fromJSON(json_str = targetPoints))
  
  targetPoints.df  <- targetPoints
     # For testing
     #targetPoints.df  <- df
     #CRSString="+proj=utm +zone=21J"
  
  
  # Getting the extent of the target zone
  x  <- c(targetPoints.df[1,"ULx"], targetPoints.df[1,"URx"],targetPoints.df[1,"DRx"],targetPoints.df[1,"DLx"])
  y  <- c(targetPoints.df[1,"ULy"], targetPoints.df[1,"URy"],targetPoints.df[1,"DRy"],targetPoints.df[1,"DLy"])
  xy <- cbind(x,y)
  S <- SpatialPoints(xy, proj4string=CRS(CRSString))
  
  
  for (fecha in dateInterval$begin:dateInterval$end){
       # For testing
       #fecha=dateInterval$begin:dateInterval$end
    
    start.time <- Sys.time()
   
    day=transDate(begin=as.Date.numeric(fecha, origin = "1970-01-01"), end=as.Date.numeric(fecha, origin = "1970-01-01"))
    day = day$beginDOY
    
    message(paste0("Downloading MODIS images for the date ", day))
    
    try({
      
      # Downloading the MODIS HDF file and extracting the HDF file layers into TIF files
      runGdal(product=dataProduct,begin=day,end=day,
              outProj = proj4string(S),
              pixelSize=250, 
              job=dataProduct,
              #forceDownload = TRUE,
              overwrite = TRUE,
              extent=extent(spTransform(S, CRS("+init=epsg:4326")))
      ) 
      
      thehdf=getHdf(product=dataProduct, begin=day, end=day, wait = 2,
                    extent=extent(spTransform(S, CRS("+init=epsg:4326")))
      )
      
      layerNames  <-  getSds(thehdf[[1]][1])$SDSnames
      
      # Extracts data from bands: sur_refl_b01_1 and sur_refl_b02_1
      layerNames  <- layerNames[2:3]
      
      for(layer in layerNames) {
        
            # For testing
            #layer="sur_refl_b01_1"
        
        message(paste0("Procesing layer ",layer," day...", day))
        
        # Looking for the names of all the tiff files 
        files  <-  preStack(path = paste0(options()$MODIS_outDirPath,dataProduct),pattern=paste0("MOD09GQ.A",day,".",layer))
        
        # Creating a raster stack for each MODIS band and day
        dataproduct= stack(files)
        
        # Crop out the target zone (250m buffer around it)
        dataproduct = crop(dataproduct,extend(extent(S),250))
        
        # Saving the data as an R object
        assign(layer,dataproduct)
      }
      
      ndvi  <-  (values(sur_refl_b02_1) - values(sur_refl_b01_1)) / (values(sur_refl_b02_1) + values(sur_refl_b01_1)) 
  
      # Scaled for values [0..255]
      ndvi.scaled  <- (ndvi+1)*127
      
      message(paste("Generating dataset day.............:",day))
      df.sur_refl <- data.frame(coordinates=coordinates(sur_refl_b01_1), date=day, sur_refl_b01=values(sur_refl_b01_1), 
                                sur_refl_b02=values(sur_refl_b02_1), NDVI=ndvi, NDVI_SCALED=ndvi.scaled)
      
      names(df.sur_refl)  <- c("x","y", "date", "sur_refl_b01", "sur_refl_b02","NDVI", "NDVI_SCALED")
      
      message(paste("Writing CSV day ....................",day))
      write.csv(df.sur_refl,file=paste0("SatelliteData//data//",dataProduct,"_",day,".csv"), row.names=F)
      message(paste0("CSV completed and stored in /Home/MODIS/SatelliteData/data/",dataProduct,"_",day,".csv"))
      
      end.time <- Sys.time()
      time.taken <- end.time - start.time
      time.taken
      message(paste("Time required.......................",time.taken/60,"minutes"))
    }, TRUE)
    
  }
}