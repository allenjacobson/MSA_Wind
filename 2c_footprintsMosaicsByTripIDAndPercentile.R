library(data.table)
#library(raster)
library(terra)
#library(sf)

# This scripts pulls builds mosaic rasters by tripid and percentile
# and then writes new rasters
# Creates a dictionary with these paths, percentiles, the IMGIDs, and TripIDs

##############################
# Functions
rasterMosaicFromPathList <- function(rasterList, thisTrip){
  length <- length(rasterList)
  if(length == 1){
    rasterMosaic <- rast(rasterList)
    return(rasterMosaic)
  } else if (length > 1){
    rasters <- lapply(X = rasterList, FUN = rast)
    rasterMosaic <- do.call(mosaic, args = c(rasters, fun = "max"))
    return(rasterMosaic)
  } else{ 
    thisWarning <- paste("Error with ", thisTrip, sep = " ")
    warning(thisWarning)
  }
}

##############################
# Set directories
pwd <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

repositoryName <- "MSA_Wind_FootprintBias"
basePath <- "C:/Users/lianne.allen-jacobso/Documents/"
pwdCheck <- paste0(basePath, "Repositories/",repositoryName)
pwd == pwdCheck

outputDir <- paste0(basePath, "Output/", repositoryName)
dataDir <- paste0(basePath, "Data/", repositoryName)

##############################
# Pull in data
dtLocalPathsForVTRB_ByPercentileIMGIDAndTripID <-
  readRDS(paste0(outputDir, "/dtLocalPathsForVTRB_ByPercentileIMGIDAndTripID.rds"))

##############################
# Raster mosaics by percentiles
uniqueTrips <- unique(dtLocalPathsForVTRB_ByPercentileIMGIDAndTripID$trip_ID)
# create empty data.table to add file paths
#dtCumulativeRasterPaths <- data.table()
dtLocalPathsForVTRBMosaic_ByPercentileAndTripID <- data.table()

for (thisTrip in uniqueTrips) {
  # break each trip table into sub tables for each percentile
  dt25 <- dtLocalPathsForVTRB_ByPercentileIMGIDAndTripID[trip_ID==thisTrip & percentile=="25th"]
  dt50 <- dtLocalPathsForVTRB_ByPercentileIMGIDAndTripID[trip_ID==thisTrip & percentile=="50th"]
  dt75 <- dtLocalPathsForVTRB_ByPercentileIMGIDAndTripID[trip_ID==thisTrip & percentile=="75th"]
  dt100 <- dtLocalPathsForVTRB_ByPercentileIMGIDAndTripID[trip_ID==thisTrip & percentile=="100th"]
  # create mosaic for each percentile - these are non-cumulative
  mosaic25<- rasterMosaicFromPathList(dt25$paths, thisTrip)
  mosaic50<- rasterMosaicFromPathList(dt50$paths, thisTrip)
  mosaic75<- rasterMosaicFromPathList(dt75$paths, thisTrip)
  mosaic100<- rasterMosaicFromPathList(dt100$paths, thisTrip)
  # create cumulative mosaic for each percentile
  rasterCumulative25 <- mosaic25
  rasterCumulative50 <- mosaic(mosaic50, rasterCumulative25, fun = "max") 
  rasterCumulative75 <- mosaic(mosaic75, rasterCumulative50, fun = "max")
  rasterCumulative100 <- mosaic(mosaic100, rasterCumulative75, fun = "max")
  # create file names and write cumulative rasters
  basePath <- paste0(outputDir, "/VTRBmosaicByPercentile_TripID")
  
  fileName25 <- paste0(basePath, "/25thPercentile_", thisTrip,".tif")
  writeRaster(rasterCumulative25, fileName25, overwrite=TRUE)
  
  fileName50 <- paste0(basePath, "/50thPercentile_", thisTrip,".tif")
  writeRaster(rasterCumulative50, fileName50, overwrite=TRUE)
  
  fileName75 <- paste0(basePath, "/75thPercentile_", thisTrip,".tif")
  writeRaster(rasterCumulative75, fileName75, overwrite=TRUE)
  
  fileName100 <- paste0(basePath, "/100thPercentile_", thisTrip,".tif")
  writeRaster(rasterCumulative100, fileName100, overwrite=TRUE)
  
  thesePaths <- data.table(trip_ID= c(thisTrip, thisTrip, thisTrip, thisTrip),
                           percentile=c("25th", "50th", "75th", "100th"),
                           paths = c(fileName25,fileName50, fileName75,fileName100))
  # Add file paths to data.table
  dtLocalPathsForVTRBMosaic_ByPercentileAndTripID <- rbindlist(list(dtLocalPathsForVTRBMosaic_ByPercentileAndTripID, thesePaths))
}

saveRDS(dtLocalPathsForVTRBMosaic_ByPercentileAndTripID,
        paste0(outputDir, "/dtLocalPathsForVTRBMosaic_ByPercentileAndTripID.rds"))
