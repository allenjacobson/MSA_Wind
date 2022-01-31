library(data.table)
#library(raster)
library(terra)
#library(sf)

# This script splits local raster by percentile and then writes new rasters
# Creates a dictionary with these paths, percentiles, the IMGIDs, and TripIDs

##############################
# Functions

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
dtLocalPathsForVTRB_ByIMGIDAndTripID <-
  readRDS(paste0(outputDir, "/dtLocalPathsForVTRB_ByIMGIDAndTripID.rds"))

##############################
# Split raster by percentile
dtLocalPathsForVTRB_ByPercentileIMGIDAndTripID <- data.table()
uniqueIMGIDs <- dtLocalPathsForVTRB_ByIMGIDAndTripID$IMGID

for (thisIMGID in uniqueIMGIDs) {
  thisTripID <- dtLocalPathsForVTRB_ByIMGIDAndTripID[IMGID==thisIMGID,
                                                     trip_ID] # find tripID for IMGID
  pathForIMGID <- dtLocalPathsForVTRB_ByIMGIDAndTripID[IMGID==thisIMGID,
                                                       newPath] #find local path
  fileCheck <- file.exists(pathForIMGID)
    if(length(fileCheck) == 0){
      thisWarning <- paste("File missing for IMGID: ", thisIMGID, sep = " ")
      warning(thisWarning) #return error if file is missing
      next
    } else if (fileCheck == TRUE){
      
      rastForIMGID <- rast(pathForIMGID)
  
      uniqueValues <- setDT(unique(rastForIMGID)) #select unique cell values in raster
      uniqueValues <- setNames(uniqueValues, "values") #rename column
      setorder(uniqueValues, -values) # reorder values
      # add column identifyiny value by its percentile - assumes that percentiles can be ranked
      # this could be wrong if there are large chunks missing from some percentiles but not others
      # however, a quick look shows the same colors for each buffer, suggesting they can be ranked
      uniqueValues[, percentile := c("25th", "50th", "75th", "100th", "outsideBuffer")]
      
      # select parts of raster equal to unique values
      raster25thPercentile<- rastForIMGID == uniqueValues[percentile=="25th", values]
      raster50thPercentile<- rastForIMGID == uniqueValues[percentile=="50th", values]
      raster75thPercentile<- rastForIMGID == uniqueValues[percentile=="75th", values]
      raster100thPercentile<- rastForIMGID == uniqueValues[percentile=="100th", values]
      
      # create file names, save rasters
      basePath <- paste0(outputDir, "/VTRBsplitByPercentile_TripID_IMGID")
      fileName25 <- paste0(basePath, "/25thPercentile_",
                           thisTripID, "_", thisIMGID, ".tif")
      writeRaster(raster25thPercentile, fileName25, overwrite=TRUE)
      
      fileName50 <-  paste0(basePath, "/50thPercentile_",
                           thisTripID, "_", thisIMGID, ".tif")
      writeRaster(raster50thPercentile, fileName50, overwrite=TRUE)
      
      fileName75 <-  paste0(basePath, "/75thPercentile_",
                           thisTripID, "_", thisIMGID, ".tif")
      writeRaster(raster75thPercentile, fileName75, overwrite=TRUE)
      
      fileName100 <-  paste0(basePath, "/100thPercentile_",
                            thisTripID, "_", thisIMGID, ".tif")
      writeRaster(raster100thPercentile, fileName100, overwrite=TRUE)
      
      # combine tripID, IMGID, percentile, and path into a data table
      thesePaths <- data.table(trip_ID= c(thisTripID, thisTripID, thisTripID, thisTripID),
                             IMGID=c(thisIMGID, thisIMGID, thisIMGID, thisIMGID),
                             percentile=c("25th", "50th", "75th", "100th"),
                             paths = c(fileName25,fileName50, fileName75,fileName100))
      
      dtLocalPathsForVTRB_ByPercentileIMGIDAndTripID <- rbindlist(list(dtLocalPathsForVTRB_ByPercentileIMGIDAndTripID, thesePaths))
      
    } else {
      thisWarning <- paste("Error with file path, file.exists is not TRUE or FALSE, for IMGID: ", thisIMGID, sep = " ")
      warning(thisWarning) #Catch all error - not sure why this would happen - would require additional exploration
    }
}

saveRDS(dtLocalPathsForVTRB_ByPercentileIMGIDAndTripID,
        paste0(outputDir, "/dtLocalPathsForVTRB_ByPercentileIMGIDAndTripID.rds"))
