library(fst)
library(data.table)
library(raster)
#library(terra)
#library(sf)

# This scripts pulls rasters from the network and saves them locally
# Creates a dictionary with these local paths, the IMGIDs, and TripIDs

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
# Pull data dictionary (fst) from network
# must be connected to VPN and
# check that the network directory is correctly mapped
# map \\net.nefsc.noaa.gov\work5 to Y:\\
# load dictionary of file locations
fstPath <- "Y:\\socialsci\\Geret_Rasters\\Data\\offshore_wind_package_data\\raster_extents_and_file_list"
filePaths <-setDT(read_fst(file.path(fstPath, 'raster_file_list.fst')))
##############################
# Pull in data
dtIMGIDmatchedToTripID <- setDT(readRDS(file=paste0(outputDir, "/dtIMGIDmatchedToTripID.rds")))

##############################
# Prep data
# Filter to only include IDNUMs with 16 characters (which narrows to trips in study fleet?)
filePaths[, IDNUM_chr := as.character(IDNUM)][
          , ncharIDNUM := nchar(filePaths$IDNUM, type = 'width')]

filePaths16 <-filePaths[ncharIDNUM == 16]

# filter filePathDictionary16 to include match IMGID to IDNUM
filePathsIMGIDmatchedToTripID <- filePaths16[IDNUM_chr %in% dtIMGIDmatchedToTripID$IMGID_chr]

# Modefy file path to match path on this computer (Versus someone from social sci)
filePathsIMGIDmatchedToTripID[, modFilePath := substr(filePathsIMGIDmatchedToTripID$FILEPATH, 11,
                                         nchar(filePathsIMGIDmatchedToTripID$FILEPATH))]
filePathsIMGIDmatchedToTripID[, finalFilePath := paste("Y:", filePathsIMGIDmatchedToTripID$modFilePath, sep = "")]
saveRDS(filePathsIMGIDmatchedToTripID, paste0(outputDir, "/filePathsIMGIDmatchedToTripID.rds"))

#driectory includes .gri and .grd - I think this points to the .gri - is .grd called indirectly?

##############################
# Pull all rasters
uniqueIMGIDs <- unique(dtIMGIDmatchedToTripID$IMGID_chr)
#dtAllIDS <- data.table()
dtLocalPathsForVTRB_ByIMGIDAndTripID <- data.table()

for (thisIMGID in uniqueIMGIDs) {
  thisTripID <- dtIMGIDmatchedToTripID[IMGID_chr==thisIMGID, trip_id_chr] #find tripID by IMGID
  thisPath <- filePathsIMGIDmatchedToTripID[IDNUM==thisIMGID, finalFilePath] #find path by IMGID
  countFilePaths <- length(thisPath)
  fileCheck <- file.exists(thisPath)
  if(countFilePaths == 0){
    thisWarning <- paste("No file path found, for IMGID: ", thisIMGID, sep = " ")
    warning(thisWarning) # path is missing, return error
    next
    } else if (countFilePaths > 1){
    thisWarning <- paste("Multiple file paths, for IMGID: ", thisIMGID, sep = " ")
    warning(thisWarning) #multiple paths - should be unique - return error
    next
    } else if (length(fileCheck) == 0){
    thisWarning <- paste("File missing for IMGID: ", thisIMGID, sep = " ")
    warning(thisWarning) #found path, but could not pull file, return error
    next
    } else if (fileCheck == TRUE){
    thisRaster <- raster(thisPath)
    fileName <- paste0(outputDir, "/localVTRBs_TripID_IMGID/", thisTripID, "_", thisIMGID, ".tif")
    writeRaster(thisRaster, fileName, overwrite=TRUE) #save raster locally
    #concatenate data for this IMGID and add to dt to save later
    theseIDS <- data.table(trip_ID=thisTripID,
                           IMGID=thisIMGID,
                           newPath = fileName)
    dtLocalPathsForVTRB_ByIMGIDAndTripID <- rbindlist(list(dtLocalPathsForVTRB_ByIMGIDAndTripID, theseIDS))
    next
    } else {
    thisWarning <- paste("Error with file path, file.exists is not TRUE or FALSE, for IMGID: ", thisIMGID, sep = " ")
    warning(thisWarning)
  }
}

# save dt that contains all local paths with IMGIDs and TripIDS
saveRDS(dtLocalPathsForVTRB_ByIMGIDAndTripID, paste0(outputDir, "/dtLocalPathsForVTRB_ByIMGIDAndTripID.rds"))

# Warning messages:
# 1: No file path found, for IMGID:  3303391612311601 
# 2: No file path found, for IMGID:  3305341707220302 
# 3: No file path found, for IMGID:  3305341712282101 
# 4: No file path found, for IMGID:  3305491812301501 
# 5: No file path found, for IMGID:  3305491812301503 
# 6: No file path found, for IMGID:  3305491812301502 
# 7: No file path found, for IMGID:  4105141612301801 