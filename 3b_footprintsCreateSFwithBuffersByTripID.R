# Loading packages
library(data.table)
library(sf)
library(dplyr)
library(terra)

# This script builds a SF containing the vtr buffers, by percentile and trip id

##############################
# Functions
rastToPolygon <- function(rast){
  polygon <- terra::as.polygons(rast) ##removed ==1 from rast
  polygonSF <- sf::st_as_sf(polygon)
  polygonSF <- polygonSF[2,] # select second row, where rast ==1
  polygonGeom <- st_geometry(polygonSF)
  polygonGeom <- st_make_valid(polygonGeom)
  return(polygonGeom)
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
dtLocalPathsForVTRBMosaic_ByPercentileAndTripID <- 
  readRDS(paste0(outputDir, "/dtLocalPathsForVTRBMosaic_ByPercentileAndTripID.rds"))

##############################
#Build SF for VTR Buffers
paths <- dtLocalPathsForVTRBMosaic_ByPercentileAndTripID$paths
listRasters <- lapply(X = paths, FUN = rast)
listPolygons <- lapply(X = listRasters, FUN = rastToPolygon)
listOfSFGs <- unlist(listPolygons, recursive = FALSE)

sfcBuffersByPercentileAndTripID <- st_sfc(listOfSFGs, crs = st_crs(listPolygons[[1]]))

sfBuffersByPercentileAndTripID <- st_sf(geometry = sfcBuffersByPercentileAndTripID)

sfBuffersByPercentileAndTripID <- cbind(geometry = sfBuffersByPercentileAndTripID,
    trip_ID = dtLocalPathsForVTRBMosaic_ByPercentileAndTripID$trip_ID,
    percentile = dtLocalPathsForVTRBMosaic_ByPercentileAndTripID$percentile)

remove(listRasters, listPolygons, listOfSFGs, sfcBuffersByPercentileAndTripID)

saveRDS(object = sfBuffersByPercentileAndTripID,
        file= paste0(outputDir, "/sfBuffersByPercentileAndTripID.rds"))

