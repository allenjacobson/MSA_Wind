# Load packages
library(data.table)
library(terra)

# This scripts pulls builds cumulative mosaic rasters by imgid and percentile
# and then writes new rasters
# Creates a dictionary with these paths, percentiles, the IMGIDs, and TripIDs

##############################
# Functions
paths_to_mosaic <- function(list_rasters, this_trip){
  length <- length(list_rasters)
  if(length == 1){
    mosaic <- rast(list_rasters)
    return(mosaic)
  } else if (length > 1){
    rasters <- lapply(X = list_rasters, FUN = rast)
    mosaic <- do.call(mosaic, args = c(rasters, fun = "max"))
    return(mosaic)
  } else{ 
    this_warning <- paste("Error with ", this_trip, sep = " ")
    warning(this_warning)
  }
}

##############################
# Set directories
pwd <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

repository <- "MSA_Wind_FootprintBias"
path_base <- "C:/Users/lianne.allen-jacobso/Documents/"
check_pwd <- paste0(path_base, "Repositories/",repository)
pwd == check_pwd

dir_output <- paste0(path_base, "Output/", repository)
dir_data <- paste0(path_base, "Data/", repository)

##############################
# Pull in data
dt_paths_vtrb_split<- readRDS(paste0(dir_output, "/dt_paths_vtrb_split_by_percentile.rds"))

##############################
# Raster mosaics by percentiles
unique_trips <- unique(dt_paths_vtrb_split$trip_area)
#this_trip <- unique_trips[[2]]

# create empty data.table to add file paths
#dtCumulativeRasterPaths <- data.table()
dt_paths_vtrb_cumulative_imgid <- data.table()


for (this_trip in unique_trips) {
  dt_25 <- dt_paths_vtrb_split[trip_area==this_trip & percentile=="25th"]
  dt_50 <- dt_paths_vtrb_split[trip_area==this_trip & percentile=="50th"]
  dt_75 <- dt_paths_vtrb_split[trip_area==this_trip & percentile=="75th"]
  dt_90 <- dt_paths_vtrb_split[trip_area==this_trip & percentile=="90th"]
  # create mosaic for each percentile - these are non-cumulative
  mosaic_25<- paths_to_mosaic(dt_25$paths, this_trip)
  mosaic_50<- paths_to_mosaic(dt_50$paths, this_trip)
  mosaic_75<- paths_to_mosaic(dt_75$paths, this_trip)
  mosaic_90<- paths_to_mosaic(dt_90$paths, this_trip)
  # create cumulative mosaic for each percentile
  cumulative_25 <- mosaic_25
  cumulative_50 <- mosaic(mosaic_50, cumulative_25, fun = "max") 
  cumulative_75 <- mosaic(mosaic_75, cumulative_50, fun = "max")
  cumulative_90 <- mosaic(mosaic_90, cumulative_75, fun = "max")
  # create file names and write cumulative rasters
  path_base <- paste0(dir_output, "/vtrbs_cumulative_imgid")
  
  path_25 <- paste0(path_base, "/25thPercentile_", this_trip,".tif")
  writeRaster(cumulative_25, path_25, overwrite=TRUE)
  
  path_50 <- paste0(path_base, "/50thPercentile_", this_trip,".tif")
  writeRaster(cumulative_50, path_50, overwrite=TRUE)
  
  path_75 <- paste0(path_base, "/75thPercentile_", this_trip,".tif")
  writeRaster(cumulative_75, path_75, overwrite=TRUE)
  
  path_90 <- paste0(path_base, "/90thPercentile_", this_trip,".tif")
  writeRaster(cumulative_90, path_90, overwrite=TRUE)
  
  this_trip_id <- unique(dt_paths_vtrb_split[trip_area == this_trip,tripid])
  this_imgid <- unique(dt_paths_vtrb_split[trip_area == this_trip,imgid])
  
  
  these_paths <- data.table(tripid =c(this_trip_id, this_trip_id, this_trip_id, this_trip_id),
                            trip_area= c(this_trip, this_trip, this_trip, this_trip),
                            imgid = c(this_imgid, this_imgid, this_imgid, this_imgid),
                            percentile=c("25th", "50th", "75th", "90th"),
                            paths = c(path_25,path_50, path_75,path_90))
  # Add file paths to data.table
  dt_paths_vtrb_cumulative_imgid <- rbindlist(list(dt_paths_vtrb_cumulative_imgid, these_paths))
}

saveRDS(dt_paths_vtrb_cumulative_imgid,
        paste0(dir_output, "/dt_paths_vtrb_cumulative_imgid.rds"))
