# Load packages
library(data.table)
library(sf)
library(dplyr)
library(terra)

# This script builds a SF containing the vtr buffers, by percentile and trip id

##############################
# Functions
rast_to_polygon <- function(rast){
  polygon <- terra::as.polygons(rast) ##removed ==1 from rast
  sf_polygon <- sf::st_as_sf(polygon)
  sf_polygon <- sf_polygon[2,] # select second row, where rast ==1
  polygon_geom <- st_geometry(sf_polygon)
  polygon_geom <- st_make_valid(polygon_geom)
  return(polygon_geom)
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
dt_paths_vtrb_split_mosaic <- 
  readRDS(paste0(dir_output, "/dt_paths_vtrb_split_mosaic.rds"))

##############################
#Build SF for VTR Buffers
paths <- dt_paths_vtrb_split_mosaic$paths
list_rasters <- lapply(X = paths, FUN = rast)
list_polygons <- lapply(X = list_rasters, FUN = rast_to_polygon)
list_sfgs <- unlist(list_polygons, recursive = FALSE)

sfc_vtrb_split_mosaic <- st_sfc(list_sfgs, crs = st_crs(list_polygons[[1]]))

sf_vtrb_split_mosaic <- st_sf(geometry = sfc_vtrb_split_mosaic)

sf_vtrb_split_mosaic <- cbind(geometry = sf_vtrb_split_mosaic,
    tripid = dt_paths_vtrb_split_mosaic$tripid,
    percentile = dt_paths_vtrb_split_mosaic$percentile)

saveRDS(object = sf_vtrb_split_mosaic,
        file= paste0(dir_output, "/sf_vtrb_split_mosaic.rds"))

