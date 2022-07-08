# Loading packages
library(data.table)
library(sf)
library(terra)
library(dplyr)

# Rasterize AFF and uniformly distribute revenue from trip
# Creates dt_paths_af_revenue_rasters.rds, which includes the following data by subtrip:
# total revenue, revenue per cell in aff, count of cells with revenue in aff,
# count of cells with revenue in vtrf,
# a test if vtrf and aff have the same number of cells, and the path to the aff

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
#sf_vtrbs <- readRDS(paste0(dir_output, "/sf_vtrb_cumulative_imgid.rds"))
sf_shapes <- readRDS(file = paste0(dir_output, "/sf_buffered_hulls_subtrip.rds"))
dt_revenue <- readRDS(paste0(dir_output, "/dt_paths_vtrb_split_matched_revenue.rds"))
dt_vtr_rev_cropped_af <- readRDS(file= paste0(dir_output, "/dt_paths_vtrb_revenue_cropped.rds"))

#only need this if recreating the base mosaic
#dt_vtr_rev <- readRDS(paste0(dir_output, "/dt_paths_vtrb_revenue.rds"))

##############################
# create mosaic of all vtrbs
# Instead of redoing this, just load mosaic 
# list_rasters <- lapply(X = dt_vtr_rev$paths, FUN = rast)
# mosaic <- do.call(terra::mosaic, args = c(list_rasters, fun = "sum"))
# mosaic <- ifel(mosaic >= 0, 1, 0)
# mosaic <- ifel(mosaic >= 0, 1, 0)
#plot(mosaic)
this_path <- paste0(dir_output, "/all_vtrbs_base_mosaic.tif")
#writeRaster(mosaic, this_path, overwrite=TRUE)

mosaic <- rast(this_path)
plot(mosaic)
##############################
# Improvements !!!
# count cells as fraction of cell
# assign value by cell fraction
##############################
# crop full mosaic by trip - to create rasters with matching resolution and extent
unique_trips <- unique(sf_shapes$imgid)
unique_confidence <- unique(dt_vtr_rev_cropped_af$confidence)

dt_paths_af_revenue_rasters <- data.table()

#this_trip <- unique_trips[[1]]
#this_confidence <- "top4"
#this_trip <- "3303391607050001"        

# edit this to make all zeros = 1, to increase matching of cell number
for(this_trip in unique_trips){
  this_rast <- mosaic
  this_shape <- sf_shapes %>% filter(imgid_chr== this_trip)
  # create polygons from multipolygon - then create union of all polygons - move this to previous step
  this_vect <- st_cast(st_union(this_shape),"POLYGON") %>% st_union %>% vect()
  all_revenue <- dt_revenue[imgid == this_trip & percentile == "90th", value_gdp]
  # Mask raster by shapes
  this_cropped_rast <- this_rast %>% mask(this_vect) %>% crop(this_vect)
  # Count cells, divide all revenue by cells, and set value
  these_cells <- unname(unlist(global(this_cropped_rast >= 0 , sum, na.rm=TRUE)))
  this_cell_revenue <- all_revenue/these_cells
  this_rast_revenue <- ifel(this_cropped_rast >= 0, this_cell_revenue, 0)
  # write raster
  this_directory <- paste0(dir_output, "/af_all_revenue/")
  this_path <- paste0(this_directory, this_trip, ".tif")
  writeRaster(this_rast_revenue, this_path, overwrite=TRUE)
  # Count cells in cropped raster for comparison
  if(dt_vtr_rev_cropped_af[imgid == this_trip & confidence == "top4", paths] ==  "no intersection"){
    these_cells_vtr = 0
  } else {
    this_vtr_rast_cropped <- rast(dt_vtr_rev_cropped_af[imgid == this_trip & confidence == "top4", paths])
    these_cells_vtr <- unname(unlist(global(this_vtr_rast_cropped >= 0, sum, na.rm=TRUE)))
  }
  cell_test <- these_cells == these_cells_vtr
  # Write raster
  these_data <- data.table(imgid= this_trip,
                           revenue_subtrip = all_revenue,
                           revenue_per_cell = this_cell_revenue,
                           cells_all_rev = these_cells,
                           cells_vtr_rev = these_cells_vtr,
                           cell_test = cell_test,
                           paths = this_path)
  dt_paths_af_revenue_rasters <- rbindlist(list(dt_paths_af_revenue_rasters, these_data), fill=TRUE)
}

dt_paths_af_revenue_rasters[cell_test == FALSE]
# 7 trips fail the cell test, 
saveRDS(object = dt_paths_af_revenue_rasters,
        file= paste0(dir_output, "/dt_paths_af_revenue_rasters.rds"))
