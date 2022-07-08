# Load packages
library(data.table)
library(dplyr)
library(terra)

# This scripts pulls builds cumulative mosaic rasters by imgid and percentile
# and then writes new rasters
# Creates a dictionary with these paths, percentiles, the IMGIDs, and TripIDs

##############################
# Functions

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
dt_revenue<- setDT(readRDS(paste0(dir_output, "/dt_revenue_matched.rds")))

# Data Prep
dt_revenue_matched <- dt_revenue[imgid_chr %in% dt_paths_vtrb_split$imgid]
dt_paths_vtrb_split_matched <- dt_paths_vtrb_split[imgid %in% dt_revenue_matched$imgid_chr]

length(unique(dt_revenue_matched$imgid_chr)) == length(unique(dt_paths_vtrb_split_matched$imgid))

# join revenue and path tables
# this should happen earlier - move to data prep, and move data pull earlier
setkey(dt_revenue_matched, imgid_chr)
setkey(dt_paths_vtrb_split_matched, imgid)
dt <- dt_paths_vtrb_split_matched[dt_revenue_matched, nomatch = 0]

dt[, revenue_per_cell_90 := value*value_gdp]
dt[, revenue_per_cell_75 := value_lower_3*value_gdp]
dt[, revenue_per_cell_50 := value_lower_2*value_gdp]
dt[, revenue_per_cell_25 := value_lower_1*value_gdp]

saveRDS(dt, paste0(dir_output, "/dt_paths_vtrb_split_matched_revenue.rds") )
##############################
# Raster mosaics by percentiles
unique_trips <- unique(dt$imgid)

# create empty data.table to add file paths
#dtCumulativeRasterPaths <- data.table()
dt_paths_vtrb_revenue <- data.table()


for (this_trip in unique_trips) {
  trip_data <- dt[imgid==this_trip]
  # create new rasters for each percentile - these are non-cumulative
  top4_25th <- ifel( rast(trip_data[percentile=="25th", paths]) == 1,
                     trip_data[percentile=="25th", revenue_per_cell_90], 0)
  top4_50th <- ifel( rast(trip_data[percentile=="50th", paths]) == 1,
                     trip_data[percentile=="50th", revenue_per_cell_90], 0)
  top4_75th <- ifel( rast(trip_data[percentile=="75th", paths]) == 1,
                     trip_data[percentile=="75th", revenue_per_cell_90], 0)
  top4_90th <- ifel( rast(trip_data[percentile=="90th", paths]) == 1,
                     trip_data[percentile=="90th", revenue_per_cell_90], 0)
  cumulative_top4 <- mosaic(top4_90th, top4_75th, fun = "sum")
  cumulative_top4 <- mosaic(cumulative_top4, top4_50th, fun = "sum")
  cumulative_top4 <- mosaic(cumulative_top4, top4_25th, fun = "sum")
  
  # Repeat for top 3 confidence levels with corresponding value
  top3_25th <- ifel( rast(trip_data[percentile=="25th", paths]) == 1,
                     trip_data[percentile=="25th", revenue_per_cell_75], 0)
  top3_50th <- ifel( rast(trip_data[percentile=="50th", paths]) == 1,
                     trip_data[percentile=="50th", revenue_per_cell_75], 0)
  top3_75th <- ifel( rast(trip_data[percentile=="75th", paths]) == 1,
                     trip_data[percentile=="75th", revenue_per_cell_75], 0)
  cumulative_top3 <- mosaic(top3_75th, top3_50th, fun = "sum")
  cumulative_top3 <- mosaic(cumulative_top3, top3_25th, fun = "sum")
  
  # Repeat for top 2 confidence levels with corresponding value
  top2_25th <- ifel( rast(trip_data[percentile=="25th", paths]) == 1,
                     trip_data[percentile=="25th", revenue_per_cell_50], 0)
  top2_50th <- ifel( rast(trip_data[percentile=="50th", paths]) == 1,
                     trip_data[percentile=="50th", revenue_per_cell_50], 0)
  cumulative_top2 <- mosaic(top2_50th, top2_25th, fun = "sum")
  
  # Repeat for top 1 confidence levels with corresponding value
  top1_25th <- ifel( rast(trip_data[percentile=="25th", paths]) == 1,
                     trip_data[percentile=="25th", revenue_per_cell_25], 0)
  cumulative_top1 <- top1_25th
  
  # create file names and write cumulative rasters
  path_base <- paste0(dir_output, "/vtrbs_cumulative_revenue_imgid")
 
  path_top4 <- paste0(path_base, "/top4_", this_trip,".tif")
  writeRaster(cumulative_top4, path_top4, overwrite=TRUE)
  
  path_top3 <- paste0(path_base, "/top3_", this_trip,".tif")
  writeRaster(cumulative_top3, path_top3, overwrite=TRUE)
  
  path_top2 <- paste0(path_base, "/top2_", this_trip,".tif")
  writeRaster(cumulative_top2, path_top2, overwrite=TRUE)
  
  path_top1 <- paste0(path_base, "/top1_", this_trip,".tif")
  writeRaster(cumulative_top1, path_top1, overwrite=TRUE)
  
  these_paths <- data.table(imgid =c(this_trip, this_trip, this_trip, this_trip),
                            confidence=c("top1", "top2", "top3", "top4"),
                            paths = c(path_top1, path_top2, path_top3, path_top4))
  # Add file paths to data.table
  dt_paths_vtrb_revenue <- rbindlist(list(dt_paths_vtrb_revenue, these_paths))
}

saveRDS(dt_paths_vtrb_revenue,
        paste0(dir_output, "/dt_paths_vtrb_revenue.rds"))

dt_paths_vtrb_revenue <- readRDS(paste0(dir_output, "/dt_paths_vtrb_revenue.rds"))
