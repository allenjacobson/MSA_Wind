# Loading packages
library(data.table)
library(sf)
library(terra)
library(dplyr)

##############################
# Set directories
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
#dt_paths <- readRDS(paste0(dir_output, "/dt_paths_vtrb_revenue.rds"))
dt_paths_cropped <- readRDS(file= paste0(dir_output, "/dt_paths_vtrb_revenue_cropped.rds"))

#path_ny_wea <- paste0(dir_data, "/NY_Lease_2022sale_merge/NY_Lease_2022sale_merge.shp")
#path_lease_areas_merged <- paste0(dir_data, "/LeaseAreas_Merged_081621/LeaseAreas_Merged_081621.shp")

#sf_ny_wea <- read_sf(path_ny_wea)
#plot(shape_ny_wea)

#sf_lease_areas_merged <- read_sf(path_lease_areas_merged)
#plot(shape_lease_areas_merged)

##############################
#

list_rasters <- lapply(X = dt_paths$paths, FUN = rast)
mosaic <- do.call(terra::mosaic, args = c(list_rasters, fun = "sum"))
mosaic <- ifel(mosaic >= 0, 1, 0)
mosaic <- ifel(mosaic >= 0, 1, 0)
plot(mosaic)
this_path <- paste0(dir_output, "/all_vtrbs_base_mosaic.tif")
writeRaster(mosaic, this_path, overwrite=TRUE)

mosaic <- rast(this_path)

##############################
#crop full mosaic by trip - to create rasters with matching gridding
unique_trips <- unique(sf_shapes$imgid)
unique_confidence <- unique(dt_paths$confidence)

dt_paths_af_revenue_rasters <- data.table()

#this_trip <- unique_trips[[1]]
#this_confidence <- "top1"
#this_trip <- "3303391607050001"        

for(this_trip in unique_trips){
  this_rast <- mosaic
  this_shape <- sf_shapes %>% filter(imgid_chr== this_trip)
  # create polygons from multipolygon - then create union of all polygons - move this to previous step
  this_vect <- st_cast(st_union(this_shape),"POLYGON") %>% st_union %>% vect()
  this_revenue <- extract(x = this_rast, y = this_vect, exact = TRUE) %>%
    rename("value" = names(this_rast)) %>%
    mutate(cell_revenue = value*fraction)%>%
    summarise(total_revenue = sum(cell_revenue))
  all_revenue <- dt_revenue[imgid == this_trip & percentile == "90th", value_gdp]
  # Sum intersecting values
  if(is.nan(this_revenue[[1]])){
    this_path <- "no intersection - error"
    this_cell_revenue <- NA
  } else{
    # Mask raster by shapes
    this_cropped_rast <- this_rast %>% mask(this_vect) %>% crop(this_vect)
    # Count cells, divide all revenue by cells, and set value
    these_cells <- unname(unlist(global(this_cropped_rast == 1, sum, na.rm=TRUE)))
    this_cell_revenue <- all_revenue/these_cells
    this_rast_revenue <- ifel(this_cropped_rast >= 0, this_cell_revenue, 0)
    # write raster
    this_directory <- paste0(dir_output, "/af_all_revenue/")
    this_path <- paste0(this_directory, this_trip, ".tif")
    writeRaster(this_rast_revenue, this_path, overwrite=TRUE)
    # Count cells in cropped raster for comparison
    if(dt_paths_cropped[imgid == this_trip & confidence == "top4", paths] ==  "no intersection"){
      these_cells_vtr = 0
    } else {
      this_vtr_rast_cropped <- rast(dt_paths_cropped[imgid == this_trip & confidence == "top4", paths])
      these_cells_vtr <- unname(unlist(global(this_vtr_rast_cropped >= 0, sum, na.rm=TRUE)))
    }
    cell_test <- these_cells == these_cells_vtr
  }
  # Write raster
  these_data <- data.table(imgid= this_trip,
                           revenue_subtrip = all_revenue,
                           cells_all_rev = this_cell_revenue,
                           cells_vtr_rev = these_cells_vtr,
                           cell_test = cell_test,
                           paths = this_path)
  dt_paths_af_revenue_rasters <- rbindlist(list(dt_paths_af_revenue_rasters, these_data), fill=TRUE)
}

saveRDS(object = dt_paths_af_revenue_rasters,
        file= paste0(dir_output, "/dt_paths_af_revenue_rasters.rds"))

##############################
#cropped by wind energy areas
dt_vtr_revenue <- readRDS(file= paste0(dir_output, "/dt_paths_vtrb_revenue_cropped.rds"))
dt_af_revenue <-readRDS(file= paste0(dir_output, "/dt_paths_af_revenue_rasters.rds"))
sf_lease_areas_merged <- read_sf(paste0(dir_data, "/LeaseAreas_Merged_081621/LeaseAreas_Merged_081621.shp"))
sf_af <- readRDS(file = paste0(dir_output, "/sf_buffered_hulls_subtrip.rds"))
sf_vtr <- readRDS(paste0(dir_output, "/sf_vtrb_cumulative_imgid.rds"))
  
unique_wea <- unique(sf_lease_areas_merged$Lease_Numb)
unique_confidence <- unique(dt_vtr_revenue$confidence)

this_trip <- unique_trips[[1]]
this_wea <- unique_wea[[1]]
this_confidence <- unique_confidence[[1]]

dt_wea_revenue <- data.table()

#test2 <- sf_af %>% filter(imgid_chr == unique_trips[[1]])

for(this_wea in unique_wea){
  this_sf_wea <- sf_lease_areas_merged %>% filter(Lease_Numb == this_wea)
  this_sf_wea <- st_transform(this_sf_wea, st_crs(sf_af %>% filter(imgid_chr == unique_trips[[1]])))
  this_vect_wea <- st_cast(st_union(this_sf_af),"POLYGON") %>% st_union %>% vect()
  #this_vect_wea <- vect(this_sf_af)
  for(this_trip in unique_trips){  
    this_rast_af <- rast(dt_af_revenue[imgid == this_trip, paths])
    this_revenue_af <- extract(x = this_rast_af, y = this_vect_wea, exact = TRUE)%>%
      rename("value" = names(this_rast_af)) %>%
      mutate(cell_revenue = value*fraction)%>%
      summarise(revenue_af = sum(cell_revenue))
    all_revenue <- dt_revenue[imgid == this_trip & percentile == "90th", value_gdp]
    if(is.nan(this_revenue_af[[1]])){
      this_revenue_af <- 0
     } else{
       this_revenue_af <- extract(x = this_rast_af, y = this_vect_wea, exact = TRUE) %>%
         rename("value" = names(this_rast_af)) %>%
         mutate(cell_revenue = value*fraction)%>%
         summarise(revenue_af = sum(cell_revenue))
       }
    for(this_confidence in unique_confidence){
      this_path_vtr <-dt_vtr_revenue[imgid == this_trip & confidence == this_confidence, paths]
      if(this_path_vtr != "no intersection") {
        this_rast_vtr <- rast(dt_vtr_revenue[imgid == this_trip & confidence == this_confidence, paths])
        this_revenue_vtr <- extract(x = this_rast_vtr, y = this_vect_wea, exact = TRUE) %>%
          rename("value" = names(this_rast_vtr)) %>%
          mutate(cell_revenue = value*fraction)%>%
          summarise(revenue_vtr = sum(cell_revenue))
        if(is.nan(this_revenue_vtr[[1]])){
          this_revenue_vtr <- 0
        } else{
          this_revenue_vtr <- extract(x = this_rast_vtr, y = this_vect_wea, exact = TRUE) %>%
            rename("value" = names(this_rast_vtr)) %>%
            mutate(cell_revenue = value*fraction)%>%
            summarise(revenue_vtr = sum(cell_revenue))
        }  
      }
      }
      these_data <- data.table(imgid= this_trip,
                               confidence= this_confidence,
                               revenue_af = this_revenue_af,
                               revenue_vtr = this_revenue_vtr,
                               revenue_subtrip = all_revenue)
      dt_wea_revenue <- rbindlist(list(dt_wea_revenue, these_data), fill=TRUE)
    }
  }
  
saveRDS(object = dt_wea_revenue,
        file= paste0(dir_output, "/dt_wea_revenue.rds"))

names(dt_wea_revenue)
  
dt_wea_revenue <- dt_wea_revenue %>%
  select(-revenue_af, -revenue_vtr) %>%
  rename( = revenue_af.revenue_af, revenue_vtr = revenue_vtr.revenue_vtr) %>%
  replace(is.na(.), 0)

saveRDS(object = dt_wea_revenue,
        file= paste0(dir_output, "/dt_wea_revenue.rds"))

dt_wea_revenue_filter <- dt_wea_revenue[revenue_af > 0 & revenue_vtr >0]
  
  
  