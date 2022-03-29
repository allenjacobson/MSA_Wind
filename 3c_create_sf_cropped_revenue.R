# Load packages
library(data.table)
library(sf)
library(dplyr)
library(terra)

# Crop revenue raster to polygon
# Creates SF to summarize cropped rasters

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
#sf_vtrbs <- readRDS(paste0(dir_output, "/sf_vtrb_cumulative_imgid.rds"))
sf_shapes <- readRDS(file = paste0(dir_output, "/sf_buffered_hulls_subtrip.rds"))
dt_revenue <- readRDS(paste0(dir_output, "/dt_paths_vtrb_split_matched_revenue.rds"))
dt_paths <- readRDS(paste0(dir_output, "/dt_paths_vtrb_revenue.rds"))

##############################
# Select example trip
unique_trips <- unique(dt_paths$imgid)
unique_confidence <- unique(dt_paths$confidence)
dt_paths_vtrb_revenue_cropped <- data.table()

#this_trip <- unique_trips[[6]]
#this_confidence <- "top1"

for(this_trip in unique_trips){
  for(this_confidence in unique_confidence){
    this_path <- dt_paths[imgid == this_trip & confidence == this_confidence, paths] 
    this_rast <- rast(this_path)
    this_shape <- sf_shapes %>% filter(imgid_chr== this_trip)
    # create polygons from multipolygon - then create union of all polygons - move this to previous step
    this_vect <- st_cast(st_union(this_shape),"POLYGON") %>% st_union %>% vect()
    this_revenue <- extract(x = this_rast, y = this_vect, exact = TRUE) %>%
      rename("value" = names(this_rast)) %>%
      mutate(cell_revenue = value*fraction)%>%
      summarise(total_revenue = sum(cell_revenue))
    # Sum intersecting values
    if(is.nan(this_revenue[[1]])){
      this_revenue <- 0
      all_revenue <- dt_revenue[imgid == this_trip & percentile == "90th", value_gdp]
      this_path <- "no intersection"
    } else{
      # Select any percentile - b/c they are all the same - this is included to select one
      this_revenue <- extract(x = this_rast, y = this_vect, exact = TRUE) %>%
        rename("value" = names(this_rast)) %>%
        mutate(cell_revenue = value*fraction)%>%
        summarise(total_revenue = sum(cell_revenue))
      all_revenue <- dt_revenue[imgid == this_trip & percentile == "90th", value_gdp]
      # Mask raster by shapes
      this_cropped_rast <- this_rast %>% mask(this_vect) %>%
        crop(this_vect)
      this_directory <- paste0(dir_output, "/vtrb_revenue_cropped/")
      this_path <- paste0(this_directory, this_confidence, "_", this_trip, ".tif")
      writeRaster(this_cropped_rast, this_path, overwrite=TRUE)
    }
    # Write raster
    these_data <- data.table(imgid= this_trip,
                             confidence= this_confidence,
                             revenue_active_fishing = this_revenue,
                             revenue_subtrip = all_revenue,
                             paths = this_path)
    dt_paths_vtrb_revenue_cropped <- rbindlist(list(dt_paths_vtrb_revenue_cropped, these_data), fill=TRUE)
  }
  }

saveRDS(object = dt_paths_vtrb_revenue_cropped,
        file= paste0(dir_output, "/dt_paths_vtrb_revenue_cropped.rds"))

dt_paths_vtrb_revenue_cropped <- readRDS(file= paste0(dir_output, "/dt_paths_vtrb_revenue_cropped.rds"))

dt_paths_vtrb_revenue_cropped <- dt_paths_vtrb_revenue_cropped %>%
  replace(is.na(.), 0) %>%
  select(-revenue_active_fishing) %>%
  rename(revenue_active_fishing = revenue_active_fishing.total_revenue) %>%
  mutate(log_ratio = log(revenue_active_fishing/revenue_subtrip),
         diff = revenue_active_fishing-revenue_subtrip,
         log_over_under = ifelse(log_ratio > 1, "over", ifelse(log_ratio < 1, "under", "equal")),
         diff_over_under := ifelse(diff > 0, "over", ifelse(log_ratio < 0, "under", "equal")))

(plot_revenue_bias <- ggplot(dt_paths_vtrb_revenue_cropped) +  
    geom_histogram( aes(x = log_ratio, fill = log_over_under),
                    position = "stack", boundary=1, bins = 15)+
    scale_fill_manual(values=c("black", "red"))+
    facet_wrap(~ confidence, nrow = 1)+
    coord_flip()+
    ylab("Count: number of subtrips")+
    xlab("Revenue bias*")+
    labs(title = "Revenue bias for subtrips",
         subtitle = "Does VTR footprint over or underestimate revenue in active fishing footprint?",
         caption = expression(paste("*Bias = Ln",
                                    bgroup("(",
                                           over(Revenue[VTR],Revenue[ActiveFishing]),
                                           ")"))), 
         fill = NULL))

(plot_revenue_diff <- ggplot(dt_paths_vtrb_revenue_cropped) +  
    geom_histogram( aes(x = diff, fill = log_over_under),
                    position = "stack", boundary=1, bins = 15)+
    scale_fill_manual(values=c("black", "red"))+
    facet_wrap(~ confidence, nrow = 1)+
    coord_flip()+
    ylab("Count: number of subtrips")+
    xlab("Revenue difference*")+
    labs(title = "Revenue difference for subtrips",
         subtitle = "Does VTR footprint over or underestimate revenue in active fishing footprint?",
         fill = NULL))
