# Loading packages
library(data.table)
library(sf)
library(terra)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggspatial)
library(stringr)

# archive this script

# This script create a data table, comparing VTRF and AFF overlap with WEA
# For each WEA, and by percentile

# Functions
paths_to_mosaic <- function(paths, type , this_wea, this_confidence){
  length <- length(paths)
  if(length == 1){
    mosaic <- rast(paths)
    return(mosaic)
  } else if (length > 1){
    rasters <- lapply(X = paths, FUN = rast)
    mosaic <- do.call(terra::mosaic, args = c(rasters, fun = "sum"))
    return(mosaic)
  } else{
    this_warning <- paste0("Error with ", type," for ", this_wea," and ", this_confidence)
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
#sf_gte <- readRDS(file = paste0(dir_output, "/sf_gte_nad83_singles.rds"))
dt_wea_revenue <- readRDS(file= paste0(dir_output, "/dt_wea_revenue.rds"))
sf_all_wea <- readRDS(paste0(dir_output, "/sf_all_wea.rds"))
#sf_lease_areas_merged <- read_sf(paste0(dir_data, "/LeaseAreas_Merged_081621/LeaseAreas_Merged_081621.shp"))
#dt_lease_areas_merged <- setDT(st_drop_geometry(sf_lease_areas_merged))

dt_vtr_revenue <- readRDS(paste0(dir_output, "/dt_paths_vtrb_revenue.rds"))
dt_af_revenue <-readRDS(file= paste0(dir_output, "/dt_paths_af_revenue_rasters.rds"))
sf_vtr <- readRDS(paste0(dir_output, "/sf_vtrb_cumulative_imgid.rds"))
sf_af <- readRDS(file = paste0(dir_output, "/sf_buffered_hulls_subtrip.rds"))

##############################
# Prep data
dt_wea_revenue[, overlaps_wea := ifelse(revenue_vtr == 0, "af",
                                        ifelse(revenue_af == 0, "vtr", "both"))]

dt_wea_revenue_af <- dt_wea_revenue[revenue_af > 0 & confidence == "top4",
                                    .(total_revenue_af = sum(revenue_af), n_af = length(unique(imgid))),
                                    by = wea_id]

dt_wea_revenue_vtr <- dt_wea_revenue[revenue_vtr > 0  & confidence == "top4",
                                     .(total_revenue_vtr = sum(revenue_vtr), n_vtr = length(unique(imgid))),
                                     by = wea_id]

setkey(dt_wea_revenue_af, wea_id)
setkey(dt_wea_revenue_vtr, wea_id)

# inner join
dt_wea_revenue_summary <- dt_wea_revenue_af[dt_wea_revenue_vtr, nomatch=0]

dt_wea_revenue_summary[, sum(n_af)]
dt_wea_revenue_summary[, sum(n_vtr)]
##############################
# Summarize data included in plots
#unique_wea <- dt_wea_revenue_summary[n_af > 2, wea_id]
# remove lease with 1 af intersection and 0 vtr intersections
unique_wea <- dt_wea_revenue_summary[, wea_id]
unique_confidence <- unique(dt_vtr_revenue$confidence)

#this_wea <- unique_wea[[4]]
#this_confidence <- unique_confidence[[4]]

#sf_use_s2(FALSE) 
# added to remove this error
# Error in (function (cond)  : error in evaluating the argument 'x' in selecting a method for function 'vect': Evaluation error: Found 1 feature with invalid spherical geometry.[1] Loop 1 is not valid: Edge 0 has duplicate vertex with edge 5.

dt_wea_revenue_overlap <- data.table()

for(this_wea in unique_wea){
  for(this_confidence in unique_confidence){
    these_trips_vtr <- dt_wea_revenue[wea_id == this_wea & revenue_vtr > 0 & confidence == this_confidence]
    these_trips_af <- dt_wea_revenue[wea_id == this_wea & revenue_af > 0 & confidence == this_confidence]
    # select wea sf and make spat vector
    this_sf_wea <-sf_all_wea %>% filter(id == this_wea) #%>% select(State)
    # count trips by footprint type
    n_vtr <-length(these_trips_vtr$imgid)
    n_af <- length(these_trips_af$imgid)
    # build mosaics if lenghts are non-zero for vtr and af trip lists
    # And calculate overlap for vtr and af footprints
    if(n_vtr == 0){
      total_revenue <- 0
      this_revenue_vtr <- data.frame(total_revenue)
    } else{
      these_vtr_paths <- dt_vtr_revenue[imgid %in% these_trips_vtr$imgid & confidence == this_confidence,paths]
      this_vtr_mosaic <- paths_to_mosaic(paths = these_vtr_paths, type = "vtr", this_wea = this_wea, this_confidence = this_confidence)
      this_sf_wea <- st_transform(this_sf_wea, st_crs(this_vtr_mosaic))
      this_vect <- st_cast(st_union(this_sf_wea),"POLYGON") %>% st_union %>% vect()
      this_wea_extent <- ext(this_vect)
      this_revenue_vtr <- extract(x = this_vtr_mosaic, y = this_vect, exact = TRUE) %>%
        rename("value" = names(this_vtr_mosaic)) %>%
        mutate(cell_revenue = value*fraction)%>%
        summarise(total_revenue = sum(cell_revenue, na.rm = TRUE))
      # Select sf_vtr to set extent for af plot - when both are non-zero
      these_sf_vtr <- sf_vtr %>% filter(imgid %in% these_trips_vtr$imgid)
      # convert to a df for plotting in two steps,
      # this_vtr_mosaic_df <- as.data.frame(this_vtr_mosaic, xy=TRUE, cells=TRUE, na.rm=TRUE)
      # this_vtr_mosaic_df<- this_vtr_mosaic_df %>% rename(value = paste0("X",names(this_vtr_mosaic)))
      # this_vtr_mosaic_df[this_vtr_mosaic_df== 0] = NA
      # this_vtr_mosaic_df <- na.omit(this_vtr_mosaic_df) 
    }
    if(n_af == 0){
      total_revenue <- 0
      this_revenue_af <- data.frame(total_revenue)
    } else {
      these_af_paths <- dt_af_revenue[imgid %in% these_trips_af$imgid, paths]
      this_af_mosaic <- paths_to_mosaic(paths = these_af_paths, type = "af", this_wea = this_wea, this_confidence = this_confidence) 
      # these_afs <- lapply(X = dt_af_revenue[imgid %in% these_trips_af$imgid, paths], FUN = rast)
      # this_af_mosaic <- do.call(terra::mosaic, args = c(these_afs, fun = "sum"))
      this_sf_wea <- st_transform(this_sf_wea, st_crs(this_af_mosaic))
      this_vect <- st_cast(st_union(this_sf_wea),"POLYGON") %>% st_union %>% vect()
      this_wea_extent <- ext(this_vect)
      this_revenue_af <- extract(x = this_af_mosaic, y = this_vect, exact = TRUE) %>%
        rename("value" = names(this_af_mosaic)) %>%
        mutate(cell_revenue = value*fraction)%>%
        summarise(total_revenue = sum(cell_revenue, na.rm = TRUE))
      # convert to a df for plotting in two steps,
      # this_af_mosaic_df <- as.data.frame(this_af_mosaic, xy=TRUE, cells=TRUE, na.rm=TRUE)
      # this_af_mosaic_df<- this_af_mosaic_df %>% rename(value = paste0("X",names(this_af_mosaic)))
      # this_af_mosaic_df[this_af_mosaic_df== 0] = NA
      # this_af_mosaic_df <- na.omit(this_af_mosaic_df) 
    }
    
    # Add data to data.table
    these_data <- data.table(confidence= this_confidence,
                             wea_id = this_wea,
                             revenue_vtr_overlap = round(this_revenue_vtr$total_revenue, digits = 0),
                             revenue_af_overlap =  round(this_revenue_af$total_revenue, digits = 0),
                             n_vtr_subtrips = length(unique(these_trips_vtr$imgid)),
                             n_af_subtrips = length(unique(these_trips_af$imgid)))
    
    dt_wea_revenue_overlap <- rbindlist(list(dt_wea_revenue_overlap, these_data), fill=TRUE)
  }
}

saveRDS(object = dt_wea_revenue_overlap,
        file= paste0(dir_output, "/dt_wea_revenue_overlap.rds"))

##############################
# Test with SF

# sf_af <- readRDS(file = paste0(dir_output, "/sf_buffered_hulls_subtrip.rds"))
# sf_vtr <- readRDS(paste0(dir_output, "/sf_vtrb_cumulative_imgid.rds"))
# 
# sf_vtrb$percentile <- factor(sf_vtrb$percentile,
#                              levels = c("25th", "50th", "75th", "90th"))
# these_sf_af <- sf_af %>% filter(imgid_chr %in% these_trips_af$imgid) %>% select (value_gdp)
# these_sf_vtr <- sf_vtr %>% filter(imgid %in% these_trips_vtr$imgid)
# 
# 
# ggplot() +
#   geom_sf(data=these_sf_vtr, fill = "dark grey", color = NA)+
#   #scale_fill_viridis_d(direction = -1)+
#   geom_sf(data=these_sf_af) +
#   geom_sf(data=this_sf_wea, color = "red", fill = NA)+
#   ggtitle("test")
# 
# ##############################
# (plot_wea<- ggplot()+
#    geom_raster(this_vtr_mosaic)+
#    geom_sf(data=this_sf_wea, color = "black", fill = NA)+
#    #scale_fill_viridis_d(direction = -1)+
#    #geom_sf(data=single_hull, aes(color=Hull) ,fill=NA, size = 4)+
#    #scale_color_manual(values = alpha("red", .5))+
#    xlab("Longitude")+
#    ylab("Latitude")+
#    labs(title = "VTR revenue within wea")+
#         #subtitle = "all trips aggregated",
#         #fill = "VTR footprint \nby percentile",
#         #color = "Active fishing \nfootprint") +
#    annotation_scale(location = "br", width_hint = 0.5) +
#    annotation_north_arrow(location = "br", which_north = "true",
#                           height = unit(.3, "in"), width = unit(.3, "in"), 
#                           pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
#                           style = north_arrow_fancy_orienteering))
# 
# height = 8
# width = height*.618
# 
# ggsave(filename = paste0(dir_output, "/plot_aggregate.png"),
#        plot = plot_aggregate, width = width, height = height)


