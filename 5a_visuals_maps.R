# Loading packages
library(data.table)
library(sf)
library(terra)
library(dplyr)
library(ggplot2)
library(patchwork)
# library(rnaturalearth)
# library(rnaturalearthdata)
# library(rnaturalearthhires)
#library(usmap)
library(ggOceanMapsData)
library(ggOceanMaps)

library(ggpubr)
library(ggspatial)
library(stringr)

library(tidyterra) # for geom_spatraster()
library(ggmap) # to pull base maps
library(ggspatial) # to add scale and north arrow to map
library(patchwork) # to plot two maps side by side


# This script creates plots for 3 aggregated data
# 1) Comparison of footprints
# 2) Mismatch
# 3) 4-panel plot, showing total catch, prop caught, cpue, and month

##############################
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

repository <- "fishing_footprint_bias_for_wind"
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
dt_wea_summary_long <- readRDS(paste0(dir_output, "/dt_wea_summary_long.rds"))

#sf_lease_areas_merged <- read_sf(paste0(dir_data, "/LeaseAreas_Merged_081621/LeaseAreas_Merged_081621.shp"))

dt_vtr_revenue <- readRDS(paste0(dir_output, "/dt_paths_vtrb_revenue.rds"))
dt_af_revenue <-readRDS(file= paste0(dir_output, "/dt_paths_af_revenue_rasters.rds"))
sf_vtr <- readRDS(paste0(dir_output, "/sf_vtrb_cumulative_imgid.rds"))
sf_af <- readRDS(file = paste0(dir_output, "/sf_buffered_hulls_subtrip.rds"))

# to look up vessel ids
dt_permit<- readRDS(paste0(dir_output, "/dt_gte.rds"))

##############################
# Prep data
dt_all_wea <- setDT(st_drop_geometry(sf_all_wea))
# look at summary stats
dt_wea_revenue[, .N, by = .(wea_id, type_intersection) ]
dt_wea_revenue[, .N, by = .(wea_id)]
# count intersections for top4
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

dt_wea_revenue_vtr <- dt_wea_revenue[revenue_vtr > 0  & confidence == "top4",
                                     .(total_revenue_vtr = sum(revenue_vtr), n_vtr = length(unique(imgid))),
                                     by = wea_id]

#dt_wea_summary <- dt_wea_revenue[, .N, by = .(confidence, type_intersection, agreement)]

##############################
# Plot all footprinst and WEAs


dt_vtr_revenue[, path_new := str_replace_all(dt_vtr_revenue$paths, "MSA_Wind_FootprintBias", "fishing_footprint_bias_for_wind")]
  
all_vtr_paths <- dt_vtr_revenue[,path_new]

all_vtr_mosaic <- paths_to_mosaic(paths = all_vtr_paths, type = "vtr", this_wea = "all", this_confidence = "all")

# these are saved - could recreate to check they are right

# writeRaster(x = all_vtr_mosaic,
#             filename = paste0(dir_output, "/all_vtr_mosaic.tif"),
#             overwrite=TRUE)

dt_af_revenue[, path_new := str_replace_all(dt_af_revenue$paths, "MSA_Wind_FootprintBias", "fishing_footprint_bias_for_wind")]
all_af_paths <- dt_af_revenue[,path_new]
all_af_mosaic <- paths_to_mosaic(paths = all_af_paths, type = "af", this_wea = "all", this_confidence = "all") 

# writeRaster(all_af_mosaic,
#             filename = paste0(dir_output, "/all_af_mosaic.tif"),
#             overwrite=TRUE)

# read rasters back in
# all_af_mosaic<- rast( paste0(dir_output, "/all_af_mosaic.tif"))
# all_vtr_mosaic<- rast(paste0(dir_output, "/all_vtr_mosaic.tif"))


# transform for plotting
sf_all_wea_84 <- st_transform(sf_all_wea, crs = 4326)
sf_vtr_84 <- st_transform(sf_vtr, crs = 4326)
all_vtr_mosaic_84 <- project(all_vtr_mosaic, "EPSG:4326")
all_af_mosaic_84 <- project(all_af_mosaic, "EPSG:4326")

all_vtr_mosaic_df <- as.data.frame(all_vtr_mosaic_84, xy=TRUE, cells=TRUE, na.rm=TRUE)
#all_vtr_mosaic<- all_vtr_mosaic %>% rename(value = paste0("X",names(all_vtr_mosaic)))
all_vtr_mosaic_df<- all_vtr_mosaic_df %>% rename(value = names(all_vtr_mosaic))

all_vtr_mosaic_df[all_vtr_mosaic_df== 0] = NA
all_vtr_mosaic_df <- na.omit(all_vtr_mosaic_df)

# convert to a df for plotting in two steps,
all_af_mosaic_df <- as.data.frame(all_af_mosaic_84, xy=TRUE, cells=TRUE, na.rm=TRUE)
#all_af_mosaic_df<- all_af_mosaic_df %>% rename(value = paste0("X",names(all_af_mosaic)))
all_af_mosaic_df<- all_af_mosaic_df %>% rename(value = names(all_af_mosaic))
all_af_mosaic_df[all_af_mosaic_df== 0] = NA
all_af_mosaic_df <- na.omit(all_af_mosaic_df)


# all_vect <- st_cast(st_union(sf_all_wea_84),"POLYGON") %>% st_union %>% vect()
# all_wea_extent <- ext(all_vect)


sf_all_wea_leased <- sf_all_wea_84 %>% filter(type == "Leased Area")
sf_all_wea_planned <- sf_all_wea_84 %>% filter(type == "Planned Area")

sf_all_wea_planned_centroids <- sf_all_wea_planned %>%
  group_by(label) %>%
  summarize(centroid = st_centroid(st_union(geometry)))

a <- sf_vtr_84 %>% select(geometry)
b <- sf_all_wea_84 %>% select(geometry)

#plot_usmap(regions = "counties") +
sf_for_extent <- rbind(a,b)


bbox <- st_bbox(sf_for_extent)
#bounding box lowerleftlon, lowerleftlat, upperrightlon, upperrightlat
this_location <- c(bbox["xmin"]-1,
                   bbox["ymin"]-1,
                   bbox["xmax"]+1,
                   bbox["ymax"]+1)

names(this_location)<- c('left', 'bottom', 'right', 'top')

this_map <- get_map(location = this_location,
                    source = "stamen",
                    maptype = "watercolor",
                    crop = FALSE)

# (test <- ggmap(this_map) +
#     #ggplot()+
#     geom_spatraster(data = all_vtr_mosaic, aes(fill=value))
#     geom_sf(data = sf,inherit.aes = FALSE, aes(color= fishing)) +
#     coord_sf(crs = st_crs(4326))+
#     scale_color_viridis_d(option = "rocket",
#                           alpha = .5, begin = 0.25, end = .75)+
#     labs(y = "Latitude", x = "Longitude",
#          title='Complete: 1/20s',
#          subtitle = paste0("Trip:", this_trip))+
#     guides(color="none")+
#     annotation_scale(location = "br", width_hint = 0.5) +
#     annotation_north_arrow(location = "br", which_north = "true",
#                            height = unit(.3, "in"), width = unit(.3, "in"),
#                            pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
#                            style = north_arrow_fancy_orienteering))


(plot_base <- basemap(data=sf_for_extent,
                      bathymetry = FALSE))


(plot_all_footprints <-  basemap(data=sf_for_extent, bathymetry = FALSE)+
    #plot_base +
    #ggplot() +
    geom_sf(data=sf_vtr_84, fill = NA, color = NA)+
    geom_tile(data = all_vtr_mosaic_df, aes(x = x, y = y, fill = value)) +
    geom_tile(data = all_af_mosaic_df, aes(x = x, y = y), fill = "black") +
    scale_fill_viridis_c(option = "magma",
                         direction = -1,
                         breaks = c(.01, .1, 1, 10, 100, 1000),
                         labels = scales::dollar_format(prefix="$ ",),
                         trans = scales::pseudo_log_trans(sigma = 0.001),
                         alpha = .5)+
    xlab("Longitude")+
    #xlim(xmin(all_wea_extent)*.998, xmax(all_wea_extent)*1.002)+
    ylab("Latitude")+
    #ylim(ymin(all_wea_extent)*.998, ymax(all_wea_extent)*1.002)+
    labs(fill = "Revenue")+
    annotation_scale(location = "br", width_hint = 0.5) +
    annotation_north_arrow(location = "br", which_north = "true",
                           height = unit(.3, "in"), width = unit(.3, "in"), 
                           pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
                           style = north_arrow_fancy_orienteering))

#plot_all_footprints + plot_base
plot_all_complete <- plot_all_footprints +
  geom_sf(data=sf_all_wea_planned, color = "dark green", fill = "dark gray", alpha = 0.45)+
  #geom_sf_text(data=sf_all_wea_planned_centroids, aes(label = label), colour = "black")+
  geom_sf(data=sf_all_wea_leased, color = "red", fill = "dark gray", alpha = 0.45)

sum(all_vtr_mosaic_df$value)
# 19,767,483
width = 11
height = 8.5

ggsave(filename = paste0(dir_output, "/plot_all_complete.png"),
       plot = plot_all_complete,
       width = width, height = height)    
# Crop aggregate map by region for viewing

##############################
# Plot rasters by wind energy area
unique_wea <- unique(dt_wea_revenue[, wea_id])
unique_confidence <- unique(dt_wea_revenue$confidence)
this_wea <- unique_wea[[8]]

# Plot VTRF at 90th percentile
this_confidence <- unique_confidence[[4]]
# figure prep
these_trips_vtr <- dt_wea_revenue[wea_id == this_wea & revenue_vtr > 0 & confidence == this_confidence]
these_trips_af <- dt_wea_revenue[wea_id == this_wea & revenue_af > 0 & confidence == this_confidence]
# select wea sf and make spat vector
this_sf_wea <-sf_all_wea %>% filter(id == this_wea) #%>% select(State)
# count trips by footprint type
n_vtr <-length(these_trips_vtr$imgid)
n_af <- length(these_trips_af$imgid)

length(unique(dt_permit[IMGID %in% these_trips_vtr$imgid, permit]))
#10 permits
length(unique(dt_permit[IMGID %in% these_trips_af$imgid, permit]))
#3 permits

# build mosaics if lengths are non-zero for vtr and af trip lists
# And calculate overlap for vtr and af footprints
these_vtr_paths <- dt_vtr_revenue[ imgid %in% these_trips_vtr$imgid & confidence == this_confidence, path_new]
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
this_vtr_mosaic_df <- as.data.frame(this_vtr_mosaic, xy=TRUE, cells=TRUE, na.rm=TRUE)
#this_vtr_mosaic_df<- this_vtr_mosaic_df %>% rename(value = paste0("X",names(this_vtr_mosaic)))
this_vtr_mosaic_df<- this_vtr_mosaic_df %>% rename(value = names(this_vtr_mosaic))

this_vtr_mosaic_df[this_vtr_mosaic_df== 0] = NA
vtr_mosaic_90 <- na.omit(this_vtr_mosaic_df)

# Plot
# Cropped plot
(example_b_vtrf_90 <- ggplot() +
    geom_sf(data=these_sf_vtr, fill = NA, color = NA)+
    geom_tile(data = vtr_mosaic_90, aes(x = x, y = y, fill = value)) +
    scale_fill_viridis_c(direction = -1, limits = c(1, 600),
                         breaks = c(200, 400, 600),
                         labels = scales::dollar_format(prefix="$ ",)
    )+
    geom_sf(data=this_sf_wea, color = "red", fill = NA)+
    xlab("Longitude")+
    xlim(xmin(this_wea_extent)*.998, xmax(this_wea_extent)*1.002)+
    ylab("Latitude")+
    ylim(ymin(this_wea_extent)*.998, ymax(this_wea_extent)*1.002)+
    labs(title = paste0("B) Cumulative Logbook-Footprint \nrestricted to 90th percentile"),
         fill = "Revenue",
    )+
    annotation_scale(location = "br", width_hint = 0.5) +
    annotation_north_arrow(location = "br", which_north = "true",
                           height = unit(.3, "in"), width = unit(.3, "in"), 
                           pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
                           style = north_arrow_fancy_orienteering))

# plot AF
these_af_paths <- dt_af_revenue[imgid %in% these_trips_af$imgid, path_new]
this_af_mosaic <- paths_to_mosaic(paths = these_af_paths, type = "af", this_wea = this_wea, this_confidence = this_confidence) 
this_sf_wea <- st_transform(this_sf_wea, st_crs(this_af_mosaic))
this_vect <- st_cast(st_union(this_sf_wea),"POLYGON") %>% st_union %>% vect()
this_wea_extent <- ext(this_vect)
this_revenue_af <- extract(x = this_af_mosaic, y = this_vect, exact = TRUE) %>%
  rename("value" = names(this_af_mosaic)) %>%
  mutate(cell_revenue = value*fraction)%>%
  summarise(total_revenue = sum(cell_revenue, na.rm = TRUE))
# convert to a df for plotting in two steps,
this_af_mosaic_df <- as.data.frame(this_af_mosaic, xy=TRUE, cells=TRUE, na.rm=TRUE)
#this_af_mosaic_df<- this_af_mosaic_df %>% rename(value = paste0("X",names(this_af_mosaic)))
this_af_mosaic_df<- this_af_mosaic_df %>% rename(value = names(this_af_mosaic))
this_af_mosaic_df[this_af_mosaic_df== 0] = NA
this_af_mosaic_df <- na.omit(this_af_mosaic_df)

(example_a_aff <- ggplot() +
    geom_tile(data = this_af_mosaic_df, aes(x = x, y = y, fill = value)) + 
    scale_fill_viridis_c(direction = -1, limits = c(0, 600),
                         breaks = c(200, 400, 600),
                         labels = scales::dollar_format(prefix="$ ",))+
    geom_sf(data=this_sf_wea, color = "red", fill = NA)+
    xlab("Longitude")+
    xlim(xmin(this_wea_extent)*.998, xmax(this_wea_extent)*1.002)+
    ylab("Latitude")+
    ylim(ymin(this_wea_extent)*.998, ymax(this_wea_extent)*1.002)+
    labs(title = paste0("A) Cumulative Active-Fishing-Footprint\n "),
         fill = "Revenue",
    )+
    annotation_scale(location = "br", width_hint = 0.5) +
    annotation_north_arrow(location = "br", which_north = "true",
                           height = unit(.3, "in"), width = unit(.3, "in"), 
                           pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
                           style = north_arrow_fancy_orienteering))

# Plot VTRF at 25th percentile
this_confidence <- unique_confidence[[1]]

# figure prep
these_trips_vtr <- dt_wea_revenue[wea_id == this_wea & revenue_vtr > 0 & confidence == this_confidence]
these_trips_af <- dt_wea_revenue[wea_id == this_wea & revenue_af > 0 & confidence == this_confidence]

length(unique(dt_permit[IMGID %in% these_trips_vtr$imgid, permit]))
# 2 permits
length(unique(dt_permit[IMGID %in% these_trips_af$imgid, permit]))
# 3 permits

# select wea sf and make spat vector
this_sf_wea <-sf_all_wea %>% filter(id == this_wea) #%>% select(State)
# count trips by footprint type
n_vtr <-length(these_trips_vtr$imgid)
n_af <- length(these_trips_af$imgid)
# build mosaics if lengths are non-zero for vtr and af trip lists
# And calculate overlap for vtr and af footprints
these_vtr_paths <- dt_vtr_revenue[ imgid %in% these_trips_vtr$imgid & confidence == this_confidence, path_new]
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
this_vtr_mosaic_df <- as.data.frame(this_vtr_mosaic, xy=TRUE, cells=TRUE, na.rm=TRUE)
#this_vtr_mosaic_df<- this_vtr_mosaic_df %>% rename(value = paste0("X",names(this_vtr_mosaic)))
this_vtr_mosaic_df<- this_vtr_mosaic_df %>% rename(value = names(this_vtr_mosaic))
this_vtr_mosaic_df[this_vtr_mosaic_df== 0] = NA
vtr_mosaic_25 <- na.omit(this_vtr_mosaic_df)

# Plot
# Cropped plot
(example_c_vtrf_25 <- ggplot() +
    geom_sf(data=these_sf_vtr, fill = NA, color = NA)+
    geom_tile(data = vtr_mosaic_25, aes(x = x, y = y, fill = value)) + 
    scale_fill_viridis_c(direction = -1, limits = c(0, 600),
                         breaks = c(200, 400, 600),
                         labels = scales::dollar_format(prefix="$ ",))+
    geom_sf(data=this_sf_wea, color = "red", fill = NA)+
    xlab("Longitude")+
    xlim(xmin(this_wea_extent)*.998, xmax(this_wea_extent)*1.002)+
    ylab("Latitude")+
    ylim(ymin(this_wea_extent)*.998, ymax(this_wea_extent)*1.002)+
    labs(title = paste0("C) Cumulative Logbook-Footprint \nrestricted to 25th percentile"),
         fill = "Revenue",
    )+
    annotation_scale(location = "br", width_hint = 0.5) +
    annotation_north_arrow(location = "br", which_north = "true",
                           height = unit(.3, "in"), width = unit(.3, "in"), 
                           pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
                           style = north_arrow_fancy_orienteering))

patchwork <- example_a_aff|example_b_vtrf_90|example_c_vtrf_25

patchwork[[2]] = patchwork[[2]] + theme(axis.title.y = element_blank(),
                                        axis.text.y = element_blank(),
                                        axis.text.x = element_blank(),
                                        legend.position="none")

patchwork[[1]] = patchwork[[1]] + theme(axis.text.y = element_blank(),
                                        axis.text.x = element_blank(),
                                        legend.position="none")

patchwork[[3]] = patchwork[[3]] + theme(axis.title.y = element_blank(),
                                        axis.text.y = element_blank(),
                                        axis.text.x = element_blank())
patchwork

width = 13
height = width*.4

ggsave(filename = paste0(dir_output, "/example_cummulative_footprints.png"),
       plot = patchwork,
       width = width, height = height)  
##############################
# replot all figures, rescaled to show heterogeneity
vtr_mosaic_90_1 <- setDT(vtr_mosaic_90)
vtr_mosaic_90_1 <- vtr_mosaic_90_1[value >1]
vtr_mosaic_90_1 <- as.data.frame(vtr_mosaic_90_1) 

(example_b_vtrf_90_ranked <- ggplot() +
    geom_sf(data=these_sf_vtr, fill = NA, color = NA)+
    geom_tile(data = vtr_mosaic_90_1, aes(x = x, y = y, fill = rank(value))) +
    scale_fill_viridis_c(option = "magma",
                         direction = -1,
                         breaks = quantile(rank(vtr_mosaic_90_1$value)),
                         labels = scales::dollar_format(prefix="$ ",),
                         #                       labels = round(quantile(vtr_mosaic_90_1$value), digits = 1)
    )+
    geom_sf(data=this_sf_wea, color = "red", fill = NA)+
    xlab("Longitude")+
    xlim(xmin(this_wea_extent)*.998, xmax(this_wea_extent)*1.002)+
    ylab("Latitude")+
    ylim(ymin(this_wea_extent)*.998, ymax(this_wea_extent)*1.002)+
    labs(title = paste0("B) Cumulative Logbook-Footprint \nrestricted to 90th percentile"),
         fill = "Ranked Revenue",
    )+
    annotation_scale(location = "br", width_hint = 0.5) +
    annotation_north_arrow(location = "br", which_north = "true",
                           height = unit(.3, "in"), width = unit(.3, "in"), 
                           pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
                           style = north_arrow_fancy_orienteering))

vtr_mosaic_25_1 <- setDT(vtr_mosaic_25)
vtr_mosaic_25_1 <- vtr_mosaic_25_1[value >1]
vtr_mosaic_25_1 <- as.data.frame(vtr_mosaic_25_1) 

(example_c_vtrf_25_ranked <- ggplot() +
    geom_sf(data=these_sf_vtr, fill = NA, color = NA)+
    geom_tile(data = vtr_mosaic_25_1, aes(x = x, y = y, fill = rank(value))) +
    scale_fill_viridis_c(option = "E",
                         direction = -1,
                         breaks = quantile(rank(vtr_mosaic_25_1$value)),
                         labels = scales::dollar_format(prefix="$ ",)
                         #labels = quantile(vtr_mosaic_25_1$value)
    )+
    geom_sf(data=this_sf_wea, color = "red", fill = NA)+
    xlab("Longitude")+
    xlim(xmin(this_wea_extent)*.998, xmax(this_wea_extent)*1.002)+
    ylab("Latitude")+
    ylim(ymin(this_wea_extent)*.998, ymax(this_wea_extent)*1.002)+
    labs(title = paste0("B) Cumulative Logbook-Footprint \nrestricted to 90th percentile"),
         fill = "Ranked Revenue",
    )+
    annotation_scale(location = "br", width_hint = 0.5) +
    annotation_north_arrow(location = "br", which_north = "true",
                           height = unit(.3, "in"), width = unit(.3, "in"), 
                           pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
                           style = north_arrow_fancy_orienteering))

this_af_mosaic_df_1 <- setDT(this_af_mosaic_df)
this_af_mosaic_df_1 <- this_af_mosaic_df_1[value >1]
this_af_mosaic_df_1 <- as.data.frame(this_af_mosaic_df_1) 

(example_a_aff_ranked <- ggplot() +
    geom_tile(data = this_af_mosaic_df_1, aes(x = x, y = y, fill = value)) + 
    scale_fill_viridis_c(option = "E",
                         direction = -1,
                         #breaks = quantile(rank(this_af_mosaic_df_1$value)),
                         #labels = scales::dollar_format(prefix="$ ",)
                         #labels = quantile(this_af_mosaic_df_1$value)
    )+
    geom_sf(data=this_sf_wea, color = "red", fill = NA)+
    xlab("Longitude")+
    xlim(xmin(this_wea_extent)*.998, xmax(this_wea_extent)*1.002)+
    ylab("Latitude")+
    ylim(ymin(this_wea_extent)*.998, ymax(this_wea_extent)*1.002)+
    labs(title = paste0("A) Cumulative Active-Fishing-Footprint\n "),
         fill = "Ranked Revenue",
    )+
    annotation_scale(location = "br", width_hint = 0.5) +
    annotation_north_arrow(location = "br", which_north = "true",
                           height = unit(.3, "in"), width = unit(.3, "in"), 
                           pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
                           style = north_arrow_fancy_orienteering))


##############################
# loop to create plots for all WEAs

dt_wea_revenue_overlap <- data.table()

# for(this_wea in unique_wea){
#   for(this_confidence in unique_confidence){
these_trips_vtr <- dt_wea_revenue[wea_id == this_wea & revenue_vtr > 0 & confidence == this_confidence]
these_trips_af <- dt_wea_revenue[wea_id == this_wea & revenue_af > 0 & confidence == this_confidence]
# select wea sf and make spat vector
this_sf_wea <-sf_all_wea %>% filter(id == this_wea) #%>% select(State)
# count trips by footprint type
n_vtr <-length(these_trips_vtr$imgid)
n_af <- length(these_trips_af$imgid)
# build mosaics if lengths are non-zero for vtr and af trip lists
# And calculate overlap for vtr and af footprints
these_vtr_paths <- dt_vtr_revenue[ imgid %in% these_trips_vtr$imgid & confidence == this_confidence,paths]
this_vtr_mosaic <- paths_to_mosaic(paths = these_vtr_paths, type = "vtr", this_wea = this_wea, this_confidence = this_confidence)
# these_vtrs <- lapply(X = dt_vtr_revenue[
#   imgid %in% these_trips_vtr$imgid & confidence == this_confidence,paths],
#   FUN = rast)
# this_vtr_mosaic <- do.call(terra::mosaic, args = c(these_vtrs, fun = "sum"))
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
this_vtr_mosaic_df <- as.data.frame(this_vtr_mosaic, xy=TRUE, cells=TRUE, na.rm=TRUE)
this_vtr_mosaic_df<- this_vtr_mosaic_df %>% rename(value = paste0("X",names(this_vtr_mosaic)))
this_vtr_mosaic_df[this_vtr_mosaic_df== 0] = NA
this_vtr_mosaic_df <- na.omit(this_vtr_mosaic_df)
# Plot
this_plot_vtr <- ggplot() +
  geom_sf(data=these_sf_vtr, fill = NA, color = NA)+
  geom_tile(data = this_vtr_mosaic_df, aes(x = x, y = y, fill = value)) + 
  scale_fill_viridis_c(direction = -1, limits = c(0, 600))+
  geom_sf(data=this_sf_wea, color = "red", fill = NA)+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(title = paste0("Cumulative Logbook-Footprint (",this_confidence,")\noverlapping with revenue within ", this_wea),
       subtitle = paste0("WEA intersects ", length(unique(these_trips_vtr$imgid)),
                         " subtrips and overlaps $",
                         round(this_revenue_vtr$total_revenue, digits = 0),
                         " revenue"),
       fill = "Revenue",
       caption = paste0("Revenue is in US dollars per 0.25 km2 and adjusted to 2019 annual GDP \nLeased to ",
                        dt_all_wea[id == this_wea],
                        "\nLease info:", dt_all_wea[id == this_wea]))+
  annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true",
                         height = unit(.3, "in"), width = unit(.3, "in"), 
                         pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering)
# Cropped plot
this_plot_vtr_cropped <- ggplot() +
  geom_sf(data=these_sf_vtr, fill = NA, color = NA)+
  geom_tile(data = this_vtr_mosaic_df, aes(x = x, y = y, fill = value)) + 
  scale_fill_viridis_c(direction = -1, limits = c(0, 600))+
  geom_sf(data=this_sf_wea, color = "red", fill = NA)+
  xlab("Longitude")+
  xlim(xmin(this_wea_extent)*.998, xmax(this_wea_extent)*1.002)+
  ylab("Latitude")+
  ylim(ymin(this_wea_extent)*.998, ymax(this_wea_extent)*1.002)+
  labs(title = paste0("B) Cumulative Logbook-Footprint \nrestricted to 90th percentile"),
       # title = paste0("Vessel Trip Report footprints (",this_confidence,")\noverlapping with revenue within ", this_wea),
       # subtitle = paste0("WEA intersects ", length(unique(these_trips_vtr$imgid)),
       #                   " subtrips and overlaps $",
       #                   round(this_revenue_vtr$total_revenue, digits = 0),
       #                   " revenue"),
       fill = "Revenue",
       # caption = paste0("Revenue is in US dollars per 0.25 km2 and adjusted to 2019 annual GDP \nLeased to ",
       #                  dt_all_wea[id == this_wea],
       #                 "\nLease info:", dt_all_wea[id == this_wea])
  )+
  annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true",
                         height = unit(.3, "in"), width = unit(.3, "in"), 
                         pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering)

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
this_af_mosaic_df <- as.data.frame(this_af_mosaic, xy=TRUE, cells=TRUE, na.rm=TRUE)
this_af_mosaic_df<- this_af_mosaic_df %>% rename(value = paste0("X",names(this_af_mosaic)))
this_af_mosaic_df[this_af_mosaic_df== 0] = NA
this_af_mosaic_df <- na.omit(this_af_mosaic_df)
this_plot_af <- ggplot() +
  geom_sf(data=these_sf_vtr, fill = "dark grey", color = NA)+
  geom_tile(data = this_af_mosaic_df, aes(x = x, y = y, fill = value)) + 
  scale_fill_viridis_c(direction = -1, limits = c(0, 600))+
  geom_sf(data=this_sf_wea, color = "red", fill = NA)+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(title = paste0("Active Fishing footprints \noverlapping with revenue within ", this_wea),
       # subtitle = paste0("WEA intersects ", length(unique(these_trips_af$imgid)),
       #                   " subtrips and overlaps $",
       #                   round(this_revenue_af$total_revenue, digits = 0),
       #                   " revenue"),
       fill = "Revenue",
       # caption = paste0("Revenue is in US dollars per 0.25 km2 and adjusted to 2019 annual GDP \nLeased to ",
       #                  dt_all_wea[id == this_wea],
       #                  "\nLease info:", dt_all_wea[id == this_wea])
  )+
  annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true",
                         height = unit(.3, "in"), width = unit(.3, "in"), 
                         pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering)
this_plot_af_cropped <- ggplot() +
  geom_tile(data = this_af_mosaic_df, aes(x = x, y = y, fill = value)) + 
  scale_fill_viridis_c(direction = -1, limits = c(0, 600))+
  geom_sf(data=this_sf_wea, color = "red", fill = NA)+
  xlab("Longitude")+
  xlim(xmin(this_wea_extent)*.998, xmax(this_wea_extent)*1.002)+
  ylab("Latitude")+
  ylim(ymin(this_wea_extent)*.998, ymax(this_wea_extent)*1.002)+
  labs(title = paste0("A) Cumulative Active-Fishing-Footprint\n "),
       #title = paste0("Active Fishing footprints \noverlapping with revenue within ", this_wea),
       # subtitle = paste0("WEA intersects ", length(unique(these_trips_af$imgid)),
       #                   " subtrips and overlaps $",
       #                   round(this_revenue_af$total_revenue, digits = 0),
       #                   " revenue"),
       fill = "Revenue",
       # caption = paste0("Revenue is in US dollars per 0.25 km2 and adjusted to 2019 annual GDP \nLeased to ",
       #                  dt_all_wea[id == this_wea],
       #                  "\nLease info:", dt_all_wea[id == this_wea])
  )+
  annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true",
                         height = unit(.3, "in"), width = unit(.3, "in"), 
                         pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering)

# Create plot list by conditions - full extent
this_wea_no_space <- gsub(" ", "_", this_wea, fixed = TRUE)
height = 8 #width*.618
width = 11 
plotlist <- list(this_plot_vtr, this_plot_af)
this_path <- paste0(dir_output, "/plots_by_wea/full/",this_wea_no_space,"_",this_confidence,"_full.png")
these_plots <- ggarrange(plotlist = plotlist, ncol = 2, nrow = 1, align = c("hv"))
ggsave(filename = this_path, plot = these_plots, width = width, height = height)  

# Create plot list by conditions - cropped plots
plotlist_cropped <- list(this_plot_vtr_cropped, this_plot_af_cropped)
this_path_cropped <- paste0(dir_output, "/plots_by_wea/cropped/",this_wea_no_space,"_",this_confidence,"_cropped.png")
these_plots_cropped <- ggarrange(plotlist = plotlist_cropped, ncol = 2, nrow = 1, align = c("hv"))
ggsave(filename = this_path_cropped, plot = these_plots_cropped, width = width, height = height)  

# Add data to data.table
these_data <- data.table(confidence= this_confidence,
                         wea_id = this_wea,
                         revenue_vtr_overlap = round(this_revenue_vtr$total_revenue, digits = 0),
                         revenue_af_overlap =  round(this_revenue_af$total_revenue, digits = 0),
                         n_vtr_subtrips = length(unique(these_trips_vtr$imgid)),
                         n_af_subtrips = length(unique(these_trips_af$imgid)),
                         revenue_subtrip = dt_wea_revenue[
                           wea_id == this_wea & confidence == this_confidence,
                           revenue_subtrip])

dt_wea_revenue_overlap <- rbindlist(list(dt_wea_revenue_overlap, these_data), fill=TRUE)
#   }
# }
##############################
# Test with SF

#sf_af <- readRDS(file = paste0(dir_output, "/sf_buffered_hulls_subtrip.rds"))
#sf_vtr <- readRDS(paste0(dir_output, "/sf_vtrb_cumulative_imgid.rds"))

sf_vtr$percentile <- factor(sf_vtr$percentile,
                            levels = c("25th", "50th", "75th", "90th"))
these_sf_af <- sf_af %>% filter(imgid_chr %in% these_trips_af$imgid) %>% select (value_gdp)
these_sf_vtr <- sf_vtr %>% filter(imgid %in% these_trips_vtr$imgid)


ggplot() +
  geom_sf(data=these_sf_vtr, fill = "dark grey", color = NA)+
  #scale_fill_viridis_d(direction = -1)+
  geom_sf(data=these_sf_af) +
  geom_sf(data=this_sf_wea, color = "red", fill = NA)+
  ggtitle("test")

##############################
(plot_wea<- ggplot()+
   geom_raster(this_vtr_mosaic)+
   geom_sf(data=this_sf_wea, color = "black", fill = NA)+
   #scale_fill_viridis_d(direction = -1)+
   #geom_sf(data=single_hull, aes(color=Hull) ,fill=NA, size = 4)+
   #scale_color_manual(values = alpha("red", .5))+
   xlab("Longitude")+
   ylab("Latitude")+
   labs(title = "VTR revenue within wea")+
   #subtitle = "all trips aggregated",
   #fill = "VTR footprint \nby percentile",
   #color = "Active fishing \nfootprint") +
   annotation_scale(location = "br", width_hint = 0.5) +
   annotation_north_arrow(location = "br", which_north = "true",
                          height = unit(.3, "in"), width = unit(.3, "in"), 
                          pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
                          style = north_arrow_fancy_orienteering))

height = 8
width = height*.618

ggsave(filename = paste0(dir_output, "/plot_aggregate.png"),
       plot = plot_aggregate, width = width, height = height)
