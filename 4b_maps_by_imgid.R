# Loading packages
library(data.table)
library(sf)
library(dplyr)
library( ggplot2)
library(ggpubr)
#library (cowplot)
library(ggspatial)
library(stringr)

# This script creates maps for each imgid

# Problem trips - multiple buffers by imgid, and  missing percentiles?!?!
#3104731611041101
#3104731611041102
#3206451704290101
#3206451704290102
#3206451705240101


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
sf_gte <- readRDS(paste0(dir_output,"/sf_gte_nad83.rds"))
sf_vtrb <- readRDS(paste0(dir_output, "/sf_vtrb_cumulative_imgid.rds"))
sf_shapes <-readRDS(file = paste0(dir_output, "/sf_buffered_hulls_subtrip"))
sf_bias <- readRDS(file= paste0(dir_output, "/sf_bias_hulls_imgid.rds"))


#sf_shapes <- readRDS(file = paste0(dir_output, "/sf_buffered_polygon_subtrip.rds"))
#sf_bias <- readRDS(file= paste0(dir_output, "/sf_bias_imgid.rds"))


# Prep data
sf_shapes <- st_make_valid(sf_shapes)
sf_vtrb <- st_make_valid(sf_vtrb)

# old files
#sf_vtrb_cumulative_imgid <- readRDS(paste0(dir_output, "/sf_vtrb_cumulative_imgid.rds"))
# sf_hulls_attributes_imgid <- readRDS(file = paste0(dir_output, "/sf_hulls_attributes_imgid.rds"))
# sf_bias <- readRDS(paste0(dir_output, "/sf_bias_imgid.rds"))
# sf_gte_nad83 <- readRDS(paste0(dir_output,"/sf_gte_nad83.rds"))

##############################
#data prep
sf_vtrb <- sf_vtrb %>%
  filter(imgid %in% sf_shapes$imgid_chr)

sf_shapes <- sf_shapes %>%
  filter(imgid_chr %in% sf_vtrb$imgid)

##############################
#Plot by trip

#sf_bias is missing trips - only contains 117 - instead of 220
unique_trips <- unique(sf_bias$imgid)
#this_trip <- unique_trips[[10]]
this_trip <- "3206451704290101"

for (this_trip in unique_trips) {
  these_vtrb <- sf_bias %>%
    filter(imgid==this_trip, type == "vtrb")
  
  this_shape <- sf_bias %>%
    filter(imgid==this_trip, type == "sfch")
  
  this_shape  <-cbind(this_shape, shape = "convex hull")
  
  these_points <- sf_gte %>%
    filter(imgid_chr==this_trip) %>%
    select(area) %>%
    mutate(area = as.factor(area))
  
  # Change ordering manually
  these_vtrb$percentile <- factor(these_vtrb$percentile,
                                  levels = c("90th", "75th", "50th", "25th"))
  
  this_plot<- ggplot()+
    geom_sf(data=these_vtrb, aes(fill = percentile), color = NA)+
    scale_fill_viridis_d(direction = -1)+
    geom_sf(data=this_shape, aes(color= shape), fill=NA, size = 2)+
    scale_color_manual(values = alpha("red", .75))+
    geom_sf(data = these_points, aes(shape = area),
            size = 1, alpha = .15, color = "white")+
    xlab("Longitude")+
    ylab("Latitude")+
    guides(shape = "none")+
    labs(title = paste0("Comparison by subtrip: ", this_trip),
         subtitle = paste0("includes ", length(these_points$area), " GPS points and ",
                           length(unique(these_points$area)), " area(s)"),
         fill = "VTR Footprint by Percentile",
         color = "Active fishing Footprint") +
    annotation_scale(location = "br", width_hint = 0.5) +
    annotation_north_arrow(location = "br", which_north = "true",
                           height = unit(.3, "in"), width = unit(.3, "in"), 
                           pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
                           style = north_arrow_fancy_orienteering)

width = 8
height = width*.618
  
ggsave(filename = paste0(dir_output, "/hulls_by_haul_vtrb_by_imgid/",this_trip,".png"),
       plot = this_plot, width = width, height = height)
  }

##############################
# Plot bias by trip

for (this_trip in unique_trips) {
  these_intersections <- sf_bias %>%
    filter(imgid==this_trip, type == "intersection")
  
  these_false_positives <- sf_bias %>%
    filter(imgid==this_trip, type == "false_positive")

  these_false_negatives <- sf_bias %>%
    filter(imgid==this_trip, type == "false_negative")

  # Change ordering manually
  these_false_negatives$percentile <- factor(these_false_negatives$percentile,
                                             levels = c("90th", "75th", "50th", "25th"))
  # Change ordering manually
  these_false_positives$percentile <- factor(these_false_positives$percentile,
                                             levels = c("90th", "75th", "50th", "25th"))
  # Change ordering manually
  these_intersections$percentile <- factor(these_intersections$percentile,
                                           levels = c("90th", "75th", "50th", "25th"))
  
  this_bias <- rbind(these_intersections,
                     these_false_positives,
                     these_false_negatives)

  this_bias$type2 <- str_replace(this_bias$type, "_", " ")
  
  this_plot <- ggplot() +
    geom_rect(data = these_intersections, aes(color=percentile), size = 2,
              fill = NA, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
    scale_color_viridis_d(direction = -1)+
    geom_sf(data=this_bias, aes(fill=type2), color = NA)+
    scale_fill_manual(values=c("red", "black", "grey"))+
    facet_wrap(~ percentile, nrow = 1)+
    xlab("Longitude") + ylab("Latitude")+
    guides(color = "none")+
    theme(axis.text.x = element_text(angle = 90))+
    labs(title = paste0("Mismatch by subtrip: ", this_trip),
         fill = NULL)
  
  width = 8
  height = width*.618
  
  ggsave(filename = paste0(dir_output, "/hulls_by_haul_mismatch_by_imgid/",this_trip,".png"),
         plot = this_plot, width = width, height = height)
  }


##############################
#these are tripids - need to look through to pick illustrative imgids

# At trip level
#33033916070114_twoSpaced
#32064517052800_three
#31047319062808_smaller
#31047319052908_two
#15077317070701_ideal

# At subtrip level
#1507731707022301 odd - lots of overlapping tracks, doesn't look as expected
#1507731707022302 problem - outside of vtrb
#1507731707070101

##############################
# Example plots of ideal trip for presentation 
this_trip = "1507731707070101"

these_vtrb <- sf_bias %>%
  filter(imgid==this_trip, type == "vtrb")

this_shape <- sf_bias %>%
  filter(imgid==this_trip, type == "sfch")

this_shape  <-cbind(this_shape, shape = "convex hull")

these_points <- sf_gte %>%
  filter(imgid_chr==this_trip) %>%
  select(area) %>%
  mutate(area = as.factor(area))

# Change ordering manually
these_vtrb$percentile <- factor(these_vtrb$percentile,
                                levels = c("90th", "75th", "50th", "25th"))

# plot convex hull with points
(this_plot<- ggplot()+
  geom_sf(data=this_shape, aes(color= shape), fill=NA, size = 2)+
  scale_color_manual(values = alpha("red", .75))+
  #geom_sf(data = these_points, aes(shape = area),
  #        size = 1, alpha = .15, color = "white")+
  geom_sf(data = these_points, size = 1, alpha = .1)+
  xlab("Longitude")+
  ylab("Latitude")+
  guides(shape = "none")+
  labs(title = paste0("Example subtrip: ", this_trip),
       subtitle = paste0("includes ", length(these_points$area), " GPS points and ",
                         length(unique(these_points$area)), " area(s)"),
       color = "Active fishing Footprint") +
  annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true",
                         height = unit(.3, "in"), width = unit(.3, "in"), 
                         pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering))

# single convex hull with points
(plot_example_sfch<- ggplot()+
  geom_sf(data=this_shape, color = "red", fill="red", alpha = .5, size = 1)+
  geom_sf(data = these_points, size = 1, alpha = .1)+
  xlab("Longitude")+
  ylab("Latitude")+
  theme(axis.text.x = element_text(angle = 90))+
  labs(title = "Active fishing footprint",
       subtitle = paste0("Convex hull from\n", length(these_points$trip_id),
                         " GPS points")))

width = 6
height = width*.618

ggsave(filename = paste0(dir_output, "/plot_example_sfch_imgid",this_trip,".png"),
       plot = plot_example_sfch, width = width, height = height)

# single vtrb without points
(plot_example_vtrb<- ggplot()+
  geom_sf(data=these_vtrb, aes(fill = percentile), color = NA)+
  scale_fill_viridis_d(direction = -1)+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(title = "VTR footprint",
       subtitle = "one VTR footprints per trip",
       fill = "Percentile"))

width = 8
height = width*.618

ggsave(filename = paste0(dir_output, "/plot_example_vtrb_imgid",this_trip,".png"),
       plot = plot_example_vtrb, width = width, height = height)

# Example plots of ideal trip for presentation 
this_trip = "32064517052800"

these_vtrb <- sf_bias %>%
  filter(imgid==this_trip, type == "vtrb")

this_shape <- sf_bias %>%
  filter(imgid==this_trip, type == "sfch")

this_shape  <-cbind(this_shape, shape = "convex hull")

these_points <- sf_gte %>%
  filter(imgid_chr==this_trip)

# Change ordering manually
these_vtrb$percentile <- factor(these_vtrb$percentile,
                                levels = c("90th", "75th", "50th", "25th"))

(plot_example_vtrb<- ggplot()+
    geom_sf(data=these_vtrb, aes(fill = percentile), color = NA)+
    scale_fill_viridis_d(direction = -1)+
    xlab("Longitude")+
    ylab("Latitude")+
    labs(title = "VTR footprint",
         subtitle = "multiple VTR footprints per trip",
         fill = "Percentile"))

width = 8
height = width*.618

ggsave(filename = paste0(dir_output, "/plot_example_vtrb_imgid",this_trip,".png"),
       plot = plot_example_vtrb, width = width, height = height)

