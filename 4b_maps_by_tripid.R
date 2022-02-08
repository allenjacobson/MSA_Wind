
# Loading packages
library(data.table)
library(sf)
library(dplyr)
library( ggplot2)
library (cowplot)
library(ggspatial)
library(stringr)

# This script creates maps for each trip

##############################
# Functions

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
sf_vtrb_split_mosaic <- readRDS(paste0(dir_output, "/sf_vtrb_split_mosaic.rds"))

sf_hulls_attributes <- readRDS(file = paste0(dir_output, "/sf_hulls_attributes.rds"))

sf_bias <- readRDS(paste0(dir_output, "/sf_bias.rds"))

##############################
#Plot by trip

#sf_bias is missing trips - only contains 117 - instead of 220
unique_trips <- unique(sf_bias$tripid)
#this_trip <- unique_trips[[4]]

for (this_trip in unique_trips) {
  these_vtrb <- sf_bias %>%
    filter(tripid==this_trip, type == "vtrb")
  
  this_sfch <- sf_bias %>%
    filter(tripid==this_trip, type == "sfch")
  
  this_sfch  <-cbind(this_sfch, shape = "convex hull")
  
  # Change ordering manually
  these_vtrb$percentile <- factor(these_vtrb$percentile,
                                  levels = c("100th", "75th", "50th", "25th"))
  
  this_plot<- ggplot()+
    geom_sf(data=these_vtrb, aes(fill = percentile), color = NA)+
    scale_fill_viridis_d(direction = -1)+
    geom_sf(data=this_sfch, aes(color= shape), fill=NA, size = 4)+
    scale_color_manual(values = alpha("red", .5))+
    xlab("Longitude")+
    ylab("Latitude")+
    labs(title = paste0("Comparison by trip: ", this_trip),
         fill = "VTR Footprint by Percentile",
         color = "Study Fleet Footprint") +
    annotation_scale(location = "br", width_hint = 0.5) +
    annotation_north_arrow(location = "br", which_north = "true", 
                           pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                           style = north_arrow_fancy_orienteering)
    #theme(legend.position = c(.1, .8)) #First: L = 0, R = 1;Second: T = 1, B = 0
  
  ggsave(filename = paste0(dir_output, "/vtrb_sfch_by_tripid/",this_trip,".png"),
         plot = this_plot, width = 11, height = 8)
  }

##############################
# Plot bias by trip

for (this_trip in unique_trips) {
  these_intersections <- sf_bias %>%
    filter(tripid==this_trip, type == "intersection")
  
  these_false_positives <- sf_bias %>%
    filter(tripid==this_trip, type == "false_positive")

  these_false_negatives <- sf_bias %>%
    filter(tripid==this_trip, type == "false_negative")

  # Change ordering manually
  these_false_negatives$percentile <- factor(these_false_negatives$percentile,
                                             levels = c("100th", "75th", "50th", "25th"))
  # Change ordering manually
  these_false_positives$percentile <- factor(these_false_positives$percentile,
                                             levels = c("100th", "75th", "50th", "25th"))
  # Change ordering manually
  these_intersections$percentile <- factor(these_intersections$percentile,
                                           levels = c("100th", "75th", "50th", "25th"))
  
  this_bias <- rbind(these_intersections,
                     these_false_positives,
                     these_false_negatives)

  this_bias$type2 <- str_replace(this_bias$type, "_", " ")
  
  this_plot <- ggplot() +
      geom_rect(data = these_intersections, aes(color=percentile), size = 2,
                fill = NA, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
      scale_color_viridis_d(direction = -1)+
      geom_sf(data=this_bias, aes(fill=type2))+
      scale_fill_manual(values=c("red", "black", "grey"))+
      facet_wrap(~ percentile, nrow = 1)+
      xlab("Longitude") + ylab("Latitude")+
      guides(color = "none")+
      labs(title = paste0("Mismatch by trip: ", this_trip),
           fill = NULL)
  
  ggsave(filename = paste0(dir_output, "/mismatch_by_tripid/",this_trip,".png"),
         plot = this_plot, width = 11, height = 4)
  }



#33033916070114_twoSpaced
#32064517052800_three
#31047319062808_smaller
#31047319052908_two
#15077317070701_ideal
