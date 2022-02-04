# 4a plot all - merged and separate then merge
# 4b plot by trip
# 4c plot summary data

# Loading packages
library(data.table)
library(sf)
library(dplyr)
library( ggplot2)
library (cowplot)
library(ggspatial)

# This script creates plots from SF

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
sf_gte_nad83 <- readRDS(paste0(dir_output,"/sf_gte_nad83.rds"))

sf_hulls_attributes <- readRDS(paste0(dir_output, "/sf_hulls_attributes.rds"))

sf_vtrb_split_mosaic <- readRDS(paste0(dir_output, "/sf_vtrb_split_mosaic.rds"))

sf_bias <- readRDS(paste0(dir_output, "/sf_bias.rds"))

##############################
#data prep

single_hull <- sf_gte_nad83 %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_convex_hull()

single_hull<- cbind(Hull = "all trips", single_hull)

sf_vtrb_split_mosaic$percentile <- factor(sf_vtrb_split_mosaic$percentile,
                                         levels = c("100th", "75th", "50th", "25th"))

# aggregate all vtrb by percentile
sf_vtrb__aggregate_split<- sf_vtrb_split_mosaic %>%
  group_by(percentile) %>%
  summarise(geometry = st_union(geometry))

# calculate intersection and intersection area
sf_intersection_aggregate <- sf_vtrb__aggregate_split %>%
  group_by(percentile) %>%
  st_intersection(single_hull) # creates new SF with intersection as geometry

sf_intersection_aggregate <- sf_intersection_aggregate %>%  # creates new sf
  mutate(area = st_area(.) %>% as.numeric()) %>%
  cbind(type = "intersection")

# calculate false positive and false positive area
sf_false_positive_aggregate <-  sf_vtrb__aggregate_split %>%
  group_by(percentile) %>%
  st_difference(single_hull) # creates new SF with intersection as geometry

sf_false_positive_aggregate <- sf_false_positive_aggregate %>%  # creates new sf
  mutate(area = st_area(.) %>% as.numeric()) %>%
  cbind(type = "false_positive")

# calculate false negative and false negative area
sf_false_negative_aggregate <- single_hull%>%
  st_difference(sf_vtrb__aggregate_split) %>%
  group_by(percentile) %>%
  ungroup()
  
sf_false_negative_aggregate <- sf_false_negative_aggregate %>%  # creates new sf
  mutate(area = st_area(.) %>% as.numeric()) %>%
  cbind(type = "false_negative")

# merge aggregated bias

sf_bias_aggregate <- rbind(sf_intersection_aggregate,
                           sf_false_positive_aggregate,
                           sf_false_negative_aggregate)
                 

##############################
#Plot all points as hull with all buffers
(plot_aggregate<- ggplot()+
  geom_sf(data=sf_vtrb_split_mosaic, aes(fill = percentile), color = NA)+
  scale_fill_viridis_d(direction = -1)+
  geom_sf(data=single_hull, aes(color=Hull) ,fill=NA, size = 4)+
  scale_color_manual(values = alpha("grey", .75))+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(title = "Aggregated data",
       subtitle = "merged VTR buffers and convex hull from Study Fleet",
       fill = "Percentile") +
  annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(legend.position = c(.1, .85))) #L = 0, R = 1, T = 1, B = 0

ggsave(filename = paste0(dir_output, "/plot_aggregate.png"),
       plot = plot_aggregate, width = 8, height = 11)

##############################
#Plot mismatch

(plot_mismatch_aggregate <- ggplot() +
   geom_rect(data = sf_bias_aggregate, aes(color=percentile), size = 2,
             fill = NA, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
   scale_color_viridis_d(direction = -1)+
   #scale_fill_viridis_d(direction = -1)+
   geom_sf(data=sf_bias_aggregate, aes(fill=type))+
   scale_fill_manual(values=c("red", "black", "grey"))+
   facet_wrap(~ percentile, nrow = 1)+
   xlab("Longitude") + ylab("Latitude"))+
  labs(title = "Mismatch for aggregated data",
       subtitle = "merged VTR buffers and convex hull from all Study Fleet trips in dataset",
       color = 'Percentile',
       fill = "Mismatch type")

ggsave(filename = paste0(dir_output, "/plot_mismatch_aggregate.png"),
       plot = plot_mismatch_aggregate, width = 11, height = 4)

##############################
#Plot all convex hulls together with features
(plot_aggregate_hull_tot_loligo <- ggplot(sf_hulls_attributes) +
   geom_sf(aes(fill=Summed_SUM_LOLIGO_CATCH), color = NA)+
   scale_fill_viridis_b(direction = -1, alpha = 0.5)+
   geom_sf(data=single_hull, aes(color=Hull) ,fill=NA, size = 4)+
   scale_color_manual(values = alpha("grey", .75))+
   xlab("Longitude")+
   ylab("Latitude")+
   labs(fill = "Total loligo catch \nby trip") +
   annotation_scale(location = "br", width_hint = 0.5) +
   annotation_north_arrow(location = "br", which_north = "true", 
                          pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                          style = north_arrow_fancy_orienteering) +
   theme(legend.position = c(.85, .35))) #L = 0, R = 1, T = 1, B = 0

(plot_aggregate_hull_prop_loligo <- ggplot(sf_hulls_attributes) +
    geom_sf(aes(fill=Mean_prop_loligo))+
    scale_fill_viridis_b(direction = -1, alpha = 0.5)+
    geom_sf(data=single_hull, aes(color=Hull) ,fill=NA, size = 4)+
    scale_color_manual(values = alpha("grey", .75))+
    xlab("Longitude")+
    ylab("Latitude")+
    labs(fill = "Proportion of loligo \nby trip") +
    annotation_scale(location = "br", width_hint = 0.5) +
    annotation_north_arrow(location = "br", which_north = "true", 
                           pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                           style = north_arrow_fancy_orienteering) +
    theme(legend.position = c(.85, .35))) #L = 0, R = 1, T = 1, B = 0

(plotAllByMonth <- ggplot(sf_hulls_attributes) +
    geom_sf(aes(fill=MONTH))+
    scale_fill_viridis_b(direction = -1, alpha = 0.5)+
    geom_sf(data=single_hull, aes(color=Hull) ,fill=NA, size = 4)+
    scale_color_manual(values = alpha("grey", .75))+
    xlab("Longitude")+
    ylab("Latitude")+
    labs(fill = "Month of trip") +
    annotation_scale(location = "br", width_hint = 0.5) +
    annotation_north_arrow(location = "br", which_north = "true", 
                           pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                           style = north_arrow_fancy_orienteering) +
    theme(legend.position = c(.85, .35))) #L = 0, R = 1, T = 1, B = 0

(p <- plot_grid(plot_aggregate_hull_tot_loligo,
                plot_aggregate_hull_prop_loligo, nrow = 1,
               align = "hv", axis = "l")) #
