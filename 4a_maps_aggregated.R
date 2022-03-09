# Loading packages
library(data.table)
library(sf)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggspatial)
library(stringr)

# This script creates plots for 3 aggregated data
# 1) Comparison of footprints
# 2) Mismatch
# 3) 4-panel plot, showing total catch, prop caught, cpue, and month

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
sf_gte <- readRDS(file = paste0(dir_output, "/sf_gte_nad83_singles.rds"))
sf_polygon <- readRDS(file = paste0(dir_output, "/sf_buffered_polygon_subtrip.rds"))
sf_vtrb <- readRDS(paste0(dir_output, "/sf_vtrb_cumulative_imgid.rds"))
sf_bias <- readRDS(file= paste0(dir_output, "/sf_bias_imgid.rds"))

sf_polygon <- st_make_valid(sf_polygon)
sf_vtrb <- st_make_valid(sf_vtrb)

#sf_hulls_attributes <- readRDS(paste0(dir_output, "/sf_hulls_attributes.rds"))
#sf_vtrb_split_mosaic <- readRDS(paste0(dir_output, "/sf_vtrb_split_mosaic.rds"))
#sf_bias <- readRDS(paste0(dir_output, "/sf_bias.rds"))

##############################
#data prep

# this is not working - need to go back to sf creation (3a) to add non-constant attributes
# catch is in lbs
# effort_dur is in hrs
# sf_polygon <- sf_polygon %>%
#   mutate(cpue = Summed_SUM_LOLIGO_CATCH/Summed_effort_dur,
#          season = ifelse(MONTH %in% c(1,2,3,4), "Jan-Apr",
#                          ifelse( MONTH %in% c(5,6,7,8), "May-Aug",
#                                  "Sep-Dec")))

single_polygon <- sf_polygon %>%
  summarise(geometry = st_combine(geometry))

plot(single_polygon)

single_polygon <- st_make_valid(single_polygon)


#rank percentiles for plot
sf_vtrb$percentile <- factor(sf_vtrb$percentile,
                             levels = c("100th", "75th", "50th", "25th"))

# aggregate all vtrb by percentile
sf_vtrb__aggregate_split<- sf_vtrb %>%
  group_by(percentile) %>%
  summarise(geometry = st_union(geometry))

# calculate intersection and intersection area
# creates new SF with intersection as geometry

## Following line gives error
sf_intersection_aggregate <- sf_vtrb__aggregate_split %>%
  group_by(percentile) %>%
  st_intersection(single_polygon) #used to be single_hull instead of sf_polygon 

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

sf_bias_aggregate$type2 <- str_replace(sf_bias_aggregate$type, "_", " ")


##############################
#Plot all points as hull with all buffers
(plot_aggregate<- ggplot()+
  geom_sf(data=sf_vtrb, aes(fill = percentile), color = NA)+
  scale_fill_viridis_d(direction = -1)+
  geom_sf(data=single_hull, aes(color=Hull) ,fill=NA, size = 4)+
  scale_color_manual(values = alpha("red", .5))+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(title = "Size bias",
       subtitle = "all trips aggregated",
       fill = "VTR footprint \nby percentile",
       color = "Active fishing \nfootprint") +
  annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true",
                         height = unit(.3, "in"), width = unit(.3, "in"), 
                         pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering))

height = 8
width = height*.618

ggsave(filename = paste0(dir_output, "/plot_aggregate.png"),
       plot = plot_aggregate, width = width, height = height)

##############################
#Plot mismatch

(plot_mismatch_aggregate <- ggplot() +
   geom_rect(data = sf_bias_aggregate, aes(color=percentile), size = 2,
             fill = NA, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
   scale_color_viridis_d(direction = -1)+
   #scale_fill_viridis_d(direction = -1)+
   geom_sf(data=sf_bias_aggregate, aes(fill=type2), color = NA)+
   scale_fill_manual(values=c("red", "black", "grey"))+
   facet_wrap(~ percentile, nrow = 1)+
   xlab("Longitude") +
   ylab("Latitude") +
   guides(color = "none")+
   labs(title = "Mismatch",
        subtitle = "all trips aggregated",
        fill = NULL))

width = 8
height = width*.618

ggsave(filename = paste0(dir_output, "/plot_mismatch_aggregate.png"),
       plot = plot_mismatch_aggregate, width = width, height = height)

#Plot mismatch, 100th only

sf_bias_aggregate_100 <- sf_bias_aggregate %>%
  filter(percentile=="100th")
  

(plot_mismatch_aggregate_100 <- ggplot() +
    geom_rect(data = sf_bias_aggregate_100, aes(color=percentile), size = 2,
              fill = NA, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
    scale_color_viridis_d(direction = -1)+
    #scale_fill_viridis_d(direction = -1)+
    geom_sf(data=sf_bias_aggregate_100, aes(fill=type2), color = NA)+
    scale_fill_manual(values=c("red", "black", "grey"))+
    facet_wrap(~ percentile, nrow = 1)+
    xlab("Longitude") +
    ylab("Latitude") +
    guides(color = "none")+
    labs(title = "Mismatch",
         subtitle = "100th percentile",
         fill = NULL))

height = 8
width = height*.618

ggsave(filename = paste0(dir_output, "/plot_mismatch_aggregate_100.png"),
       plot = plot_mismatch_aggregate_100, width = width, height = height)


##############################
#Plot all convex hulls together with features
(plot_aggregate_hull_tot_loligo <- ggplot(sf_polygon) +
   geom_sf(aes(fill=Summed_SUM_LOLIGO_CATCH), color = NA)+
   scale_fill_viridis_c(direction = -1, alpha = 0.5, option = "magma")+
   geom_sf(data=single_hull, aes(color=Hull) ,fill=NA, size = 4)+
   scale_color_manual(values = alpha("grey", .75))+
   xlab("Longitude")+
   ylab("Latitude")+
   theme(axis.text.x = element_text(angle = 90))+
   guides(color = "none")+
   labs(fill = "Total catch - lbs") +
   annotation_scale(location = "br", width_hint = 0.5) +
   annotation_north_arrow(location = "br", which_north = "true",
                          height = unit(.3, "in"), width = unit(.3, "in"), 
                          pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
                          style = north_arrow_fancy_orienteering))

(plot_aggregate_hull_prop_loligo <- ggplot(sf_polygon) +
    geom_sf(aes(fill=Mean_prop_loligo), color = NA)+
    scale_fill_viridis_c(direction = -1, alpha = 0.5, option = "magma")+
    geom_sf(data=single_hull, aes(color=Hull) ,fill=NA, size = 4)+
    scale_color_manual(values = alpha("grey", .75))+
    xlab("Longitude")+
    ylab("Latitude")+
    guides(color = "none")+
    labs(fill = "Proportion \nof catch") +
    annotation_scale(location = "br", width_hint = 0.5) +
    annotation_north_arrow(location = "br", which_north = "true",
                           height = unit(.3, "in"), width = unit(.3, "in"), 
                           pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
                           style = north_arrow_fancy_orienteering))

(plot_aggregate_hull_cpue <- ggplot(sf_polygon) +
    geom_sf(aes(fill=cpue), color = NA)+
    scale_fill_viridis_c(direction = -1, alpha = 0.5, option = "magma", trans ="log")+
    geom_sf(data=single_hull, aes(color=Hull) ,fill=NA, size = 4)+
    scale_color_manual(values = alpha("grey", .75))+
    xlab("Longitude")+
    ylab("Latitude")+
    guides(color = "none")+
    labs(fill = "Catch per unit \neffort - lb per hr") +
    annotation_scale(location = "br", width_hint = 0.5) +
    annotation_north_arrow(location = "br", which_north = "true",
                           height = unit(.3, "in"), width = unit(.3, "in"), 
                           pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
                           style = north_arrow_fancy_orienteering))

(plot_aggregate_hull_season <- ggplot(sf_polygon) +
    geom_sf(aes(fill=season), color = NA)+
    scale_fill_viridis_d(alpha = 0.3, option = "magma", end = .8)+
    geom_sf(data=single_hull, aes(color=Hull) ,fill=NA, size = 4)+
    scale_color_manual(values = alpha("grey", .75))+
    xlab("Longitude")+
    ylab("Latitude")+
    guides(color = "none")+
    labs(fill = "Month") +
    annotation_scale(location = "br", width_hint = 0.5) +
    annotation_north_arrow(location = "br", which_north = "true",
                           height = unit(.3, "in"), width = unit(.3, "in"), 
                           pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
                           style = north_arrow_fancy_orienteering))

plotlist <- list(plot_aggregate_hull_tot_loligo,
                 plot_aggregate_hull_prop_loligo,
                 plot_aggregate_hull_cpue,
                 plot_aggregate_hull_season)

plot_catch_aggregate <- ggarrange(plotlist = plotlist,
                                  ncol = 2, nrow = 2, align = c("hv"))

height = 11
width = 8 #height*.618

ggsave2(filename = paste0(dir_output, "/plot_catch_aggregate.png"),
        plot = plot_catch_aggregate, width = width, height = height)

## Create table of aggregate data
#filter attributes for subset
dt_attributes <- as.data.table(sf_polygon)

dt_attributes<- dt_attributes[tripid_chr %in% sf_vtrb$tripid]

length(unique(dt_attributes$VESSEL_NAME.x))
length(unique(dt_attributes$tripid_chr))
min(dt_attributes$YEAR)
max(dt_attributes$YEAR)
unique(dt_attributes$MONTH)

length(unique(sf_vtrb$tripid))

