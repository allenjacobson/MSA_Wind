# Loading packages
library(data.table)
library(sf)
library(terra)
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
sf_gte <- readRDS(file = paste0(dir_output, "/sf_gte_nad83_singles.rds"))
dt_wea_revenue <- readRDS(file= paste0(dir_output, "/dt_wea_revenue.rds"))
sf_lease_areas_merged <- read_sf(paste0(dir_data, "/LeaseAreas_Merged_081621/LeaseAreas_Merged_081621.shp"))
dt_lease_areas_merged <- setDT(st_drop_geometry(sf_lease_areas_merged))
dt_vtr_revenue <- readRDS(paste0(dir_output, "/dt_paths_vtrb_revenue.rds"))
dt_af_revenue <-readRDS(file= paste0(dir_output, "/dt_paths_af_revenue_rasters.rds"))
sf_vtr <- readRDS(paste0(dir_output, "/sf_vtrb_cumulative_imgid.rds"))
sf_af <- readRDS(file = paste0(dir_output, "/sf_buffered_hulls_subtrip.rds"))

##############################
# Prep data
sf_lease_areas_merged <- st_transform(sf_lease_areas_merged, st_crs(sf_af))

dt_wea_revenue[, overlaps_wea := ifelse(revenue_vtr == 0, "af",
                                        ifelse(revenue_af == 0, "vtr", "both"))]

dt_wea_revenue[, .N, by = .(wea_lease_numb, overlaps_wea) ]

dt_wea_revenue[, .N, by = .(wea_lease_numb) ]

dt_wea_revenue_af <- dt_wea_revenue[revenue_af > 0 & confidence == "top4",
                                   .(total_revenue_af = sum(revenue_af), n_af = length(unique(imgid))),
                                   by = wea_lease_numb]

dt_wea_revenue_vtr <- dt_wea_revenue[revenue_vtr > 0  & confidence == "top4",
                                   .(total_revenue_vtr = sum(revenue_vtr), n_vtr = length(unique(imgid))),
                                   by = wea_lease_numb]

setkey(dt_wea_revenue_af, wea_lease_numb)
setkey(dt_wea_revenue_vtr, wea_lease_numb)

# inner join
dt_wea_revenue_summary <- dt_wea_revenue_af[dt_wea_revenue_vtr, nomatch=0]

dt_wea_revenue_summary[, sum(n_af)]
dt_wea_revenue_summary[, sum(n_vtr)]
##############################
# Plot rasters by wind energy area
#unique_wea <- dt_wea_revenue_summary[n_af > 2, wea_lease_numb]
# remove lease with 1 af intersection and 0 vtr intersections
unique_wea <- dt_wea_revenue_summary[, wea_lease_numb]
unique_confidence <- unique(dt_vtr_revenue$confidence)

this_wea <- unique_wea[[4]]
this_confidence <- unique_confidence[[4]]

sf_use_s2(FALSE) 
# added to remove this error
# Error in (function (cond)  : error in evaluating the argument 'x' in selecting a method for function 'vect': Evaluation error: Found 1 feature with invalid spherical geometry.[1] Loop 1 is not valid: Edge 0 has duplicate vertex with edge 5.

dt_wea_revenue_overlap <- data.table()

for(this_wea in unique_wea){
  for(this_confidence in unique_confidence){
    these_trips_vtr <- dt_wea_revenue[wea_lease_numb == this_wea & revenue_vtr > 0 & confidence == this_confidence]
    these_trips_af <- dt_wea_revenue[wea_lease_numb == this_wea & revenue_af > 0 & confidence == this_confidence]
    # select wea sf and make spat vector
    this_sf_wea <-sf_lease_areas_merged %>% filter(Lease_Numb == this_wea) %>% select(State)
    # count trips by footprint type
    n_vtr <-length(these_trips_vtr$imgid)
    n_af <- length(these_trips_af$imgid)
    # build mosaics if lenghts are non-zero for vtr and af trip lists
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
      labs(title = paste0("Vessel Trip Report footprints (",this_confidence,")\noverlapping with revenue within ", this_wea),
           subtitle = paste0("WEA intersects ", length(unique(these_trips_vtr$imgid)),
                             " subtrips and overlaps $",
                             round(this_revenue_vtr$total_revenue, digits = 0),
                             " revenue"),
           fill = "Revenue",
           caption = paste0("Revenue is in US dollars per 0.25 km2 and adjusted to 2019 annual GDP \nLeased to ",
                            dt_lease_areas_merged[Lease_Numb == this_wea, Company],
                            "\nLease info:", dt_lease_areas_merged[Lease_Numb == this_wea, INFO]))+
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
      labs(title = paste0("Vessel Trip Report footprints (",this_confidence,")\noverlapping with revenue within ", this_wea),
           subtitle = paste0("WEA intersects ", length(unique(these_trips_vtr$imgid)),
                             " subtrips and overlaps $",
                             round(this_revenue_vtr$total_revenue, digits = 0),
                             " revenue"),
           fill = "Revenue",
           caption = paste0("Revenue is in US dollars per 0.25 km2 and adjusted to 2019 annual GDP \nLeased to ",
                            dt_lease_areas_merged[Lease_Numb == this_wea, Company],
                            "\nLease info:", dt_lease_areas_merged[Lease_Numb == this_wea, INFO]))+
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
             subtitle = paste0("WEA intersects ", length(unique(these_trips_af$imgid)),
                               " subtrips and overlaps $",
                               round(this_revenue_af$total_revenue, digits = 0),
                               " revenue"),
             fill = "Revenue",
             caption = paste0("Revenue is in US dollars per 0.25 km2 and adjusted to 2019 annual GDP \nLeased to ",
                              dt_lease_areas_merged[Lease_Numb == this_wea, Company],
                              "\nLease info:", dt_lease_areas_merged[Lease_Numb == this_wea, INFO]))+
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
        labs(title = paste0("Active Fishing footprints \noverlapping with revenue within ", this_wea),
             subtitle = paste0("WEA intersects ", length(unique(these_trips_af$imgid)),
                               " subtrips and overlaps $",
                               round(this_revenue_af$total_revenue, digits = 0),
                               " revenue"),
             fill = "Revenue",
             caption = paste0("Revenue is in US dollars per 0.25 km2 and adjusted to 2019 annual GDP \nLeased to ",
                              dt_lease_areas_merged[Lease_Numb == this_wea, Company],
                              "\nLease info:", dt_lease_areas_merged[Lease_Numb == this_wea, INFO]))+
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
                             wea_lease_numb = this_wea,
                             revenue_vtr_overlap = round(this_revenue_vtr$total_revenue, digits = 0),
                             revenue_af_overlap =  round(this_revenue_af$total_revenue, digits = 0),
                             n_vtr_subtrips = length(unique(these_trips_vtr$imgid)),
                             n_af_subtrips = length(unique(these_trips_af$imgid)),
                             revenue_subtrip = dt_wea_revenue[
                               wea_lease_numb == this_wea & confidence == this_confidence,
                               revenue_subtrip])
    
    dt_wea_revenue_overlap <- rbindlist(list(dt_wea_revenue_overlap, these_data), fill=TRUE)
  }
}

##############################
# Test with SF

sf_af <- readRDS(file = paste0(dir_output, "/sf_buffered_hulls_subtrip.rds"))
sf_vtr <- readRDS(paste0(dir_output, "/sf_vtrb_cumulative_imgid.rds"))

sf_vtrb$percentile <- factor(sf_vtrb$percentile,
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


