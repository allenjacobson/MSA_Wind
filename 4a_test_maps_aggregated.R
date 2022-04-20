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
# paths_to_mosaic <- function(list_rasters){
#   length <- length(list_rasters)
#   if(length == 1){
#     mosaic <- rast(list_rasters)
#     return(mosaic)
#   } else if (length > 1){
#     rasters <- lapply(X = list_rasters, FUN = rast)
#     mosaic <- do.call(mosaic, args = c(rasters, fun = "sum"))
#     return(mosaic)
#   } else{ 
#     this_warning <- paste("Error")
#     warning(this_warning)
#   }
# }

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
#sf_polygon <- readRDS(file = paste0(dir_output, "/sf_buffered_polygon_subtrip.rds"))
#sf_vtrb <- readRDS(paste0(dir_output, "/sf_vtrb_cumulative_imgid.rds"))
#sf_bias <- readRDS(file= paste0(dir_output, "/sf_bias_imgid.rds"))

#dt_wea_revenue <- readRDS(file= paste0(dir_output, "/dt_wea_revenue.rds"))
dt_wea_revenue <- readRDS(file= paste0(dir_output, "/dt_wea_revenue.rds"))
sf_lease_areas_merged <- read_sf(paste0(dir_data, "/LeaseAreas_Merged_081621/LeaseAreas_Merged_081621.shp"))
dt_lease_areas_merged <- setDT(st_drop_geometry(sf_lease_areas_merged))

#sf_af <- readRDS(file = paste0(dir_output, "/sf_buffered_hulls_subtrip.rds"))
#sf_vtr <- readRDS(paste0(dir_output, "/sf_vtrb_cumulative_imgid.rds"))

dt_vtr_revenue <- readRDS(paste0(dir_output, "/dt_paths_vtrb_revenue.rds"))
dt_af_revenue <-readRDS(file= paste0(dir_output, "/dt_paths_af_revenue_rasters.rds"))

#dt_vtr_in_af <- readRDS(file= paste0(dir_output, "/dt_paths_vtrb_revenue_cropped.rds"))

sf_vtr <- readRDS(paste0(dir_output, "/sf_vtrb_cumulative_imgid.rds"))

##############################
# Prep data
dt_wea_revenue[, overlaps_wea := ifelse(revenue_vtr == 0, "af",
                                        ifelse(revenue_af == 0, "vtr", "both"))]

dt_wea_revenue[, .N, by = .(wea_lease_numb, overlaps_wea) ]

dt_wea_revenue[, .N, by = .(wea_lease_numb) ]

##############################
# Merge rasters by confidence
# dt_paths_top4 <- dt_vtr_in_af[confidence == "top4" & paths != "no intersection", paths]
# list_rasters_top4 <- lapply(X = dt_paths_top4, FUN = rast)
# mosaic_top4 <- do.call(terra::mosaic, args = c(list_rasters_top4, fun = "sum"))
# plot(mosaic_top4)
# 
# dt_paths_top3 <- dt_vtr_in_af[confidence == "top3" & paths != "no intersection", paths]
# list_rasters_top3 <- lapply(X = dt_paths_top3, FUN = rast)
# mosaic_top3 <- do.call(mosaic, args = c(list_rasters_top3, fun = "sum"))
# plot(mosaic_top3)
# 
# dt_paths_top2 <- dt_vtr_in_af[confidence == "top2" & paths != "no intersection", paths]
# list_rasters_top2 <- lapply(X = dt_paths_top2, FUN = rast)
# mosaic_top2 <- do.call(mosaic, args = c(list_rasters_top2, fun = "sum"))
# plot(mosaic_top2)
# 
# dt_paths_top1 <- dt_vtr_in_af[confidence == "top1" & paths != "no intersection", paths]
# list_rasters_top1 <- lapply(X = dt_paths_top1, FUN = rast)
# mosaic_top1 <- do.call(mosaic, args = c(list_rasters_top1, fun = "sum"))
# plot(mosaic_top1)
# 
# 
# TEST <- sf_polygon %>%
#   rename(imgid = imgid_chr) %>%
#   inner_join(dt_vtr_in_af[confidence == "top4", .(imgid, revenue_active_fishing, revenue_subtrip, log_ratio, diff)])
# 
# test2 <- TEST %>%
#   select(revenue_subtrip) %>%
#   plot()

##############################
# Plot rasters by winde energy area

unique_wea <- unique(dt_wea_revenue$wea_lease_numb)
unique_confidence <- unique(dt_vtr_revenue$confidence)

this_wea <- unique_wea[[5]]
this_confidence <- unique_confidence[[4]]

sf_use_s2(FALSE) 
# added to remove this error
# Error in (function (cond)  : error in evaluating the argument 'x' in selecting a method for function 'vect': Evaluation error: Found 1 feature with invalid spherical geometry.[1] Loop 1 is not valid: Edge 0 has duplicate vertex with edge 5.

test <- dt_wea_revenue[total_revenue_af := sum()]
length(unique(test$wea_lease_numb))

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
    if(n_vtr < 2 & n_af < 2){
      warning(paste0("VTR (", n_vtr,") and AF (",n_af,") footprints had zero or too few intersections this WEA (", this_wea, ") at this confidence level (",this_confidence,")"))
      next
    }
    else if(n_vtr > 1){
      these_vtrs <- lapply(X = dt_vtr_revenue[
        imgid %in% these_trips_vtr$imgid & confidence == this_confidence,paths],
        FUN = rast)
      this_vtr_mosaic <- do.call(terra::mosaic, args = c(these_vtrs, fun = "sum"))
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
        labs(title = paste0("Vessel Trip Report footprints \noverlapping with revenue within ", this_wea),
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
        xlim(xmin(this_wea_extent)*.999, xmax(this_wea_extent)*1.001)+
        ylab("Latitude")+
        ylim(ymin(this_wea_extent)*.999, ymax(this_wea_extent)*1.001)+
        labs(title = paste0("Vessel Trip Report footprints \noverlapping with revenue within ", this_wea),
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
      
    }
    else if(n_af > 1){
      these_afs <- lapply(X = dt_af_revenue[imgid %in% these_trips_af$imgid, paths], FUN = rast)
      this_af_mosaic <- do.call(terra::mosaic, args = c(these_afs, fun = "sum"))
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
      if(n_vtr > 1){
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
          xlim(xmin(this_wea_extent)*.999, xmax(this_wea_extent)*1.001)+
          ylab("Latitude")+
          ylim(ymin(this_wea_extent)*.999, ymax(this_wea_extent)*1.001)+
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
      } else {
        this_plot_af <- ggplot() +
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
        # Cropped plot
        this_plot_af_cropped <- ggplot() +
          geom_tile(data = this_af_mosaic_df, aes(x = x, y = y, fill = value)) + 
          scale_fill_viridis_c(direction = -1, limits = c(0, 600))+
          geom_sf(data=this_sf_wea, color = "red", fill = NA)+
          xlab("Longitude")+
          xlim(xmin(this_wea_extent)*.999, xmax(this_wea_extent)*1.001)+
          ylab("Latitude")+
          ylim(ymin(this_wea_extent)*.999, ymax(this_wea_extent)*1.001)+
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
      }
    }
    # Create plot list by conditions - full extent
    this_wea_no_space <- gsub(" ", "_", this_wea, fixed = TRUE)
    
    plotlist <- ifelse(n_af > 0 & n_vtr > 0, list(this_plot_vtr, this_plot_af),
                       ifelse(n_af > 0 & n_vtr == 0, list(this_plot_af),
                              ifelse(n_af == 0 & n_vtr > 0, list(this_plot_vtr),
                                     ifelse(n_af == 0 & n_vtr > 0,
                                            warning(paste0("no trips overlap ", this_wea)),
                                            warning(paste0("unknown error for ", this_wea))))))
    # Create multi panel plots
    this_path <- paste0(dir_output, "/plots_by_wea/",this_wea_no_space,"_",this_confidence,"_full_extent.png")
    if(length(plotlist) ==2 ){
      height = 8 #width*.618
      width = 11 
      these_plots <- ggarrange(plotlist = plotlist, ncol = 2, nrow = 1, align = c("hv"))
      ggsave(filename = this_path, plot = these_plots, width = width, height = height)  
    } else{
      height = 8 #width*.618
      width = 5.5 
      these_plots <- ggarrange(plotlist = plotlist, ncol = 1, nrow = 1, align = c("hv"))
      ggsave(filename = this_path, plot = these_plots, width = width, height = height)  
    }
    # Create plot list by conditions - cropped plots
    plotlist_cropped <- ifelse(n_af > 0 & n_vtr > 0, list(this_plot_vtr_cropped, this_plot_af_cropped),
                       ifelse(n_af > 0 & n_vtr == 0, list(this_plot_af_cropped),
                              ifelse(n_af == 0 & n_vtr > 0, list(this_plot_vtr_cropped),
                                     ifelse(n_af == 0 & n_vtr > 0,
                                            warning(paste0("no trips overlap cropped ", this_wea)),
                                            warning(paste0("unknown error for cropped", this_wea))))))
    # Create multi panel plots
    this_path_cropped <- paste0(dir_output, "/plots_by_wea/",this_wea_no_space,"_",this_confidence,"_cropped.png")
    if(length(plotlist_cropped) ==2 ){
      height = 8 #width*.618
      width = 11 
      this_path <- paste0(dir_output, "/plots_by_wea/",this_wea_no_space,"_full_extent.png")
      these_plots <- ggarrange(plotlist = plotlist_cropped,
                               ncol = 2, nrow = 1, align = c("hv"))
      ggsave(filename = this_path_cropped,
             plot = these_plots, width = width, height = height)  
    } else{
      height = 8 #width*.618
      width = 5.5 
      this_path <- paste0(dir_output, "/plots_by_wea/",this_wea_no_space,"_full_extent.png")
      these_plots <- ggarrange(plotlist = plotlist_cropped,
                               ncol = 1, nrow = 1, align = c("hv"))
      ggsave(filename = this_path_cropped,
             plot = these_plots, width = width, height = height)  
    }
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
   geom_sf(data=this_sf_wea, acolor = "black", fill = NA)+
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


