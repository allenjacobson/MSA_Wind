# Loading packages
library(data.table)
library(sf)
library(terra)
library(dplyr)
library(ggplot2)

# this script pulls in wea shape files
# identifies footprints that intersect wea
# exports dt tracking intersections by footprint and percentile
# creates summary plots

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
# sf_shapes <- readRDS(file = paste0(dir_output, "/sf_buffered_hulls_subtrip.rds"))
# dt_revenue <- readRDS(paste0(dir_output, "/dt_paths_vtrb_split_matched_revenue.rds"))
# dt_paths_cropped <- readRDS(file= paste0(dir_output, "/dt_paths_vtrb_revenue_cropped.rds"))
# dt_paths <- readRDS(paste0(dir_output, "/dt_paths_vtrb_revenue.rds"))

##############################
#cropped by wind energy areas
#dt_vtr_revenue <- readRDS(file= paste0(dir_output, "/dt_paths_vtrb_revenue_cropped.rds"))
dt_vtr_revenue <- readRDS(paste0(dir_output, "/dt_paths_vtrb_revenue.rds"))
dt_af_revenue <-readRDS(file= paste0(dir_output, "/dt_paths_af_revenue_rasters.rds"))
dt_revenue <- readRDS(paste0(dir_output, "/dt_paths_vtrb_split_matched_revenue.rds"))
sf_af <- readRDS(file = paste0(dir_output, "/sf_buffered_hulls_subtrip.rds"))
sf_vtr <- readRDS(paste0(dir_output, "/sf_vtrb_cumulative_imgid.rds"))


sf_gom_wea <- read_sf(paste0(dir_data, "/Gulf_of_Maine_planning_area_shapefiles/Gulf_of_Maine_planning_area_utm19_outline.shp"))
sf_planning_wea <- read_sf(paste0(dir_data, "/BOEM-Renewable-Energy-Shapefiles_6/BOEMWindPlanningAreaOutlines_6_1_2022.shp"))
sf_leased_wea <- read_sf(paste0(dir_data, "/BOEM-Renewable-Energy-Shapefiles_6/BOEMWindLeaseOutlines_6_1_2022.shp"))

##############################
# Prep data

# select trips where the number of cells in each raster is the same
# this only removes 7 sub trips
# will need to go back later and get all to match - i think the problem is  b/c of the vtrs not overlapping with the af
dt_af_revenue <- dt_af_revenue[cell_test == TRUE]
dt_vtr_revenue <- dt_vtr_revenue[imgid %in% dt_af_revenue$imgid]

# confirm that the crs are the same for vtr and af
st_crs(sf_vtr) == st_crs(sf_af)
# transform wea to match crs for sf_af
sf_leased_wea <- st_transform(sf_leased_wea, st_crs(sf_af))
sf_planning_wea <- st_transform(sf_planning_wea, st_crs(sf_af))
sf_gom_wea <- st_transform(sf_gom_wea, st_crs(sf_af))

these_planning_areas <- c("New York Wind Energy Area","South Carolina Call Area", "Central Atlantic Call Area")

sf_planning_wea <- sf_planning_wea %>%
  filter(CATEGORY1 %in% these_planning_areas) %>%
  mutate(id = paste0("Planning_",PROTRACTIO, "_", ADDITIONAL))

sf_gom_wea <- sf_gom_wea %>%
  mutate(id = paste0("Preplanning_", PROT_NUMBE, "_GulfOfMaine"))

sf_leased_wea <- sf_leased_wea %>%
  mutate(id = paste0("Leased_", LEASE_NUMB, "_", COMPANY))

sf_leased_wea_select <- sf_leased_wea %>% select(id)
sf_planning_wea_select <- sf_planning_wea %>% select(id)
sf_gom_wea_select <- sf_gom_wea %>% select(id)

sf_all_wea <- rbind(sf_leased_wea_select,sf_planning_wea_select, sf_gom_wea_select)

sf_all_wea <- sf_all_wea %>%
  mutate(area_m2 = st_area(sf_all_wea)) %>%
  mutate(area_hectare = area_m2/10000)

saveRDS(object = sf_all_wea,
        file = paste0(dir_output, "/sf_all_wea.rds"))

unique_trips <- unique(dt_af_revenue$imgid)
unique_wea <- unique(sf_all_wea$id)
unique_confidence <- unique(dt_vtr_revenue$confidence)

length(sf_leased_wea_select$id)
length(sf_planning_wea_select$id)
length(sf_gom_wea_select$id)
length(sf_all_wea$id)
length(unique_wea)
length(unique_trips)

median(sf_all_wea$area_hectare)
min(sf_all_wea$area_hectare)
max(sf_all_wea$area_hectare)

# this_trip <- unique_trips[[1]]
# this_wea <- unique_wea[[1]]
# this_confidence <- unique_confidence[[1]]

dt_wea_revenue <- data.table()

# build a dt (dt_wea_revenue) that checks pairwise intersections
# between AFF and VTF at 4 pecentile levels

for(this_wea in unique_wea){
  # select sf for wea, and create single vector
  this_sf_wea <- sf_all_wea %>% filter(id == this_wea)
  this_vect_wea <- st_cast(st_union(this_sf_wea),"POLYGON") %>% st_union %>% vect()
  for(this_trip in unique_trips){  
    # for each trip select the aff raster
    this_rast_af <- rast(dt_af_revenue[imgid == this_trip, paths])
    # select cells that overlap in aff and wea, and sum all revenue
    this_revenue_af <- extract(x = this_rast_af, y = this_vect_wea, exact = TRUE)%>%
      rename("value" = names(this_rast_af)) %>%
      mutate(cell_revenue = value*fraction)%>%
      summarise(revenue_af = sum(cell_revenue, na.rm = TRUE))  %>%
      unlist()
    # pull the total revenue for this trip
    all_revenue <- dt_revenue[imgid == this_trip & percentile == "90th", value_gdp]
    for(this_confidence in unique_confidence){
      # for each trip and each confidence level select the vtrf raster
      this_path_vtr <-dt_vtr_revenue[imgid == this_trip & confidence == this_confidence, paths]
      this_rast_vtr <- rast(dt_vtr_revenue[imgid == this_trip & confidence == this_confidence, paths])
      # select cells that overlap in vtrf and wea, and sum all revenue
      this_revenue_vtr <- extract(x = this_rast_vtr, y = this_vect_wea, exact = TRUE) %>%
          rename("value" = names(this_rast_vtr)) %>%
          mutate(cell_revenue = value*fraction)%>%
          summarise(revenue_vtr = sum(cell_revenue, na.rm = TRUE)) %>%
          unlist()
      # add all values to a data table, and rbind with summary table
      these_data <- data.table(imgid= this_trip,
                               confidence= this_confidence,
                               wea_id = this_wea,
                               revenue_af = this_revenue_af,
                               revenue_vtr = this_revenue_vtr,
                               revenue_subtrip = all_revenue)
      dt_wea_revenue <- rbindlist(list(dt_wea_revenue, these_data), fill=TRUE)
      }
    }
  }

dt_wea_revenue[, type_intersection := ifelse(revenue_af == 0 & revenue_vtr == 0,
                                             "none",
                                             ifelse(revenue_af > 0 & revenue_vtr > 0,
                                                    "both",
                                                    ifelse(revenue_af > 0 & revenue_vtr == 0,
                                                           "af only",
                                                           ifelse(revenue_vtr > 0 & revenue_af == 0,
                                                                  "vtr only",
                                                                  "error"))))]
dt_wea_revenue[, agreement :=
                ifelse(type_intersection == "none" | type_intersection == "both",
                       "agree",
                       "disagree")]
                       
saveRDS(object = dt_wea_revenue,
        file= paste0(dir_output, "/dt_wea_revenue.rds"))

dt_wea_revenue <- readRDS(paste0(dir_output, "/dt_wea_revenue.rds"))
##############################
# make this a new script
# plots
# dt_wea_revenue$type_intersection <-
#   factor(dt_wea_revenue$type_intersection,
#          levels = c("none", "both", "vtr only", "af only"))
# 
# dt_wea_revenue$confidence <-
#   factor(dt_wea_revenue$confidence,
#          levels = c("top4", "top3", "top2", "top1"))
# 
# ###############################
# # plots w/ ggplot counting
# # top_4_af <- data.table(confidence = "top4",
# #                        type_intersection = "af only",
# #                        agreement = "disagree",
# #                        N = 0)
# # 
# # dt_wea_summary <- dt_wea_revenue[, .N, by = .(confidence, type_intersection, agreement)]
# # 
# # dt_wea_summary<- rbind(dt_wea_summary, top_4_af)
# # 
# # (plot_footprints_intersecting_wea_lin <- ggplot(dt_wea_summary, aes(x=type_intersection, y=N, fill=agreement)) +
# #   geom_bar(stat='identity', position='dodge')+
# #   scale_fill_viridis_d(begin = 0, end = .7, option = "plasma")+
# #   geom_text(aes(label = N), vjust = -0.2)+
# #   ylab("Number of subtrips")+
# #   xlab("Footprint Intersects with WEA")+
# #   facet_grid(~ confidence))
# # 
# # width = 10
# # height = width*.4
# # 
# # ggsave(filename = paste0(dir_output, "/plot_footprints_intersecting_wea_lin.png"),
# #        plot = plot_footprints_intersecting_wea_lin, width = width, height = height)
# 
# (plot_footprints_intersecting_wea_log <- ggplot(dt_wea_summary, aes(x=type_intersection, y=N, fill=agreement)) +
#     geom_bar(stat='identity', position='dodge')+
#     scale_y_log10(labels = function(x) format(x, scientific = FALSE))+
#     scale_fill_viridis_d(begin = 0, end = .7, option = "plasma")+
#     geom_text(aes(label = N), vjust = -0.2)+
#     ylab("Number of subtrips")+
#     xlab("Footprint Intersects with WEA")+
#     facet_grid(~ confidence))
# 
# width = 10
# height = width*.4
# 
# ggsave(filename = paste0(dir_output, "/plot_footprints_intersecting_wea_log.png"),
#        plot = plot_footprints_intersecting_wea_log, width = width, height = height)
# 
# dt_wea_summary_top4 <- dt_wea_summary[confidence == "top4"]
# 
# (plot_footprints_intersecting_wea_log_top4 <-
#     ggplot(dt_wea_summary_top4, aes(x=type_intersection, y=N, fill=agreement)) +
#     geom_bar(stat='identity', position='dodge')+
#     scale_y_log10(labels = function(x) format(x, scientific = FALSE))+
#     scale_fill_viridis_d(begin = 0, end = .7, option = "plasma")+
#     #geom_text(aes(label = N), vjust = 0.2)+
#     ylab("Number of subtrips")+
#     xlab("Footprint Intersects with WEA")+
#     facet_grid(~ confidence))
# 
# width = 6
# height = width*.3
# 
# ggsave(filename = paste0(dir_output, "/plot_footprints_intersecting_wea_log_top4.png"),
#        plot = plot_footprints_intersecting_wea_log_top4, width = width, height = height)
