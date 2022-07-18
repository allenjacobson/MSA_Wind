# Loading packages
library(data.table)
library(sf)
library(terra)
library(dplyr)
library(ggplot2)

# this script pulls in wea shape files
# creates a single sf with all wea geometries and unique identifiers
# coerces weas into a common crs

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
sf_af <- readRDS(file = paste0(dir_output, "/sf_buffered_hulls_subtrip.rds"))
sf_vtr <- readRDS(paste0(dir_output, "/sf_vtrb_cumulative_imgid.rds"))

sf_gom_wea <- read_sf(paste0(dir_data, "/Gulf_of_Maine_planning_area_shapefiles/Gulf_of_Maine_planning_area_utm19_outline.shp"))
sf_planning_wea <- read_sf(paste0(dir_data, "/BOEM-Renewable-Energy-Shapefiles_6/BOEMWindPlanningAreaOutlines_6_1_2022.shp"))
sf_leased_wea <- read_sf(paste0(dir_data, "/BOEM-Renewable-Energy-Shapefiles_6/BOEMWindLeaseOutlines_6_1_2022.shp"))

##############################
# Prep data

# confirm that the crs are the same for vtr and af
st_crs(sf_vtr) == st_crs(sf_af)
# transform wea to match crs for sf_af
sf_leased_wea <- st_transform(sf_leased_wea, st_crs(sf_af))
sf_planning_wea <- st_transform(sf_planning_wea, st_crs(sf_af))
sf_gom_wea <- st_transform(sf_gom_wea, st_crs(sf_af))

these_planning_areas <- c("New York Wind Energy Area","Central Atlantic Call Area")

sf_planning_wea <- sf_planning_wea %>%
  filter(CATEGORY1 %in% these_planning_areas) %>%
  mutate(id = paste0("Planning_",PROTRACTIO, "_", ADDITIONAL))

sf_gom_wea <- sf_gom_wea %>%
  mutate(id = paste0("Preplanning_", PROT_NUMBE, "_GulfOfMaine"))

sf_leased_wea <- sf_leased_wea %>%
  mutate(id = paste0("Leased_", LEASE_NUMB, "_", COMPANY))


sf_leased_wea_select <- sf_leased_wea %>% 
  mutate(type = "Leased Area", label = "") %>%
  select(id, type, label)

sf_planning_wea_select <- sf_planning_wea %>%
  mutate(type = "Planned Area", label = CATEGORY1) %>%
  select(id, type, label)
                                                                    
sf_gom_wea_select <- sf_gom_wea %>%
  mutate(type = "Planned Area", label = "Gulf of Maine Planning Area") %>%
  select(id, type, label) 

sf_all_wea <- rbind(sf_leased_wea_select,sf_planning_wea_select, sf_gom_wea_select)

sf_all_wea <- sf_all_wea %>%
  mutate(area_m2 = st_area(sf_all_wea)) %>%
  mutate(area_mi2 = units::set_units(x = area_m2, value = mi2))

sf_all_wea <-mutate(sf_all_wea, type = as.factor(type))

saveRDS(object = sf_all_wea,
        file = paste0(dir_output, "/sf_all_wea.rds"))

#unique_trips <- unique(dt_af_revenue$imgid)
unique_wea <- unique(sf_all_wea$id)
#unique_confidence <- unique(dt_vtr_revenue$confidence)

length(sf_leased_wea_select$id)
length(sf_planning_wea_select$id)
length(sf_gom_wea_select$id)
length(sf_all_wea$id)
length(unique_wea)

median(sf_all_wea$area_mi2)
min(sf_all_wea$area_mi2)
max(sf_all_wea$area_mi2)
