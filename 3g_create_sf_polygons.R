# Load packages
library(data.table)
library(sf)
library(dplyr)

# This script builds a SF from the Study Fleet GTE cleaned data
# Creates one convex hulls per imgid
# Summarizes features by trip as means, sums, or
# single values that represent entire trip

##############################
# Functions
#haul_id <- "testTestTest"
#group_as_string <- "haul_id"

# select_unique_by_group <- function(dt_attributes, group_as_string){
#   dt_attributes_count <- dt_attributes[, lapply(.SD, uniqueN),
#                                              by = group_as_string]
#   dt_attributes_max <- dt_attributes_count[, lapply(.SD, max)]
#   
#   attribute_names <- names(dt_attributes_max)
#   attribute_count <- unlist(dt_attributes_max[1])
#   
#   dt_attributes_select <- as.data.table(cbind(attribute_names, attribute_count))
#   dt_attributes_select<- dt_attributes_select[attribute_count==1, attribute_names]
#   dt_attributes_select <- append(dt_attributes_select, group_as_string)
#   #multiple imgid_chr per haul_id - so need to remove
#   #dt_attributes_select <- append(dt_attributes_select, "imgid_chr")
#   
#   dt_attributes_group <- dt_attributes[, mget(dt_attributes_select)]
#   
#   dt_attributes_group_constant<- unique(dt_attributes_group)
#   return(dt_attributes_group_constant)
# }

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
sf_gte_nad83 <- readRDS(paste0(dir_output,"/sf_gte_nad83.rds"))

dt_paths_vtrb <- readRDS(paste0(dir_output, "/dt_paths_vtrb.rds"))

##############################
# data prep
sf_filtered <- sf_gte_nad83 %>%
  filter(imgid_chr %in% dt_paths_vtrb$imgid)

length(sf_gte_nad83$trip_area)-length(sf_filtered$trip_area)
# removes 7235 gps points

# select ideas and filter to unique combinations
ids <- st_drop_geometry(sf_gte_nad83[, c("trip_area", "imgid_chr", "haul_id")])
unique_ids <- unique(ids)

# count imgids by haul_id
imgid_by_haulid <- unique_ids[ , .(count = length(unique(imgid_chr))), by = haul_id]

# subset ideas as simple, problem, and selected
# simple = 1 imgid per haulid
# problem = multiple imgids per haulid
# selected = problem ids filtered to one imgid per haulid
simple_ids <- unique_ids[haul_id %in% imgid_by_haulid[count == 1]$haul_id]
problem_ids <- unique_ids[haul_id %in% imgid_by_haulid[count >1]$haul_id]
selected_ids <- problem_ids[, .SD[imgid_chr == min(imgid_chr)], by = haul_id]

# combine simple and selected ids to create a curated dataset
ids_final <- rbind(simple_ids, selected_ids)

# filter dataset using simple and selected ids

sf <- sf_filtered %>%
  filter(imgid_chr %in% ids_final$imgid_chr)

##############################
# create polygons from study fleet data by haul_id
# then merge by imgid, trip_area, tripid
polygons <- sf %>%
  group_by(haul_id) %>%
  filter(n() > 3) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

##############################
# build feature table at group level
dt_attributes <- st_drop_geometry(sf)

# Build subtable for for features that are uniform for entire trip
dt_attributes_haulid <- select_unique_by_group(dt_attributes,
                                               "haul_id")

dt_attributes_haulid <- dt_attributes_haulid[haul_id %in% polygons$haul_id]

names_all <- names(dt_attributes)
names_haulid <- names(dt_attributes_haulid)
names_missing <- subset(names_all , !(names_all %in% names_haulid))

#merge polygons with attributes
dt_polygon_haulid <- inner_join(dt_attributes_haulid,
                                  polygons,
                                  by = "haul_id")
#coerce into sf
sf_polygon_haulid <- st_set_geometry(dt_polygon_haulid,
                                     dt_polygon_haulid$geometry)

# make sure s2 is turned on
sf_use_s2(TRUE)

#add buffer
# need to look up units - think this will confirm if in meters
st_crs(sf_polygon_haulid)$units

sf_buffered_polygon_haulid<- st_buffer(sf_polygon_haulid,
          dist = 50, # assuming in meters 50m ~ 150ft
          nQuadSegs =30, #default
          endCapStyle = "ROUND", #default
          joinStyle = "ROUND", #default
          mitreLimit = 1, #default
          singleSide = FALSE) #default

# export by haul_id
saveRDS(object = sf_buffered_polygon_haulid,
        file = paste0(dir_output, "/sf_buffered_polygon_haulid.rds"))

# merge by trip_area - make sure to include imgid - problem when not unique
sf_buffered_polygon_subtrip <-
  sf_buffered_polygon_haulid %>%
  group_by(trip_area) %>%
  summarise(geometry = st_combine(geometry))

dt_attributes <- st_drop_geometry(sf_buffered_polygon_haulid)

# Build subtable for for features that are uniform for entire trip
dt_attributes_subtrip <- select_unique_by_group(dt_attributes,
                                               "trip_area")

dt_attributes_subtrip <- dt_attributes_subtrip[trip_area %in%
                                                 sf_buffered_polygon_subtrip$trip_area]

#merge polygons with attributes
dt_polygon_subtrip <- inner_join(dt_attributes_subtrip,
                                 sf_buffered_polygon_subtrip,
                                 by = "trip_area")
#coerce into sf
sf_polygon_subtrip <- st_set_geometry(dt_polygon_subtrip,
                                      dt_polygon_subtrip$geometry)

names_subtrip <- names(dt_attributes_subtrip)
names_haulid <- names(dt_attributes_haulid)
names_missing <- subset(names_haulid , !(names_haulid %in% names_subtrip))

# merge by trip_id


