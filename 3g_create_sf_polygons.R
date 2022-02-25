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
#group_as_string <- "haul_id"
sf_by_group <- function(sf_attributes, group_as_string, polygons){
  dt_attributes <- st_drop_geometry(sf_attributes)
  dt_attributes_count <- dt_attributes[, lapply(.SD, uniqueN), by = group_as_string]
  dt_attributes_max <- dt_attributes_count[, lapply(.SD, max)]
  attribute_names <- names(dt_attributes_max)
  attribute_count <- unlist(dt_attributes_max[1])
  
  dt_attributes_select <- as.data.table(cbind(attribute_names, attribute_count))
  dt_attributes_select<- dt_attributes_select[attribute_count==1, attribute_names]
  dt_attributes_select <- append(dt_attributes_select, group_as_string)
  
  dt_attributes_group <- dt_attributes[, mget(dt_attributes_select)]
  
  dt_attributes_constant<- unique(dt_attributes_group)
  
  group <- which( colnames(dt_attributes_constant)==group_as_string )
  
  dt_attributes_group_filtered <- dt_attributes_constant[dt_attributes_constant[[group]] %in%
                                                        get(group_as_string, polygons)]
  
  names_all <- names(dt_attributes)
  names_constant <- names(dt_attributes_constant)
  names_missing <- subset(names_all , !(names_all %in% names_constant))
  
  #merge polygons with attributes
  dt_polygon_group <- inner_join(dt_attributes_constant,
                                 polygons,
                                 by = group_as_string)
  #coerce into sf
  sf_polygon_group <- st_set_geometry(dt_polygon_group,
                                      dt_polygon_group$geometry)
  
  return(sf_polygon_group)
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

remove(list=setdiff(ls(), c("sf", "sf_by_group", "repository", "path_base",
                            "dir_output", "dir_data")))

##############################
# create polygons from study fleet data by haul_id
# then merge by imgid, trip_area, tripid
polygons <- sf %>%
  group_by(haul_id) %>%
  filter(n() > 3) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

##############################
# build feature table by haul_id
sf_polygon_haulid <- sf_by_group(sf_attributes = sf,
            group_as_string = "haul_id",
            polygons = polygons)

# add buffer
# confirm units
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

##############################
# build feature table by subtrip
sf_buffered_polygon_subtrip <-
  sf_buffered_polygon_haulid %>%
  group_by(trip_area) %>%
  summarise(geometry = st_combine(geometry))

sf_buffered_polygon_subtrip <- sf_by_group(sf_attributes = sf_buffered_polygon_haulid,
                                           group_as_string = "trip_area",
                                           polygons = sf_buffered_polygon_subtrip)

# export by haul_id
saveRDS(object = sf_buffered_polygon_subtrip,
        file = paste0(dir_output, "/sf_buffered_polygon_subtrip.rds"))

##############################
# build feature table by trip
sf_buffered_polygon_trip <-
  sf_buffered_polygon_subtrip %>%
  group_by(tripid_chr) %>%
  summarise(geometry = st_combine(geometry))

sf_buffered_polygon_trip <- sf_by_group(sf_attributes = sf_buffered_polygon_subtrip,
                                        group_as_string = "tripid_chr",
                                        polygons = sf_buffered_polygon_trip)

# export by haul_id
saveRDS(object = sf_buffered_polygon_trip,
        file = paste0(dir_output, "/sf_buffered_polygon_trip.rds"))
