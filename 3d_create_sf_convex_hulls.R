# Load packages
library(data.table)
library(sf)
library(dplyr)

# This script builds a SF from the Study Fleet GTE cleaned data
# Creates one convex hulls per haul
# Then merges hull by subtrip and trip
# Identifies features that are unique by grouping variable and adds to sf features by trip as means, sums, or
# single values that represent entire trip

##############################
# Functions
sf_by_group <- function(sf_attributes, group_as_string, shapes){
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
                                                           get(group_as_string, shapes)]
  
  names_all <- names(dt_attributes)
  names_constant <- names(dt_attributes_constant)
  names_missing <- subset(names_all , !(names_all %in% names_constant))
  
  #merge shapes with attributes
  dt_shapes_group <- inner_join(dt_attributes_constant,
                                 shapes,
                                 by = group_as_string)
  #coerce into sf
  sf_shapes_group <- st_set_geometry(dt_shapes_group,
                                      dt_shapes_group$geometry)
  
  return(sf_shapes_group)
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
sf <- sf_gte_nad83 %>%
  filter(imgid_chr %in% dt_paths_vtrb$imgid)

##############################
# create Convex Hull from study fleet data by trip
#group and summarise by species, and draw hulls
hulls <- sf_gte_nad83 %>%
  group_by(haul_id) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_convex_hull()


############################## Need to modify
# build feature table by haul_id
sf_hulls_haulid <- sf_by_group(sf_attributes = sf,
                                 group_as_string = "haul_id",
                                 shapes = hulls)

# add buffer
# confirm units
st_crs(sf_hulls_haulid)$units

sf_buffered_hulls_haulid<- st_buffer(sf_hulls_haulid,
                                       dist = 50, # in meters 50m ~ 150ft
                                       nQuadSegs =30, #default
                                       endCapStyle = "ROUND", #default
                                       joinStyle = "ROUND", #default
                                       mitreLimit = 1, #default
                                       singleSide = FALSE) #default

# export by haul_id
saveRDS(object = sf_buffered_hulls_haulid,
        file = paste0(dir_output, "/sf_buffered_hulls_haulid.rds"))

##############################
# build feature table by subtrip
sf_buffered_hulls_subtrip <-
  sf_buffered_hulls_haulid %>%
  group_by(imgid_chr) %>%
  summarise(geometry = st_combine(geometry))

sf_buffered_hulls_subtrip <- sf_by_group(sf_attributes = sf_buffered_hulls_haulid,
                                           group_as_string = "imgid_chr",
                                           shapes = sf_buffered_hulls_subtrip)

# export by haul_id
saveRDS(object = sf_buffered_hulls_subtrip,
        file = paste0(dir_output, "/sf_buffered_hulls_subtrip.rds"))

##############################
# build feature table by trip
sf_buffered_hulls_trip <-
  sf_buffered_hulls_subtrip %>%
  group_by(tripid_chr) %>%
  summarise(geometry = st_combine(geometry))

sf_buffered_hulls_trip <- sf_by_group(sf_attributes = sf_buffered_hulls_subtrip,
                                        group_as_string = "tripid_chr",
                                      shapes = sf_buffered_hulls_trip)

# export by haul_id
saveRDS(object = sf_buffered_hulls_trip,
        file = paste0(dir_output, "/sf_buffered_hulls_trip.rds"))

