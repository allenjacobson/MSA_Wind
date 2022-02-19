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
haul_id <- "testTestTest"
group_as_string <- "haul_id"

select_unique_by_group <- function(dt_attributes, group_as_string){
  dt_attributes_count <- dt_attributes[, lapply(.SD, uniqueN),
                                             by = group_as_string]
  dt_attributes_max <- dt_attributes_count[, lapply(.SD, max)]
  
  attribute_names <- names(dt_attributes_max)
  attribute_count <- unlist(dt_attributes_max[1])
  
  dt_attributes_select <- as.data.table(cbind(attribute_names, attribute_count))
  dt_attributes_select<- dt_attributes_select[attribute_count==1, attribute_names]
  dt_attributes_select <- append(dt_attributes_select, group_as_string)
  #multiple imgid_chr per haul_id - so need to remove
  #dt_attributes_select <- append(dt_attributes_select, "imgid_chr")
  
  dt_attributes_group <- dt_attributes[, mget(dt_attributes_select)]
  
  dt_attributes_group_constant<- unique(dt_attributes_group)
  return(dt_attributes_group_constant)
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

##############################
# create polygons from study fleet data by haul_id
# then merge by imgid, trip_area, tripid
polygons <- sf_gte_nad83 %>%
  group_by(haul_id) %>%
  filter(n() > 3) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

##############################
# build feature table at group level
dt_attributes <- st_drop_geometry(sf_gte_nad83)

# Build subtable for for features that are uniform for entire trip
dt_attributes_haulid <- select_unique_by_group(dt_attributes,
                                               "haul_id")

dt_attributes_haulid <- dt_attributes_haulid[haul_id %in% polygons$haul_id]

#merge polygons with attributes
dt_polygon_haulid <- inner_join(dt_attributes_haulid,
                                  polygons,
                                  by = "haul_id")
#coerce into sf
sf_polygon_haulid <- st_set_geometry(dt_polygon_haulid,
                                     dt_polygon_haulid$geometry)

# assign attribute types - constant/sum/..

# export by haul_id
saveRDS(object = sf_polygon_haulid,
        file = paste0(dir_output, "/sf_polygon_haulid.rds"))

# merge by trip_area - make sure to include imgid - problem when not unique

# merge by trip_id