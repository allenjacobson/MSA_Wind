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
# create Convex Hull from study fleet data by imgid
#group and summarise by species, and draw hulls
hulls <- sf_gte_nad83 %>%
  group_by(trip_area) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_convex_hull()

##############################
# build feature table at trip level
dt_attributes <- st_drop_geometry(sf_gte_nad83)

# Build subtable for for features that are uniform for entire trip
# removed gear_code_vtr and gear_code_obs because one trip has multiple
dt_attributes_imgid <- dt_attributes[, .(permit, area, sail_date, TOT_CATCH, TOT_LOLIGO_CATCH,
                                          source, year, prop_loligo, tripid_chr, trip_area, imgid_chr)]

# Used these two lines to find features with multiple values by trip
# all features were unique for imgidid
# first count number of unique values by imgiid
#dt_attributes_imgid_count <- dt_attributes_imgid[, lapply(.SD, uniqueN), 
#                                                  by = imgid_chr]
# then take the maximum value, if 1, then constant across imgid
#dt_attributes_imgid_max <- dt_attributes_imgid_count[, lapply(.SD, max)]

dt_attributes_imgid_constant<- unique(dt_attributes_imgid)

#join all features to sf - summarized by tripid
dt_hulls_attributes <- inner_join(dt_attributes_imgid_constant,
                                  hulls,
                                  by = "trip_area")
#coerce dt_hulls_attributes into sf
sf_hulls_attributes <- st_set_geometry(dt_hulls_attributes,
                                      dt_hulls_attributes$geometry)

saveRDS(object = sf_hulls_attributes,
        file = paste0(dir_output, "/sf_hulls_attributes_imgid.rds"))
