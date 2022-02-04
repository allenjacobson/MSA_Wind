# Load packages
library(data.table)
library(sf)
library(dplyr)

# This script builds a SF from the Study Fleet GTE cleaned data
# Creates one convex hulls per trip
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
# create Convex Hull from study fleet data by trip
#group and summarise by species, and draw hulls
hulls <- sf_gte_nad83 %>%
  group_by(tripid_chr, VESSEL_NAME) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_convex_hull()

##############################
# build feature table at trip level
dt_attributes <- st_drop_geometry(sf_gte_nad83)

# Build subtable for for features that are uniform for entire trip
# removed gear_code_vtr and gear_code_obs because one trip has multiple
dt_attributes_tripid <- dt_attributes[, .(permit, sail_date, TOT_CATCH, TOT_LOLIGO_CATCH,
                                     source, YEAR, MONTH, YDAY,DATE, PROP_LOLIGO,
                                     VESSEL_NAME, prop_loligo, tripid_chr)]

# Used these two lines to find features with multiple values by trip
# removed these two features, and then re-ran unique - now features are unique by trip
#dt_attributes_tripidCount <- dt_attributes_tripid[, lapply(.SD, uniqueN),
#                                            by = tripid_chr]
#dt_attributes_tripidMax <- dt_attributes_tripidCount[, lapply(.SD, max)]

dt_attributes_tripid_constant<- unique(dt_attributes_tripid)

# Build subtable for for features that should be summed for entire trip
these_attributes <- c("effort_dur", "LOLIGO_KEPTWT", "LOLIGO_DISCARDTWT", "SUM_LOLIGO_CATCH")

dt_attributes_tripid_sum <- dt_attributes[, lapply(.SD, sum), by = .(tripid_chr),
                                       .SDcols = these_attributes]

new_names <- c("Summed_effort_dur", "Summed_LOLIGO_KEPTWT",
              "Summed_LOLIGO_DISCARDTWT", "Summed_SUM_LOLIGO_CATCH")

# Add CPUE - catch per effort duration?

setnames(dt_attributes_tripid_sum, these_attributes, new_names)

# Build subtable for for features that should be averaged for entire trip
these_attributes <- c("BOT_DEPTH_M", "GEAR_DEPTH", "prop_loligo")

dt_attributes_tripid_avg <- dt_attributes[, lapply(.SD, mean), by = .(tripid_chr),
                                     .SDcols = these_attributes]

new_names <- c("Mean_BOT_DEPTH_M", "Mean_GEAR_DEPTH", "Mean_prop_loligo")

setnames(dt_attributes_tripid_avg, these_attributes, new_names)


#join features - unique, sum, average, min, max
dt_attributes_joined <- dt_attributes_tripid_sum[dt_attributes_tripid_avg,
                                             on = .(tripid_chr = tripid_chr)]

dt_attributes_joined <-dt_attributes_tripid_constant[dt_attributes_joined,
                                            on = .(tripid_chr = tripid_chr)]

#join all features to sf - summarized by tripid
dt_hulls_attributes <- inner_join(dt_attributes_joined,hulls, by = "tripid_chr")
sf_hulls_attributes <- st_set_geometry(dt_hulls_attributes,
                                      dt_hulls_attributes$geometry)

saveRDS(object = sf_hulls_attributes,
        file = paste0(dir_output, "/sf_hulls_attributes.rds"))
