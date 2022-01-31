# Loading packages
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

repositoryName <- "MSA_Wind_FootprintBias"
basePath <- "C:/Users/lianne.allen-jacobso/Documents/"
pwdCheck <- paste0(basePath, "Repositories/",repositoryName)
pwd == pwdCheck

outputDir <- paste0(basePath, "Output/", repositoryName)
dataDir <- paste0(basePath, "Data/", repositoryName)

##############################
# Pull in data
sfHasGTEinNAD83 <- readRDS(paste0(outputDir,"/sfHasGTEinNAD83.rds"))
allPoints <- sfHasGTEinNAD83

##Need to find where these are made

# dtConvexHull <- readRDS("output/dtConvexHull.rds")# where is this made? below?
# vtrbCumulative100 <- rast("output/rasterCumulative100.tif")
# polygonFrom100VTRB <- rastToPolygon(vtrbCumulative100)
# theMap <- polygonFrom100VTRB

##############################
# create Convex Hull from study fleet data by trip
#group and summarise by species, and draw hulls
hulls <- allPoints %>%
  group_by(trip_id_chr, VESSEL_NAME) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_convex_hull()

##############################
# build feature table at trip level
dtFeatures <- st_drop_geometry(allPoints)

# Build subtable for for features that are uniform for entire trip
# removed gear_code_vtr and gear_code_obs because one trip has multiple
dtFeaturesByTripID <- dtFeatures[, .(permit, sail_date, TOT_CATCH, TOT_LOLIGO_CATCH,
                                     source, YEAR, MONTH, YDAY,DATE, PROP_LOLIGO,
                                     VESSEL_NAME, loligoPerTotalCatch, trip_id_chr)]

# Used these two lines to find features with multiple values by trip
# removed these two features, and then re-ran unique - now features are unique by trip
#dtFeaturesByTripIDCount <- dtFeaturesByTripID[, lapply(.SD, uniqueN),
#                                            by = trip_id_chr]
#dtFeaturesByTripIDMax <- dtFeaturesByTripIDCount[, lapply(.SD, max)]

dtFeaturesByTripIDUnique<- unique(dtFeaturesByTripID)

# Build subtable for for features that should be summed for entire trip
featuresSelected <- c("effort_dur", "LOLIGO_KEPTWT", "LOLIGO_DISCARDTWT", "SUM_LOLIGO_CATCH")

dtFeaturesByTripIDSummed <- dtFeatures[, lapply(.SD, sum), by = .(trip_id_chr),
                                       .SDcols = featuresSelected]

newNames <- c("Summed_effort_dur", "Summed_LOLIGO_KEPTWT",
              "Summed_LOLIGO_DISCARDTWT", "Summed_SUM_LOLIGO_CATCH")

# Add CPUE - catch per effort duration?

setnames(dtFeaturesByTripIDSummed, featuresSelected, newNames)

# Build subtable for for features that should be averaged for entire trip
featuresSelected <- c("BOT_DEPTH_M", "GEAR_DEPTH", "loligoPerTotalCatch")

dtFeaturesByTripIDMean <- dtFeatures[, lapply(.SD, mean), by = .(trip_id_chr),
                                     .SDcols = featuresSelected]

newNames <- c("Mean_BOT_DEPTH_M", "Mean_GEAR_DEPTH", "Mean_loligoPerTotalCatch")

setnames(dtFeaturesByTripIDMean, featuresSelected, newNames)


#join features - unique, sum, average, min, max
dtFeaturesByTrip <- dtFeaturesByTripIDSummed[dtFeaturesByTripIDMean,
                                             on = .(trip_id_chr = trip_id_chr)]

dtFeaturesByTrip <-dtFeaturesByTripIDUnique[dtFeaturesByTrip,
                                            on = .(trip_id_chr = trip_id_chr)]

#join all features to sf
dtHullsAndFeatures <- inner_join(dtFeaturesByTrip,hulls, by = "trip_id_chr")
sfHullsByTripID <- st_set_geometry(dtHullsAndFeatures,
                                      dtHullsAndFeatures$geometry)

saveRDS(object = sfHullsByTripID,
        file = paste0(outputDir, "/sfHullsByTripID.rds"))
