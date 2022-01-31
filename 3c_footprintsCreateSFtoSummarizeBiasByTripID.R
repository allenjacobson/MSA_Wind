
# Loading packages
library(data.table)
library(sf)
library(dplyr)

# Creates SF to summarize bias
# summarizes bias by generating geometries for overlap, and mismatch
# adds features for area of sfch, vtrb, intersection, false positive and false negative

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
sfBuffersByPercentileAndTripID <- 
  readRDS(paste0(outputDir, "/sfBuffersByPercentileAndTripID.rds"))

sfHullsByTripID <- readRDS( file = paste0(outputDir, "/sfHullsByTripID.rds"))

##############################
# create empty data.table, will be coerced into SF when sf rows are added below
sfBias_ByPercentileAndTripID <- data.table()
uniqueTrips <- as.data.table(unique(sfHullsByTripID$trip_id_chr))
#thisTrip <- uniqueTrips[[1]]

for (thisTrip in uniqueTrips) {
  # filter rows in CH that match trip_id
  # select id only (with geometry)
  thisSFCH <- sfHullsByTripID %>%
    filter(trip_id_chr == thisTrip) %>%
    select(trip_id_chr)
  
  # filter rows in VTRB that match trip_id
  # select percentile only (with geometry)
  thisVTRB <- sfBuffersByPercentileAndTripID %>%
    filter(trip_ID == thisTrip) %>%
    select(trip_ID, percentile)
  
  # calculate intersection (later repeat this for difference)
  sfIntersection <- st_intersection(thisSFCH, thisVTRB) # creates new SF with intersection as geometry
  
  # calculate intersection area
  sfIntersection <- sfIntersection %>%  # creates new sf
    mutate(area = st_area(.) %>% as.numeric()) # adds area as feature
  
  sfIntersection <- cbind(sfIntersection, type = "intersection")
  
  # calculate false positives and negatives: geometry and area
  sfFalsePositive <- st_difference(thisVTRB, thisSFCH) # creates new SF with intersection as geometry
  sfFalsePositive <- sfFalsePositive %>%  # creates new sf
    mutate(area = st_area(.) %>% as.numeric()) %>%
    cbind(type = "falsePositive")
  
  sfFalseNegative <- st_difference(thisSFCH, thisVTRB) # creates new SF with intersection as geometry
  sfFalseNegative <- sfFalseNegative %>%  # creates new sf
    mutate(area = st_area(.) %>% as.numeric()) %>%
    cbind(type = "falseNegative")
  
  sfSFCH <- thisSFCH %>%
    mutate(area = st_area(.) %>% as.numeric()) %>%
    cbind(type = "sfch", percentile = NA, trip_ID = thisSFCH$trip_id_chr)
  
  sfVTRB <- thisVTRB %>%
    mutate(area = st_area(.) %>% as.numeric()) %>%
    cbind(type = "vtrb", trip_id_chr = thisVTRB$trip_ID)
  
  sfBias_ByPercentileAndTripID <- rbind(sfSFCH, sfVTRB, sfIntersection, sfFalsePositive, sfFalseNegative)  
  
}

saveRDS(object = sfBias_ByPercentileAndTripID,
        file= paste0(outputDir, "sfBias_ByPercentileAndTripID.rds"))
