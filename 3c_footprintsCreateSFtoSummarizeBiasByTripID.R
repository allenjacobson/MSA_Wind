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
# apply crs from sfBuffers
tmp <- st_sfc()
class(tmp)[1] <- "sfc_GEOMETRY" # for geometry collection
# set names/structure to match sf is loop below
sfBias_ByPercentileAndTripID <- st_sf(trip_id_chr=character(0),
                                      area = numeric(0),
                                      type=character(0),
                                      percentile=character(0),
                                      trip_ID=character(0),
                                      geometry=tmp)

thisCRS <- st_crs(sfBuffersByPercentileAndTripID) #extract CRS from exiting SF

st_crs(sfBias_ByPercentileAndTripID) <- thisCRS #set CRS to match existing SF

#uniqueTrips <- unique(sfHullsByTripID$trip_id_chr) # not all ids in SFCH are in VTRB set
uniqueTrips <- unique(sfBuffersByPercentileAndTripID$trip_ID)

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
    mutate(area = st_area(.) %>% as.numeric()) %>%
    cbind(type = "intersection")
  
  # calculate false positives and negatives: geometry and area
  sfFalsePositive <- st_difference(thisVTRB, thisSFCH) # creates new SF with intersection as geometry
  if(length(sfFalsePositive$trip_ID) > 0 ){
    sfFalsePositive <- sfFalsePositive %>%  # creates new sf
      mutate(area = st_area(.) %>% as.numeric()) %>%
      cbind(type = "falsePositive")
  }
  
  sfFalseNegative <- st_difference(thisSFCH, thisVTRB) # creates new SF with intersection as geometry
  if(length(sfFalseNegative$trip_ID) > 0 ){
    sfFalseNegative <- sfFalseNegative %>%  # creates new sf
      mutate(area = st_area(.) %>% as.numeric()) %>%
      cbind(type = "falseNegative")
  }
  
  sfSFCH <- thisSFCH %>%
    mutate(area = st_area(.) %>% as.numeric()) %>%
    cbind(type = "sfch", percentile = "", trip_ID = thisSFCH$trip_id_chr) %>%
    st_cast()
  
  sfVTRB <- thisVTRB %>%
    mutate(area = st_area(.) %>% as.numeric()) %>%
    cbind(type = "vtrb", trip_id_chr = thisVTRB$trip_ID)
  
  
  if(length(sfFalsePositive$trip_ID) == 0 ){
    sfBias_ByPercentileAndTripID <- rbind(sfBias_ByPercentileAndTripID, sfVTRB, sfSFCH,
                                          sfIntersection, sfFalseNegative)
  } else if (length(sfFalseNegative$trip_ID) == 0 ){
    sfBias_ByPercentileAndTripID <- rbind(sfBias_ByPercentileAndTripID, sfVTRB, sfSFCH,
                                          sfIntersection, sfFalsePositive)
  } else {
    sfBias_ByPercentileAndTripID <- rbind(sfBias_ByPercentileAndTripID, sfVTRB, sfSFCH,
                                          sfIntersection, sfFalsePositive, sfFalseNegative) 
  }
  }

saveRDS(object = sfBias_ByPercentileAndTripID,
        file= paste0(outputDir, "/sfBias_ByPercentileAndTripID.rds"))
