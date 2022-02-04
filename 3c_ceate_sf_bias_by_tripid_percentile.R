# Load packages
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

repository <- "MSA_Wind_FootprintBias"
path_base <- "C:/Users/lianne.allen-jacobso/Documents/"
check_pwd <- paste0(path_base, "Repositories/",repository)
pwd == check_pwd

dir_output <- paste0(path_base, "Output/", repository)
dir_data <- paste0(path_base, "Data/", repository)

##############################
# Pull in data
sf_vtrb_split_mosaic <- readRDS(paste0(dir_output, "/sf_vtrb_split_mosaic.rds"))

sf_hulls_attributes <- readRDS(file = paste0(dir_output, "/sf_hulls_attributes.rds"))

##############################
# create empty data.table, will be coerced into SF when sf rows are added below
# apply crs from sfBuffers
tmp <- st_sfc()
class(tmp)[1] <- "sfc_GEOMETRY" # for geometry collection
# set names/structure to match sf is loop below
sf_bias <- st_sf(tripid_chr=character(0), area = numeric(0), type=character(0),
                 percentile=character(0), tripid=character(0), geometry=tmp)

thisCRS <- st_crs(sf_vtrb_split_mosaic) #extract CRS from exiting SF

st_crs(sf_bias) <- thisCRS #set CRS to match existing SF

#uniqueTrips <- unique(sf_hulls_attributes$tripid_chr) # not all ids in SFCH are in VTRB set
uniqueTrips <- unique(sf_vtrb_split_mosaic$tripid)

#this_trip <- uniqueTrips[[1]]
for (this_trip in uniqueTrips) {
  # filter rows in CH that match trip_id
  # select id only (with geometry)
  thisSFCH <- sf_hulls_attributes %>%
    filter(tripid_chr == this_trip) %>%
    select(tripid_chr)
  
  # filter rows in VTRB that match trip_id
  # select percentile only (with geometry)
  thisVTRB <- sf_vtrb_split_mosaic %>%
    filter(tripid == this_trip) %>%
    select(tripid, percentile)
  
  # calculate intersection (later repeat this for difference)
  sfIntersection <- st_intersection(thisSFCH, thisVTRB) # creates new SF with intersection as geometry
  
  # calculate intersection area
  sfIntersection <- sfIntersection %>%  # creates new sf
    mutate(area = st_area(.) %>% as.numeric()) %>%
    cbind(type = "intersection")
  
  # calculate false positives and negatives: geometry and area
  sfFalsePositive <- st_difference(thisVTRB, thisSFCH) # creates new SF with intersection as geometry
  if(length(sfFalsePositive$tripid) > 0 ){
    sfFalsePositive <- sfFalsePositive %>%  # creates new sf
      mutate(area = st_area(.) %>% as.numeric()) %>%
      cbind(type = "falsePositive")
  }
  
  sfFalseNegative <- st_difference(thisSFCH, thisVTRB) # creates new SF with intersection as geometry
  if(length(sfFalseNegative$tripid) > 0 ){
    sfFalseNegative <- sfFalseNegative %>%  # creates new sf
      mutate(area = st_area(.) %>% as.numeric()) %>%
      cbind(type = "falseNegative")
  }
  
  sfSFCH <- thisSFCH %>%
    mutate(area = st_area(.) %>% as.numeric()) %>%
    cbind(type = "sfch", percentile = "", tripid = thisSFCH$tripid_chr) %>%
    st_cast()
  
  sfVTRB <- thisVTRB %>%
    mutate(area = st_area(.) %>% as.numeric()) %>%
    cbind(type = "vtrb", tripid_chr = thisVTRB$tripid)
  
  
  if(length(sfFalsePositive$tripid) == 0 ){
    sf_bias <- rbind(sf_bias, sfVTRB, sfSFCH,
                                          sfIntersection, sfFalseNegative)
  } else if (length(sfFalseNegative$tripid) == 0 ){
    sf_bias <- rbind(sf_bias, sfVTRB, sfSFCH,
                                          sfIntersection, sfFalsePositive)
  } else {
    sf_bias <- rbind(sf_bias, sfVTRB, sfSFCH,
                                          sfIntersection, sfFalsePositive, sfFalseNegative) 
  }
  }

saveRDS(object = sf_bias,
        file= paste0(dir_output, "/sf_bias.rds"))
