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

this_crs <- st_crs(sf_vtrb_split_mosaic) #extract CRS from exiting SF

st_crs(sf_bias) <- this_crs #set CRS to match existing SF

#unique_trips <- unique(sf_hulls_attributes$tripid_chr) # not all ids in SFCH are in VTRB set
unique_trips <- unique(sf_vtrb_split_mosaic$tripid)

#this_trip <- unique_trips[[1]]
for (this_trip in unique_trips) {
  # filter rows in CH that match trip_id
  # select id only (with geometry)
  this_sfch <- sf_hulls_attributes %>%
    filter(tripid_chr == this_trip) %>%
    select(tripid_chr)
  
  # filter rows in VTRB that match trip_id
  # select percentile only (with geometry)
  this_vtrb <- sf_vtrb_split_mosaic %>%
    filter(tripid == this_trip) %>%
    select(tripid, percentile)
  
  # calculate intersection (later repeat this for difference)
  sf_intersection <- st_intersection(this_sfch, this_vtrb) # creates new SF with intersection as geometry
  
  # calculate intersection area
  sf_intersection <- sf_intersection %>%  # creates new sf
    mutate(area = st_area(.) %>% as.numeric()) %>%
    cbind(type = "intersection")
  
  # calculate false positives and negatives: geometry and area
  sf_false_positive <- st_difference(this_vtrb, this_sfch) # creates new SF with intersection as geometry
  if(length(sf_false_positive$tripid) > 0 ){
    sf_false_positive <- sf_false_positive %>%  # creates new sf
      mutate(area = st_area(.) %>% as.numeric()) %>%
      cbind(type = "false_positive")
  }
  
  sf_false_negative <- st_difference(this_sfch, this_vtrb) # creates new SF with intersection as geometry
  if(length(sf_false_negative$tripid) > 0 ){
    sf_false_negative <- sf_false_negative %>%  # creates new sf
      mutate(area = st_area(.) %>% as.numeric()) %>%
      cbind(type = "false_negative")
  }
  
  sf_sfch <- this_sfch %>%
    mutate(area = st_area(.) %>% as.numeric()) %>%
    cbind(type = "sfch", percentile = "", tripid = this_sfch$tripid_chr) %>%
    st_cast()
  
  sf_vtrb <- this_vtrb %>%
    mutate(area = st_area(.) %>% as.numeric()) %>%
    cbind(type = "vtrb", tripid_chr = this_vtrb$tripid)
  
  
  if(length(sf_false_positive$tripid) == 0 ){
    sf_bias <- rbind(sf_bias, sf_vtrb, sf_sfch,
                     sf_intersection, sf_false_negative)
    print(paste0("no false positive for this trip:", this_trip))
  } else if (length(sf_false_negative$tripid) == 0 ){
    sf_bias <- rbind(sf_bias, sf_vtrb, sf_sfch,
                     sf_intersection, sf_false_positive)
    print(paste0("no false negative for this trip:", this_trip))
  } else {
    sf_bias <- rbind(sf_bias, sf_vtrb, sf_sfch,
                     sf_intersection, sf_false_positive, sf_false_negative) 
  }
  }

saveRDS(object = sf_bias,
        file= paste0(dir_output, "/sf_bias.rds"))

#only one warning printed
#"no false negative for this trip:33033916051107"

# includes 221 unique trips = wittout fixing code
