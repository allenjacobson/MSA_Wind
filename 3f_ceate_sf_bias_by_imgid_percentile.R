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
sf_vtrb_cumulative_imgid <- readRDS(paste0(dir_output, "/sf_vtrb_cumulative_imgid.rds"))

sf_hulls_attributes_imgid <- readRDS(file = paste0(dir_output, "/sf_hulls_attributes_imgid.rds"))

##############################
# create empty data.table, will be coerced into SF when sf rows are added below
# apply crs from sfBuffers
tmp <- st_sfc()
class(tmp)[1] <- "sfc_GEOMETRY" # for geometry collection
# set names/structure to match sf is loop below
sf_bias <- st_sf(imgid_chr=character(0), area = numeric(0), type=character(0),
                 percentile=character(0), imgid=character(0), geometry=tmp)

this_crs <- st_crs(sf_vtrb_cumulative_imgid) #extract CRS from exiting SF

st_crs(sf_bias) <- this_crs #set CRS to match existing SF

#unique_trips <- unique(sf_hulls_attributes_imgid$tripid_chr) # not all ids in SFCH are in VTRB set
unique_trips <- unique(sf_vtrb_cumulative_imgid$imgid)
this_trip <- unique_trips[[29]]

for (this_trip in unique_trips) {
  # filter rows in CH that match trip_id
  # select id only (with geometry)
  this_sfch <- sf_hulls_attributes_imgid %>%
    filter(imgid_chr == this_trip) %>%
    select(imgid_chr)
  
  # filter rows in VTRB that match trip_id
  # select percentile only (with geometry)
  this_vtrb <- sf_vtrb_cumulative_imgid %>%
    filter(imgid == this_trip) %>%
    select(imgid, percentile)
  
  # calculate intersection (later repeat this for difference)
  sf_intersection <- st_intersection(this_sfch, this_vtrb) # creates new SF with intersection as geometry
  if(length(sf_intersection$imgid) > 0 ){
    sf_intersection <- sf_intersection %>%  # creates new sf
      mutate(area = st_area(.) %>% as.numeric()) %>%
      cbind(type = "intersection")
  }
  
  # calculate false positives and negatives: geometry and area
  sf_false_positive <- st_difference(this_vtrb, this_sfch) # creates new SF with intersection as geometry
  if(length(sf_false_positive$imgid) > 0 ){
    sf_false_positive <- sf_false_positive %>%  # creates new sf
      mutate(area = st_area(.) %>% as.numeric()) %>%
      cbind(type = "false_positive")
  }
  
  sf_false_negative <- st_difference(this_sfch, this_vtrb) # creates new SF with intersection as geometry
  if(length(sf_false_negative$imgid) > 0 ){
    sf_false_negative <- sf_false_negative %>%  # creates new sf
      mutate(area = st_area(.) %>% as.numeric()) %>%
      cbind(type = "false_negative")
  }
  
  sf_sfch <- this_sfch %>%
    mutate(area = st_area(.) %>% as.numeric()) %>%
    cbind(type = "sfch", percentile = "", imgid = this_sfch$imgid_chr) %>%
    st_cast()
  
  sf_vtrb <- this_vtrb %>%
    mutate(area = st_area(.) %>% as.numeric()) %>%
    cbind(type = "vtrb", imgid_chr = this_vtrb$imgid)
  
  
  if(length(sf_false_positive$imgid) == 0 ){
    sf_bias <- rbind(sf_bias, sf_vtrb, sf_sfch,
                     sf_intersection, sf_false_negative)
    print(paste0("no false positive for this trip:", this_trip))
  } else if (length(sf_false_negative$imgid) == 0 ){
    sf_bias <- rbind(sf_bias, sf_vtrb, sf_sfch,
                     sf_intersection, sf_false_positive)
    print(paste0("no false negative for this trip:", this_trip))
  } else if (length(sf_intersection$imgid) == 0 ){
    sf_bias <- rbind(sf_bias, sf_vtrb, sf_sfch,
                     sf_false_positive, sf_false_negative)
    print(paste0("no intersection for this trip:", this_trip))
  } else {
    sf_bias <- rbind(sf_bias, sf_vtrb, sf_sfch,
                     sf_intersection, sf_false_positive, sf_false_negative) 
  }
  }

saveRDS(object = sf_bias,
        file= paste0(dir_output, "/sf_bias_imgid.rds"))

#six warnings printed
# [1] "no intersection for this trip:1507731707022302"
# [1] "no intersection for this trip:3303391607270102"
# [1] "no intersection for this trip:3305341809210201"
# [1] "no intersection for this trip:4103421602171802"
# [1] "no intersection for this trip:4103421712161801"
# [1] "no intersection for this trip:4105141602202201"

