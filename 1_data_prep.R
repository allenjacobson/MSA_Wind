# Loading packages
library(data.table)
library(sf)
library(dplyr)

# Select data with >.39 loligo catch and by year
# remove trips with alphanumeric ids
# select trips with GTE
# remove trips with NA for longitude or latitude
# match tripID to IMGID
# export matched ids
# coerce to SF and change crs to match vtrb - export sf
# add imgid to sf with GTE data

##############################
# Functions
numbers_only <- function(x) !grepl("\\D", x)

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
dt <- setDT(readRDS(file = paste0(dir_data,"/longfin_catch_gps_data.rds")))

# Pull in CRS from VTR buffers
crs_nad83 <- readRDS(paste0(dir_data, "/crs_vtr_buffer.rds"))

# Add fields to pull VTR buffers later
# IMGIDs - which should be the same as the docid used to annotate VTR buffers
dt_imgids <- fread(paste0(dir_data, "/SF_MATCHED_TO_APSD_AREA.csv"))
dt_imgids[,tripid_chr :=as.character(trip_id)][
  ,imgid_chr :=as.character(IMGID)]


## tests
# test <- dt[ , .(count = length(unique(haul_id))), by = trip_area]
# max(test$count)
# test2 <- dt_imgids[ , .(count = length(unique(imgid_chr))), by = trip_area]
# max(test2$count)

##############################
# Trim data to only include that meet pre-specified criteria
# First, calculate loglio catch
dt_trimmed <- dt[, prop_loligo := TOT_LOLIGO_CATCH/TOT_CATCH][
  , year := substr(dt$sail_date, 1, 4)][
  year>2014][ # Then, filter by year
  prop_loligo>.3999][ # Filter by prop loglio catch
  , isNum := numbers_only(trip_id)][ #add column, T = numbers only
  isNum == TRUE] #Filter alphanumeric trips

# new version does not trim - maybe Andy already filtered data 
dt <- dt_trimmed

##############################
# Prep data to coerce into spatial points data frame:
# Clean, subset, and export data:
dt_gte <- dt[has_GTE=="YES"] # Select trips with GPS data (GTE)
dt_gte <- na.omit(dt_gte , c("LONGITUDE", "LATITUDE")) # rows with NAs in lon and lat
dt_gte <- dt_gte[,tripid_chr :=as.character(trip_id)]

# Match cleaned dataset with IMGID dataset
# remove rows that do not have trip_area in other dataset
dt_imgids_matched <- dt_imgids[trip_area %in% dt_gte$trip_area]
dt_gte_matched <- dt_gte[trip_area %in% dt_imgids_matched$trip_area]

# join cleaned imgids to dt_gte_matched
dt_for_join <- dt_imgids_matched[, .(trip_area, imgid_chr)]
setkey(dt_for_join, trip_area)
setkey(dt_gte_matched, trip_area)

dt_gte_joined <- dt_gte_matched[dt_for_join, nomatch = 0, allow.cartesian=TRUE]

# Filter by unique imgid-triparea combination

#dt_prefilter <- dt_gte_joined[ , .(count = length(unique(imgid_chr))), by = trip_area]
#dt_filter <- dt_prefilter[count == 1]
#length(dt_prefilter$trip_area)-length(dt_filter$trip_area)
# filter removes 17 trips
#dt_gte_final <- dt_gte_joined[trip_area %in% dt_filter$trip_area]

#length(dt_gte_joined$trip_area)-length(dt_gte_final$trip_area)
# this filter removes 40985 gte rows

dt_gte_final <- dt_gte_joined
saveRDS(dt_gte_final, paste0(dir_output,"/dt_gte.rds"))
saveRDS(dt_imgids_matched, paste0(dir_output, "/dt_imgids_matched.rds"))

##############################
# Coerce into SF (simple features)
#crs_proj <- st_crs("+init=epsg:4326") # EPSG code for WGS84, which taks XY or long lat

sf_gte_wgs84 <- st_as_sf(x = dt_gte_final,                         
               coords = c("LONGITUDE", "LATITUDE"),
               crs = 4326)

sf_gte_nad83 <- st_transform(sf_gte_wgs84, crs_nad83)

saveRDS(sf_gte_nad83, paste0(dir_output,"/sf_gte_nad83.rds"))

# can't use this - b/c names are too long for a shp file, must be <10 chr
# st_write(sf_gte_nad83, paste(dir_output, "/sf_gte_nad83.shp"), append = FALSE)