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
dt <- setDT(readRDS(file = paste0(dir_data,"/longfin_catch_gps_data.rds"))) # gte data with trips
dt_imgids <- as.data.table(readRDS(paste0(dir_data, "/VERSWH.IMAGES_TO_EFFORTS.rds"))) # to match hauls to subtrips
crs_nad83 <- readRDS(paste0(dir_data, "/crs_vtr_buffer.rds")) # Pull in CRS from VTR buffers
dt_revenue <- setDT(readRDS(file = paste0(dir_data, "/apsd.dmis_all_years_squid_2015_on.rds")))

##############################
# Prep Data
dt_imgids[,tripid_chr :=as.character(TRIP_ID)][
  ,imgid_chr :=as.character(IMG_ID)]

dt_revenue[,docid_chr :=as.character(DOCID)][
  , char_docid := nchar(docid_chr)][
    ,link_chr :=as.character(LINK)][
    , char_link := nchar(link_chr)]

dt[,tripid_chr :=as.character(trip_id)][
  , char_tripid := nchar(tripid_chr)]

# look at CAtch Acountingn and Monitoring System for info about APSD

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
remove(dt_trimmed)

##############################
# Prep data to coerce into spatial points data frame:
# Clean, subset, and export data:
dt_gte <- dt[has_GTE=="YES"] # Select trips with GPS data (GTE)
dt_gte <- na.omit(dt_gte , c("LONGITUDE", "LATITUDE")) # rows with NAs in lon and lat

# Match cleaned dataset with IMGID dataset
# remove rows that do not have trip_id in other dataset
dt_imgids_matched <- dt_imgids[tripid_chr %in% dt_gte$tripid_chr]
dt_gte_matched <- dt_gte[tripid_chr %in% dt_imgids_matched$tripid_chr]
dt_revenue_matched <- dt_revenue[DOCID %in% dt_imgids_matched$trip_id]

##############################
# Create Grouping variable for hauls - using 
#group by stat area, gear size, mesh size
dt_imgids_matched[, group_id := .GRP, by = .(tripid_chr, EFFORT_NUM)]
dt_gte_matched[, group_id := .GRP, by = .(tripid_chr, EFFORT_NUM)]

##############################
# Join tables by trip_area and group_id
# join cleaned imgids to dt_gte_matched
dt_for_join <- dt_imgids_matched[, .(group_id, imgid_chr)]
setkey(dt_for_join, imgid_chr)
setkey(dt_revenue_matched, imgid_chr)

# this creates a problem
dt_gte_joined <- dt_gte_matched[dt_for_join, nomatch = 0] #, allow.cartesian=TRUE]
setkey(dt_gte_joined, group_id)
setkey(dt_gte_matched, group_id)



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

##############################
# Confirm there is only one imgid for each group_id
dt_imgids_test <- dt_imgids_matched[, .(imgid_chr, group_id)]
dt_imgids_summary <- dt_imgids_test[, .N, by = group_id]
dt_multiple_imgids <- dt_imgids_summary[N>1] # this should be an empty table
