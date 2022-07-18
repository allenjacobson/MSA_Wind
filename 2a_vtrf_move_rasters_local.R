library(fst)
library(data.table)
library(raster)

# This scripts pulls rasters from the network and saves them locally
# Creates a dictionary with these local paths, the IMGIDs, and TripIDs

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
# Pull data dictionary (fst) from network
# must be connected to VPN and
# check that the network directory is correctly mapped
# map \\net.nefsc.noaa.gov\work5 to Y:\\
# load dictionary of file locations
path_fst <- "Y:\\socialsci\\Geret_Rasters\\Data\\offshore_wind_package_data\\raster_extents_and_file_list"
dt_paths <-setDT(read_fst(file.path(path_fst, 'raster_file_list.fst')))
##############################
# Pull in data
dt_imgids_matched <- setDT(readRDS(file=paste0(dir_output, "/dt_imgids_matched.rds")))

##############################
# Prep data
# Filter to only include IDNUMs with 16 characters (which narrows to trips in study fleet?)
dt_paths[, IDNUM_chr := as.character(IDNUM)][
          , nchar_IDNUM := nchar(dt_paths$IDNUM, type = 'width')]

dt_paths_16chr <-dt_paths[nchar_IDNUM == 16]

# filter filePathDictionary16 to include match IMGID to IDNUM
dt_paths_matched <- dt_paths_16chr[IDNUM_chr %in% dt_imgids_matched$imgid_chr]

# Modefy file path to match path on this computer (Versus someone from social sci)
dt_paths_matched[, modefied_path := substr(dt_paths_matched$FILEPATH, 11,
                                         nchar(dt_paths_matched$FILEPATH))]
dt_paths_matched[, final_path := paste("Y:", dt_paths_matched$modefied_path, sep = "")]
saveRDS(dt_paths_matched, paste0(dir_output, "/dt_paths_matched.rds"))

#driectory includes .gri and .grd - I think this points to the .gri - is .grd called indirectly?

##############################
# Pull all rasters
unique_imgids <- unique(dt_imgids_matched$imgid_chr)
#dtAllIDS <- data.table()
dt_paths_vtrb <- data.table()

for (this_imgid in unique_imgids) {
  this_tripid <- dt_imgids_matched[imgid_chr==this_imgid, tripid_chr] #find tripID by IMGID
  this_trip_area <- dt_imgids_matched[imgid_chr==this_imgid, trip_area] #find tripID by IMGID
  this_path <- dt_paths_matched[IDNUM==this_imgid, final_path] #find path by IMGID
  count_paths <- length(this_path) # see if there are zero or multiple paths for one imgid
  check_path <- file.exists(this_path) # check if path exists - some are missing
  if(count_paths == 0){
    this_warning <- paste("No file path found, for IMGID: ", this_imgid, sep = " ")
    warning(this_warning) # path is missing, return error
    next
    } else if (count_paths > 1){
    this_warning <- paste("Multiple file paths, for IMGID: ", this_imgid, sep = " ")
    warning(this_warning) #multiple paths - should be unique - return error
    next
    } else if (length(check_path) == 0){
    this_warning <- paste("File missing for IMGID: ", this_imgid, sep = " ")
    warning(this_warning) #found path, but could not pull file, return error
    next
    } else if (check_path == TRUE){
    this_raster <- raster(this_path)
    new_path <- paste0(dir_output, "/vtrbs/", this_tripid, "_", this_imgid, ".tif")
    writeRaster(this_raster, new_path, overwrite=TRUE) #save raster locally
    #concatenate data for this IMGID and add to dt to save later
    these_ids <- data.table(tripid=this_tripid,
                            trip_area = this_trip_area,
                           imgid=this_imgid,
                           path = new_path)
    dt_paths_vtrb <- rbindlist(list(dt_paths_vtrb, these_ids)) #add data to summary table
    next
    } else {
    this_warning <- paste("Error with file path, file.exists is not TRUE or FALSE, for IMGID: ", this_imgid, sep = " ")
    warning(this_warning)
  }
}

# save dt that contains all local paths with IMGIDs and TripIDS
saveRDS(dt_paths_vtrb, paste0(dir_output, "/dt_paths_vtrb.rds"))

# Warning messages:
# 1: No file path found, for IMGID:  3303391612311601 
# 2: No file path found, for IMGID:  3305341712282101 
# 3: No file path found, for IMGID:  3305491812301501 
# 4: No file path found, for IMGID:  3305491812301503 
# 5: No file path found, for IMGID:  3305491812301502 
# 6: No file path found, for IMGID:  4105141612301801 