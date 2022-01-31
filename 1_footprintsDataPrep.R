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

##############################
# Functions
numbers_only <- function(x) !grepl("\\D", x)

##############################
# Set directories
pwd <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

repositoryName <- "MSA_Wind_FootprintBias"
basePath <- "C:/Users/lianne.allen-jacobso/Documents/Repositories/"
pwdCheck <- paste0(basePath, repositoryName)
pwd == pwdCheck

outputDir <- paste0(basePath, repositoryName)
dataDir <- paste0(basePath, repositoryName)

##############################
# Pull in data
dt <- setDT(readRDS(file = paste0(dataDir,"/plot_data_comb_gte_join_v2.rds")))

# Pull in CRS from VTR buffers
# Must come back to do this after running step 4
crsNAD83 <- readRDS(paste0(outputDir, "/crsFromVTRBuffer.rds"))

# Add fields to pull VTR buffers later
# IMGIDs - which should be the same as the docid used to annotate VTR buffers
dtIMGID <- fread(paste0(dataDir, "/SF_MATCHED_TO_APSD.csv"))
dtIMGID[,trip_id_chr :=as.character(trip_id)][
        ,IMGID_chr :=as.character(IMGID)]

##############################
# Trim data to only include that meet pre-specified criteria
# First, calculate loglio catch
dtTrimmed <- dt[, loligoPerTotalCatch := TOT_LOLIGO_CATCH/TOT_CATCH][
                YEAR>2014][ # Then, filter by year
                  loligoPerTotalCatch>.3999][ # Filter by prop loglio catch
                    , isNum := numbers_only(trip_id)][ #add column, T = numbers only
                      isNum == TRUE] #Filter alphanumeric trips

dt <- dtTrimmed

##############################
# Prep data to coerce into spatial points data frame:
# Clean, subset, and export data:
dtHasGTE <- dt[has_GTE=="YES"] # Select trips with GPS data (GTE)
dtHasGTE <- na.omit(dtHasGTE , c("LONGITUDE", "LATITUDE")) # rows with NAs in lon and lat
dtHasGTE <- dtHasGTE[,trip_id_chr :=as.character(trip_id)]

# Match cleaned dataset with IMGID dataset
dtIMGIDmatchedToTripID <- dtIMGID[trip_id_chr %in% dtHasGTE$trip_id_chr]

saveRDS(dtHasGTE, paste0(outputDir,"/dtHasGTE.rds"))
saveRDS(dtIMGIDmatchedToTripID, paste0(outputDir, "/dtIMGIDmatchedToTripID.rds"))

##############################
# Coerce into SF (simple features)
projCRS <- st_crs("+init=epsg:4326") # EPSG code for WGS84, which taks YX or long lat

sfHasGTEinWGS84 <- st_as_sf(x = dtHasGTE,                         
               coords = c("LONGITUDE", "LATITUDE"),
               crs = projCRS)

sfHasGTEinNAD83 <- st_transform(sfHasGTEinWGS84, crsNAD83)

saveRDS(sfHasGTEinNAD83, paste0(outputDir,"/sfHasGTEinNAD83.rds"))

# can't use this - b/c names are too long for a shp file, must be <10 chr
# st_write(sfGTEinNAD83, paste(outputDir, "/sfGTEinNAD83.shp"), append = FALSE)