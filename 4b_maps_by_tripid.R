# 4a plot all - merged and separate then merge
# 4b plot by trip
# 4c plot summary data

# Loading packages
library(data.table)
library(sf)
library(dplyr)
library( ggplot2)
library (cowplot)

# This script creates plots from SF

##############################
# Functions

##############################
# Set directories
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

sf_bias <- readRDS(paste0(dir_output, "/sf_bias.rds"))

##############################
#Plot by trip

#labels="AUTO") #c('All VTR buffers for trip', 'Zoomed in on all GPS points from trip')

#fileName <- paste0("output/tripImages/plotAllByCatchTotalAndRatio.png")
#save_plot(filename = fileName, plot = p,
#          base_height = 8,
#          base_width = 11)


## Plots of VTRB

# rbind difference result to summary table
#vtrbColors <- c("#388E3C", "#689F38", "#AFB42B", "#FDD835") # 25, 50, 75, 100

vtrbColors <- c("#FDD835", "#AFB42B", "#689F38", "#388E3C") # 100, 75, 50, 25

# Error in how trip_ID is added to sf - go back to 3c

uniqueTrips <- unique(sfBias_ByPercentileAndTripID$trip_ID)
thisTrip <- uniqueTrips[[4]]

#for (thisTrip in uniqueTrips) {

theseVTRB <- sfBias_ByPercentileAndTripID %>%
  filter(trip_ID==thisTrip, type == "vtrb")

thisSFCH <- sfBias_ByPercentileAndTripID %>%
  filter(trip_ID==thisTrip, type == "sfch")

theseIntersections <- sfBias_ByPercentileAndTripID %>%
  filter(trip_ID==thisTrip, type == "intersection")

theseFalsePositives <- sfBias_ByPercentileAndTripID %>%
  filter(trip_ID==thisTrip, type == "falsePositive")

theseFalseNegatives <- sfBias_ByPercentileAndTripID %>%
  filter(trip_ID==thisTrip, type == "falseNegative")

# Change ordering manually
theseVTRB$percentile <- factor(theseVTRB$percentile,
                              levels = c("100th", "75th", "50th", "25th"))

# Change ordering manually
theseFalseNegatives$percentile <- factor(theseFalseNegatives$percentile,
                               levels = c("100th", "75th", "50th", "25th"))
# Change ordering manually
theseFalsePositives$percentile <- factor(theseFalsePositives$percentile,
                               levels = c("100th", "75th", "50th", "25th"))
# Change ordering manually
theseIntersections$percentile <- factor(theseIntersections$percentile,
                               levels = c("100th", "75th", "50th", "25th"))
## Separate for loop to plot
# plot to test
(plotTest <- ggplot() +
  geom_sf(data = theseVTRB, aes(fill = percentile), colour = NA)+
  scale_fill_viridis_d(direction = -1)+
  #scale_fill_manual(values=vtrbColors)+
  geom_sf(data=thisSFCH, color= "red", fill="red", alpha = 0.5, size = 1, linetype="dotted")+
  xlab("Longitude") + ylab("Latitude"))

#thesePercentiles <- theseFalseNegatives$percentile
#thisPercentile <- thesePercentiles[[1]]
#thisPercentile <- thesePercentiles[[2]]
# 
# plotTest <- ggplot(data = theseVTRB[theseVTRB$percentile == "100th",]) +
#   geom_sf(color = "grey", fill = NA)+
#   geom_sf(data = theseVTRB[theseVTRB$percentile == thisPercentile,], fill = "#388E3C")+
#   geom_sf(data=theseFalseNegatives[theseFalseNegatives$percentile == thisPercentile,], fill="red")+
#   geom_sf(data=theseFalsePositives[theseFalsePositives$percentile == thisPercentile,], fill="black")+
#   xlab("Longitude") + ylab("Latitude")


(plotTest <- ggplot() +
    geom_rect(data = theseIntersections, aes(color=percentile), size = 2,
              fill = NA, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
    scale_color_viridis_d(direction = -1)+
    #scale_fill_viridis_d(direction = -1)+
    geom_sf(data=theseFalsePositives, fill="black")+
    geom_sf(data=theseFalseNegatives, fill="red")+
    geom_sf(data = theseIntersections, fill="grey")+
    facet_wrap(~ percentile, nrow = 1,  )+
    xlab("Longitude") + ylab("Latitude"))