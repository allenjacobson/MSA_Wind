# Loading packages
library(data.table)
library(sf)
library(ggplot2)
library(cowplot)

##############################
# Functions

MinMeanSEMMax <- function(x) {
  v <- c(min(x), mean(x) - sd(x)/sqrt(length(x)), mean(x), mean(x) + sd(x)/sqrt(length(x)), max(x))
  names(v) <- c("ymin", "lower", "middle", "upper", "ymax")
  v
}

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
sf_gte_nad83 <- readRDS(paste0(dir_output,"/sf_gte_nad83.rds"))

sf_hulls_attributes <- readRDS(paste0(dir_output, "/sf_hulls_attributes.rds"))

sf_vtrb_split_mosaic <- readRDS(paste0(dir_output, "/sf_vtrb_split_mosaic.rds"))

sf_bias <- readRDS(paste0(dir_output, "/sf_bias.rds"))

##############################
#plot area diff - over/underestimation

dt_bias <- as.data.table(sf_bias)

dt_bias_size <- dt_bias[type %in% c("false_positive", "false_negative"),
                        .(tripid, percentile, area, type, tripid_chr)]
  
dtOverUnderEstimation <-  data.table(percentile = as.factor(dtSummaryByTripID[, percentile]),
                                     areaDiffVTRBminusCH = dtSummaryByTripID$areaDiffVTRBminusCH,
                                     areaVTR = dtSummaryByTripID$areaVTR,
                                     areaSF = dtSummaryByTripID$areaSF)

dtOverUnderEstimation[, lnArea_VTR_SF := log(dtOverUnderEstimation$areaVTR/dtOverUnderEstimation$areaSF)]

vtrbColors <- c("#388E3C", "#689F38", "#AFB42B", "#FDD835") # 25, 50, 75, 100

dtOverUnderEstimation$percentile <- factor(dtOverUnderEstimation$percentile,       # Change ordering manually
                                           levels = c("25th", "50th", "75th", "100th"))

plotOverUnderEstimation <- ggplot(data = dtOverUnderEstimation,
                                  aes(x = percentile, y = lnArea_VTR_SF,
                                      fill = percentile, na.rm = TRUE))+
  #geom_boxplot()+
  geom_violin()+
  geom_dotplot(binaxis = "y",
               stackdir = "center",
               dotsize = 0.25,
               alpha = 0.5,
               method = "histodot") +
  scale_fill_manual(values=vtrbColors)+
  xlab("Percentile")+
  ylab("Log Ratio: log10(VTRB/SFCH)")+
  labs(title = "Which VTRB percentile matches SFCH by area?")

ggsave("output/plotOverUnderEstimation.png", plot = plotOverUnderEstimation)

##############################

summaryOverUnder <-  dtOverUnderEstimation[, .( n = length(lnArea_VTR_SF),
                                                mean = mean(lnArea_VTR_SF),
                                                min = min(lnArea_VTR_SF),
                                                max = max(lnArea_VTR_SF),
                                                se = sd(lnArea_VTR_SF)/length(lnArea_VTR_SF)),
                                           by = percentile]

g1 <- ggplot(data = summaryOverUnder) +
  geom_bar(aes(x=percentile, y=mean),
           stat="identity") +
  geom_errorbar(aes(x=percentile, ymin=mean-se, ymax=mean+se),
                width=0.4, size=1.3)

##############################
# Log ratio with mean, +/- se, and min/max
plotLogRatio <- ggplot(data = dtOverUnderEstimation,
             aes(x = percentile, y = lnArea_VTR_SF,
                 fill = percentile, na.rm = TRUE)) +
  scale_fill_manual(values=vtrbColors)+
  stat_summary(fun.data=MinMeanSEMMax, geom="boxplot", show.legend = FALSE) + 
  theme_classic(base_size = 16)+
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  xlab("Percentile")+
  ylab(paste0("Log Ratio"))
  #ggtitle("Over/Under estimation by percentile")
          #subtitle = "Center: mean \nBox bounds: +/- standard error \nBar: range")

ggsave("output/plotLogRatio.png", plot = plotLogRatio,
       width = 9,
       height = 6,
       units ="in")

##############################
#Plot without 100
dtOverUnderEstimationTrimmed <- dtOverUnderEstimation[percentile != "100th"]

vtrbColors <- c("#388E3C", "#689F38", "#AFB42B") # 25, 50, 75, 100

dtOverUnderEstimationTrimmed$percentile <- factor(dtOverUnderEstimationTrimmed$percentile,       # Change ordering manually
                                                  levels = c("25th", "50th", "75th"))

plotOverUnderEstimationTrimmed <- ggplot(data = dtOverUnderEstimationTrimmed,
                                         aes(x = percentile, y = areaDiffVTRBminusCH,
                                             fill = percentile, na.rm = TRUE))+
  geom_boxplot()+
  scale_fill_manual(values=vtrbColors)+
  xlab("Percentile")+
  ylab("Difference VTRB - SFCH (m2)")+
  labs(title = "Which VTRB percentile matches SFCH by area?")

ggsave("output/plotOverUnderEstimationTrimmed.png", plot = plotOverUnderEstimationTrimmed)

##############################
#Plot areas diff as histogram  
dtOverUnderEstimation[, overUnder :=
                        ifelse(dtOverUnderEstimation$areaDiffVTRBminusCH >0, "Over",
                               ifelse(dtOverUnderEstimation$areaDiffVTRBminusCH < 0,
                                      "Under", "No Difference"))]

dtOverUnderEstimation[, overUnder := as.factor(overUnder)]

dtOverUnderEstimation[, areaDiffVTRBminusCHHectares := areaDiffVTRBminusCH*0.0001]

histColors <- c("#FDD835", "#388E3C")
dtOverUnderEstimation[, .N, by= .(percentile, overUnder)]

histOverUnder<- ggplot(dtOverUnderEstimation, 
                       aes(x = areaDiffVTRBminusCHHectares, fill = overUnder)) + 
  geom_histogram(position = "stack", boundary=0, bins = 15)+
  scale_fill_manual(values=histColors)+
  facet_wrap(~ percentile, nrow = 4)+
  scale_y_log10()

##############################
#Plot total mismatch
dtTotalMismatch <- data.table(percentile = as.factor(dtSummaryByTripID[, percentile]),
                              falsePositive = dtSummaryByTripID$falsePositiveVTRB,
                              falseNegative = dtSummaryByTripID$falseNegativeVTRB)

dtTotalMismatch[, totalMismatch := falsePositive+falseNegative]

vtrbColors <- c("#388E3C", "#689F38", "#AFB42B", "#FDD835") # 25, 50, 75, 100

dtTotalMismatch$percentile <- factor(dtTotalMismatch$percentile,       # Change ordering manually
                                     levels = c("25th", "50th", "75th", "100th"))

ploTotalMismatch <- ggplot(data = dtTotalMismatch,
                           aes(x = percentile, y = totalMismatch,
                               fill = percentile, na.rm = TRUE))+
  geom_boxplot()+
  scale_fill_manual(values=vtrbColors)+
  xlab("Percentile")+
  ylab("Total Mismatch (m2)")+
  labs(title = "Total Mismatch")
##############################
#Plot mismatch by false positive vs negative
dtMismatchPositives <-  data.table(percentile = as.factor(dtSummaryByTripID[, percentile]),
                                   falseArea = dtSummaryByTripID$falsePositiveVTRB,
                                   PositiveNegative = "Positive")

dtMismatchNegatives <-  data.table(percentile = as.factor(dtSummaryByTripID[, percentile]),
                                   falseArea = dtSummaryByTripID$falseNegativeVTRB,
                                   PositiveNegative = "Negative")

dtMismatch <- rbind(dtMismatchPositives, dtMismatchNegatives)

vtrbColors <- c("#388E3C", "#689F38", "#AFB42B", "#FDD835") # 25, 50, 75, 100

dtMismatch$percentile <- factor(dtMismatch$percentile,       # Change ordering manually
                                levels = c("25th", "50th", "75th", "100th"))

summaryMismatch <- dtMismatch[, .(sumFalseArea = sum(falseArea),
                                  meanFalseArea = mean(falseArea),
                                  minFalseArea = min(falseArea),
                                  maxFalseArea = max(falseArea),
                                  seFalseArea = sd(falseArea)/length(falseArea)),
                              by = .(percentile, PositiveNegative)]

summaryMismatch[, sumFalseAreaHecatres := sumFalseArea*0.0001]

plotMismatch <- ggplot(data = summaryMismatch,
                       aes(x = percentile, y = meanFalseArea,
                           fill = PositiveNegative))+
  #by = percentile,
  #na.rm = TRUE))+
  geom_bar()+# position = "stack", stat = "identity")+
  scale_fill_manual(values=c("red", "black"))+
  #scale_y_log10()+
  xlab("Percentile")+
  ylab("False Area (hectare)")+
  #facet_wrap(~ percentile, nrow = 1)+
  labs(title = "False positives and Negatives")

ggsave("output/plotOverUnderEstimation.png", plot = plotOverUnderEstimation)

##############################
#Plot data for trip
uniqueTrips <- unique(dtCumulativeRasterPaths$trip_ID)
thisTrip <- uniqueTrips[1]
dtPlottedTrips <- data.table()

for (thisTrip in uniqueTrips) {
  dtTrip <- dtCumulativeRasterPaths[trip_ID==thisTrip]
  dt25 <- dtTrip[percentile=="25th"]
  dt50 <- dtTrip[percentile=="50th"]
  dt75 <- dtTrip[percentile=="75th"]
  dt100 <- dtTrip[percentile=="100th"]
  
  polygon25 <- dt25$polygons[[1]]
  polygon50 <- dt50$polygons[[1]]
  polygon75 <- dt75$polygons[[1]]
  polygon100 <- dt100$polygons[[1]]
  
  convexHullTrip <- dt100$convexHulls[[1]]
  
  bboxTripBuffer<- st_bbox(dt100$polygons[[1]])
  xNudgeBuffer <- (bboxTripBuffer$xmax-bboxTripBuffer$xmin)*0.05
  yNudgeBuffer <- (bboxTripBuffer$ymax-bboxTripBuffer$ymin)*0.05
  
  plotBuffer <- ggplot(data = theMap) +
    geom_sf(color = NA, fill = "#1976D2")+
    geom_sf(data = polygon100, color = NA, fill = "#FDD835")+
    geom_sf(data = polygon75, color = NA, fill = "#AFB42B")+
    geom_sf(data = polygon50, color = NA, fill = "#689F38")+
    geom_sf(data = polygon25, color = NA, fill = "#388E3C")+
    geom_sf(data=convexHullTrip, color = "red", fill="red", alpha = 0.25, size = 1)+
    coord_sf(xlim = c(bboxTripBuffer$xmin-xNudgeBuffer, bboxTripBuffer$xmax+(xNudgeBuffer)),
             ylim = c(bboxTripBuffer$ymin-yNudgeBuffer, bboxTripBuffer$ymax+(yNudgeBuffer)),
             expand = TRUE)+
    xlab("Longitude") + ylab("Latitude")
  
  # plot points, modify shape of point by effort area
  bboxTrip<- st_bbox(convexHullTrip)
  
  plotGPS <- ggplot(data = theMap) +
    geom_sf(color = NA, fill = "#1976D2")+
    geom_sf(data = polygon100, color = NA, fill = "#FDD835")+
    geom_sf(data = polygon75, color = NA, fill = "#AFB42B")+
    geom_sf(data = polygon50, color = NA, fill = "#689F38")+
    geom_sf(data = polygon25, color = NA, fill = "#388E3C")+
    geom_sf(data=convexHullTrip, color= "red", fill="red", alpha = 0.25, size = 1)+
    #geom_sf(data=polygonTrip, fill=NA, color = "white", size = 1, alpha = .5)+
    #geom_sf(data=thesePoints, aes(shape=area, color = area))+
    coord_sf(xlim = c(bboxTrip$xmin, bboxTrip$xmax),
             ylim = c(bboxTrip$ymin, bboxTrip$ymax),
             expand = TRUE)+
    xlab("Longitude") + ylab("Latitude")
  
  # merge plots
  
  p <- plot_grid(plotBuffer, plotGPS, nrow = 2) #
  #labels="AUTO") #c('All VTR buffers for trip', 'Zoomed in on all GPS points from trip')
  
  title <- ggdraw() +
    draw_label(paste0("Trip:", thisTrip,
                      "\nVTR buffers: yellow | SF convex hull: red")
               , fontface='bold')
  
  plotTrip <- plot_grid(title, p, ncol=1,rel_heights=c(0.1, 1) ) # rel_heights values control title margins
  
  fileName <- paste("output/tripImages/vtrbANDsfch/",thisTrip, sep = "_")
  fileName <- paste(fileName, ".png", sep = "")
  save_plot(filename = fileName, plot = plotTrip)
}

##############################
#Identify overlapping areas for cumulative rasters
intersectionArea <- mapply(function(vtrb, sfch) st_intersection(vtrb, sfch[1]),
                           dtCumulativeRasterPaths$polygons, dtCumulativeRasterPaths$convexHulls) 

dtCumulativeRasterPaths <-cbind(dtCumulativeRasterPaths, intersectsTest)
#saveRDS(dtCumulativeRasterPaths, "output/dtCumulativeRasterPaths.rds")

areaCumulativeVTRB <- lapply(X = dtCumulativeRasterPaths$polygons, FUN = st_area)
areaConvexHull <- lapply(X = dtCumulativeRasterPaths$convexHulls, FUN = st_area)
dtCumulativeRasterPaths <-cbind(dtCumulativeRasterPaths, areaCumulativeVTRBM2 = unlist(areaCumulativeVTRB))
dtCumulativeRasterPaths <-cbind(dtCumulativeRasterPaths, areaConvexHullM2 = unlist(areaConvexHull))
dtCumulativeRasterPaths[, areaDiffVTRBminusCH := areaCumulativeVTRBM2 - areaConvexHullM2]

#saveRDS(dtCumulativeRasterPaths, "output/dtCumulativeRasterPaths.rds")
