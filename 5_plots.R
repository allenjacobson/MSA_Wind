# Loading packages
library(data.table)
library(sf)
library(dplyr)
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
#data prep

# estimate area of of sfch
sf_hulls_attributes <- sf_hulls_attributes %>%
  mutate(area = st_area(.) %>% as.numeric())

# eatimate area of vtrb
sf_vtrb_split_mosaic <- sf_vtrb_split_mosaic %>%
  mutate(area = st_area(.) %>% as.numeric())

# create new dt with both
dt_vtrb <- as.data.table(sf_vtrb_split_mosaic)
setnames(dt_vtrb, "area", "vtrb_area")

dt_sfch <- as.data.table(sf_hulls_attributes)
dt_sfch <- dt_sfch[, .(tripid_chr, area)]
setnames(dt_sfch, "area", "sfch_area")

#join two tables
setkey(dt_vtrb, tripid)
setkey(dt_sfch, tripid_chr)
dt_area <- dt_vtrb[dt_sfch, nomatch = 0]

# take the log ratio
dt_area[, log_ratio := log(vtrb_area/sfch_area)][
  , diff := vtrb_area - sfch_area][
  , log_over_under := ifelse(log_ratio > 1, "over", ifelse(log_ratio < 1, "under", "equal"))][
  , diff_over_under := ifelse(diff > 0, "over", ifelse(log_ratio < 0, "under", "equal"))]

dt_area$percentile <- factor(dt_area$percentile,
                             levels = c("25th", "50th", "75th", "100th"))

# prep bias dt
dt_bias <- as.data.table(sf_bias)

dt_bias <- dt_bias[type %in% c("false_positive", "false_negative"),
                        .(tripid, percentile, area, type)]
  
dt_bias$percentile <- factor(dt_bias$percentile,
                                  levels = c("25th", "50th", "75th", "100th"))

dt_bias_summary <- dt_bias[, .(total_bias = sum(area)), by = .(percentile, type)]


##############################
#plot area diff - over/underestimationggplot(data = dt_bias_size)+

(plot_ratio <- ggplot(data = dt_area) +
   geom_boxplot(aes(x=percentile, y=log_ratio, fill=percentile)))
   

(plot_log_ratio <- ggplot(data = dt_area) +
  stat_summary(aes(x = percentile, y = log_ratio, fill = percentile),
               fun.data=MinMeanSEMMax, geom="boxplot", show.legend = FALSE) + 
  theme_classic(base_size = 16)+
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  xlab("Percentile"))

(plot_hist_log <- ggplot(dt_area) +  
  geom_histogram( aes(x = log_ratio, fill = log_over_under),
                  position = "stack", boundary=1, bins = 15)+
  scale_fill_manual(values=c("black", "red"))+
  facet_wrap(~ percentile, nrow = 4))

(plot_hist_diff <- ggplot(dt_area) +  
    geom_histogram( aes(x = diff, fill = diff_over_under),
                    position = "stack", boundary=0, bins = 15)+
    scale_fill_manual(values=c("black", "red"))+
    facet_wrap(~ percentile, nrow = 4))

# plot mismatch
(plot_mismatch <- ggplot( data = dt_bias_summary)+
  geom_bar(aes(x = percentile, y = total_bias, fill = type), stat = "identity")+
  scale_fill_manual(values=c("red", "black"))+
  #scale_y_log10()+
  xlab("Percentile")+
  ylab("False Area")+
  labs(title = "False positives and negatives: cumulative area"))
  
##############################
# old plots

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


