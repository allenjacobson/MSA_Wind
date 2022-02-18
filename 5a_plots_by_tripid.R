# Loading packages
library(data.table)
library(sf)
library(dplyr)
library(ggplot2)
library(cowplot)
library(stringr)


##############################
# Functions

# MinMeanSEMMax <- function(x) {
#   v <- c(min(x), mean(x) - sd(x)/sqrt(length(x)), mean(x), mean(x) + sd(x)/sqrt(length(x)), max(x))
#   names(v) <- c("ymin", "lower", "middle", "upper", "ymax")
#   v
# }

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
sf_hull <- readRDS(paste0(dir_output, "/sf_hulls_attributes_tripid.rds"))
sf_vtrb <- readRDS(paste0(dir_output, "/sf_vtrb_cumulative_tripid.rds"))
sf_bias <- readRDS(paste0(dir_output, "/sf_bias_tripid.rds"))

##############################
#data prep

# estimate area of of sfch
sf_hull <- sf_hull %>%
  mutate(area = st_area(.) %>% as.numeric())

# eatimate area of vtrb
sf_vtrb <- sf_vtrb %>%
  mutate(area = st_area(.) %>% as.numeric())

# create new dt with both
dt_vtrb <- as.data.table(sf_vtrb)
setnames(dt_vtrb, "area", "vtrb_area")

dt_sfch <- as.data.table(sf_hull)
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
                             levels = c("100th", "75th", "50th", "25th"))

# prep bias dt
dt_bias <- as.data.table(sf_bias)

dt_bias <- dt_bias[type %in% c("false_positive", "false_negative"),
                        .(tripid, percentile, area, type)]
  
dt_bias$percentile <- factor(dt_bias$percentile,
                             levels = c("100th", "75th", "50th", "25th"))

dt_bias_summary <- dt_bias[, .(total_bias = sum(area)), by = .(percentile, type)]

dt_bias_summary$type2 <- str_replace(dt_bias_summary$type, "_", " ")


##############################
#plot area diff - over/underestimationggplot(data = dt_bias_size)+

# (plot_ratio <- ggplot(data = dt_area) +
#    geom_boxplot(aes(x=percentile, y=log_ratio, fill=percentile)))
   

(plot_hist_log <- ggplot(dt_area) +  
  geom_histogram( aes(x = log_ratio, fill = log_over_under),
                  position = "stack", boundary=1, bins = 15)+
  scale_fill_manual(values=c("black", "red"))+
  facet_wrap(~ percentile, nrow = 1)+
  coord_flip()+
  ylab("Count: number of trips")+
  xlab("Size bias*")+
  labs(title = "Size bias",
       subtitle = "Does VTR footprint over or underestimate active fishing footprint?",
       caption = expression(paste("*Bias = Ln",
                                   bgroup("(",
                                          over(Area[VTR],Area[ActiveFishing]),
                                          ")"))), 
       fill = NULL))

width = 8
height = width*.618

ggsave(filename = paste0(dir_output, "/plot_hist_log.png"),
       plot = plot_hist_log, width = width, height = height)


# (plot_hist_diff <- ggplot(dt_area) +  
#     geom_histogram( aes(x = diff, fill = diff_over_under),
#                     position = "stack", boundary=0, bins = 15)+
#     scale_fill_manual(values=c("black", "red"))+
#     facet_wrap(~ percentile, nrow = 4)+
#     coord_flip()+
#     ylab("Count: number of trips")+
#     xlab("Bias in VTR Footprint*")+
#     labs(title = "Size bias",
#          subtitle = "Does VTR footprint over or underestimate active fishing footprint?",
#          caption = expression(paste("*Bias = ", Area[VTR] - Area[ActiveFishing])), 
#          fill = NULL))

# plot mismatch
(plot_mismatch <- ggplot( data = dt_bias_summary)+
  geom_bar(aes(x = percentile, y = total_bias, fill = type2), stat = "identity")+
  scale_fill_manual(values=c("red", "black"))+
  #scale_y_log10()+
  xlab("Percentile")+
  ylab("Mismatch*")+
  labs(title = "Mismatch",
       subtitle ="Can we minimize false positives and negatives?",
       caption = "*cumulative area across all trips",
       fill = NULL))


width = 8
height = width*.618

ggsave(filename = paste0(dir_output, "/plot_mismatch.png"),
       plot = plot_mismatch, width = width, height = height)

