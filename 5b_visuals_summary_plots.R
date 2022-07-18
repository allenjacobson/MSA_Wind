# Loading packages
library(data.table)
library(sf)
#library(terra)
library(dplyr)
library(ggplot2)
library(patchwork)
#library(ggpubr)
#library(ggspatial)
library(stringr)

# This script creates multiple summary tables
# Creates summary plots
# Calculates summary values for text of manuscript

##############################
# Functions

##############################
# Set directories
pwd <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

repository <- "fishing_footprint_bias_for_wind"
path_base <- "C:/Users/lianne.allen-jacobso/Documents/"
check_pwd <- paste0(path_base, "Repositories/",repository)
pwd == check_pwd

dir_output <- paste0(path_base, "Output/", repository)
dir_data <- paste0(path_base, "Data/", repository)

##############################
# Pull in data
#dt_wea_revenue_overlap <- readRDS(paste0(dir_output, "/dt_wea_revenue_overlap.rds"))
dt_wea_revenue <- readRDS(file= paste0(dir_output, "/dt_wea_revenue.rds")) #from 3d_revenue_by_wea.R
sf_all_wea <- readRDS(paste0(dir_output, "/sf_all_wea.rds"))
dt_all_wea <- setDT(st_drop_geometry(sf_all_wea))
##############################
# Prep data
# create summary table by wea for VTRF at each percentile level, and for AFF
dt_wea_revenue_sums_25 <-
  dt_wea_revenue[confidence == "top1" & revenue_vtr > 0,
                 .(total_revenue_vtrf25 = sum(revenue_vtr), total_trips_vtrf25 = length(unique(imgid))),
                 by = .(wea_id)]
dt_wea_revenue_sums_50 <-
  dt_wea_revenue[confidence == "top2" & revenue_vtr > 0,
                 .(total_revenue_vtrf50 = sum(revenue_vtr), total_trips_vtrf50 = length(unique(imgid))),
                 by = .(wea_id)]
dt_wea_revenue_sums_75 <-
  dt_wea_revenue[confidence == "top3" & revenue_vtr > 0,
                 .(total_revenue_vtrf75 = sum(revenue_vtr), total_trips_vtrf75 = length(unique(imgid))),
                 by = .(wea_id)]
dt_wea_revenue_sums_90 <-
  dt_wea_revenue[confidence == "top4" & revenue_vtr > 0,
                 .(total_revenue_vtrf90 = sum(revenue_vtr), total_trips_vtrf90 = length(unique(imgid))),
                 by = .(wea_id)]
dt_wea_revenue_sums_af <-
  dt_wea_revenue[confidence == "top1" & revenue_af > 0,
                 .(total_revenue_aff = sum(revenue_af), total_trips_af = length(unique(imgid))),
                 by = .(wea_id)]

# Set Key for join
setkey(dt_wea_revenue_sums_25, wea_id)
setkey(dt_wea_revenue_sums_50, wea_id)
setkey(dt_wea_revenue_sums_75, wea_id)
setkey(dt_wea_revenue_sums_90, wea_id)
setkey(dt_wea_revenue_sums_af, wea_id)

#count intersection by wea
count_by_wea <- dt_wea_revenue[type_intersection != "none", .N, by = wea_id]
wea_no_intersection <- dt_all_wea[!(dt_all_wea$id %in% count_by_wea$wea_id)]
wea_no_intersection$id

# Iteratively full join to creat single data table
dt_wea_revenue_summary <- merge(dt_wea_revenue_sums_25,dt_wea_revenue_sums_50, all=TRUE)
dt_wea_revenue_summary <- merge(dt_wea_revenue_summary,dt_wea_revenue_sums_75, all=TRUE)
dt_wea_revenue_summary <- merge(dt_wea_revenue_summary,dt_wea_revenue_sums_90, all=TRUE)
dt_wea_revenue_summary <- merge(dt_wea_revenue_summary,dt_wea_revenue_sums_af, all=TRUE)

remove(dt_wea_revenue_sums_25, dt_wea_revenue_sums_50, dt_wea_revenue_sums_75, dt_wea_revenue_sums_90, dt_wea_revenue_sums_af)

#replace na with 0
dt_wea_revenue_summary[is.na(dt_wea_revenue_summary)] <- 0

#remove geometry from sf, select columns, set key, and join with summary table
dt_all_wea <- setDT(st_drop_geometry(sf_all_wea))
#dt_all_wea[, area_hectare := NULL]
setnames(dt_all_wea, old = "id", new = "wea_id")
setkey(dt_all_wea, wea_id)
dt_wea_revenue_summary <- merge(dt_wea_revenue_summary,dt_all_wea, all.x=TRUE)
setnames(dt_wea_revenue_summary, old = "total_trips_af", new = "total_trips_aff")

saveRDS(object = dt_wea_revenue_summary,
        file= paste0(dir_output, "/dt_wea_summary.rds"))

all_wea_area <- sum(dt_all_wea$area_m2)
units::set_units(x = all_wea_area, value = mi2)
dt_all_wea[, area_mi2 := units::set_units(x = dt_all_wea$area_m2, value = mi2)]
median(dt_all_wea$area_mi2)
min(dt_all_wea$area_mi2)
max(dt_all_wea$area_mi2)

# reshape from wide to long
dt_wea_revenue_summary[, total_trips_vtrf90 := as.numeric(total_trips_vtrf90)]

dt_wea_revenue_long <-
  melt(dt_wea_revenue_summary, id.vars = c("wea_id", "total_revenue_aff", "total_trips_aff", "area_m2"),
             measure.vars = c("total_revenue_vtrf25", "total_revenue_vtrf50", "total_revenue_vtrf75", "total_revenue_vtrf90"))
setnames(dt_wea_revenue_long, old = "value", new = "total_revenue")
setnames(dt_wea_revenue_long, old = "variable", new = "vtrf")
dt_wea_revenue_long[, vtrf := as.factor(vtrf)]
dt_wea_revenue_long[vtrf == "total_revenue_vtrf25", vtrf := "vtrf25"][
  vtrf == "total_revenue_vtrf50", vtrf := "vtrf50"][
    vtrf == "total_revenue_vtrf75", vtrf := "vtrf75"][
      vtrf == "total_revenue_vtrf90", vtrf := "vtrf90"]

dt_wea_trips_long <-
  melt(dt_wea_revenue_summary, id.vars = c("wea_id"),
       measure.vars = c("total_trips_vtrf25", "total_trips_vtrf50", "total_trips_vtrf75", "total_trips_vtrf90"))
setnames(dt_wea_trips_long, old = "value", new = "total_trips")
setnames(dt_wea_trips_long, old = "variable", new = "vtrf")
dt_wea_trips_long[vtrf == "total_trips_vtrf25", vtrf := "vtrf25"][
  vtrf == "total_trips_vtrf50", vtrf := "vtrf50"][
    vtrf == "total_trips_vtrf75", vtrf := "vtrf75"][
      vtrf == "total_trips_vtrf90", vtrf := "vtrf90"]

setkeyv(dt_wea_revenue_long, c("wea_id", "vtrf"))
setkeyv(dt_wea_trips_long, c("wea_id", "vtrf"))

dt_wea_summary_long <- merge(dt_wea_revenue_long, dt_wea_trips_long, all=TRUE)
setnames(dt_wea_summary_long, old = "total_revenue", new = "total_revenue_vtrf")
setnames(dt_wea_summary_long, old = "total_trips", new = "total_trips_vtrf")

remove(dt_wea_revenue_long, dt_wea_trips_long)
saveRDS(object = dt_wea_summary_long,
        file= paste0(dir_output, "/dt_wea_summary_long.rds"))

# calculate difference between aff and vtrf
# revenue difference
dt_wea_summary_long[, revenue_diff := total_revenue_vtrf-total_revenue_aff]
dt_wea_summary_long[, trips_diff := total_trips_vtrf-total_trips_aff]

##############################
# Rename things
# modify labels for "type intersection" for figure
dt_wea_revenue[, agreement :=
                 ifelse(type_intersection == "none" | type_intersection == "both",
                        "yes",
                        "no")]

dt_wea_revenue$agreement <-
  factor(dt_wea_revenue$agreement,
         levels = c("yes",
                    "no"))

dt_wea_revenue[type_intersection == "none", type_intersection := "neither Logbook- or Active-Fishing-Footprint"][
  type_intersection == "vtr_only", type_intersection := "only Logbook-Footprint"][
    type_intersection == "af_only", type_intersection := "only Active-Fishing-Footprint"][
      type_intersection == "both", type_intersection := "both Logbook- and Active-Fishing-Footprint"]

dt_wea_revenue$type_intersection <-
  factor(dt_wea_revenue$type_intersection,
         levels = c("neither Logbook- or Active-Fishing-Footprint",
                    "both Logbook- and Active-Fishing-Footprint",
                    "only Logbook-Footprint",
                    "only Active-Fishing-Footprint"))

dt_wea_revenue[confidence == "top1", confidence := "D) Logbook-Footprint restricted to 25th percentile"][
  confidence == "top2", confidence := "C) Logbook-Footprint restricted to 50th percentile"][
    confidence == "top3", confidence := "B) Logbook-Footprint restricted to 75th percentile"][
      confidence == "top4", confidence := "A) Logbook-Footprint restricted to 90th percentile"]

dt_wea_revenue$confidence <-
  factor(dt_wea_revenue$confidence,
         levels = c("A) Logbook-Footprint restricted to 90th percentile",
                    "B) Logbook-Footprint restricted to 75th percentile",
                    "C) Logbook-Footprint restricted to 50th percentile",
                    "D) Logbook-Footprint restricted to 25th percentile"))

# rename for figure wea figures

dt_wea_summary_long[vtrf == "vtrf25", vtrf := "D) Logbook-Footprint restricted to 25th percentile"][
  vtrf == "vtrf50", vtrf := "C) Logbook-Footprint restricted to 50th percentile"][
    vtrf == "vtrf75", vtrf := "B) Logbook-Footprint restricted to 75th percentile"][
      vtrf == "vtrf90", vtrf := "A) Logbook-Footprint restricted to 90th percentile"]

dt_wea_summary_long$vtrf <-
  factor(dt_wea_summary_long$vtrf,
         levels = c("A) Logbook-Footprint restricted to 90th percentile",
                    "B) Logbook-Footprint restricted to 75th percentile",
                    "C) Logbook-Footprint restricted to 50th percentile",
                    "D) Logbook-Footprint restricted to 25th percentile"))

# dt_wea_summary_long$vtrf <-
#    factor(dt_wea_summary_long$vtrf,
#           levels = c("vtrf90", "vtrf75", "vtrf50", "vtrf25"))

dt_wea_summary_long[, wea_1000 := ifelse(total_revenue_aff>1000,wea_id, "Other: exposure (AFF) for this WEA is <$1K")]

dt_wea_summary_long[wea_1000 == "Leased_OCS-A 0500_Bay State Wind LLC", wea_1000 := "Leased: Bay State Wind LLC (OCS-A 0500)"][
  wea_1000 == "Leased_OCS-A 0501_Vineyard Wind 1 LLC", wea_1000 := "Leased: Vineyard Wind 1 LLC (OCS-A 0501)"][
    wea_1000 == "Leased_OCS-A 0534_Park City Wind LLC", wea_1000 := "Leased: Park City Wind LLC (OCS-A 0534)"][
      wea_1000 == "Leased_OCS-A 0487_Sunrise Wind, LLC", wea_1000 := "Leased: Sunrise Wind, LLC (OCS-A 0487)"][
        wea_1000 == "Leased_OCS-A 0520_Beacon Wind LLC", wea_1000 := "Leased: Beacon Wind LLC (OCS-A 0520)"][
          wea_1000 == "Planning_NJ18-08_Central Atlantic Call Area B", wea_1000 := "Planning: Central Atlantic Call Area B (NJ18-08)"]
      

dt_wea_summary_long$wea_1000 <-
  factor(dt_wea_summary_long$wea_1000,
         levels = c("Leased: Bay State Wind LLC (OCS-A 0500)",
                    "Leased: Vineyard Wind 1 LLC (OCS-A 0501)",
                    "Leased: Park City Wind LLC (OCS-A 0534)",
                    "Leased: Sunrise Wind, LLC (OCS-A 0487)",
                    "Leased: Beacon Wind LLC (OCS-A 0520)",
                    "Planning: Central Atlantic Call Area B (NJ18-08)",
                    "Other: exposure (AFF) for this WEA is <$1K"))

names(dt_wea_summary_long)

# New facet label names for vtrf variable
# vtrf_labs <- c("A) Logbook-Footprint restricted to 90th percentile",
#                "B) Logbook-Footprint restricted to 75th percentile",
#                "C) Logbook-Footprint restricted to 50th percentile",
#                "D) Logbook-Footprint restricted to 25th percentile")
# names(vtrf_labs) <- c("vtrf90", "vtrf75", "vtrf50", "vtrf25")

##############################
# Plots
# frequency plots

dt_wea_frequency <- dt_wea_revenue[, .N, by = .(confidence, type_intersection, agreement)]

#dt_wea_frequency<- rbind(dt_wea_frequency, top_4_af)



(plot_footprints_intersecting_wea <- ggplot(dt_wea_frequency, aes(x=type_intersection, y=N, fill=agreement)) +
    geom_bar(stat='identity', position='dodge')+
    scale_fill_viridis_d(begin = 0, end = .7, option = "plasma", name= "Do analyses with both types of footprints concur?")+
    scale_y_log10(labels = function(x) format(x, scientific = FALSE),
                  limits=c(1, 25000),breaks=c(1, 10, 100, 1000, 10000)
    )+
    scale_x_discrete(labels = function(x) str_wrap(x, width = 20))+
    geom_text(aes(label = N), vjust = -0.2, size = 3)+
    ylab(paste0("Number of trips \n(by logbook identifer)"))+
    xlab("Footprint Intersects with wind farm")+
    theme(legend.position="bottom")+
    #theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=.5), legend.position="bottom")+
    #facet_grid(~ confidence)
    facet_grid(rows = vars(confidence), labeller = labeller(confidence = label_wrap_gen(21))))

height = 8
width = height

ggsave(filename = paste0(dir_output, "/plot_footprints_intersecting_wea.png"),
       plot = plot_footprints_intersecting_wea, width = width, height = height)



# revenue by WEA - and diff between two footprint methods
(plot_overlapping_revenue_diff <-
    ggplot(dt_wea_summary_long, aes(x = total_revenue_aff/1000,
                                       y = revenue_diff/1000)) +
    geom_abline(intercept = 0 , slope =0, color = "dark grey", size = .5)+
    geom_smooth(method = 'lm', color = "black")+#, fullrange = TRUE)+#, se=TRUE)+
    geom_point(aes(shape = wea_1000), fill = "black", size = 3, alpha= .75)+
    scale_shape_manual(name = "Wind Farm", values = c(21, 22, 23, 24, 25, 7, 1))+
    #scale_fill_viridis_d(direction = -1, option = "magma", end = .8, begin = .2)+
    facet_grid(cols = vars(vtrf), labeller = labeller(vtrf = label_wrap_gen(30)))+
#   facet_wrap( ~ vtrf, ncol=4, labeller = labeller(vtrf = vtrf_labs, label_wrap_gen(21)))+
    scale_y_continuous(labels = scales::dollar_format(prefix="$ ", suffix = " K"))+
    scale_x_continuous(labels = scales::dollar_format(prefix="$ ", suffix = " K"))+
    xlab(expression("Exposure"["AFF"]))+
    ylab(expression(atop("Revenue Difference (D)", "Exposure"["LF"]*" - Exposure"["AFF"])))+
    #scale_size_continuous(name = expression("Exposure"["AFF"]), labels = scales::dollar_format(prefix="$ ", suffix = " K"))+
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=.5),
          legend.position="bottom")+
    guides(shape = guide_legend(nrow = 4)))

width = 11
height = width*.4

ggsave(filename = paste0(dir_output, "/plot_overlapping_revenue_diff.png"),
       plot = plot_overlapping_revenue_diff,
       width = width, height = height)  

length(unique(dt_wea_summary_long$wea_id))
dt_wea_summary_long[, .N, by = .(vtrf, wea_1000)]

##############################
# Summary plot
dt_wea_revenue_intersect <- dt_wea_revenue[type_intersection != "neither Logbook- or Active-Fishing-Footprint"]
dt_wea_revenue_intersect[, revenue_af :=revenue_af/1000]
dt_wea_revenue_intersect[, revenue_vtr :=revenue_vtr/1000]


n_all_vtrf <- dt_wea_revenue_intersect[type_intersection != "only Active-Fishing-Footprint", .N, by = confidence]
n_all_aff <- dt_wea_revenue_intersect[type_intersection != "only Logbook-Footprint", .N, by = confidence]

sum_all_vtrf <- dt_wea_revenue_intersect[type_intersection != "only Active-Fishing-Footprint", lapply(.(revenue_vtr), sum), by = confidence]
sum_all_aff <- dt_wea_revenue_intersect[type_intersection != "only Logbook-Footprint", lapply(.(revenue_af), sum), by = confidence]

means_by_trip_vtrf <- dt_wea_revenue_intersect[type_intersection != "only Active-Fishing-Footprint", lapply(.(revenue_vtr), mean), by = confidence]
means_by_trip_aff <- dt_wea_revenue_intersect[type_intersection != "only Logbook-Footprint", lapply(.(revenue_af), mean), by = confidence]

se_by_trip_vtrf <-
  dt_wea_revenue_intersect[type_intersection != "only Active-Fishing-Footprint",
                            sapply(.(revenue_vtr),
                                   function(x) list(se = sd(x)/length(x))),
                            by = confidence]
se_by_trip_aff <-
  dt_wea_revenue_intersect[type_intersection != "only Logbook-Footprint",
                           sapply(.(revenue_af),
                                  function(x) list(se = sd(x)/length(x))),
                           by = confidence]
                           
summary_table <- data.table(
  type = c("Logbook-Footprint restricted to the",
           "Logbook-Footprint restricted to the",
           "Logbook-Footprint restricted to the",
           "Logbook-Footprint restricted to the",
           " "),
  footprint = c("90th %ile",
                "75th %ile",
                "50th %ile",
                "25th %ile",
                "AFF"),
  total_trips = append(n_all_vtrf$N, n_all_aff$N[1]),
  total_exposure = append(sum_all_vtrf$V1, sum_all_aff$V1[1]),
  mean_trip_exposure = append(means_by_trip_vtrf$V1, means_by_trip_aff$V1[1]),
  se_trip_exposure =append(se_by_trip_vtrf$se, se_by_trip_aff$se[1]))

summary_table$type <-
  factor(summary_table$type,
         levels = c("Logbook-Footprint restricted to the",
                    " "))

summary_table$footprint <-
  factor(summary_table$footprint,
         levels =c("90th %ile",
                   "75th %ile",
                   "50th %ile",
                   "25th %ile",
                   "AFF"))

plot_total_exposure <- ggplot(summary_table, aes(x = footprint, y = total_exposure))+
  geom_col()+
  xlab(NULL)+
  ylab("Total Exposure (all trips)")+
  facet_grid(~type, 
             scales = "free_x", # Let the x axis vary across facets.
             space = "free_x",  # Let the width of facets vary and force all bars to have the same width.
             switch = "x")+      # Move the facet labels to the bottom.)+
  scale_y_continuous(labels = scales::dollar_format(prefix="$ ", suffix = " K"))
  #+theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=.5), legend.position="bottom")

plot_total_trips <- ggplot(summary_table, aes(x = footprint, y = total_trips))+
  geom_col()+
  xlab(NULL)+
  ylab("Total # of Exposed Trips")+
  facet_grid(~type, 
           scales = "free_x", # Let the x axis vary across facets.
           space = "free_x",  # Let the width of facets vary and force all bars to have the same width.
           switch = "x")      # Move the facet labels to the bottom.)

plot_avg_trip_exposure <- ggplot(summary_table, aes(x = footprint, y = mean_trip_exposure))+
  geom_col()+
  geom_errorbar(aes(x=footprint,
                     ymin=mean_trip_exposure-se_trip_exposure,
                     ymax=mean_trip_exposure+se_trip_exposure),
                width = 0.2,
                size = .7)+
  xlab(NULL)+
  ylab("Average Exposure (per trip)")+
  facet_grid(~type, 
             scales = "free_x", # Let the x axis vary across facets.
             space = "free_x",  # Let the width of facets vary and force all bars to have the same width.
             switch = "x")+      # Move the facet labels to the bottom.)
  scale_y_continuous(labels = scales::dollar_format(prefix="$ ", suffix = " K"))

patchwork <- plot_total_trips/ plot_total_exposure / plot_avg_trip_exposure

patchwork[[2]] = patchwork[[2]] + theme(axis.text.x = element_blank(),
                                        axis.ticks.x = element_blank(),
                                        axis.title.x = element_blank(),
                                        strip.background = element_blank(),
                                        strip.text.x = element_blank())

patchwork[[1]] = patchwork[[1]] + theme(axis.text.x = element_blank(),
                                        axis.ticks.x = element_blank(),
                                        axis.title.x = element_blank(),
                                        strip.background = element_blank(),
                                        strip.text.x = element_blank())


summary_plot <- patchwork + plot_annotation(tag_levels = "A")

height = 11
width = height*.4

ggsave(filename = paste0(dir_output, "/summary_plot.png"),
       plot = summary_plot,
       width = width, height = height)  

##############################
# Fidelity calculations
dt_wea_frequency_wide<- dcast(dt_wea_frequency, confidence ~ type_intersection, value.var = "N")
setnames(dt_wea_frequency_wide, old = "neither VTRF or AFF", new = "neither")
setnames(dt_wea_frequency_wide, old = "both VTRF and AFF", new = "both")
setnames(dt_wea_frequency_wide, old = "only VTRF", new = "VTRF")
setnames(dt_wea_frequency_wide, old = "only AFF", new = "AFF")

dt_wea_frequency_wide[, fidelity := both/(both+AFF)]
