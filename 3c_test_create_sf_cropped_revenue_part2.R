# Loading packages
library(data.table)
library(sf)
library(terra)
library(dplyr)

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
#sf_vtrbs <- readRDS(paste0(dir_output, "/sf_vtrb_cumulative_imgid.rds"))
sf_shapes <- readRDS(file = paste0(dir_output, "/sf_buffered_hulls_subtrip.rds"))
dt_revenue <- readRDS(paste0(dir_output, "/dt_paths_vtrb_split_matched_revenue.rds"))
dt_paths_cropped <- readRDS(file= paste0(dir_output, "/dt_paths_vtrb_revenue_cropped.rds"))
dt_paths <- readRDS(paste0(dir_output, "/dt_paths_vtrb_revenue.rds"))

##############################
# make this a new script

##############################
#cropped by wind energy areas
#dt_vtr_revenue <- readRDS(file= paste0(dir_output, "/dt_paths_vtrb_revenue_cropped.rds"))
dt_vtr_revenue <- readRDS(paste0(dir_output, "/dt_paths_vtrb_revenue.rds"))
dt_af_revenue <-readRDS(file= paste0(dir_output, "/dt_paths_af_revenue_rasters.rds"))
dt_revenue <- readRDS(paste0(dir_output, "/dt_paths_vtrb_split_matched_revenue.rds"))
sf_lease_areas_merged <- read_sf(paste0(dir_data, "/LeaseAreas_Merged_081621/LeaseAreas_Merged_081621.shp"))
sf_af <- readRDS(file = paste0(dir_output, "/sf_buffered_hulls_subtrip.rds"))
sf_vtr <- readRDS(paste0(dir_output, "/sf_vtrb_cumulative_imgid.rds"))
  
# select trips where the number of cells in each raster is the same
# this only removes 7 sub trips
# will need to go back later and get all to match - i think the problem is  b/c of the vtrs not overlapping with the af
dt_af_revenue <- dt_af_revenue[cell_test == TRUE]
dt_vtr_revenue <- dt_vtr_revenue[imgid %in% dt_af_revenue$imgid]

# confirm that the crs are the same for vtr and af
st_crs(sf_vtr) == st_crs(sf_af)
# transform wea to match crs for sf_af
sf_lease_areas_merged <- st_transform(sf_lease_areas_merged, st_crs(sf_af))

unique_trips <- unique(dt_af_revenue$imgid)
unique_wea <- unique(sf_lease_areas_merged$Lease_Numb)
unique_confidence <- unique(dt_vtr_revenue$confidence)

this_trip <- unique_trips[[1]]
this_wea <- unique_wea[[1]]
this_confidence <- unique_confidence[[1]]

dt_wea_revenue <- data.table()

for(this_wea in unique_wea){
  this_sf_wea <- sf_lease_areas_merged %>% filter(Lease_Numb == this_wea)
  this_vect_wea <- st_cast(st_union(this_sf_wea),"POLYGON") %>% st_union %>% vect()
  for(this_trip in unique_trips){  
    this_rast_af <- rast(dt_af_revenue[imgid == this_trip, paths])
    this_revenue_af <- extract(x = this_rast_af, y = this_vect_wea, exact = TRUE)%>%
      rename("value" = names(this_rast_af)) %>%
      mutate(cell_revenue = value*fraction)%>%
      summarise(revenue_af = sum(cell_revenue, na.rm = TRUE))  %>%
      unlist()
    all_revenue <- dt_revenue[imgid == this_trip & percentile == "90th", value_gdp]
    for(this_confidence in unique_confidence){
      this_path_vtr <-dt_vtr_revenue[imgid == this_trip & confidence == this_confidence, paths]
      this_rast_vtr <- rast(dt_vtr_revenue[imgid == this_trip & confidence == this_confidence, paths])
      this_revenue_vtr <- extract(x = this_rast_vtr, y = this_vect_wea, exact = TRUE) %>%
          rename("value" = names(this_rast_vtr)) %>%
          mutate(cell_revenue = value*fraction)%>%
          summarise(revenue_vtr = sum(cell_revenue, na.rm = TRUE)) %>%
          unlist()

      these_data <- data.table(imgid= this_trip,
                               confidence= this_confidence,
                               wea_lease_numb = this_wea,
                               revenue_af = this_revenue_af,
                               revenue_vtr = this_revenue_vtr,
                               revenue_subtrip = all_revenue)
      dt_wea_revenue <- rbindlist(list(dt_wea_revenue, these_data), fill=TRUE)
      }
    }
  }

dt_wea_revenue[, type_intersection := ifelse(revenue_af == 0 & revenue_vtr == 0,
                                             "none",
                                             ifelse(revenue_af > 0 & revenue_vtr > 0,
                                                    "both",
                                                    ifelse(revenue_af > 0 & revenue_vtr == 0,
                                                           "af_only",
                                                           ifelse(revenue_vtr > 0 & revenue_af == 0,
                                                                  "vtr_only",
                                                                  "error"))))]
saveRDS(object = dt_wea_revenue,
        file= paste0(dir_output, "/dt_wea_revenue.rds"))

##############################
# make this a new script
# plots
plot_both_zeros <- ggplot(dt_wea_revenue_zeros, aes(x = confidence))+
  geom_bar()

plot_af_zero_only <- ggplot(dt_wea_revenue_over, aes(x = confidence))+
  geom_bar()

plot_vtr_zero_only <- ggplot(dt_wea_revenue_under, aes(x = confidence))+
  geom_bar()

(plot_revenue_bias <- ggplot(dt_wea_revenue_filter) +  
    geom_histogram( aes(x = revenue_log_ratio, fill = log_over_under),
                    position = "stack", boundary=1, bins = 15)+
    scale_fill_manual(values=c("black", "red"))+
    facet_wrap(~ confidence, nrow = 1)+
    coord_flip()+
    ylab("Count: number of subtrips")+
    xlab("Revenue bias*")+
    labs(title = "Revenue bias for subtrips",
         subtitle = "Does VTR footprint over or underestimate revenue in active fishing footprint?",
         caption = expression(paste("*Bias = Ln",
                                    bgroup("(",
                                           over(Revenue[VTR],Revenue[ActiveFishing]),
                                           ")"))), 
         fill = NULL))

(plot_revenue_diff <- ggplot(dt_wea_revenue_filter) +  
    geom_histogram(aes(x = revenue_dif, fill = diff_over_under),
                   position = "stack", boundary=1, bins = 15)+
    scale_fill_manual(values=c("black", "red"))+
    facet_wrap(~ confidence, nrow = 1)+
    coord_flip()+
    #    scale_x_continuous(breaks = c(1000, 30000, 60000, 90000))+
    ylab("Count: number of subtrips")+
    xlab("Revenue difference*")+
    labs(title = "Revenue difference for subtrips",
         subtitle = "Does VTR footprint over or underestimate revenue in active fishing footprint?",
         fill = NULL))
