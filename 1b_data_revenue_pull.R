library(keyring)
library(ROracle)
library(tidyverse)
library(data.table)

# This script pulls revenue and catch data for Longfin Squid
# Then sums revenue by  year, f.nespp3, s.sppnm , s.fmp, IMGID
# We adjust revenue to 2019 GDP by multiplying revenue by the inverse of the 2019 deflator value
# Exports file as "/dt_revenue.rds"

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
# connect using environment with user name, database specifics, and keyring
usr <- c("ljacobson")
drv <- dbDriver("Oracle")
host <- "sole.nefsc.noaa.gov"
port <- 1526
sid <- "sole"
connect.string <- paste(
  "(DESCRIPTION=",
  "(ADDRESS=(PROTOCOL=tcp)(HOST=", host, ")(PORT=", port, "))",
  "(CONNECT_DATA=(SID=", sid, ")))", sep = "")
con <- dbConnect(drv, username = usr,
                 password = keyring::key_get(service = sid, username = usr),
                 dbname = connect.string)
###########################
#Setting the role - andy used this to access the FVTR schema -not sure if its needed
dbGetQuery(con,"SET ROLE ALL")

#test access
wind_head <- dbGetQuery(con,"SELECT * FROM APSD.ALL_WEA_2008_2019_070221@garfo_nefsc.world WHERE ROWNUM < 5")
fish_head <- dbGetQuery(con,"SELECT * FROM apsd.dmis_wind_test@garfo_nefsc.world  WHERE ROWNUM < 5")
species_head <- dbGetQuery(con,"SELECT * FROM apsd.nespp3_fmp@garfo_nefsc.world WHERE ROWNUM < 5") 

#look at column names
wind_names <- names(wind_head)
fish_names <- names(fish_head)
species_names <- names(species_head)

remove(wind_head, fish_head, species_head)
###########################
# Get GDP values adjusted to 2019 annual
GDPDEF_annual <- offshoreWind::GDPDEF_annual %>% 
  dplyr::select(GDPDEF, Year) #  reduce columns

y = 2019
y_gdp = GDPDEF_annual$GDPDEF[GDPDEF_annual$Year == y]

GDPDEF_annual <- GDPDEF_annual %>% 
  mutate(GDP_2019 = GDPDEF/y_gdp
         , YEAR = as.character(Year)) %>% 
  dplyr::select(YEAR, GDP_2019)

###########################
# get FMP/species list
# add link for all apsd - @garfo_nefsc.world
#SPECIES = tbl(con, sql('select * from apsd.nespp3_fmp@garfo_nefsc.world')) %>% 
#  collect()

###########################
# make a summary by species
dt_prep = tbl(con, sql("
SELECT
        year
        , imgid
      , f.nespp3
      , nvl (SUM (dollar), 0) AS dollar_total
      , nvl (SUM (landed), 0) AS landed_total
      , nvl (SUM (trip_length), 0) AS days_total
      , s.sppnm
      , s.fmp
    FROM
        apsd.dmis_wind_test@garfo_nefsc.world f
        LEFT JOIN apsd.nespp3_fmp@garfo_nefsc.world s
        ON f.nespp3 = s.nespp3
    WHERE
        sppnm = 'LONGFIN SQUID'
    GROUP BY
        year
        , f.nespp3
        , s.sppnm
        , s.fmp
        , IMGID
"))

dt_revenue = dt_prep %>% 
  collect()

length(unique(dt_revenue$IMGID))

# Add GDP deflator value
dt_revenue <- dt_revenue %>% 
  left_join(., GDPDEF_annual, by = 'YEAR') %>% 
  mutate(value_gdp = DOLLAR_TOTAL/GDP_2019)

dt_revenue %>% 
  arrange(desc(value_gdp))

dt_revenue %>% 
  arrange(desc(DOLLAR_TOTAL))

saveRDS(dt_revenue, paste0(dir_output,"/dt_revenue.rds"))

dt_revenue <- setDT(readRDS( paste0(dir_output,"/dt_revenue.rds")))
