
library(keyring)
library(ROracle)
library(tidyverse)
#library(lubridate)
#library(MASS)
#library(DBI)
#library(offshoreWind)

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

wind_names <- names(wind_head)
fish_names <- names(fish_head)
species_names <- names(species_head)

#from Ben:
# Year = year the trips happened (end of trip)
# inside = perc = percentage of trip in that wind energy area
# X.O = I don't recall and just drop it :)

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
a = tbl(con, sql("
WITH wind AS (
    SELECT
        year
      , area
      , idnum AS imgid
      , SUM (perc) perc
    FROM
        apsd.all_wea_2008_2019_070221@garfo_nefsc.world
    GROUP BY
        year
      , area
      , idnum
), fish_total AS (
    SELECT
        year
      , f.nespp3
      , nvl (round (SUM (dollar), 0), 0) AS dollar_total
      , nvl (round (SUM (landed), 0), 0) AS landed_total
      , nvl (round (SUM (trip_length), 0), 0) AS days_total
      , CASE
            WHEN sppnm LIKE '%COD%' THEN 'COD'
            WHEN sppnm LIKE '%MONK%' THEN 'MONK'
            WHEN sppnm LIKE '%WEAKFISH%' THEN 'WEAKFISH'
            ELSE sppnm
          END AS sppnm
    FROM
        apsd.dmis_wind_test@garfo_nefsc.world f
        LEFT JOIN apsd.nespp3_fmp@garfo_nefsc.world s
        ON f.nespp3 = s.nespp3
    WHERE
        sppnm IS NOT NULL
    GROUP BY
        year
      , f.nespp3
      , CASE
                WHEN sppnm LIKE '%COD%' THEN 'COD'
                WHEN sppnm LIKE '%MONK%' THEN 'MONK'
                WHEN sppnm LIKE '%WEAKFISH%' THEN 'WEAKFISH'
                ELSE sppnm
        END
), fish_all AS (
    SELECT
        f.*
      , s.sppnm
      , s.fmp
    FROM
        apsd.dmis_wind_test@garfo_nefsc.world f
        LEFT JOIN apsd.nespp3_fmp@garfo_nefsc.world s
        ON f.nespp3 = s.nespp3
), fish_and_wind AS (
    SELECT
        fish_all.year
      , wind.area -- add this if doing multiple areas
      , fish_all.nespp3
      , CASE
            WHEN sppnm LIKE '%COD%' THEN 'COD'
            WHEN sppnm LIKE '%MONK%' THEN 'MONK'
            WHEN sppnm LIKE '%WEAKFISH%' THEN 'WEAKFISH'
            ELSE sppnm
          END AS sppnm
      , COUNT (DISTINCT (permit)) AS npermits
      , COUNT (DISTINCT (dealnum)) AS ndealers
      , COUNT (DISTINCT (docid)) AS ntrips
      , COUNT (DISTINCT (wind.imgid)) AS nsubtrips
      , nvl (round (SUM (fish_all.trip_length * wind.perc), 3), 0) AS wea_days_fished
      , nvl (round (SUM (wind.perc * fish_all.dollar), 0), 0) AS wea_dollar_total
      , nvl (round (SUM (wind.perc * fish_all.landed), 0), 0) AS wea_landed_total
    FROM
        wind
        LEFT JOIN fish_all
        ON wind.imgid = fish_all.imgid
    WHERE
        fish_all.sppnm IS NOT NULL
    GROUP BY
        fish_all.year
      , wind.area
      , fish_all.nespp3
      , fish_all.sppnm
)
SELECT
    fish_and_wind.*
  , fish_total.dollar_total AS fishery_dollar_total
  , fish_total.landed_total AS fishery_landed_total
  , fish_total.days_total AS fishery_days_total
FROM
    fish_and_wind
    LEFT JOIN fish_total
    ON (fish_and_wind.year = fish_total.year AND fish_and_wind.nespp3 = fish_total.nespp3)
WHERE
        wea_dollar_total > 0 AND wea_landed_total > 0 AND wea_days_fished > 0
ORDER BY
    wea_dollar_total DESC
"))

a_collect = a %>% 
  collect()

a_collect = a_collect %>% 
  mutate(high = WEA_DOLLAR_TOTAL >=5e4
         , CONF = NDEALERS < 3 | NPERMITS < 3
         , FISHERY_DOLLAR_PERCENTAGE = WEA_DOLLAR_TOTAL/FISHERY_DOLLAR_TOTAL
         , FISHERY_LANDED_PERCENTAGE = WEA_LANDED_TOTAL/FISHERY_LANDED_TOTAL
         , FISHERY_DAYS_PERCENTAGE = WEA_DAYS_FISHED/FISHERY_DAYS_TOTAL
  ) %>%
  mutate(SPPNM_CONF = ifelse(CONF == T, 'ALL_OTHERS', SPPNM))

# keep this
a_collect$SPPNM_CONF[is.na(a_collect$SPPNM_CONF)] = 'ALL_OTHERS'


# add FMP
a_collect = a_collect %>% 
  left_join(., SPECIES, by = 'NESPP3')

all_others = a_collect %>% 
  filter(SPPNM_CONF == 'ALL_OTHERS') %>% 
  mutate(FMP = "") %>% 
  group_by(SPPNM_CONF, FMP, YEAR, AREA) %>% 
  summarise(value = round(sum(WEA_DOLLAR_TOTAL, na.rm = T))
            , landings = round(sum(WEA_LANDED_TOTAL, na.rm = T))
            , FISHERY_DOLLAR_PERCENTAGE = NA
            , FISHERY_LANDED_PERCENTAGE = NA
            # , FISHERY_DAYS_PERCENTAGE = NA
  ) %>% 
  rename(SPPNM = SPPNM_CONF) %>% 
  ungroup()

b = a_collect %>% 
  filter(SPPNM_CONF != 'ALL_OTHERS') %>% 
  group_by(SPPNM_CONF, FMP, YEAR, AREA, CONF) %>%
  mutate(value = WEA_DOLLAR_TOTAL
         , landings = WEA_LANDED_TOTAL
         , FISHERY_DOLLAR_PERCENTAGE =  FISHERY_DOLLAR_PERCENTAGE
         , FISHERY_LANDED_PERCENTAGE = FISHERY_LANDED_PERCENTAGE
         # , FISHERY_DAYS_PERCENTAGE = FISHERY_DAYS_PERCENTAGE
  ) %>% 
  ungroup() %>% 
  dplyr::select(YEAR
                , AREA
                , SPPNM
                , FMP
                , value
                , landings
                , FISHERY_DOLLAR_PERCENTAGE
                , FISHERY_LANDED_PERCENTAGE
                # , FISHERY_DAYS_PERCENTAGE
  ) %>% 
  rbind(all_others) %>% 
  filter(!is.na(YEAR))

# Add GDP deflator value
b <- b %>% 
  left_join(., GDPDEF_annual, by = 'YEAR') %>% 
  mutate(value_gdp = value/GDP_2019)

b %>% 
  arrange(desc(value_gdp))

b %>% 
  arrange(desc(value))

###########################
# make a summary by gear

aa = tbl(con, sql("
  WITH wind AS (
    SELECT
        year
      , area
      , idnum AS imgid
      , SUM (perc) perc
    FROM
        apsd.all_wea_2008_2019_070221@garfo_nefsc.world
    GROUP BY
        year
      , area
      , idnum
), fish_total AS (
    SELECT
        year
      , nvl (round (SUM (dollar), 0), 0) AS dollar_total
      , nvl (round (SUM (landed), 0), 0) AS landed_total
      , nvl (round (SUM (trip_length), 0), 0) AS days_total
      , secgearfish
      , gearcode
    FROM
        apsd.dmis_wind_test@garfo_nefsc.world
    GROUP BY
        year
      , secgearfish
      , gearcode
), fish_all AS (
    SELECT
        f.*
      , s.sppnm
      , s.fmp
    FROM
        apsd.dmis_wind_test@garfo_nefsc.world f
        LEFT JOIN apsd.nespp3_fmp@garfo_nefsc.world s
        ON f.nespp3 = s.nespp3
), fish_and_wind AS (
    SELECT
        fish_all.year
      , wind.area -- add this if doing multiple areas
      , fish_all.gearcode
      , fish_all.secgearfish
      , COUNT (DISTINCT (permit)) AS npermits
      , COUNT (DISTINCT (dealnum)) AS ndealers
      , COUNT (DISTINCT (docid)) AS ntrips
      , COUNT (DISTINCT (wind.imgid)) AS nsubtrips
      , nvl (round (SUM (fish_all.trip_length * wind.perc), 3), 0) AS wea_days_fished
      , nvl (round (SUM (wind.perc * fish_all.dollar), 0), 0) AS wea_dollar_total
      , nvl (round (SUM (wind.perc * fish_all.landed), 0), 0) AS wea_landed_total
    FROM
        wind
        LEFT JOIN fish_all
        ON wind.imgid = fish_all.imgid
    GROUP BY
        fish_all.year
      , wind.area
      , fish_all.secgearfish
      , fish_all.gearcode
)
SELECT
    fish_and_wind.*
  , fish_total.dollar_total AS fishery_dollar_total
  , fish_total.landed_total AS fishery_landed_total
  , fish_total.days_total AS fishery_days_total
FROM
    fish_and_wind
    LEFT JOIN fish_total
    ON (fish_and_wind.year = fish_total.year AND fish_and_wind.secgearfish = fish_total.secgearfish)
WHERE
        wea_dollar_total > 0 AND wea_landed_total > 0 AND wea_days_fished > 0
ORDER BY
    wea_dollar_total DESC
  "
))

aa_collect = aa %>% 
  collect()

# aa_collect$GEARCODE[aa_collect$GEARCODE == 'dredge'] = 'DREDGE'

aa_collect = aa_collect %>% 
  # case_when(GEARCODE == 'dredge' ~ 'DREDGE')  %>% 
  mutate(high = WEA_DOLLAR_TOTAL >=5e4
         , FISHERY_DOLLAR_PERCENTAGE = WEA_DOLLAR_TOTAL/FISHERY_DOLLAR_TOTAL
         , FISHERY_LANDED_PERCENTAGE = WEA_LANDED_TOTAL/FISHERY_LANDED_TOTAL
         , FISHERY_DAYS_PERCENTAGE = WEA_DAYS_FISHED/FISHERY_DAYS_TOTAL
         , CONF = NDEALERS < 3 | NPERMITS < 3) %>%
  mutate(GEAR_CONF = ifelse(CONF == T, 'ALL_OTHERS', GEARCODE))

all_others = aa_collect %>% 
  filter(GEAR_CONF == 'ALL_OTHERS') %>% 
  group_by(GEAR_CONF, YEAR, AREA) %>% 
  summarise(value = round(sum(WEA_DOLLAR_TOTAL, na.rm = T))
            , landings = round(sum(WEA_LANDED_TOTAL, na.rm = T))
            , FISHERY_DOLLAR_PERCENTAGE = NA
            , FISHERY_LANDED_PERCENTAGE = NA
            # , FISHERY_DAYS_PERCENTAGE = NA
  ) %>% 
  rename(GEARCODE = GEAR_CONF) %>% 
  ungroup()

bb = aa_collect %>% 
  filter(GEAR_CONF != 'ALL_OTHERS') %>% 
  group_by(GEAR_CONF, YEAR, AREA, CONF) %>%
  mutate(value = WEA_DOLLAR_TOTAL
         , landings = WEA_LANDED_TOTAL
         , FISHERY_DOLLAR_PERCENTAGE =  FISHERY_DOLLAR_PERCENTAGE
         , FISHERY_LANDED_PERCENTAGE = FISHERY_LANDED_PERCENTAGE
         # , FISHERY_DAYS_PERCENTAGE = FISHERY_DAYS_PERCENTAGE
  ) %>% 
  ungroup() %>% 
  dplyr::select(YEAR
                , AREA
                , GEARCODE
                , value
                , landings
                , FISHERY_DOLLAR_PERCENTAGE
                , FISHERY_LANDED_PERCENTAGE
                # , FISHERY_DAYS_PERCENTAGE
  ) %>% 
  rbind(all_others) %>% 
  filter(!is.na(YEAR))

bb = bb %>% 
  filter(!is.na(YEAR)) %>% 
  left_join(., GDPDEF_annual, by = 'YEAR') %>% 
  mutate(value_gdp = value/GDP_2019)

###########################
# summarise by port

aaa = tbl(con, sql("
WITH wind AS (
    SELECT
        year
      , area
      , idnum AS imgid
      , SUM (perc) perc
    FROM
        apsd.all_wea_2008_2019_070221@garfo_nefsc.world
    GROUP BY
        year
      , area
      , idnum
), fish_total AS (
    SELECT
        year
      , nvl (round (SUM (dollar), 0), 0) AS dollar_total
      , nvl (round (SUM (landed), 0), 0) AS landed_total
      , nvl (round (SUM (trip_length), 0), 0) AS days_total
      , vtr_port
    FROM
        apsd.dmis_wind_test@garfo_nefsc.world
    GROUP BY
        year
      , vtr_port
), fish_all AS (
    SELECT
        f.*
      , s.sppnm
      , s.fmp
    FROM
        apsd.dmis_wind_test@garfo_nefsc.world f
        LEFT JOIN apsd.nespp3_fmp@garfo_nefsc.world s
        ON f.nespp3 = s.nespp3
), fish_and_wind AS (
    SELECT
        fish_all.year
      , wind.area -- add this if doing multiple areas
      , fish_all.vtr_port
      , COUNT (DISTINCT (permit)) AS npermits
      , COUNT (DISTINCT (dealnum)) AS ndealers
      , COUNT (DISTINCT (docid)) AS ntrips
      , COUNT (DISTINCT (wind.imgid)) AS nsubtrips
      , nvl (round (SUM (fish_all.trip_length * wind.perc), 3), 0) AS wea_days_fished
      , nvl (round (SUM (wind.perc * fish_all.dollar), 0), 0) AS wea_dollar_total
      , nvl (round (SUM (wind.perc * fish_all.landed), 0), 0) AS wea_landed_total
    FROM
        wind
        LEFT JOIN fish_all
        ON wind.imgid = fish_all.imgid
    GROUP BY
        fish_all.year
      , wind.area
      , fish_all.vtr_port
)
SELECT
    fish_and_wind.*
  , fish_total.dollar_total AS fishery_dollar_total
  , fish_total.landed_total AS fishery_landed_total
  , fish_total.days_total AS fishery_days_total
FROM
    fish_and_wind
    LEFT JOIN fish_total
    ON (fish_and_wind.year = fish_total.year AND fish_and_wind.vtr_port = fish_total.vtr_port)
WHERE
        wea_dollar_total > 0 AND wea_landed_total > 0 AND wea_days_fished > 0
ORDER BY
    wea_dollar_total DESC
")

aaa_collect = aaa %>% collect()

aaa_collect = aaa_collect %>% 
  mutate(high = WEA_DOLLAR_TOTAL >=5e4
         , FISHERY_DOLLAR_PERCENTAGE = WEA_DOLLAR_TOTAL/FISHERY_DOLLAR_TOTAL
         , FISHERY_LANDED_PERCENTAGE = WEA_LANDED_TOTAL/FISHERY_LANDED_TOTAL
         , FISHERY_DAYS_PERCENTAGE = WEA_DAYS_FISHED/FISHERY_DAYS_TOTAL
         , CONF = NDEALERS < 3 | NPERMITS < 3) %>%
  mutate(PORT_CONF = ifelse(CONF == T, 'ALL_OTHERS', VTR_PORT)) 


all_others = aaa_collect %>% 
  filter(PORT_CONF == 'ALL_OTHERS') %>% 
  group_by(PORT_CONF, YEAR, AREA) %>% 
  summarise(value = round(sum(WEA_DOLLAR_TOTAL, na.rm = T))
            , landings = round(sum(WEA_LANDED_TOTAL, na.rm = T))
            , PORT_DOLLAR_PERCENTAGE = NA
            , PORT_LANDED_PERCENTAGE = NA
            # , FISHERY_DAYS_PERCENTAGE = NA
  ) %>% 
  rename(VTR_PORT = PORT_CONF) %>% 
  ungroup()

bbb = aaa_collect %>% 
  filter(PORT_CONF != 'ALL_OTHERS') %>% 
  group_by(PORT_CONF, YEAR, AREA, CONF) %>%
  mutate(value = WEA_DOLLAR_TOTAL
         , landings = WEA_LANDED_TOTAL
         , PORT_DOLLAR_PERCENTAGE =  FISHERY_DOLLAR_PERCENTAGE
         , PORT_LANDED_PERCENTAGE = FISHERY_LANDED_PERCENTAGE
         # , FISHERY_DAYS_PERCENTAGE = FISHERY_DAYS_PERCENTAGE
  ) %>% 
  ungroup() %>% 
  dplyr::select(YEAR
                , AREA
                , VTR_PORT
                , value
                , landings
                , PORT_DOLLAR_PERCENTAGE
                , PORT_LANDED_PERCENTAGE
                # , FISHERY_DAYS_PERCENTAGE
  ) %>% 
  rbind(all_others) %>% 
  filter(!is.na(YEAR))


bbb = bbb %>% 
  filter(!is.na(YEAR))%>% 
  left_join(., GDPDEF_annual, by = 'YEAR') %>% 
  mutate(value_gdp = value/GDP_2019)

bbb %>% arrange(desc(value))

###########################
# Summarise by state

aaaa = tbl(con, sql("
WITH wind AS (
    SELECT
        year
      , area
      , idnum AS imgid
      , SUM (perc) perc
    FROM
        apsd.all_wea_2008_2019_070221@garfo_nefsc.world
    GROUP BY
        year
      , area
      , idnum
), fish_total AS (
    SELECT
        year
      , nvl (round (SUM (dollar), 0), 0) AS dollar_total
      , nvl (round (SUM (landed), 0), 0) AS landed_total
      , nvl (round (SUM (trip_length), 0), 0) AS days_total
      , vtr_state
    FROM
        apsd.dmis_wind_test@garfo_nefsc.world
    GROUP BY
        year
      , vtr_state
), fish_all AS (
    SELECT
        f.*
      , s.sppnm
      , s.fmp
    FROM
        apsd.dmis_wind_test@garfo_nefsc.world f
        LEFT JOIN apsd.nespp3_fmp@garfo_nefsc.world s
        ON f.nespp3 = s.nespp3
), fish_and_wind AS (
    SELECT
        fish_all.year
      , wind.area -- add this if doing multiple areas
      , fish_all.vtr_state
      , COUNT (DISTINCT (permit)) AS npermits
      , COUNT (DISTINCT (dealnum)) AS ndealers
      , COUNT (DISTINCT (docid)) AS ntrips
      , COUNT (DISTINCT (wind.imgid)) AS nsubtrips
      , nvl (round (SUM (fish_all.trip_length * wind.perc), 3), 0) AS wea_days_fished
      , nvl (round (SUM (wind.perc * fish_all.dollar), 0), 0) AS wea_dollar_total
      , nvl (round (SUM (wind.perc * fish_all.landed), 0), 0) AS wea_landed_total
    FROM
        wind
        LEFT JOIN fish_all
        ON wind.imgid = fish_all.imgid
    GROUP BY
        fish_all.year
      , wind.area
      , fish_all.vtr_state
)
SELECT
    fish_and_wind.*
  , fish_total.dollar_total AS fishery_dollar_total
  , fish_total.landed_total AS fishery_landed_total
  , fish_total.days_total AS fishery_days_total
FROM
    fish_and_wind
    LEFT JOIN fish_total
    ON (fish_and_wind.year = fish_total.year AND fish_and_wind.vtr_state = fish_total.vtr_state)
WHERE
        wea_dollar_total > 0 AND wea_landed_total > 0 AND wea_days_fished > 0
ORDER BY
    wea_dollar_total DESC
")
)

aaaa_collect = aaaa %>% collect()

aaaa_collect = aaaa_collect %>% 
  mutate(high = WEA_DOLLAR_TOTAL >=5e4
         , FISHERY_DOLLAR_PERCENTAGE = WEA_DOLLAR_TOTAL/FISHERY_DOLLAR_TOTAL
         , FISHERY_LANDED_PERCENTAGE = WEA_LANDED_TOTAL/FISHERY_LANDED_TOTAL
         , FISHERY_DAYS_PERCENTAGE = WEA_DAYS_FISHED/FISHERY_DAYS_TOTAL
         , CONF = NDEALERS < 3 | NPERMITS < 3) %>%
  mutate(STATE_CONF = ifelse(CONF == T, 'ALL_OTHERS', VTR_STATE)) 

all_others = aaaa_collect %>% 
  filter(STATE_CONF == 'ALL_OTHERS') %>% 
  group_by(STATE_CONF, YEAR, AREA) %>% 
  summarise(value = round(sum(WEA_DOLLAR_TOTAL, na.rm = T))
            , landings = round(sum(WEA_LANDED_TOTAL, na.rm = T))
            , STATE_DOLLAR_PERCENTAGE = NA
            , STATE_LANDED_PERCENTAGE = NA
            # , FISHERY_DAYS_PERCENTAGE = NA
  ) %>% 
  rename(VTR_STATE = STATE_CONF) %>% 
  ungroup()

bbbb = aaaa_collect %>% 
  filter(STATE_CONF != 'ALL_OTHERS') %>% 
  group_by(STATE_CONF, YEAR, AREA, CONF) %>%
  mutate(value = WEA_DOLLAR_TOTAL
         , landings = WEA_LANDED_TOTAL
         , STATE_DOLLAR_PERCENTAGE =  FISHERY_DOLLAR_PERCENTAGE
         , STATE_LANDED_PERCENTAGE = FISHERY_LANDED_PERCENTAGE
         # , FISHERY_DAYS_PERCENTAGE = FISHERY_DAYS_PERCENTAGE
  ) %>% 
  ungroup() %>% 
  dplyr::select(YEAR
                , AREA
                , VTR_STATE
                , value
                , landings
                , STATE_DOLLAR_PERCENTAGE
                , STATE_LANDED_PERCENTAGE
                # , FISHERY_DAYS_PERCENTAGE
  ) %>% 
  rbind(all_others) %>% 
  filter(!is.na(YEAR))

bbbb = bbbb %>% 
  # filter(!is.na(STATE_CONF)) %>% 
  filter(!is.na(YEAR))%>% 
  left_join(., GDPDEF_annual, by = 'YEAR') %>% 
  mutate(value_gdp = value/GDP_2019)

bbbb %>% arrange(desc(value))

###########################
# export_summaries
library(writexl)

out = list(Species = b #%>% filter(WEA == 'OCS-A 0520')
           , Port = bbb #%>% filter(WEA == 'OCS-A 0520')
           , Gear = bb #%>% filter(WEA == 'OCS-A 0520')
           , State = bbbb #%>% filter(WEA == 'OCS-A 0520')
           , allspecies = data.frame(SPECIES = unique(a_collect$SPPNM))
           , allports = data.frame(PORT = unique(aaa_collect$VTR_PORT))
           , allgears = data.frame(GEAR = unique(aa_collect$GEARCODE))
           , allstates = data.frame(STATE = unique(aaaa_collect$VTR_STATE))
)

writexl::write_xlsx(out, path = "ALL_WEA_SUMMARY_BY_AREA_1008221.xlsx")

save.image('ALL_WEA_SUMMARY_BY_AREA_100821.Rdata')
