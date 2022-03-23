
library(keyring)
library(ROracle)
library(tidyverse)
#library(lubridate)
#library(MASS)
#library(DBI)
#library(offshoreWind)

## target db for revenue

#select * from APSD.ALL_WEA_2008_2019_070221@garfo_nefsc.world
#select * from apsd.dmis_wind_test@garfo_nefsc.world

##############################
# connect using environment
usr <- c("ljacobson")
#pswd <- .rs.askForPassword('Password')

#Database specifics
drv <- dbDriver("Oracle")
host <- "sole.nefsc.noaa.gov"
port <- 1526
sid <- "sole"

#Putting all of that together
connect.string <- paste(
  "(DESCRIPTION=",
  "(ADDRESS=(PROTOCOL=tcp)(HOST=", host, ")(PORT=", port, "))",
  "(CONNECT_DATA=(SID=", sid, ")))", sep = "")

## Use username/password authentication.
con <- dbConnect(drv, username = usr,
                 password = keyring::key_get(service = sid, username = usr),
                 dbname = connect.string)

###########################
#Setting the role - andy used this to access the FVTR schema -not sure if its needed
dbGetQuery(con,"SET ROLE ALL")

#test access
#test1 <- dbGetQuery(con,"select * from APSD.ALL_WEA_2008_2019_070221@garfo_nefsc.world")
#test2 <- dbGetQuery(con,"select * from apsd.dmis_wind_test@garfo_nefsc.world")

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
SPECIES = tbl(con, sql('select * from apsd.nespp3_fmp@garfo_nefsc.world')) %>% 
  collect()

###########################
# make a summary by species
a = tbl(con, sql("
  WITH w as(
    SELECT
        year
        , area
        , idnum imgid
        , SUM(perc) perc
    FROM APSD.ALL_WEA_2008_2019_070221@garfo_nefsc.world
    GROUP BY
        year
        , area
        , idnum
)
, t as(
    SELECT
        year
        , nespp3
        , NVL(round(sum(dollar), 0),0) as dollar_total
        , NVL(round(sum(landed), 0),0) as landed_total
        , NVL(round(sum(trip_length), 0),0) as days_total
        , case when SPPNAME like '%COD%' then 'COD'
            when SPPNAME like '%MONK%' then 'MONK'
            when SPPNAME like '%WEAKFISH%' then 'WEAKFISH'
            else SPPNAME end as SPPNAME
    FROM apsd.dmis_wind_test@garfo_nefsc.world  
    WHERE SPPNAME is not NULL
    GROUP BY
        year
        , nespp3
        , case when SPPNAME like '%COD%' then 'COD'
            when SPPNAME like '%MONK%' then 'MONK'
            when SPPNAME like '%WEAKFISH%' then 'WEAKFISH'
            else SPPNAME end
)
, c as(
 SELECT *
 FROM apsd.dmis_wind_test@garfo_nefsc.world
)
, d as(
    SELECT 
        c.year
        , w.area -- add this if doing multiple areas
        , c.nespp3
        , case when SPPNAME like '%COD%' then 'COD'
            when SPPNAME like '%MONK%' then 'MONK'
            when SPPNAME like '%WEAKFISH%' then 'WEAKFISH'
            else SPPNAME end as SPPNAME
        , count(distinct(permit)) as npermits
        , count(distinct(dealnum)) as ndealers
        , count(distinct(docid)) as ntrips
        , count(distinct(w.imgid)) as nsubtrips
        , NVL(round(sum(c.trip_length*w.perc), 3), 0) as wea_days_fished
        , NVL(round(sum(w.perc*c.dollar), 0),0) as wea_dollar_total  
        , NVL(round(sum(w.perc*c.landed), 0),0) as wea_landed_total
    FROM  w
        LEFT JOIN c
          ON  w.IMGID = c.IMGID
    WHERE c.SPPNAME is not NULL
    GROUP BY
        c.year
        , w.area
        , c.nespp3
        , c.SPPNAME
)
SELECT
    d.*
    , t.dollar_total as fishery_dollar_total
    , t.landed_total as fishery_landed_total
    , t.days_total as fishery_days_total
FROM d
    LEFT JOIN t
      On (d.year = t.year AND d.nespp3 = t.nespp3) 
WHERE wea_dollar_total > 0
    AND wea_landed_total > 0
    AND wea_days_fished > 0
    ORDER BY wea_dollar_total desc
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
  mutate(SPPNAME_CONF = ifelse(CONF == T, 'ALL_OTHERS', SPPNAME))

# keep this
a_collect$SPPNAME_CONF[is.na(a_collect$SPPNAME_CONF)] = 'ALL_OTHERS'


# add FMP
a_collect = a_collect %>% 
  left_join(., SPECIES, by = 'NESPP3')

all_others = a_collect %>% 
  filter(SPPNAME_CONF == 'ALL_OTHERS') %>% 
  mutate(FMP = "") %>% 
  group_by(SPPNAME_CONF, FMP, YEAR, AREA) %>% 
  summarise(value = round(sum(WEA_DOLLAR_TOTAL, na.rm = T))
            , landings = round(sum(WEA_LANDED_TOTAL, na.rm = T))
            , FISHERY_DOLLAR_PERCENTAGE = NA
            , FISHERY_LANDED_PERCENTAGE = NA
            # , FISHERY_DAYS_PERCENTAGE = NA
  ) %>% 
  rename(SPPNAME = SPPNAME_CONF) %>% 
  ungroup()

b = a_collect %>% 
  filter(SPPNAME_CONF != 'ALL_OTHERS') %>% 
  group_by(SPPNAME_CONF, FMP, YEAR, AREA, CONF) %>%
  mutate(value = WEA_DOLLAR_TOTAL
         , landings = WEA_LANDED_TOTAL
         , FISHERY_DOLLAR_PERCENTAGE =  FISHERY_DOLLAR_PERCENTAGE
         , FISHERY_LANDED_PERCENTAGE = FISHERY_LANDED_PERCENTAGE
         # , FISHERY_DAYS_PERCENTAGE = FISHERY_DAYS_PERCENTAGE
  ) %>% 
  ungroup() %>% 
  dplyr::select(YEAR
                , AREA
                , SPPNAME
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

"))

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
WITH wea as w(
    SELECT
        year
        , area
        ,idnum imgid
        ,SUM(perc) perc
    FROM APSD.ALL_WEA_2008_2019_070221@garfo_nefsc.world
    GROUP BY
        year
        , area
        ,idnum
)
, total as t(
    SELECT
        year
        , NVL(round(sum(dollar), 0),0) as dollar_total
        , NVL(round(sum(landed), 0),0) as landed_total
        , NVL(round(sum(trip_length), 0),0) as days_total
        , VTR_PORT
    FROM apsd.dmis_wind_test
    GROUP BY
        year
        , VTR_PORT
)
, c as(
    SELECT *  
    FROM apsd.dmis_wind_test@garfo_nefsc.world
)
, d as(
    SELECT 
        c.year
        , w.area -- add this if doing multiple areas
        , c.VTR_PORT
        , count(distinct(permit)) as npermits
        , count(distinct(dealnum)) as ndealers
        , count(distinct(docid)) as ntrips
        , count(distinct(w.imgid)) as nsubtrips
        , NVL(round(sum(c.trip_length*w.perc), 3), 0) as wea_days_fished
        , NVL(round(sum(w.perc*c.dollar), 0),0) as wea_dollar_total  
        , NVL(round(sum(w.perc*c.landed), 0),0) as wea_landed_total
    FROM w
    LEFT JOIN c
        ON  w.IMGID = c.IMGID
    GROUP BY
        c.year
        , w.area
        , c.VTR_PORT
)
SELECT
    d.*
    , t.dollar_total as fishery_dollar_total
    , t.landed_total as fishery_landed_total
    , t.days_total as fishery_days_total
FROM d
LEFT JOIN t
    ON (d.year = t.year AND d.VTR_PORT = t.VTR_PORT) 
    WHERE wea_dollar_total > 0
    AND wea_landed_total > 0
    AND wea_days_fished > 0
ORDER BY wea_dollar_total desc
")
)

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
WITH wea as w(
    SELECT
        year
        , area
        ,idnum imgid
        ,SUM(perc) perc
    FROM APSD.ALL_WEA_2008_2019_070221
    GROUP BY
        year
        , area
        ,idnum
)
, total as t(
    SELECT
        year
        , NVL(round(sum(dollar), 0),0) as dollar_total
        , NVL(round(sum(landed), 0),0) as landed_total
        , NVL(round(sum(trip_length), 0),0) as days_total    --, NVL(round(sum(w.perc*dollar)/sum(dollar), 5),0) as WEA_dollar_percentage
        ,VTR_STATE
    FROM apsd.dmis_wind_test
    GROUP BY
        year
        ,VTR_STATE
)


select d.*
, t.dollar_total as fishery_dollar_total
, t.landed_total as fishery_landed_total
, t.days_total as fishery_days_total

from(

    select 
    c.year
    , w.area -- add this if doing multiple areas
    , c.VTR_STATE
    , count(distinct(permit)) as npermits
    , count(distinct(dealnum)) as ndealers
    , count(distinct(docid)) as ntrips
    , count(distinct(w.imgid)) as nsubtrips
    , NVL(round(sum(c.trip_length*w.perc), 3), 0) as wea_days_fished
    , NVL(round(sum(w.perc*c.dollar), 0),0) as wea_dollar_total  
    , NVL(round(sum(w.perc*c.landed), 0),0) as wea_landed_total
    from  wea w
     left join 
    (
        select a.*   
        from apsd.dmis_wind_test a
    ) c
   
    on  w.IMGID = c.IMGID
    
group by c.year
, w.area
, c.VTR_STATE
)
d

left join total t
on (d.year = t.year AND d.VTR_STATE = t.VTR_STATE) 
where wea_dollar_total > 0
AND wea_landed_total > 0
AND wea_days_fished > 0
order by wea_dollar_total desc
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
           , allspecies = data.frame(SPECIES = unique(a_collect$SPPNAME))
           , allports = data.frame(PORT = unique(aaa_collect$VTR_PORT))
           , allgears = data.frame(GEAR = unique(aa_collect$GEARCODE))
           , allstates = data.frame(STATE = unique(aaaa_collect$VTR_STATE))
)

writexl::write_xlsx(out, path = "ALL_WEA_SUMMARY_BY_AREA_1008221.xlsx")

save.image('ALL_WEA_SUMMARY_BY_AREA_100821.Rdata')
