#getSightings_ODBC




rm(list=ls())
source('Cc_SCB_functions.R')
library(RODBC)
SWFSC <- T
save.files <- F

if (SWFSC){
  turtle <- odbcConnect(dsn = 'Turtle', uid = "", pwd = "")
  sightings_tbl <- sqlQuery(channel = turtle, 
                            "select ID, Cruise_Number, Date_Observed, Time_Observed, 
                            Latitude, Longitude, Species_ID, Animal_Count,
                            Bearing, Distance, JFR, Observation_Description,
                            Reticle, Observed_By_Type from tbl_Sighting")
  odbcClose(turtle)
  
  common <- odbcConnect(dsn = 'Common', uid = "", pwd = "")
  spID_tbl <- sqlQuery(channel = common,
                       "select ID, Genus, Species, spType, CommonName from tblSpecies")
  
  odbcClose(common)
  write_rds(sightings_tbl, 'RData/sightings_tbl.rds')
  write_rds(spID_tbl, 'RData/spID_tbl.rds')
} else {
  sightings_tbl <- read_rds('RData/sightings_tbl.rds')
  spID_tbl <- read_rds('RData/spID_tbl.rds')
}

turtle_sp <- filter(spID_tbl, spType == 'T')

sightings_tbl <- left_join(sightings_tbl, turtle_sp, by = c("Species_ID" = "ID"))

# limit to the study area:
sightings_tbl <- filter(sightings_tbl, Latitude >= 28 & Latitude <= 36 &
                          Longitude >= -127.5 & Longitude <= -115)

Cc_sightings <- filter(sightings_tbl, Genus == 'Caretta' | Genus == 'Unid')

# this includes one CalCofi cruise... remove it
Cc_sightings_MMcruise <- filter(Cc_sightings, !is.na(Cruise_Number)) %>%
  filter(., Cruise_Number != 'CC1604H') %>%
  select(., -Observation_Description)

Cc_sightings_aerial <- filter(Cc_sightings, 
                              Observation_Description == 'Aerial survey sighting' | 
                                Observation_Description == 'Aerial Survey') %>%
  select(.,-Observation_Description)

Cc_sightings_public <- filter(Cc_sightings, 
                              (is.na(Cruise_Number) | Cruise_Number == 'CC1604H') &
                                Observation_Description != 'Aerial survey sighting' & 
                                Observation_Description != 'Aerial Survey') %>%
  select(.,-Observation_Description)

Cc_sightings_nonAerial <- filter(Cc_sightings,
                                 Observation_Description != 'Aerial survey sighting' &
                                   Observation_Description != 'Aerial Survey') %>%
  select(., -Observation_Description)

# the flyer was created on 3/23/2015, so the hotline was considered to be up and running
# on April 1, 2015
Cc_sightings_hotline <- filter(Cc_sightings_public, 
                               Date_Observed > format(as.Date('2015-04-01'), 
                                                      '%Y-%m-%d'))

if (save.files){
  write.csv(Cc_sightings_nonAerial,
            file = 'data/nonAerialSightings_Cc.csv',
            quote = F, row.names = F)
  
  write.csv(Cc_sightings_MMcruise,
            file = 'data/MM_CruiseSightings_Cc.csv',
            quote = F, row.names = F)

  write.csv(Cc_sightings_aerial,
            file = 'data/AerialSightings_Cc.csv',
            quote = F, row.names = F)
  
  write.csv(Cc_sightings_hotline,
            file = 'data/HotlineSightings_Cc.csv',
            quote = F, row.names = F)
  write.csv(Cc_sightings,
            file = 'data/AllSightings_Cc.csv',
            quote = F, row.names = F)
  
  
}





