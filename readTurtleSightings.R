#readTurtleSightings


rm(list=ls())
source('Cc_SCB_functions.R')
library(RODBC)

save.files <- F

col_def <- cols(Cruise = col_integer(),
                Date = col_date('%d-%b-%y'),
                Time = col_time('%H:%M:%S'),
                Sight = col_character(),
                Lat = col_double(),
                Lon = col_double(),
                Species = col_character(),
                Best = col_integer(),
                Bearing = col_integer(),
                Distance = col_double(),
                Maturity = col_character(),
                Captured = col_character(),
                DetecBy = col_integer(),
                AssocJFR = col_character(),
                EffortType = col_character(),
                Beaufort = col_integer(),
                SwellHt = col_double(),
                SwellDir = col_integer(),
                WindSpd = col_double(),
                FogRain = col_character(),
                HzSun = col_integer(),
                VtSun = col_integer(),
                WindDir = col_double(),
                Visibility = col_double())

sights.data <- read_csv(file = 'data/TurtleSights_1.csv', 
                        col_types = col_def)

sights.data <- dplyr::filter(sights.data, 
                             Lat >= 28.0 & Lat <= 36 & 
                               Lon >= -127.5 & Lon <= -115)

sights.data$Year <- as.numeric(format(sights.data$Date, format = '%Y'))
#sights.cc <- sights.data[sights.data$Species == 'CC',]

# change Best estimate of # of individuals == NA to == 1
sights.data[is.na(sights.data$Best), 'Best'] <- 1

sights.data.BftLT4 <- dplyr::filter(sights.data,
                                    Beaufort < 4 | is.na(Beaufort) == T)

sights.data %>% 
  dplyr::group_by(Year, Cruise, Species) %>%
  dplyr::summarise(number = sum(Best, na.rm = T)) -> sights.by.Yr.Cr.Sp

sights.data %>% 
  dplyr::group_by(Year, Species) %>%
  dplyr::summarise(number = sum(Best, na.rm = T)) -> sights.by.Yr.Sp

sights.data.BftLT4 %>% 
  dplyr::group_by(Year, Cruise, Species) %>%
  dplyr::summarise(number = sum(Best, na.rm = T)) -> sights.BftLT4.by.Yr.Cr.Sp

sights.data.BftLT4 %>% 
  dplyr::group_by(Year, Species) %>%
  dplyr::summarise(number = sum(Best, na.rm = T)) -> sights.BftLT4.by.Yr.Sp

if (save.files){
  write.csv(sights.by.Yr.Cr.Sp, row.names = F, quote = F,
            file = 'data/SightingsByYrCruiseSp.csv')
  
  write.csv(sights.by.Yr.Sp, row.names = F, quote = F,
            file = 'data/SightingsByYrSp.csv')

  write.csv(sights.BftLT4.by.Yr.Cr.Sp, row.names = F, quote = F,
            file = 'data/SightingsByYrCruiseSpBftLT4.csv')
  
  write.csv(sights.BftLT4.by.Yr.Sp, row.names = F, quote = F,
            file = 'data/SightingsByYrSpBftLT4.csv')
  
}


