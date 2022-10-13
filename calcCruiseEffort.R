#calcCruiseEffort


# computes cruise effort by cruise number and by year

rm(list=ls())
source('Cc_SCB_functions.R')
save.files <- F

# define column types:
col_def <- cols(Cr = col_integer(),
                Yr = col_integer(),
                Mo = col_integer(),
                Dy = col_integer(),
                Time = col_time("%H:%M:%S"),
                E = col_character(),
                Lat = col_double(),
                Lon = col_double(),
                Bft = col_integer(),
                SH = col_double(),
                FR = col_integer(),
                HS = col_integer(),
                VS = col_integer(),
                Vis = col_double(),
                WnSp = col_double(),
                R = col_character(),
                Dist_km = col_double(),
                Time_sec = col_double(),
                chunk = col_integer())

# effort while Bft < 4
effort.data <- read_csv(file = 'data/EffortWithDistanceChunksBftLT4.csv', 
                        col_types = col_def)

# with all effort
#effort.data <- read.csv(file = 'data/EffortWithDistance.csv')

# Find effort in km and hr:
effort.data %>%
  dplyr::group_by(Cr) %>%
  dplyr::summarise(effort_km = sum(Dist_km, na.rm = T),
                   effort_hr = sum(Time_sec, na.rm=T)/3600) -> effort.by.Cr

effort.data %>%
  dplyr::group_by(Yr) %>%
  dplyr::summarise(effort_km = sum(Dist_km, na.rm = T),
                   effort_hr = sum(Time_sec, na.rm=T)/3600) -> effort.by.Yr

effort.data %>%
  dplyr::group_by(Yr, Cr) %>%
  dplyr::summarise(effort_km = sum(Dist_km, na.rm = T),
                   effort_hr = sum(Time_sec, na.rm=T)/3600) -> effort.by.Yr.Cr

if (save.files){
  write_csv(effort.by.Yr.Cr, 
            path = 'data/EffortByYearCruise.csv')
  
  write_csv(effort.by.Yr,
            path = 'data/EffortByYear.csv')  
}
