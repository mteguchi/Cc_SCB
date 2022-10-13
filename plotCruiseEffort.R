#plotCruiseEffort


rm(list=ls())
source('Cc_SCB_functions.R')


# read effort data:
# define column types:
col_def.eft <- cols(Cr = col_integer(),
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
                R = col_character())

effort.data <- read_csv(file = 'data/Effort4Tomo_2.txt', 
                        col_types = col_def.eft)

# use geom_path... how do I split them into chunks? 
# answer is to create a group variable and use it in geom_path
effort.data$chunk <- NA
c <- 1
for (k in 2:dim(effort.data)[1]){
  effort.data[k-1, 'chunk'] <- c 
  if (effort.data[(k-1), 'E'] == 'E'){
    c <- c + 1
  }
}

write_csv(effort.data, 
          path = 'data/EffortWithChunks.csv')


# the results file above is used in CcSCB_AerialSurvey
# to plot the effort along with sightings. 

# Get the sightings data file:

col_def.sight <- cols(Cruise = col_integer(),
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
                        col_types = col_def.sight)


