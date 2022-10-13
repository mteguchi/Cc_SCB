#readEffort


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
                R = col_character())

effort.data <- read_csv(file = 'data/Effort4Tomo_2.txt', 
                        col_types = col_def)
# this is 43548 x 16
# when Bft > 3 are removed, it becomes 16348 x 16 without NA and
# 20443 x 16 with NA (NAs are needed). To save all Bft levels, 
# comment out this line:
#effort.data <- dplyr::filter(effort.data, Bft < 4 | is.na(Bft) == T)

effort.data$Dist_km <- NA
effort.data$Time_sec <- NA
effort.data$chunk <- NA

c <- 1
for (k in 2:dim(effort.data)[1]){
  effort.data[k-1, 'chunk'] <- c 
  if (effort.data[(k-1), 'E'] == 'E'){
    c <- c + 1
  }
  if ((effort.data[(k-1), 'E'] != 'E')){
    effort.data[k, 'Dist_km'] <- distGeo(effort.data[k, c('Lon', 'Lat')],
                                         effort.data[k-1, c('Lon', 'Lat')])/1000
    effort.data[k, 'Time_sec'] <- effort.data[k, 'Time'] - effort.data[k-1, 'Time']
  }
}

# using write_csv changes 1000 to 1e3... WTF?  19 May 2017

if (save.files){
  if (max(effort.data$Bft, na.rm = T) > 4){
    write.csv(effort.data, quote = F, row.names = F,
              file = 'data/EffortWithDistanceChunks.csv')
    
  } else {
    write.csv(effort.data, quote = F, row.names = F,
              file = 'data/EffortWithDistanceChunksBftLT4.csv')
    
  }
}


