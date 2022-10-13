#plot_SST
# plots SST maps

rm(list=ls())
# sysInfo <- Sys.info()
# ifelse(sysInfo[1] == 'Linux',
#        source('~/Documents/R/TomosFunctions.R'),
#        source('~/R/TomosFunctions.R'))

source('Cc_SCB_functions.R')
library(viridis)
library(ncdf4)
library(httr)
library(lubridate)


land.color <- '#333333'
alpha.value <- 0.8
wf <- 100

plot.dates <- c('2006-04-16', '2006-05-16', '2006-06-16', '2006-07-16', '2006-08-16',
                '2014-08-16', '2014-09-16', '2014-10-16', '2014-11-16', '2014-12-16',
                '2011-09-16', '2011-10-16', '2015-09-16', '2015-10-16')

# also get the ship-based survey effort:
ship.effort <- read.csv('data/EffortWithDistanceChunksBftLT4.csv')
#add spatial units:
ship.effort.XY <- ship.effort[,c('Lon', 'Lat')]
colnames(ship.effort.XY) <- c('X', 'Y')
ship.effort.Sp <- latlon2sp(ship.effort.XY,
                            center.UTM = center.UTM)
ship.effort$X <- ship.effort.Sp$newX
ship.effort$Y <- ship.effort.Sp$newY
ship.effort$fYr <- as.factor(ship.effort$Yr)


#effort.data <- read.csv(file = 'data/Effort4Tomo_2.txt')
sights.data <- read.csv(file = 'data/TurtleSights_1.csv')
sights.data$Date2 <- format(as.Date(sights.data$Date, 
                                    format = '%d-%b-%y'), 
                            format = '%Y-%m-%d')
sights.data$X <- sights.data$Lon
sights.data$Y <- sights.data$Lat
sights.data.Sp <- latlon2sp(sights.data, center.UTM)
sights.data.xy <- sights.data.Sp@data

xy.df.1 <- sights.data.xy

xy.df <- xy.df.1[xy.df.1$newX <= 500 &
                   xy.df.1$newX >= -1000 &
                   xy.df.1$newY <= 650 &
                   xy.df.1$newY >= -500, ]

dplyr::select(xy.df, newX, newY) %>%
  sp2latlon(., center.UTM) %>%
  cbind.data.frame(., xy.df$Date2) -> xy.latlon

xy.latlon$Year <- as.numeric(format(as.Date(xy.latlon$'xy.df$Date2'), '%Y'))
colnames(xy.latlon) <- c('Lon', 'Lat', 'Date', 'Year')
xy.latlon$Month <- as.numeric(format(as.Date(xy.latlon$Date), '%m'))
xy.latlon$X <- xy.df$newX
xy.latlon$Y <- xy.df$newY

# replace the coast.line.df in StudyArea.RData to a new one here:
coast.line <- getCoastLine('~/R/OceanDepths/coast/coast_Epac.txt',
                           lon.limits = c(-131, -115),
                           lat.limits = c(28, 36))

coast.line.df <- do.call(rbind, coast.line)
colnames(coast.line.df) <- c('X', 'Y', 'idx')
coast.line.Sp <- latlon2sp(coast.line.df, center.UTM)

k <- 1
for (k in 1:length(plot.dates)){
  # get SST data
  
  sst.file.name <- paste0("data/ncfiles/sst_", plot.dates[k], ".nc")
  
  if (file.exists(sst.file.name) == F | file.size(sst.file.name) == 0){
    sstURL <- paste0('https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplMURSST41mday.nc?sst[(', 
                     plot.dates[k], 
                     'T00:00:00Z)][(28.0):(36.0)][(-127.5):(-115.0)]')

        test <- download.file(sstURL,
                          destfile= paste0("data/ncfiles/sst_", 
                                           plot.dates[k], ".nc"), 
                          mode='wb')
    
  }
  
  datafileID <- nc_open(sst.file.name)
  lon <- ncvar_get(datafileID, varid="longitude")
  lat <- ncvar_get(datafileID, varid="latitude")
  time <- ncvar_get(datafileID, varid="time")
  time <- as.POSIXlt(time,origin='1970-01-01',tz= "GMT")
  sst <- ncvar_get(datafileID, varid = 'sst')
  # u<-ncvar_get(datafileID, varid="u_current")
  # v<-ncvar_get(datafileID, varid="v_current")
  nc_close(datafileID)
  
  # convert the data into a data frame
  sst.df <- as.data.frame(cbind(expand.grid(lon, lat), as.vector(sst)))
  colnames(sst.df) <- c('X', 'Y', 'sst')
  
  # convert lat/lon into the cartesian coordinates:
  sst.df.Sp <- latlon2sp(sst.df, center.UTM)
  sst.df.xy <- na.omit(sst.df.Sp@data)
  
  out.filename <- paste0('figures/sst_', plot.dates[k], '.png')

  # get effort data:
  Yr <- as.numeric(format(as.Date(plot.dates[k]), format = '%Y'))
  Mo <- as.numeric(format(as.Date(plot.dates[k]), format = '%m'))
  ship.effort.tmp <- ship.effort[ship.effort$Yr == Yr &
                                   ship.effort$Mo == Mo, ]  
  
  xy.latlon.tmp <- xy.latlon[xy.latlon$Year == Yr &
                               xy.latlon$Month == Mo,]
  

  # get back the land color - somehow it's all lost... 5/30/2017
  p1 <- ggplot() +
    # dataset is too big for geom_raster... 
    #geom_raster(data = sst.df.xy,
    #            aes(x = newX, y = newY, fill = sst)) +
    # So, use geom_point instead:
    geom_point(data = sst.df.xy,
               aes(x = newX, y = newY, color = sst)) + 
    # plot ship effort for the time period. 
    geom_path(data = ship.effort.tmp,
              aes(x = X, y = Y,
                  group = chunk),
              size = 1.2, color = 'red') +
    scale_color_viridis(na.value = land.color,
                       limits = c(10, 30)) +
    # scale_fill_gradient(na.value = land.color,
    #                      limits = c(10, 30),
    #                      guide = viridis(n = 20)) +
    geom_polygon(fill = land.color,
                 data = all.islands.df,
                 aes(x=newX, y=newY, group = name),
                 inherit.aes = F) +
    # geom_polygon(fill = land.color,
    #              data = coast.line.Sp@data,   # needs to be expanded a little
    #              aes(x=newX, y=newY, group = idx))  +
    geom_polygon(fill = land.color,
                data = coast.line.Sp@data,
                aes(x=newX, y=newY, group = idx))  +

    geom_path(data = map.LTCA.Sp@data,
              aes(x = newX, y = newY),
              linetype = 2, size = 1.5,
              alpha = 0.6) +
    geom_point(data = xy.latlon.tmp,
               aes(x = X, y = Y),
               size = 2) +
    #coord_map() +
    ylab("Northing (km)") +
    xlab("Easting (km)")  + 
    ggtitle(plot.dates[k]) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.title = element_text(size = 10, hjust = 0.5),
          legend.text = element_text(size = 8, vjust = 0))
  
  ggsave(plot = p1,
         dpi = 1200,
         width = 8.96,
         height = 5.74,
         file = out.filename)
}


