# Look at satellite data around lost turtle sightings at the end of 2014.
# Get NRT geostrophic current data and maps of images 
# with multiple turtle sightings. 

# Originally written by C. Wilson Aug 2015
# Modified by T Eguchi

rm(list=ls())

library(ncdf4)
library(httr)
library(lubridate)
library(xtractomatic)
library(animation)
library(colorRamps)
require(grid)

require(mapdata)
require(ggplot2)
require(RColorBrewer)
require(reshape2)
require(plyr)

#These ranges are slightly bigger that the lat, lon ranges of turtle dataset 

xlim<-c(-127.5,-115)
ylim<-c(28,36)
#dates<-as.Date(c("2014-11-28","2014-12-06","2014-12-07","2015-04-09","2015-07-21"))

#setwd('~/Data/lostturtles')
#turtle<-read.csv('loggerhead_sightings_04Aug2015.csv',header=TRUE)
# turtle<-read.csv('loggerhead_sightings_Sept2015_16Sep2015.csv',
#                  header=TRUE)
turtle<-read.csv('data/loggerhead_sightings_18Sep2015.csv',
                 header=TRUE)

turtle$Lon<-abs(turtle$Longitude)*(-1)
turtle$Date<-as.Date(turtle$Date_Observed,format='%m/%d/%Y')
dates <- unique(turtle$Date)

tlim<-range(turtle$Date)
#xlim<-range(turtle$Longitude)
#ylim<-range(turtle$Latitude)

coast <- map_data("worldHires", ylim = ylim, xlim = xlim)
ggplot(data=turtle,
       aes(x=Longitude,y=Latitude))+
  geom_point(aes(colour = Date),size=4)


melt_data <- function(longitude,latitude,Time,Data) {
  dimnames(Data) <- list(long = longitude, lat = latitude)
  ret <- melt(Data, value.name = "var")
  cbind(date = Time, ret)
}


n.dates<-length(dates)
plotList <- vector(mode = 'list', length = n.dates)
for (i in 1:n.dates) {
  
  currFrame<- function(longitude,latitude,u,v){
    dims<-dim(u) 
    u<-array(u,dims[1]*dims[2])
    v<-array(v,dims[1]*dims[2])
    currDF<-expand.grid(x=longitude,y=latitude) 
    currDF$u<-u
    currDF$v<-v
    names(currDF)[names(currDF)=="x"] <- "lon"
    names(currDF)[names(currDF)=="y"] <- "lat"
    return(currDF)
  }
  
  #Get current data, its not in xtractomatic.  
  
    # myURL <- paste('http://coastwatch.pfeg.noaa.gov/erddap/griddap/miamicurrents.nc?',
    #                'u_current[(',min(tlim),'):1:(',max(tlim),')]',
    #                '[(',ylim[1],'):1:(',ylim[2],')]',
    #                '[(',xlim[1],'):1:(',xlim[2],')],',
    #                'v_current[(',min(tlim),'):1:(',max(tlim),')]',
    #                '[(',ylim[1],'):1:(',ylim[2],')]',
    #                '[(',xlim[1],'):1:(',xlim[2],')]',sep="")
  myURL <- paste('http://coastwatch.pfeg.noaa.gov/erddap/griddap/miamicurrents.nc?',
                 'u_current[(',dates[i],'):1:(',dates[i],')]',
                 '[(',ylim[1],'):1:(',ylim[2],')]',
                 '[(',xlim[1],'):1:(',xlim[2],')],',
                 'v_current[(',dates[i],'):1:(',dates[i],')]',
                 '[(',ylim[1],'):1:(',ylim[2],')]',
                 '[(',xlim[1],'):1:(',xlim[2],')]',sep="")

    filename <- paste0('data/download', dates[i], '.nc')
  
  if (file.exists(filename) == FALSE | file.size(filename) == 0){
    test<-download.file(myURL, destfile = filename, mode='wb')
  } 
    
  # now read the ncdf file 
  datafileID<-nc_open(filename)
  lon<-ncvar_get(datafileID, varid="longitude")
  lat<-ncvar_get(datafileID, varid="latitude")
  time<-ncvar_get(datafileID, varid="time")
  time<-as.POSIXlt(time,origin='1970-01-01',tz= "GMT")
  u<-ncvar_get(datafileID, varid="u_current")
  v<-ncvar_get(datafileID, varid="v_current")
  nc_close(datafileID)
  
  currDF<-currFrame(lon,lat,u,v)
  
  #Get MODIS chl, GRSST SST data, and NRL SSH. They are in xtractomatic , so use that
  
  #tlim<-c("2014-10-14","2015-01-29")
  #xlim<-xlim-360
  tlim<-c(dates[i],dates[i])
#   chl<-xtracto_3D(xlim,ylim,tlim,"erdMWchla8day")
  sst<-xtracto_3D(xlim,ylim,tlim,"jplMURSST")
#   ssh<-xtracto_3D(xlim,ylim,tlim,"nrlHycomGLBu008e911S")
#   
#   chl$time<-as.Date(chl$time)
  sst$time<-as.Date(sst$time)
#   ssh$time<-as.Date(ssh$time)
#   
#   # Now make maps 
  wf<-2
#   
#   # Chlorophyll first 
#   longitude<-chl$longitude
#   latitude<-chl$latitude
#   iday<-1:length(chl$time)
#   tmp <- lapply(iday, 
#                 function(x) melt_data(longitude,latitude,chl$time[x],chl$data))
#   allchl <- do.call(rbind, tmp)
  turtle1day<-subset(turtle,Date==dates[i])
#   
#   
#   png(paste('Chl',dates[i],'.png',sep=""))
#   
#   print(ggplot(data = allchl, aes(x = long, y = lat)) + 
#           geom_polygon(data = coast, 
#                        aes(x=long, y = lat, group = group), 
#                        fill = "grey80") +
#           geom_raster(aes(fill=var), interpolate = TRUE) +
#           scale_fill_gradientn(colours = matlab.like2(15),limits=c(0.02,12), 
#                                breaks=c(0.1,0.3,1,3,10),labels=c(0.1,0.3,1,3,10), 
#                                na.value = NA,name="Chl",trans="log") +
#           geom_segment(data=currDF, 
#                        aes(x=lon,xend=lon+u*wf,y=lat,yend=lat+v*wf), 
#                        arrow=arrow(length=unit(0.2, "cm"))) + 
#           geom_point(data=turtle1day,
#                      aes(x=Longitude,y=Latitude,
#                          group = factor(Date)),size=4) + 
#           geom_point(data=turtle1day,
#                      aes(x=Longitude,y=Latitude,
#                          group = factor(Date)),color="pink",size=3) +
#           theme_bw() + xlab('Longitude') + ylab('Latitude') +
#           coord_fixed(1.3,xlim = xlim, ylim = ylim) +
#           ggtitle(dates[i]))
#   
#   dev.off()
#   
#   
  # Now SST 
  longitude<-sst$longitude
  latitude<-sst$latitude
  iday<-1:length(sst$time)
  tmp <- lapply(iday, function(x) melt_data(longitude,
                                            latitude,
                                            sst$time[x],
                                            sst$data))
  
  allsst <- do.call(rbind, tmp)
  
  png(paste('SST',dates[i],'.png',sep=""))
  print(ggplot(data = allsst, aes(x = long, y = lat)) + 
          geom_polygon(data = coast, 
                       aes(x=long, y = lat, group = group), 
                       fill = "grey80") +
          geom_raster(aes(fill=var), interpolate = TRUE) +
          scale_fill_gradientn(colours = matlab.like2(15), 
                               limits=c(12,28), 
                               na.value = NA, 
                               name="SST") +
          geom_segment(data=currDF, 
                       aes(x=lon, xend=lon+u*wf,
                           y=lat, yend=lat+v*wf), 
                       arrow = arrow(length=unit(0.2, "cm"))) + 
          geom_point(data=turtle1day,
                     aes(x=Longitude,
                         y=Latitude,
                         group = factor(Date)),
                     size=4) + 
          geom_point(data=turtle1day,
                     aes(x=Longitude,
                         y=Latitude,
                         group = factor(Date)),
                     color="pink", 
                     size=3) +
          theme_bw() + xlab('Longitude') + ylab('Latitude') +
          coord_fixed(1.3,xlim = xlim, ylim = ylim) +
          ggtitle(dates[i]))
  
  dev.off()
#   
#   # Now SSH 
#   longitude<-ssh$longitude
#   latitude<-ssh$latitude
#   iday<-1:length(ssh$time)
#   tmp <- lapply(iday, function(x) melt_data(longitude,latitude,ssh$time[x],ssh$data))
#   allssh <- do.call(rbind, tmp)
#   
#   png(paste('SSH',dates[i],'.png',sep=""))
#   
#   print(ggplot(data = allssh, aes(x = long, y = lat)) + 
#           geom_polygon(data = coast, 
#                        aes(x=long, y = lat, group = group), 
#                        fill = "grey80") +
#           geom_raster(aes(fill=var), interpolate = TRUE) +
#           scale_fill_gradientn(colours = matlab.like2(15),
#                                limits=c(0,.4), na.value = NA,name="SSH") +
#           geom_segment(data=currDF, 
#                        aes(x=lon,xend=lon+u*wf,
#                            y=lat,yend=lat+v*wf), 
#                        arrow=arrow(length=unit(0.2, "cm"))) + 
#           geom_point(data=turtle1day,
#                      aes(x=Longitude,y=Latitude,
#                          group = factor(Date)),size=4) + 
#           geom_point(data=turtle1day,
#                      aes(x=Longitude,y=Latitude,
#                          group = factor(Date)),color="pink",size=3) +
#           theme_bw() + xlab('Longitude') + ylab('Latitude') +
#           coord_fixed(1.3,xlim = xlim, ylim = ylim) +
#           ggtitle(dates[i]))
#   
#   dev.off()
  
}

