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
library(grid)

library(mapdata)
library(ggplot2)
library(RColorBrewer)
library(reshape2)
library(plyr)
library(dplyr)
#These ranges are slightly bigger that the lat, lon ranges of turtle dataset 

xlim<-c(-127.5,-115)
ylim<-c(28,36)
#dates<-as.Date(c("2014-11-28","2014-12-06","2014-12-07","2015-04-09","2015-07-21"))

#setwd('~/Data/lostturtles')
#turtle<-read.csv('loggerhead_sightings_04Aug2015.csv',header=TRUE)
# turtle<-read.csv('loggerhead_sightings_Sept2015_16Sep2015.csv',
#                  header=TRUE)
#turtle<-read.csv('data/loggerhead_sightings_18Sep2015.csv',
#                 header=TRUE)
sightings.data <- readr::read_csv('data/CcSightingsQuery_28Feb2017.txt') %>%
  select(.,Date_Observed, Latitude, Longitude)
sightings.data$Date <- parse_date_time(sightings.data$Date_Observed,
                                       orders = 'm/d/Y HMS') 
select(sightings.data, Date, Latitude, Longitude) %>%
  filter(., Longitude >= xlim[1] & Longitude <= xlim[2] &
           Latitude >= ylim[1] & Latitude <= ylim[2] & 
           Date >= as.Date('2006-01-01')) -> turtle

#turtle$Lon<-abs(turtle$Longitude)*(-1)
#turtle$Date<-as.Date(turtle$Date_Observed,format='%m/%d/%Y')
dates <- unique(turtle$Date)

tlim<-range(turtle$Date)
#xlim<-range(turtle$Longitude)
#ylim<-range(turtle$Latitude)

wf<-2

coast <- map_data("worldHires", ylim = ylim, xlim = xlim)
# ggplot(data=turtle,
#        aes(x=Longitude,y=Latitude))+
#   geom_point(aes(colour = Date),size=4)


melt_data <- function(longitude,latitude,Time,Data) {
  dimnames(Data) <- list(long = longitude, lat = latitude)
  ret <- melt(Data, value.name = "var")
  cbind(date = Time, ret)
}

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

sstFrame <- function(longitude, latitude, sst){
  dims <- dim(sst)
  sst <- array(sst, dims[1] * dims[2])
  sstDF <- expand.grid(x = longitude, y = latitude)
  sstDF$sst <- sst
  names(sstDF)[names(sstDF) == 'x'] <- 'lon'
  names(sstDF)[names(sstDF) == 'y'] <- 'lat'
  return(sstDF)
}
n.dates<-length(dates)
plotList <- vector(mode = 'list', length = n.dates)
for (i in 23:n.dates) {
  
  
  g.xlim <- xlim + 360
  #Get current data, its not in xtractomatic.  
    
  if (dates[i] >= as.Date('2013-01-01')){
    myURL <- paste0('https://upwell.pfeg.noaa.gov/erddap/griddap/SCUD_Pac.nc?u[(',
                    dates[i], '):1:(', dates[i], ')][(', ylim[1], '):1:(',
                    ylim[2], ')][(', g.xlim[1], '):1:(', g.xlim[2], ')],v[(',
                    dates[i], '):1:(', dates[i], ')][(', ylim[1], '):1:(',
                    ylim[2], ')][(', g.xlim[1], '):1:(', g.xlim[2], ')]')
    
  } else {
    myURL <- paste0('https://upwell.pfeg.noaa.gov/erddap/griddap/erdTAgeo1day_LonPM180.nc?u_current[(',
                    dates[i], '):1:(', dates[i], ')][(0.0):1:(0.0)][(', ylim[1], '):1:(',
                    ylim[2], ')][(', xlim[1], '):1:(', xlim[2], ')],v_current[(',
                    dates[i], '):1:(', dates[i], ')][(0.0):1:(0.0)][(', ylim[1], '):1:(',
                    ylim[2], ')][(', xlim[1], '):1:(', xlim[2], ')]')
    
  }

  filename <- paste0('data/curr_', dates[i], '.nc')
  
  if (file.exists(filename) == FALSE | file.size(filename) == 0){
    test<-download.file(myURL, destfile = filename, mode='wb')
  } 
    
  # now read the ncdf file 
  datafileID<-nc_open(filename)
  lon<-ncvar_get(datafileID, varid="longitude")
  lat<-ncvar_get(datafileID, varid="latitude")
  time<-ncvar_get(datafileID, varid="time")
  time<-as.POSIXlt(time,origin='1970-01-01',tz= "GMT")
  if (dates[i] >= as.Date('2013-01-01')){
    u<-ncvar_get(datafileID, varid="u")
    v<-ncvar_get(datafileID, varid="v")
    nc_close(datafileID)
    
    currDF<-currFrame(lon-360,lat,u,v)
  } else {
    u<-ncvar_get(datafileID, varid="u_current")
    v<-ncvar_get(datafileID, varid="v_current")
    nc_close(datafileID)
    
    currDF<-currFrame(lon,lat,u,v)
    
  }  
  
  #Get MODIS chl, GRSST SST data, and NRL SSH. They are in xtractomatic , so use that
  
  #tlim<-c("2014-10-14","2015-01-29")
  #xlim<-xlim-360
  tlim<-c(dates[i],dates[i])
#   chl<-xtracto_3D(xlim,ylim,tlim,"erdMWchla8day")
  #sst<-xtracto_3D(xlim,ylim,tlim,"erdMWsstd1day")
  if (dates[i] < as.Date('2011-10-03')){
    myURL <- paste0('https://upwell.pfeg.noaa.gov/erddap/griddap/erdAAssta1day_LonPM180.nc?sst[(',
                    dates[i], '):1:(', dates[i], ')][(0.0):1:(0.0)][(', ylim[1], '):1:(',
                    ylim[2], ')][(', xlim[1], '):1:(', xlim[2], ')]')
    #https://coastwatch.pfeg.noaa.gov/erddap/griddap/erdAAssta1day_LonPM180.nc?sst[(2011-10-03T12:00:00Z):1:(2011-10-03T12:00:00Z)][(0.0):1:(0.0)][(-89.875):1:(89.875)][(-179.875):1:(179.875)]
    # 8 day one starts in 2007 even though the title says starting 2002...
    #https://coastwatch.pfeg.noaa.gov/erddap/griddap/erdAAssta8day_LonPM180.nc?sst[(2011-10-07T00:00:00Z):1:(2011-10-07T00:00:00Z)][(0.0):1:(0.0)][(-89.875):1:(89.875)][(-179.875):1:(179.875)]
  } else {
    myURL <- paste0('https://upwell.pfeg.noaa.gov/erddap/griddap/jplG1SST.nc?SST[(',
                    dates[i], '):1:(', dates[i], ')][(', ylim[1], '):1:(',
                    ylim[2], ')][(', xlim[1], '):1:(', xlim[2], ')]')
    #https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplG1SST.nc?SST[(2017-05-17T00:00:00Z):1:(2017-05-17T00:00:00Z)][(-79.995):1:(79.995)][(-179.995):1:(179.995)],mask[(2017-05-17T00:00:00Z):1:(2017-05-17T00:00:00Z)][(-79.995):1:(79.995)][(-179.995):1:(179.995)],analysis_error[(2017-05-17T00:00:00Z):1:(2017-05-17T00:00:00Z)][(-79.995):1:(79.995)][(-179.995):1:(179.995)]
  }
  
  filename <- paste0('data/sst_', dates[i], '.nc')
  
  if (file.exists(filename) == FALSE | file.size(filename) == 0){
    test<-download.file(myURL, destfile = filename, mode='wb')
  } 
  
  # now read the ncdf file 
  datafileID<-nc_open(filename)
  lon<-ncvar_get(datafileID, varid="longitude")
  lat<-ncvar_get(datafileID, varid="latitude")
  time<-ncvar_get(datafileID, varid="time")
  time<-as.POSIXlt(time,origin='1970-01-01',tz= "GMT")
  if (dates[i] < as.Date('2011-10-03')){
    sst<-ncvar_get(datafileID, varid="sst")
  } else {
    sst<-ncvar_get(datafileID, varid="SST")
  }
  nc_close(datafileID)
  
  sst <-sstFrame(lon,lat,sst)
#   ssh<-xtracto_3D(xlim,ylim,tlim,"nrlHycomGLBu008e911S")
#   
#   chl$time<-as.Date(chl$time)
  #sst$time<-as.Date(sst$time)
#   ssh$time<-as.Date(ssh$time)
#   
#   # Now make maps 
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
  longitude<-sst$lon
  latitude<-sst$lat
  #iday<-1:length(sst$time)
  # tmp <- lapply(iday, function(x) melt_data(longitude,
  #                                           latitude,
  #                                           sst$time[x],
  #                                           sst$data))
  # 
  #allsst <- na.omit(do.call(rbind, tmp))
  allsst <- na.omit(sst)
  currDF <- na.omit(currDF)
  
  #png(paste('SST',dates[i],'.png',sep=""))
  p1 <- ggplot() + 
    geom_polygon(data = coast, 
                 aes(x=long, y = lat, group = group), 
                 fill = "grey80") +
    geom_raster(data = allsst, 
                aes(x=lon, y=lat, fill=sst), 
                interpolate = TRUE) +
    scale_fill_gradientn(colours = matlab.like2(20), 
                         limits=c(5,30), 
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
    ggtitle(dates[i])
  
  ggsave(plot = p1,
         dpi = 1200,
         file = paste0('figures/SST_', dates[i], '.png'))
  
  #dev.off()
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

