# Look at satellite data around lost turtle sightings at the end of 2014.
# Get NRT geostrophic current data and maps of images 
# with multiple turtle sightings. 


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

xlim<-c(-128,-118)
ylim<-c(30,36)
dates<-as.Date(c("2014-11-28","2014-12-06","2014-12-07","2015-04-09","2015-07-21"))

#setwd('~/Data/lostturtles')
turtle<-read.csv('loggerhead_Aug2015.csv',header=TRUE)
turtle$Longitude<-abs(turtle$Lon)*(-1)
turtle$Date<-as.Date(turtle$Date,format='%m-%d-%Y')
tlim<-range(turtle$Date)
xlim<-range(turtle$Longitude)
ylim<-range(turtle$Latitude)
coast <- map_data("worldHires", ylim = ylim, xlim = xlim)
ggplot(data=turtle,aes(x=Longitude,y=Latitude))+geom_point(aes(colour = Date),size=4)


melt_data <- function(longitude,latitude,Time,Data) {
  dimnames(Data) <- list(long = longitude, lat = latitude)
  ret <- melt(Data, value.name = "var")
  cbind(date = Time, ret)
}


n.dates<-length(dates)

for (i in 1:n.dates) {

  currFrame<- function(longitude,latitude,u,v){
    dims<-dim(u) 
    u<-array(u,dims[1]*dims[2])
    v<-array(v,dims[1]*dims[2])
    currFrame<-expand.grid(x=longitude,y=latitude) 
    currFrame$u<-u
    currFrame$v<-v
    names(currFrame)[names(currFrame)=="x"] <- "lon"
    names(currFrame)[names(currFrame)=="y"] <- "lat"
    return(currFrame)
  }
  
#Get current data, its not in xtractomatic.  
  
myURL=paste('http://coastwatch.pfeg.noaa.gov/erddap/griddap/miamicurrents.nc?',
              'u_current[(',min(tlim),'):1:(',max(tlim),')]',
              '[(',ylim[1],'):1:(',ylim[2],')]',
              '[(',xlim[1],'):1:(',xlim[2],')],',
              'v_current[(',min(tlim),'):1:(',max(tlim),')]',
              '[(',ylim[1],'):1:(',ylim[2],')]',
              '[(',xlim[1],'):1:(',xlim[2],')]',sep="")
test<-download.file(myURL,destfile="test.nc",mode='wb')

# now read the ncdf file 

datafileID<-nc_open('test.nc')
lon<-ncvar_get(datafileID, varid="longitude")
lat<-ncvar_get(datafileID, varid="latitude")
time<-ncvar_get(datafileID, varid="time")
time<-as.POSIXlt(time,origin='1970-01-01',tz= "GMT")
u<-ncvar_get(datafileID, varid="u_current")
v<-ncvar_get(datafileID, varid="v_current")
nc_close(datafileID)

currFrame<-currFrame(lon,lat,u,v)

#Get MODIS chl, GRSST SST data, and NRL SSH. They are in xtractomatic , so use that

#tlim<-c("2014-10-14","2015-01-29")
#xlim<-xlim-360
tlim<-c(dates[i],dates[i])
chl<-xtracto_3D(xlim,ylim,tlim,"erdMWchla8day")
sst<-xtracto_3D(xlim,ylim,tlim,"jplMURSST")
ssh<-xtracto_3D(xlim,ylim,tlim,"nrlHycomGLBu008e911S")

chl$time<-as.Date(chl$time)
sst$time<-as.Date(sst$time)
ssh$time<-as.Date(ssh$time)

# Now make maps 
wf<-2

# Chlorophyll first 
longitude<-chl$longitude
latitude<-chl$latitude
iday<-1:length(chl$time)
tmp <- lapply(iday, function(x) melt_data(longitude,latitude,chl$time[x],chl$data))
allchl <- do.call(rbind, tmp)
turtle1day<-subset(turtle,Date==dates[i])


png(paste('Chl',dates[i],'.png',sep=""))

print(ggplot(data = allchl, aes(x = long, y = lat)) + 
  geom_polygon(data = coast, aes(x=long, y = lat, group = group), fill = "grey80") +
  geom_raster(aes(fill=var), interpolate = TRUE) +
  scale_fill_gradientn(colours = matlab.like2(15),limits=c(0.02,12), 
              breaks=c(0.1,0.3,1,3,10),labels=c(0.1,0.3,1,3,10), 
              na.value = NA,name="Chl",trans="log") +
  geom_segment(data=currFrame, aes(x=lon,xend=lon+u*wf,y=lat,yend=lat+v*wf), arrow=arrow(length=unit(0.2, "cm"))) + 
  geom_point(data=turtle1day,aes(x=Longitude,y=Latitude,group = factor(Date)),size=4) + 
  geom_point(data=turtle1day,aes(x=Longitude,y=Latitude,group = factor(Date)),color="pink",size=3) +
  theme_bw() + xlab('Longitude') + ylab('Latitude') +
  coord_fixed(1.3,xlim = xlim, ylim = ylim) +
  ggtitle(dates[i]))

dev.off()


# Now SST 
longitude<-sst$longitude
latitude<-sst$latitude
iday<-1:length(sst$time)
tmp <- lapply(iday, function(x) melt_data(longitude,latitude,sst$time[x],sst$data))
allsst <- do.call(rbind, tmp)

png(paste('SST',dates[i],'.png',sep=""))
print(ggplot(data = allsst, aes(x = long, y = lat)) + 
  geom_polygon(data = coast, aes(x=long, y = lat, group = group), fill = "grey80") +
  geom_raster(aes(fill=var), interpolate = TRUE) +
  scale_fill_gradientn(colours = matlab.like2(15),limits=c(12,22), na.value = NA, name="SST") +
  geom_segment(data=currFrame, aes(x=lon,xend=lon+u*wf,y=lat,yend=lat+v*wf), arrow=arrow(length=unit(0.2, "cm"))) + 
  geom_point(data=turtle1day,aes(x=Longitude,y=Latitude,group = factor(Date)),size=4) + 
  geom_point(data=turtle1day,aes(x=Longitude,y=Latitude,group = factor(Date)),color="pink",size=3) +
  theme_bw() + xlab('Longitude') + ylab('Latitude') +
  coord_fixed(1.3,xlim = xlim, ylim = ylim) +
  ggtitle(dates[i]))

dev.off()

# Now SSH 
longitude<-ssh$longitude
latitude<-ssh$latitude
iday<-1:length(ssh$time)
tmp <- lapply(iday, function(x) melt_data(longitude,latitude,ssh$time[x],ssh$data))
allssh <- do.call(rbind, tmp)

png(paste('SSH',dates[i],'.png',sep=""))

print(ggplot(data = allssh, aes(x = long, y = lat)) + 
  geom_polygon(data = coast, aes(x=long, y = lat, group = group), fill = "grey80") +
  geom_raster(aes(fill=var), interpolate = TRUE) +
  scale_fill_gradientn(colours = matlab.like2(15),limits=c(0,.4), na.value = NA,name="SSH") +
  geom_segment(data=currFrame, aes(x=lon,xend=lon+u*wf,y=lat,yend=lat+v*wf), arrow=arrow(length=unit(0.2, "cm"))) + 
  geom_point(data=turtle1day,aes(x=Longitude,y=Latitude,group = factor(Date)),size=4) + 
  geom_point(data=turtle1day,aes(x=Longitude,y=Latitude,group = factor(Date)),color="pink",size=3) +
  theme_bw() + xlab('Longitude') + ylab('Latitude') +
  coord_fixed(1.3,xlim = xlim, ylim = ylim) +
  ggtitle(dates[i]))

dev.off()

}

