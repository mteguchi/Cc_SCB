#SCB_loggerhead functions

# some common functions for this project are defined here.
# also fixed parameters are defined here.

# load libraries
library(dsm)
library(rgdal)
library(Distance)
library(sp)
#library(xtractomatic)
library(reshape2)
library(ncdf4)
library(grid)
library(RNetCDF)
library(tidyverse)


#getCoastLine
# A function to extract a coast line given longitude and latitude
# limits for the Pacific Ocean

# Data files - E and W Pacific are separated; got these from Rich Cosgrove

# Used to be in Matlab

# Tomo Eguchi
# 16 December 2016
getCoastLine <- function(filename, lon.limits, lat.limits){
  data1 <- read.table(filename, header = F, row.names = NULL, sep = ',')
  # fix longitudes to -180 to 180
  lon.limits[lon.limits > 180] <- lon.limits[lon.limits > 180] - 360
  out.df <- data1[data1[,1] >= min(lon.limits) &
                    data1[,1] <= max(lon.limits) &
                    data1[,2] >= min(lat.limits) &
                    data1[,2] <= max(lat.limits),]
  colnames(out.df) <- c('Longitude', 'Latitude')
  out.df$idx <- NA
  idx.rows <- 1:nrow(out.df)
  na.idx <- is.na(out.df$Longitude)  # T/F for NA
  dif.na.idx <- na.idx[2:length(na.idx)] - na.idx[1:(length(na.idx)-1)]
  idx.neg1 <- idx.rows[dif.na.idx == -1]  # beginning of segments
  idx.pos1 <- idx.rows[dif.na.idx == 1]   # end of segments
  
  for (k in 1:length(idx.neg1)) {
    out.df[idx.neg1[k]:idx.pos1[k], 'idx'] <- k
  }
  
  # change index to a factor variable
  out.df$idx <- as.factor(out.df$idx)
  out.df <- na.omit(out.df)
  # this splits all segments into separate dataframes
  out.list <- split(out.df, out.df$idx)
  
  for (k in 1:length(out.list)){
    line.1 <- out.list[[k]][1,]
    line.end <- out.list[[k]][nrow(out.list[[k]]),]
    if (line.1$Longitude != line.end$Longitude){
      tmp <- out.list[[k]]
      tmp <- rbind(c(max(tmp$Longitude), max(tmp$Latitude), k),
                   tmp, c(max(tmp$Longitude), max(tmp$Latitude), k))
      out.list[[k]] <- tmp
    }
  }
  return(out.list)
  
}

ft2m <- function(x){
  return(0.3048 * x)
}


deg2rad <- function(angleInDegrees){
  angleInRadians <- (pi/180) * angleInDegrees
  return(angleInRadians)
}

rad2deg <- function(angleInRadians){
  angleInDegrees <- angleInRadians * (180/pi)
  return(angleInDegrees)
}


## convert nm to sm
nm2sm <- function(nm){
  sm <- 1.15078 * nm
  return(sm)
}

## convert sm to nm
sm2nm <- function(sm){
  nm <- sm / 1.15078
  return(nm)
}

## Convert nm to km
nm2km <- function(nm){
  km <- nm * 1.852
  return(km)
}

# convert km to nm
km2nm <- function(km){
  nm <- km / 1.852
  return(nm)
}

km2sm <- function(km){
  sm <- km/1.609
  return(sm)
}

sm2km <- function(sm){
  km <- sm * 1.609
  return(km)
}
# ifelse(Sys.info()[1] == 'Linux',
#        source('~/Documents/R/tools/TomosFunctions.R'),
#        source('~/R/tools/TomosFunctions.R'))

land.color <- '#333333'

# convert the lat/lon into northing/easting
# the study area covers zones 10 and 11. An arbitrary center point
# was created here.
approx.center <- data.frame(X=-119.967, Y=33.092)
coordinates(approx.center) <- c("X", "Y")
proj4string(approx.center) <- CRS("+proj=longlat +datum=WGS84")
center.UTM <- spTransform(approx.center,
                          CRS("+proj=utm +zone=10 ellps=WGS84"))

## Functions are defined here:
melt_data <- function(longitude,latitude,Time,Data) {
  dimnames(Data) <- list(long = longitude, lat = latitude)
  ret <- melt(Data, value.name = "var")
  cbind(date = Time, ret)
}

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

sstFrame<- function(longitude,latitude,sst){
  dims<-dim(sst)
  sst<-array(sst,dims[1]*dims[2])

  sstFrame<-expand.grid(x=longitude,y=latitude)
  sstFrame$sst<-sst

  names(sstFrame)[names(sstFrame)=="x"] <- "lon"
  names(sstFrame)[names(sstFrame)=="y"] <- "lat"
  return(sstFrame)
}

# function to convert lat/lon data frame into spatial data frame
latlon2sp <- function(in.df, center.UTM = center.UTM){
  coordinates(in.df) <- c("X", "Y")
  proj4string(in.df) <- CRS("+proj=longlat +datum=WGS84")
  out.df <- spTransform(in.df, CRS("+proj=utm +zone=10 ellps=WGS84"))
  out.df$newX <- (out.df$X - center.UTM$X)/1000
  out.df$newY <- (out.df$Y - center.UTM$Y)/1000
  return(out.df)
}

# function to convert the new coordinate system back to lat/lon
sp2latlon <- function(in.xy, center.UTM = center.UTM){
  X <- in.xy$newX * 1000 + center.UTM$X
  Y <- in.xy$newY * 1000 + center.UTM$Y
  in.df <- data.frame(X = X, Y = Y)
  coordinates(in.df) <- c('X', 'Y')
  proj4string(in.df) <- CRS("+proj=utm +zone=10 ellps=WGS84")
  out.df <- spTransform(in.df, CRS("+proj=longlat +datum=WGS84"))
  return(out.df)
}


# function to get islands and convert lat/lon into
# the common scale.
get.island <- function(filename, center.UTM = center.UTM){
  dat <- read.csv(paste0('Data/islands/', filename), header = F)
  colnames(dat) <- c('X', 'Y')
  dat$X <- dat$X - 360
  coordinates(dat) <- c("X", "Y")
  proj4string(dat) <- CRS("+proj=longlat +datum=WGS84")
  dat.Sp <- spTransform(dat, CRS("+proj=utm +zone=10 ellps=WGS84"))
  dat.Sp$newX <- (dat.Sp$X - center.UTM$X)/1000
  dat.Sp$newY <- (dat.Sp$Y - center.UTM$Y)/1000

  out <- list(df = data.frame(lat = dat$Y,
                              lon = dat$X,
                              newX = dat.Sp$newX,
                              newY = dat.Sp$newY,
                              name = unlist(strsplit(filename, '.csv'))),
              xy = data.frame(x = dat.Sp$newX,
                              y = dat.Sp$newY),
              name = unlist(strsplit(filename, '.csv')))

  return(out)
}

get.island.polygon <- function(df){
  poly <- Polygon(df)
  return(poly)
}

get.oce.data <- function(varname, date, xlim, ylim, tlim){
  filename <- paste0('RData/SegEnviro/', varname, date, '.RData')
  if (file.exists(filename) == FALSE){
    tmp<-xtracto_3D(xlim, ylim, tlim, varname)
    tmp$time<-as.Date(tmp$time)

    longitude <- tmp$longitude
    latitude <- tmp$latitude
    iday <- 1 : length(tmp$time)
    tmp2 <- lapply(iday,
                   function(x) melt_data(longitude,
                                         latitude,
                                         tmp$time[x],
                                         tmp$data))

    alltmp <- do.call(rbind, tmp2)
    save(alltmp, file = filename)
    return(1)
  }
}

get.oce.data.bkgd <- function(varname, date, xlim, ylim, tlim){
  filename <- paste0('RData/bkgd/', varname, date, '_bkgd.RData')
  if (file.exists(filename) == FALSE){
    tmp<-xtracto_3D(xlim, ylim, tlim, varname)
    tmp$time<-as.Date(tmp$time)

    longitude <- tmp$longitude
    latitude <- tmp$latitude
    iday <- 1 : length(tmp$time)
    tmp2 <- lapply(iday,
                   function(x) melt_data(longitude,
                                         latitude,
                                         tmp$time[x],
                                         tmp$data))

    alltmp <- do.call(rbind, tmp2)
    save(alltmp, file = filename)
    return(1)
  }
}

get.geo.data <- function(date.str, xlim, ylim){
  file.name <- paste0('Data/uv_data/uvgeo', date.str, '_bkgd.nc')
  if (file.exists(file.name) == F | file.info(file.name)$size == 0){
    curr.df <- get.geo.bkgd(date.str,
                            c(-122.8,-116.4),
                            c(30.85,34.59))
  } else {
    datafile <- nc_open(file.name)
    lon <- ncvar_get(datafile, varid = 'longitude')
    lat <- ncvar_get(datafile, varid = 'latitude')

    if (date.str > as.Date('2012-03-17')){
      u <- ncvar_get(datafile, varid = 'u')
      v <- ncvar_get(datafile, varid = 'v')
    } else {
      u <- ncvar_get(datafile, varid = 'u_current')
      v <- ncvar_get(datafile, varid = 'v_current')
    }

    nc_close(datafile)
    curr.df <- na.omit(currFrame(lon,lat,u,v))
    curr.df$date <- date.str
    colnames(curr.df) <-  c('long', 'lat', 'u', 'v', 'date')
  }

  if (curr.df$long[1] > 0) curr.df$long <- curr.df$long - 360
  data.out <- curr.df[curr.df$long >= xlim[1] &
                        curr.df$long <= xlim[2] &
                        curr.df$lat >= ylim[1] &
                        curr.df$lat <= ylim[2],]
  return(data.out)

}

get.sst.dt <- function(xlim, ylim, tlim){
  # Metadata:
  # http://upwell.pfeg.noaa.gov/erddap/info/jplMURSST41/index.html
  file.name <- paste0('Data/sst_data/sst_jplMURSST41_', tlim[1], '_', tlim[2] , '.nc')
  if (file.exists(file.name) == F | file.info(file.name)$size == 0){
    sstURL <- paste0('https://coastwatch.pfeg.noaa.gov/erddap/griddap/',
                     'jplMURSST41.nc?analysed_sst[(',
                     tlim[1], '):1:(', tlim[2], ')][(',
                     ylim[1], '):(', ylim[2], ')][(',
                     xlim[1], '):(', xlim[2], ')]')

    test <- download.file(sstURL,
                          destfile= file.name,
                          mode='wb')

  } else {
    test <- NA
  }
  return(test)

}

# http://upwell.pfeg.noaa.gov/erddap/griddap/jplMURSST41.nc?analysed_sst[(2015-10-16T09:00:00Z):1:(2015-10-30T09:00:00Z)][(28):1:(36)][(-127.5):1:(-115)],analysis_error[(2015-10-16T09:00:00Z):1:(2015-10-30T09:00:00Z)][(28):1:(36)][(-127.5):1:(-115)],mask[(2015-10-16T09:00:00Z):1:(2015-10-30T09:00:00Z)][(28):1:(36)][(-127.5):1:(-115)],sea_ice_fraction[(2015-10-16T09:00:00Z):1:(2015-10-30T09:00:00Z)][(28):1:(36)][(-127.5):1:(-115)]


get.geo.dt.jplOscar <- function(xlim, ylim, tlim){
  # Metadata:
  # http://coastwatch.pfeg.noaa.gov/erddap/info/jplOscar/index.html

  file.name <- paste0('Data/uv_data/uvgeo_jplOscar_', tlim[1], '_', tlim[2] , '.nc')

  if (file.exists(file.name) == F | file.info(file.name)$size == 0){
    #print('there!')
    xlim[xlim < 0] <- xlim[xlim < 0] + 360

    my.URL <- paste0('http://coastwatch.pfeg.noaa.gov/erddap/griddap/',
                     'jplOscar.nc?u[(', tlim[1], 'T00:00:00Z)][(15.0)][(',
                     ylim[2], '):(', ylim[1], ')][(',
                     xlim[1], '):(', xlim[2], ')],v[(',
                     tlim[1], 'T00:00:00Z)][(15.0)][(',
                     ylim[2], '):(', ylim[1], ')][(',
                     xlim[1], '):(', xlim[2], ')]')
    test <- download.file(my.URL, destfile= file.name, mode='wb')
    datafile <- nc_open(file.name)
    lon <- ncvar_get(datafile, varid = 'longitude')
    lat <- ncvar_get(datafile, varid = 'latitude')
    u <- ncvar_get(datafile, varid = 'u')
    v <- ncvar_get(datafile, varid = 'v')
    t.units <- datafile$dim$time$units
    t <- utcal.nc(t.units,
                  ncvar_get(datafile, varid = 'time'),
                  type = 's')

    nc_close(datafile)

    u.avg <- apply(u, MARGIN = c(1,2), FUN = mean, na.rm = T)
    v.avg <- apply(v, MARGIN = c(1,2), FUN = mean, na.rm = T)

    curr.df <- na.omit(currFrame(lon,lat,u.avg,v.avg))
    curr.df$date <- tlim[1]
    colnames(curr.df) <-  c('long', 'lat', 'u', 'v', 'date')
  }
}

get.geo.dt <- function(xlim, ylim, tlim){
  # metadata:
  # http://upwell.pfeg.noaa.gov/erddap/info/hawaii_soest_57b3_469e_7dc7/index.html
  file.name <- paste0('Data/uv_data/uvgeo_', tlim[1], '_', tlim[2] , '.nc')

  if (file.exists(file.name) == F | file.info(file.name)$size == 0){
    if (tlim[1] > as.Date('2012-03-17')){
      #print('there!')
      xlim[xlim < 0] <- xlim[xlim < 0] + 360
      my.URL <- paste0('https://upwell.pfeg.noaa.gov/erddap/griddap/',
                       'hawaii_soest_57b3_469e_7dc7.nc?u[(',
                       tlim[1], '):1:(', tlim[2], ')][(', ylim[1], '):1:(',
                       ylim[2], ')][(', xlim[1], '):1:(', xlim[2], ')],v[(',
                       tlim[1], '):1:(', tlim[2], ')][(', ylim[1], '):1:(',
                       ylim[2], ')][(', xlim[1], '):1:(', xlim[2], ')]')

      test <- download.file(my.URL, destfile= file.name, mode='wb')
      datafile <- nc_open(file.name)
      lon <- ncvar_get(datafile, varid = 'longitude')
      lat <- ncvar_get(datafile, varid = 'latitude')
      u <- ncvar_get(datafile, varid = 'u')
      v <- ncvar_get(datafile, varid = 'v')
      t.units <- datafile$dim$time$units
      t <- utcal.nc(t.units,
                    ncvar_get(datafile, varid = 'time'),
                    type = 's')

      nc_close(datafile)

      u.avg <- apply(u, MARGIN = c(1,2), FUN = mean, na.rm = T)
      v.avg <- apply(v, MARGIN = c(1,2), FUN = mean, na.rm = T)

      curr.df <- na.omit(currFrame(lon,lat,u.avg,v.avg))
      curr.df$date <- tlim[1]
      colnames(curr.df) <-  c('long', 'lat', 'u', 'v', 'date')

    } else {
      #print('here!')
      xlim[xlim > 180] <- xlim[xlim > 180] - 360
      my.URL <- paste0('https://upwell.pfeg.noaa.gov/erddap/griddap/',
                       'erdTAgeo1day_LonPM180.nc?u_current[(',
                       tlim[1], '):1:(', tlim[2], ')][(0.0):1:(0.0)][(',
                       floor(ylim[1]), '):1:(', ceiling(ylim[2]), ')][(',
                       floor(xlim[1]), '):1:(', ceiling(xlim[2]), ')],v_current[(',
                       tlim[1], '):1:(', tlim[2], ')][(0.0):1:(0.0)][(',
                       floor(ylim[1]), '):1:(',
                       ceiling(ylim[2]), ')][(',
                       floor(xlim[1]), '):1:(', ceiling(xlim[2]), ')]')

      test <- download.file(my.URL, destfile= file.name, mode='wb')
      datafile <- nc_open(file.name)
      lon <- ncvar_get(datafile, varid = 'longitude')
      lat <- ncvar_get(datafile, varid = 'latitude')
      u <- ncvar_get(datafile, varid = 'u_current')
      v <- ncvar_get(datafile, varid = 'v_current')
      t.units <- datafile$dim$time$units
      t <- utcal.nc(t.units,
                    ncvar_get(datafile, varid = 'time'),
                    type = 's')

      nc_close(datafile)

      u.avg <- apply(u, MARGIN = c(1,2), FUN = mean, na.rm = T)
      v.avg <- apply(v, MARGIN = c(1,2), FUN = mean, na.rm = T)

      curr.df <- na.omit(currFrame(lon,lat,u.avg,v.avg))
      curr.df$date <- tlim[1]

      colnames(curr.df) <-  c('long', 'lat', 'u', 'v', 'date')
    }

  } else {
    if (tlim[1] > as.Date('2012-03-17')){
      datafile <- nc_open(file.name)
      lon <- ncvar_get(datafile, varid = 'longitude')
      lat <- ncvar_get(datafile, varid = 'latitude')
      u <- ncvar_get(datafile, varid = 'u')
      v <- ncvar_get(datafile, varid = 'v')
      t.units <- datafile$dim$time$units
      t <- utcal.nc(t.units,
                    ncvar_get(datafile, varid = 'time'),
                    type = 's')

      nc_close(datafile)

      u.avg <- apply(u, MARGIN = c(1,2), FUN = mean, na.rm = T)
      v.avg <- apply(v, MARGIN = c(1,2), FUN = mean, na.rm = T)

      curr.df <- na.omit(currFrame(lon,lat,u.avg,v.avg))
      curr.df$date <- tlim[1]
      colnames(curr.df) <-  c('long', 'lat', 'u', 'v', 'date')

    } else {
      #print('here!')
      datafile <- nc_open(file.name)
      lon <- ncvar_get(datafile, varid = 'longitude')
      lat <- ncvar_get(datafile, varid = 'latitude')
      u <- ncvar_get(datafile, varid = 'u_current')
      v <- ncvar_get(datafile, varid = 'v_current')
      t.units <- datafile$dim$time$units
      t <- utcal.nc(t.units,
                    ncvar_get(datafile, varid = 'time'),
                    type = 's')

      nc_close(datafile)

      u.avg <- apply(u, MARGIN = c(1,2), FUN = mean, na.rm = T)
      v.avg <- apply(v, MARGIN = c(1,2), FUN = mean, na.rm = T)

      curr.df <- na.omit(currFrame(lon,lat,u.avg,v.avg))
      curr.df$date <- tlim[1]

      colnames(curr.df) <-  c('long', 'lat', 'u', 'v', 'date')

    }
  }

  return(curr.df)
}


get.geo.bkgd <- function(date.str, xlim, ylim){
  file.name <- paste0('Data/uv_data/uvgeo', date.str , '_bkgd.nc')

  tlim <- c(date.str, date.str)
  if (file.exists(file.name) == F | file.info(file.name)$size == 0){
    if (date.str > as.Date('2012-03-17')){
      #print('there!')
      my.URL <- paste0('https://upwell.pfeg.noaa.gov/erddap/griddap/',
                       'hawaii_soest_57b3_469e_7dc7.nc?u[(',
                       date.str, '):1:(', date.str, ')][(', ylim[1], '):1:(',
                       ylim[2], ')][(', xlim[1], '):1:(', xlim[2], ')],v[(',
                       tlim[1], '):1:(', tlim[2], ')][(', ylim[1], '):1:(',
                       ylim[2], ')][(', xlim[1], '):1:(', xlim[2], ')]')

      test <- download.file(my.URL, destfile= file.name, mode='wb')
      datafile <- nc_open(file.name)
      lon <- ncvar_get(datafile, varid = 'longitude')
      lat <- ncvar_get(datafile, varid = 'latitude')
      u <- ncvar_get(datafile, varid = 'u')
      v <- ncvar_get(datafile, varid = 'v')
      nc_close(datafile)
      curr.df <- na.omit(currFrame(lon,lat,u,v))
      curr.df$date <- date.str
      colnames(curr.df) <-  c('long', 'lat', 'u', 'v', 'date')

    } else {
      #print('here!')
      xlim[xlim > 180] <- xlim[xlim > 180] - 360
      my.URL <- paste0('https://upwell.pfeg.noaa.gov/erddap/griddap/',
                       'erdTAgeo1day_LonPM180.nc?u_current[(',
                       date.str, '):1:(', date.str, ')][(0.0):1:(0.0)][(',
                       floor(ylim[1]), '):1:(', ceiling(ylim[2]), ')][(',
                       floor(xlim[1]), '):1:(', ceiling(xlim[2]), ')],v_current[(',
                       tlim[1], '):1:(', tlim[2], ')][(0.0):1:(0.0)][(',
                       floor(ylim[1]), '):1:(',
                       ceiling(ylim[2]), ')][(',
                       floor(xlim[1]), '):1:(', ceiling(xlim[2]), ')]')

      test <- download.file(my.URL, destfile= file.name, mode='wb')
      datafile <- nc_open(file.name)
      lon <- ncvar_get(datafile, varid = 'longitude')
      lat <- ncvar_get(datafile, varid = 'latitude')
      u <- ncvar_get(datafile, varid = 'u_current')
      v <- ncvar_get(datafile, varid = 'v_current')
      nc_close(datafile)
      curr.df <- na.omit(currFrame(lon,lat,u,v))
      curr.df$date <- date.str
      colnames(curr.df) <-  c('long', 'lat', 'u', 'v', 'date')
    }

  } else {
    if (date.str > as.Date('2012-03-17')){
      datafile <- nc_open(file.name)
      lon <- ncvar_get(datafile, varid = 'longitude')
      lat <- ncvar_get(datafile, varid = 'latitude')
      u <- ncvar_get(datafile, varid = 'u')
      v <- ncvar_get(datafile, varid = 'v')
      nc_close(datafile)
      curr.df <- na.omit(currFrame(lon,lat,u,v))
      curr.df$date <- date.str
      colnames(curr.df) <-  c('long', 'lat', 'u', 'v', 'date')

    } else {
      #print('here!')
      datafile <- nc_open(file.name)
      lon <- ncvar_get(datafile, varid = 'longitude')
      lat <- ncvar_get(datafile, varid = 'latitude')
      u <- ncvar_get(datafile, varid = 'u_current')
      v <- ncvar_get(datafile, varid = 'v_current')
      nc_close(datafile)
      curr.df <- na.omit(currFrame(lon,lat,u,v))
      curr.df$date <- date.str
      colnames(curr.df) <-  c('long', 'lat', 'u', 'v', 'date')

    }
  }

  return(curr.df)
}

get.geo.large.bkgd.pre09Dec2012 <- function(date.str, xlim, ylim, tlim){

  get.oce.data.large.bkgd('erdTAugeo1day', date.str, xlim, ylim, tlim)
  get.oce.data.large.bkgd('erdTAvgeo1day', date.str, xlim, ylim, tlim)
  load(paste0('RData/large_bkgd/erdTAugeo1day',
               date.str, '_bkgd.RData'))
  ugeo.df <- na.omit(alltmp)

  load(paste0('RData/large_bkgd/erdTAvgeo1day',
                date.str, '_bkgd.RData'))
  vgeo.df <- na.omit(alltmp)

  uvgeo.df <- ugeo.df
  colnames(uvgeo.df) <- c('date', 'long', 'lat', 'u')
  uvgeo.df$v <- NA

  # mrege ugeo and vgeo:
  for (k1 in 1:dim(ugeo.df)[1]){
    tmp <- vgeo.df[round(vgeo.df$long,5) == round(ugeo.df$long[k1],5) &
                    round(vgeo.df$lat,2) == round(ugeo.df$lat[k1],2), 'var']
    if (length(tmp) == 1){
      uvgeo.df$v[k1] <- tmp
    }
  }
  uvgeo.df <- na.omit(uvgeo.df)
  return(uvgeo.df)
}

get.geo.large.bkgd <- function(date.str, xlim, ylim, tlim){
  file.name <- paste0('Data/uv_data/uvgeo', date.str, '_large_bkgd.nc')
  if (file.exists(file.name) == F){
    my.URL <- paste0('http://upwell.pfeg.noaa.gov/erddap/griddap/',
                     'hawaii_soest_57b3_469e_7dc7.nc?u[(',
                     date.str, '):1:(', date.str, ')][(', ylim[1], '):1:(',
                     ylim[2], ')][(', xlim[1], '):1:(', xlim[2], ')],v[(',
                     date.str, '):1:(', date.str, ')][(', ylim[1], '):1:(',
                     ylim[2], ')][(', xlim[1], '):1:(', xlim[2], ')]')

    test <- download.file(my.URL, destfile= file.name, mode='wb')

  }

  datafile <- nc_open(file.name)
  lon <- ncvar_get(datafile, varid = 'longitude')
  lat <- ncvar_get(datafile, varid = 'latitude')
  u <- ncvar_get(datafile, varid = 'u')
  v <- ncvar_get(datafile, varid = 'v')
  nc_close(datafile)
  curr.df <- na.omit(currFrame(lon,lat,u,v))
  curr.df$date <- date.str
  colnames(curr.df) <-  c('long', 'lat', 'u', 'v', 'date')
  return(curr.df)
}

get.oce.data.large.bkgd <- function(varname, date, xlim, ylim, tlim){
  filename <- paste0('RData/large_bkgd/', varname, date, '_bkgd.RData')
  if (file.exists(filename) == FALSE){
    print(tlim)
    tmp<-xtracto_3D(xlim, ylim, tlim, varname)
    tmp$time<-as.Date(tmp$time)
    # Chlorophyll first
    longitude <- tmp$longitude
    latitude <- tmp$latitude
    iday <- 1 : length(tmp$time)
    tmp2 <- lapply(iday,
                   function(x) melt_data(longitude,
                                         latitude,
                                         tmp$time[x],
                                         tmp$data))

    alltmp <- do.call(rbind, tmp2)
    save(alltmp, file = filename)
    return(1)
  }
}

extract.cov <- function(varname, date1, center.UTM = center.UTM, xy){

  if (file.exists(paste0('RData/SegEnviro/',
                         varname, date1, '.RData')) == FALSE){

    # study area is defined by these lat/lon limits
    # get.oce.data(varname, date1, c(237.2, 243.4),
    #              c(30.98, 34.58), c(date1, date1))
    get.oce.data(varname, date1, c(237, 245),
                 c(28, 36), c(date1, date1))
  }

  load(paste0('RData/SegEnviro/', varname, date1, '.RData'))
  x.min <- xy[1] - 1
  x.max <- xy[1] + 1
  y.min <- xy[2] - 1
  y.max <- xy[2] + 1

  tmp.df <- data.frame(X = alltmp$long,
                       Y = alltmp$lat,
                       var = alltmp$var)

  var.df.Sp <- latlon2sp(tmp.df, center.UTM)
  var.df <- data.frame(X = var.df.Sp$newX,
                       Y = var.df.Sp$newY,
                       var = tmp.df$var)

  var.xy <- var.df[var.df$X >= x.min & var.df$X <= x.max &
                     var.df$Y >= y.min & var.df$Y <= y.max, 'var']

  if (!is.null(length(var.xy))){
    var.out <- c(mean = mean(var.xy, na.rm = T),
                 sd = sd(var.xy, na.rm = T),
                 n = length(var.xy))
  }

  return(var.out)

}

# extract data within a box limits; this is slower...
subset.data.filter <- function(xy.min.max, var.df){
  var.xy <- filter(var.df, X >= xy.min.max[1] &
                     X <= xy.min.max[2] &
                     Y >= xy.min.max[3] &
                     Y <= xy.min.max[4])
}

# extract data within a box limits; this is faster than the one above
# not using function is even faster because of the overhead...
subset.data <- function(xy.min.max, var.df){
  var.xy <- var.df[var.df[,1] >= xy.min.max[1] &
                     var.df[,1] <= xy.min.max[2] &
                     var.df[,2] >= xy.min.max[3] &
                     var.df[,2] <= xy.min.max[4], ]
}

extract.cov.bkgd <- function(varname, date1, center.UTM = center.UTM, xy){
  if (file.exists(paste0('RData/indexed_SCB/', varname,
                         date1, '_idx_SCB.RData')) == F){
    print(paste('file', paste0('RData/indexed_SCB/', varname,
                               date1, '_idx_SCB.RData'),
                'did not exist.'))
    run <- TRUE
  } else {
    load(paste0('RData/indexed_SCB/', varname,
                date1, '_idx_SCB.RData'))
    # these if statements were created when some datasets were too small
    # I think... so I changed from dim(xy.means)[1] != 59400 to < (March 2017)
    if (exists('xy.means')){
      if (dim(xy.means)[1] < 59400)
        rm(list = 'xy.means')
      run <- TRUE
    }

    if (exists('xy.ranges')){
      if (dim(xy.ranges)[1] < 59400){
        rm(list = 'xy.ranges')
        run <- TRUE
      } else {
        run <- FALSE
      }
    } else {
      run <- TRUE
    }

  }

  if (run == TRUE){

    if (file.exists(paste0('RData/bkgd/',
                           varname, date1, '_bkgd.RData')) == FALSE){
      tlim <- c(date1, date1)
      xy.latlon <- xy2latlon(xy, center.UTM)
      xlim <- range(xy.latlon$x)
      ylim <- range(xy.latlon$y)
      tmp <- get.oce.data.bkgd(varname, date1, xlim, ylim, tlim)
    }

    load(paste0('RData/bkgd/', varname, date1, '_bkgd.RData'))

    # 2 km boxes
    x.range <- seq(from = min(xy$x),
                   to = max(xy$x), by = 2)

    y.range <- seq(from = min(xy$y),
                   to = max(xy$y), by = 2)

    # create the output data frame  ##########################
    xy.ranges <- expand.grid(x.range, y.range)
    names(xy.ranges) <- c('X', 'Y')
    xy.idx <- expand.grid(1:length(x.range), 1:length(y.range))
    names(xy.idx) <- c('X.idx', 'Y.idx')
    xy.ranges <- cbind(xy.ranges, xy.idx)

    # create a data frame just with lat, lon, variable
    tmp.df <- data.frame(X = alltmp$long,
                         Y = alltmp$lat,
                         var = alltmp$var)

    # convert it to a spatial df and add X/Y columns
    var.df.Sp <- latlon2sp(tmp.df, center.UTM)
    # convert back to non-spatial dff
    var.df <- data.frame(X = var.df.Sp$newX,
                         Y = var.df.Sp$newY,
                         var = tmp.df$var)

    # initialize index vectors
    # faster to make two loops than combined them together
    # missing.x <- vector(mode = 'numeric', length = length(x.range))
    for (x in 1:length(x.range)){
      var.df$X.idx[var.df$X >= x.range[x] &
                     var.df$X < (x.range[x]+2)] <- x
    }

    # missing.y <- vector(mode = 'numeric', length = length(y.range))
    for (y in 1:length(y.range)){
      var.df$Y.idx[var.df$Y >= y.range[y] &
                     var.df$Y < (y.range[y]+2)] <- y
    }
    # # Use ddply in 'plyr' package
    xy.means <- ddply(var.df, .(X.idx, Y.idx),
                      summarize,
                      mean = mean(var, na.rm = TRUE))

    xy.means <- na.omit(xy.means)
    xy.ranges$mean <- NA
    for (k in 1:dim(xy.ranges)[1]){
      tmp.val <- xy.means[xy.means$X.idx == xy.ranges[k, 'X.idx'] &
                            xy.means$Y.idx == xy.ranges[k, 'Y.idx'],
                          'mean']
      if (length(tmp.val) > 0) xy.ranges$mean[k] <- tmp.val
    }

    save(xy.ranges, file = paste0('RData/indexed_SCB/', varname,
                                 date1, '_idx_SCB.RData'))
  }
  return(xy.ranges)
}

extract.geo.bkgd <- function(date.str, center.UTM = center.UTM, xy){
  if (file.exists(paste0('RData/indexed_SCB/erdTAuvgeo',
                         date.str, '_idx_SCB.RData')) == F){
    run <- TRUE
  } else {
    load(paste0('RData/indexed_SCB/erdTAuvgeo',
                date.str, '_idx_SCB.RData'))
    if (exists('xy.ranges') == FALSE) {
      xy.ranges <- xy.means
      run <- FALSE
    }

    ifelse(nrow(xy.ranges) != 59400, run <- TRUE, run <- FALSE)
  }
  tlim <- c(date.str, date.str)
  xy.latlon <- xy2latlon(xy, center.UTM)
  xlim <- range(xy.latlon$x)
  ylim <- range(xy.latlon$y)

  if (run){
    if (file.exists(paste0('RData/bkgd/erdTAugeo1day',
                           date.str, '_bkgd.RData')) == F){

      # get both ugeo and vgeo
      if (date.str < as.Date('2012-12-09')){

        # these next two lines save .RData files if they haven't been
        # downloaded. If they have been, nothing happens.
        get.oce.data.bkgd('erdTAugeo1day', date.str, xlim, ylim, tlim)
        get.oce.data.bkgd('erdTAvgeo1day', date.str, xlim, ylim, tlim)

        load(paste0('RData/bkgd/erdTAugeo1day',
                    date.str, '_bkgd.RData'))
        ugeo.df <- na.omit(alltmp)

        load(paste0('RData/bkgd/erdTAvgeo1day',
                    date.str, '_bkgd.RData'))
        vgeo.df <- na.omit(alltmp)

        uvgeo.df <- ugeo.df
        colnames(uvgeo.df) <- c('date', 'long', 'lat', 'u')
        uvgeo.df$v <- NA

        # merge ugeo and vgeo:
        for (k1 in 1:dim(ugeo.df)[1]){
          tmp <- vgeo.df[round(vgeo.df$long,5) == round(ugeo.df$long[k1],5) &
                           round(vgeo.df$lat,2) == round(ugeo.df$lat[k1],2),
                         'var']
          if (length(tmp) == 1){
            uvgeo.df$v[k1] <- tmp
          }

        }
        uvgeo.df <- na.omit(uvgeo.df)
      } else {
        if (xlim[1] < 0) xlim <- xlim + 360
        uvgeo.df <- get.geo.bkgd(date.str, xlim, ylim)
        if (uvgeo.df[1,'long'] > 0) uvgeo.df[,'long'] <- uvgeo.df[,'long'] - 360
      }

    } else {
      if (xlim[1] < 0) xlim <- xlim + 360
      uvgeo.df <- get.geo.bkgd(date.str, xlim, ylim)
      if (uvgeo.df[1,'long'] > 0) uvgeo.df[,'long'] <- uvgeo.df[,'long'] - 360
    }

    dx <- 2
    tmp.df <- data.frame(X = uvgeo.df$long,
                         Y = uvgeo.df$lat,
                         u = uvgeo.df$u,
                         v = uvgeo.df$v)

    # var.df.Sp <- latlon2sp(tmp.df, center.UTM)
    # var.df <- data.frame(X = var.df.Sp$newX,
    #                      Y = var.df.Sp$newY,
    #                      u = var.df.Sp$u,
    #                      v = var.df.Sp$v)

    # dx km boxes to make plots visible
    x.range <- seq(from = min(xy$x),
                   to = max(xy$x), by = 2)

    y.range <- seq(from = min(xy$y),
                   to = max(xy$y), by = 2)

    #############################
    # create the output data frame
    xy.ranges <- expand.grid(x.range, y.range)
    names(xy.ranges) <- c('newX', 'newY')
    xy.ranges.Sp <- sp2latlon(xy.ranges, center.UTM)

    xy.idx <- as.data.frame(xy.ranges.Sp@coords)
    xy.idx$newX <- xy.ranges$newX
    xy.idx$newY <- xy.ranges$newY

    xy.idx$X.idx <- xy.idx$Y.idx <- NA
    xy.idx$u <- xy.idx$v <- NA

    unique.lat <- unique(tmp.df$Y)
    unique.lon <- unique(tmp.df$X)

    for (x in 1:length(unique.lon)){
      xy.idx[xy.idx$X >= unique.lon[x] &
               xy.idx$X < (unique.lon[x] + 0.25), 'X.idx'] <- x
      for (y in 1:length(unique.lat)){
        xy.idx[xy.idx$Y >= unique.lat[y] &
                 xy.idx$Y < (unique.lat[y] + 0.25), 'Y.idx'] <- y

        uv <- tmp.df[tmp.df$X == unique.lon[x] & tmp.df$Y == unique.lat[y],
                     c('u', 'v')]

        if (nrow(uv) > 0){
          xy.idx[xy.idx$X >= unique.lon[x] &
                   xy.idx$X < (unique.lon[x] + 0.25) &
                   xy.idx$Y >= unique.lat[y] &
                   xy.idx$Y < (unique.lat[y] + 0.25), 'u'] <- uv['u']

          xy.idx[xy.idx$X >= unique.lon[x] &
                   xy.idx$X < (unique.lon[x] + 0.25) &
                   xy.idx$Y >= unique.lat[y] &
                   xy.idx$Y < (unique.lat[y] + 0.25), 'v'] <- uv['v']

        }

      }

    }

    xy.ranges <- xy.idx
    # xy.means <- na.omit(xy.means)
    # xy.ranges$u <- xy.ranges$v <- NA
    # for (k in 1:dim(xy.ranges)[1]){
    #   tmp.val.u <- xy.means[xy.means$X.idx == xy.ranges[k, 'X.idx'] &
    #                         xy.means$Y.idx == xy.ranges[k, 'Y.idx'],
    #                         'u']
    #   tmp.val.v <- xy.means[xy.means$X.idx == xy.ranges[k, 'X.idx'] &
    #                           xy.means$Y.idx == xy.ranges[k, 'Y.idx'],
    #                         'v']
    #   if (length(tmp.val.u) > 0) xy.ranges$u[k] <- tmp.val.u
    #   if (length(tmp.val.v) > 0) xy.ranges$v[k] <- tmp.val.v
    # }
    #
    ############################

    save(xy.ranges, file = paste0('RData/indexed_SCB/erdTAuvgeo',
                                 date.str, '_idx_SCB.RData'))

  }

  return(xy.ranges)

}

extract.cov.large.bkgd <- function(varname, date1, center.UTM = center.UTM){

  if (file.exists(paste0('RData/indexed/', varname, date1, '_idx.RData')) == FALSE){
    load(paste0('RData/large_bkgd/', varname, date1, '_bkgd.RData'))
    alltmp <- na.omit(alltmp)
    tmp.df <- data.frame(X = alltmp$long,
                         Y = alltmp$lat,
                         var = alltmp$var)

    var.df.Sp <- latlon2sp(tmp.df, center.UTM)
    var.df <- data.frame(X = var.df.Sp$newX,
                         Y = var.df.Sp$newY,
                         var = var.df.Sp$var)

    # 2 km boxes
    x.range <- seq(from = min(var.df$X),
                   to = max(var.df$X), by = 2)
    y.range <- seq(from = min(var.df$Y),
                   to = max(var.df$Y), by = 2)

    # create the output data frame  ##########################
    xy.ranges <- expand.grid(x.range, y.range)
    names(xy.ranges) <- c('X', 'Y')
    xy.idx <- expand.grid(1:length(x.range), 1:length(y.range))
    names(xy.idx) <- c('X.idx', 'Y.idx')
    xy.ranges <- cbind(xy.ranges, xy.idx)

    # create a data frame just with lat, lon, variable
    tmp.df <- data.frame(X = alltmp$long,
                         Y = alltmp$lat,
                         var = alltmp$var)

    # convert it to a spatial df and add X/Y columns
    var.df.Sp <- latlon2sp(tmp.df, center.UTM)
    # convert back to non-spatial dff
    var.df <- data.frame(X = var.df.Sp$newX,
                         Y = var.df.Sp$newY,
                         var = tmp.df$var)

    # initialize index vectors
    # faster to make two loops than combined them together
    # missing.x <- vector(mode = 'numeric', length = length(x.range))
    for (x in 1:length(x.range)){
      var.df$X.idx[var.df$X >= x.range[x] &
                     var.df$X < (x.range[x]+2)] <- x
    }

    # missing.y <- vector(mode = 'numeric', length = length(y.range))
    for (y in 1:length(y.range)){
      var.df$Y.idx[var.df$Y >= y.range[y] &
                     var.df$Y < (y.range[y]+2)] <- y
    }
    # # Use ddply in 'plyr' package
    xy.means <- ddply(var.df, .(X.idx, Y.idx),
                      summarize,
                      mean = mean(var, na.rm = TRUE))

    # add foreach package and use parallel to see if faster
    # ,    .parallel = TRUE
    xy.means <- na.omit(xy.means)
    xy.ranges$var <- NA
    for (k in 1:dim(xy.ranges)[1]){
      tmp.val <- xy.means[xy.means$X.idx == xy.ranges[k, 'X.idx'] &
                            xy.means$Y.idx == xy.ranges[k, 'Y.idx'],
                          'mean']
      if (length(tmp.val) > 0) xy.ranges$var[k] <- tmp.val
    }
    ##################################################

    save(xy.ranges,
         file = paste0('RData/indexed/', varname, date1, '_idx.RData'))

  } else {
    load(paste0('RData/indexed/', varname, date1, '_idx.RData'))
    if (exists('xy.ranges', mode = 'list') == FALSE){
      xy.ranges <- xy.means   # shold be either xy.ranges or xy.means
    }
  }

  return(xy.ranges)

}


extract.geo.large.bkgd <- function(date.str, center.UTM = center.UTM){

  if (file.exists(paste0('RData/indexed/erdTAuvgeo',
                         date.str, '_idx.RData')) == F){
    run <- TRUE
  } else {
    load(paste0('RData/indexed/erdTAuvgeo',
                date.str, '_idx.RData'))
    if (exists('xy.ranges', mode = 'list') == FALSE){
      xy.ranges <- xy.means
      xy.ranges$newX <- xy.ranges$X
      xy.ranges$newY <- xy.ranges$Y
      run <- FALSE
    } else {
      run <- TRUE
    }
  }

  tlim <- c(date.str, date.str)
  xlim <- c(205, 243.4)
  ylim <- c(22, 40)
  if (run){

    uvgeo.df <- get.geo.large.bkgd(date.str, xlim, ylim, tlim)

    if (uvgeo.df[1,'long'] > 0) uvgeo.df[,'long'] <- uvgeo.df[,'long'] - 360

    dx <- 30  # make it large enough for the big picture...
    tmp.df <- data.frame(X = uvgeo.df$long,
                         Y = uvgeo.df$lat,
                         u = uvgeo.df$u,
                         v = uvgeo.df$v)

    tmp.df.Sp <- latlon2sp(tmp.df, center.UTM)
    xy <- tmp.df.Sp@data

    # dx km boxes to make plots visible
    x.range <- seq(from = min(xy$newX),
                   to = max(xy$newX), by = dx)
    y.range <- seq(from = min(xy$newY),
                   to = max(xy$newY), by = dx)

    # create the output data frame  ##########################
    xy.ranges <- expand.grid(x.range, y.range)
    names(xy.ranges) <- c('newX', 'newY')
    xy.ranges.Sp <- sp2latlon(xy.ranges, center.UTM)

    xy.idx <- as.data.frame(xy.ranges.Sp@coords)
    xy.idx$newX <- xy.ranges$newX
    xy.idx$newY <- xy.ranges$newY

    xy.idx$X.idx <- xy.idx$Y.idx <- NA
    xy.idx$u <- xy.idx$v <- NA

    unique.lat <- unique(tmp.df$Y)
    unique.lon <- unique(tmp.df$X)

    # resolution of geostrophic currents is 0.25 degrees
    # but use dx defined above?
    x <- y <- 1
    for (x in 1:length(unique.lon)){
      xy.idx[xy.idx$X >= unique.lon[x] &
               xy.idx$X < (unique.lon[x] + 0.25), 'X.idx'] <- x
      for (y in 1:length(unique.lat)){
        xy.idx[xy.idx$Y >= unique.lat[y] &
                 xy.idx$Y < (unique.lat[y] + 0.25), 'Y.idx'] <- y

        uv <- tmp.df[tmp.df$X == unique.lon[x] & tmp.df$Y == unique.lat[y],
                     c('u', 'v')]

        if (nrow(uv) > 0){
          xy.idx[xy.idx$X >= unique.lon[x] &
                   xy.idx$X < (unique.lon[x] + 0.25) &
                   xy.idx$Y >= unique.lat[y] &
                   xy.idx$Y < (unique.lat[y] + 0.25), 'u'] <- uv['u']

          xy.idx[xy.idx$X >= unique.lon[x] &
                   xy.idx$X < (unique.lon[x] + 0.25) &
                   xy.idx$Y >= unique.lat[y] &
                   xy.idx$Y < (unique.lat[y] + 0.25), 'v'] <- uv['v']

        }

      }

    }

    xy.ranges <- xy.idx
    save(xy.ranges, file = paste0('RData/indexed/erdTAuvgeo',
                                  date.str, '_idx.RData'))

  }

  return(xy.ranges)

}

extract.geo.ndays.large.bkgd <- function(date.str,
                                         ndays = 14,
                                         center.UTM = center.UTM){

  if (file.exists(paste0('RData/indexed/erdTAuvgeo_', ndays, '_',
                         date.str, '_idx.RData')) == F){
    run <- TRUE
  } else {
    load(paste0('RData/indexed/erdTAuvgeo_', ndays, '_',
                date.str, '_idx.RData'))
    if (exists('xy.ranges', mode = 'list') == FALSE){
      xy.ranges <- xy.means
      xy.ranges$newX <- xy.ranges$X
      xy.ranges$newY <- xy.ranges$Y
      run <- FALSE
    } else {
      run <- TRUE
    }
  }

  tlim <- c(date.str, as.Date(date.str) + (ndays-1))
  xlim <- c(205, 243.4)
  ylim <- c(22, 40)
  if (run){

    #uvgeo.df <- get.geo.large.bkgd(date.str, xlim, ylim, tlim)
    uvgeo.df <- get.geo.dt(xlim, ylim, tlim)

    if (uvgeo.df[1,'long'] > 0) uvgeo.df[,'long'] <- uvgeo.df[,'long'] - 360

    dx <- 30  # make it large enough for the big picture...
    tmp.df <- data.frame(X = uvgeo.df$long,
                         Y = uvgeo.df$lat,
                         u = uvgeo.df$u,
                         v = uvgeo.df$v)

    tmp.df.Sp <- latlon2sp(tmp.df, center.UTM)
    xy <- tmp.df.Sp@data

    # dx km boxes to make plots visible
    x.range <- seq(from = min(xy$newX),
                   to = max(xy$newX), by = dx)
    y.range <- seq(from = min(xy$newY),
                   to = max(xy$newY), by = dx)

    # create the output data frame  ##########################
    xy.ranges <- expand.grid(x.range, y.range)
    names(xy.ranges) <- c('newX', 'newY')
    xy.ranges.Sp <- sp2latlon(xy.ranges, center.UTM)

    xy.idx <- as.data.frame(xy.ranges.Sp@coords)
    xy.idx$newX <- xy.ranges$newX
    xy.idx$newY <- xy.ranges$newY

    xy.idx$X.idx <- xy.idx$Y.idx <- NA
    xy.idx$u <- xy.idx$v <- NA

    unique.lat <- unique(tmp.df$Y)
    unique.lon <- unique(tmp.df$X)

    # resolution of geostrophic currents is 0.25 degrees
    # but use dx defined above?
    x <- y <- 1
    for (x in 1:length(unique.lon)){
      xy.idx[xy.idx$X >= unique.lon[x] &
               xy.idx$X < (unique.lon[x] + 0.25), 'X.idx'] <- x
      for (y in 1:length(unique.lat)){
        xy.idx[xy.idx$Y >= unique.lat[y] &
                 xy.idx$Y < (unique.lat[y] + 0.25), 'Y.idx'] <- y

        uv <- tmp.df[tmp.df$X == unique.lon[x] & tmp.df$Y == unique.lat[y],
                     c('u', 'v')]

        if (nrow(uv) > 0){
          xy.idx[xy.idx$X >= unique.lon[x] &
                   xy.idx$X < (unique.lon[x] + 0.25) &
                   xy.idx$Y >= unique.lat[y] &
                   xy.idx$Y < (unique.lat[y] + 0.25), 'u'] <- uv['u']

          xy.idx[xy.idx$X >= unique.lon[x] &
                   xy.idx$X < (unique.lon[x] + 0.25) &
                   xy.idx$Y >= unique.lat[y] &
                   xy.idx$Y < (unique.lat[y] + 0.25), 'v'] <- uv['v']

        }

      }

    }

    xy.ranges <- xy.idx
    save(xy.ranges, file = paste0('RData/indexed/erdTAuvgeo_', ndays, '_',
                                  date.str, '_idx.RData'))

  }

  return(xy.ranges)

}


# converts my coordinates to lat/lon
xy2latlon <- function(xy, center.UTM = center.UTM){

  # first convert back into UTM coordinates for zone 10
  x <- (xy[,1] * 1000) + center.UTM$X
  y <- (xy[,2] * 1000) + center.UTM$Y
  # define these are UTM +10
  xy.UTM <- SpatialPoints(cbind(x, y), proj4string = CRS("+proj=utm +zone=10"))

  # then convert them into lat lon using WGS84
  xy.latlon <- spTransform(xy.UTM, CRS("+proj=longlat"))
  return(xy.latlon)
}

# get track lines
get.track.lines <- function(filename){
  lines.1 <- read.csv(filename)
  lines.1$month <- round(lines.1$Date1/10000)
  lines.1$day <- round((lines.1$Date1 - lines.1$month * 10000)/100)
  lines.1$year <- 2000 + round(lines.1$Date1 - 100 * (round(lines.1$Date1/100)))
  lines.1$date <- paste(lines.1$year,
                        formatC(lines.1$month, width=2,
                                format="d", flag="0"),
                        formatC(lines.1$day, width=2,
                                format="d", flag="0"),
                        sep = '-')
  lines.1.begin <- lines.1[, c('Date1', 'Time1', 'Lat1', 'Lon1',
                               'Distance', 'Line')]
  lines.1.begin$X <- lines.1.begin$Lon1
  lines.1.begin$Y <- lines.1.begin$Lat1
  lines.1.end <- lines.1[, c('Date2', 'Time2', 'Lat2', 'Lon2',
                             'Distance', 'Line')]
  lines.1.end$X <- lines.1.end$Lon2
  lines.1.end$Y <- lines.1.end$Lat2

  lines.1.begin.sp <- latlon2sp(lines.1.begin, center.UTM)
  lines.1.end.sp <- latlon2sp(lines.1.end, center.UTM)
  lines.1.begin.end <- cbind(lines.1.begin.sp@data[,c("Date1",
                                                      "newX",
                                                      "newY")],
                             lines.1.end.sp@data[,c("newX", "newY")],
                             lines.1.begin$Distance)

  names(lines.1.begin.end) <- c("date", "beginX",
                                "beginY", "endX", "endY", "Distance_nm")
  lines.1.begin.end$Distance_km <- nm2km(lines.1.begin.end$Distance_nm)

  effort.by.day <- aggregate(lines.1.begin.end$Distance_nm,
                             by = list(lines.1.begin.end$date),
                             FUN = sum)
  colnames(effort.by.day) <- c('date', 'nm')
  effort.by.day$km <- nm2km(effort.by.day$nm)

  effort.total <- data.frame(nm = sum(lines.1$Distance),
                             km = nm2km(sum(lines.1$Distance)))

  out.list <- list(lines = lines.1.begin.end,
                   effort.total = effort.total,
                   effort.day = effort.by.day,
                   data = lines.1)
  return(out.list)
}

dsm.dev.explained <- function(M){
  dev.explained <- (M$null.deviance - M$deviance)/M$null.deviance
  return(dev.explained)
}

## some constants are defined here:
#load('RData/studyArea.RData')
# 
# # Montgomery-Gibbs airport location:
# MYF <- data.frame(lat = 32.8158, lon = -117.1394)
# coordinates(MYF) <- c('lon', 'lat')
# proj4string(MYF) <- CRS("+proj=longlat +datum=WGS84")
# 
# # McClellan-Palomar airport
# CRQ <- data.frame(lat = 33.1283, lon = -117.2800)
# coordinates(CRQ) <- c('lon', 'lat')
# proj4string(CRQ) <- CRS("+proj=longlat +datum=WGS84")
# 
# # Ramona airport
# RNM <- data.frame(lat = 33.0392, lon = -116.9153)
# coordinates(RNM) <- c('lon', 'lat')
# proj4string(RNM) <- CRS("+proj=longlat +datum=WGS84")
# 
# # get the data in:
# # 2011 and 2015 data that are split in 2 km chunks are here
# # these were created using AirSegChoCc_2016_04_11.R on merged data files
# # which were created using combineFiles.m in Matlab. AirSegChop script
# # was first created by Elizabeth Becker and Karin Forney and was modified
# # for turtle sightings by me.
# dat2011 <- read.csv('Data/processed/SEGDATA_SCB2011_DAS_2km_2016-06-06.csv')
# dat2015 <- read.csv('Data/processed/SEGDATA_SCB2015_DAS_2km_2016-06-06.csv')
# 
# # dist variable in the above files is the length of each segment and NOT
# # distances of the objects from the track line.
# 
# # sightings data are in "SITEINFO_SCB" files
# Sdata.2011 <- read.csv('Data/processed/SITEINFO_SCB2011_DAS_2km_2016-06-06.csv')
# Sdata.2015 <- read.csv('Data/processed/SITEINFO_SCB2015_DAS_2km_2016-06-06.csv')
# 
# # Add date info merged between Sdata and dat
# date.2011.df <- dat2011[dat2011$segnum %in% Sdata.2011$segnum,
#                         c('segnum', 'year', 'month', 'day')]
# date.2015.df <- dat2015[dat2015$segnum %in% Sdata.2015$segnum,
#                         c('segnum', 'year', 'month', 'day')]
# # then assign the averages to each sighting - there probably is a more elegant
# # way of doing this but loops do the job...
# Sdata.2011$year <- Sdata.2011$month <- Sdata.2011$day <- NA
# for (k in 1:dim(Sdata.2011)[1]){
#   Sdata.2011[k, c('year',
#                   'month',
#                   'day')] <- date.2011.df[Sdata.2011$segnum[k] == date.2011.df$segnum,
#                                           c('year', 'month', 'day')]
# }
# Sdata.2015$year <- Sdata.2015$month <- Sdata.2015$day <- NA
# for (k in 1:dim(Sdata.2015)[1]){
#   Sdata.2015[k, c('year',
#                   'month',
#                   'day')] <- date.2015.df[Sdata.2015$segnum[k] == date.2015.df$segnum,
#                                           c('year', 'month', 'day')]
# }
# 
# # create Sdata that includes both 2011 and 2015
# Sdata.2011$transectID <- paste(Sdata.2011$year,
#                                Sdata.2011$transectNum,
#                                sep = '_')
# 
# Sdata.2015$transectID <- paste(Sdata.2015$year,
#                                Sdata.2015$transectNum,
#                                sep = '_')
# 
# bft.2011.df <- dat2011[dat2011$segnum %in% Sdata.2011$segnum,
#                        c('segnum', 'aveBF')]
# bft.2015.df <- dat2015[dat2015$segnum %in% Sdata.2015$segnum,
#                        c('segnum', 'aveBF')]
# # then assign the averages to each sighting - there probably is a more elegant
# # way of doing this but loops do the job...
# Sdata.2011$Beaufort <- NA
# for (k in 1:dim(Sdata.2011)[1]){
#   Sdata.2011$Beaufort[k] <- bft.2011.df$aveBF[Sdata.2011$segnum[k] == bft.2011.df$segnum]
# }
# Sdata.2015$Beaufort <- NA
# for (k in 1:dim(Sdata.2015)[1]){
#   Sdata.2015$Beaufort[k] <- bft.2015.df$aveBF[Sdata.2015$segnum[k] == bft.2015.df$segnum]
# }
# 
# Sdata <- rbind(Sdata.2011, Sdata.2015)
# 
# # compute the perpendicular distances in meters
# Sdata$PerpDist <- ft2m(alt * tan(deg2rad(90 - abs(Sdata$DecAngle))))
# 
# # fix slon to all negative values:
# Sdata$slon[Sdata$slon > 0] <- Sdata$slon[Sdata$slon > 0] - 360
# 
# ccData <- subset(Sdata, species == 'cc')
# #ccDataOn <- subset(ccData, Effort == 1)
# 
# # perp distance in km
# ccData$distance <- ccData$PerpDist/1000
# 
# # convert lat/lon to x/y
# ccData$X <- ccData$mlon
# ccData$Y <- ccData$mlat
# ccData.Sp <- latlon2sp(ccData, center.UTM)

# get line# length of each line from the coast line (m)
line.length <- nm2km(20) * 1000 
s
#lines.2015 <- get.track.lines('Data/tmpTracks_2015.txt')
# Distance is in nm
# lines.2015 <- get.track.lines('Data/tmpTracks_Nov2016.txt')
# lines.2011 <- get.track.lines('Data/tmpTracks_2011.txt')
# lines.2011$lines$Year <- 2011
# lines.2015$lines$Year <- 2015
# 
# lines.df <- rbind(cbind(lines.2011$lines, Date = lines.2011$data$date),
#                   cbind(lines.2015$lines, Date = lines.2015$data$date))
# 
# lines.df$distance <- sqrt((lines.df$endX - lines.df$beginX)^2 +
#                             (lines.df$endY - lines.df$beginY)^2)
# 
# dplyr::select(lines.df, starts_with("begin")) %>%  # get begin and end points
#   dplyr::rename(., newX = beginX, newY = beginY) %>%  # rename them so sp2latlon can work
#   sp2latlon(., center.UTM) %>%  # convert spatial points to lat/lon
#   as.data.frame() %>%           # change it to a regular dataframe
#   dplyr::rename(., beginX = X, beginY = Y) -> lines.df.begin.latlon  # rename them to beginX.beginY
# 
# dplyr::select(lines.df, endX, endY) %>%
#   dplyr::rename(., newX = endX, newY = endY) %>%
#   sp2latlon(., center.UTM)  %>%
#   as.data.frame() %>%
#   dplyr::rename(., endX = X, endY = Y) -> lines.df.end.latlon
# 
# lines.df.latlon <- data.frame(date = as.Date(lines.df$Date),
#                               Year = lines.df$Year,
#                               beginX = lines.df.begin.latlon$beginX,
#                               beginY = lines.df.begin.latlon$beginY,
#                               endX = lines.df.end.latlon$endX,
#                               endY = lines.df.end.latlon$endY,
#                               distance = lines.df$distance)
# 
# # sightings data are here: - new data file includes all of them
# #sightings.data <- read.csv('Data/CcSightingsQuery_28Feb2017.txt')
# #sightings.data$date <- as.character(strptime(sightings.data$Date_Observed,
# #                                             format = "%m/%d/%Y"))
# 
# # more sightings from MM cruises:
# # sightings.data.2 <- read.csv('Data/CcSightingsFromCruises.csv')
# # sightings.data.2$date <- as.character(strptime(sightings.data.2$Date,
# #                                                format = "%m/%d/%Y"))
# #tmp2 <- sightings.data.2[, c('date', 'Lat', 'Lon')]
# #colnames(tmp2) <- c('date', 'Latitude', 'Longitude')
# 
# #sightings.data <- rbind(sightings.data.1[, c('date',
#                         #                      'Latitude',
#                         #                      'Longitude')],
#                         # tmp2)
# 
# #colnames(sightings.data) <- c('Date', 'Latitude', 'Longitude')
# 
# # segmented data are here:
# # chopped up segments of transect lines
# # covariates should be included in these.
# # use the output from addCovs2Segs.R
# # Convert segdata into a spatial data frame
# dat2015$X <- dat2015$mlon
# dat2015$Y <- dat2015$mlat
# dat2015.Sp <- latlon2sp(dat2015, center.UTM)
# 
# # and for 2011
# dat2011$X <- dat2011$mlon
# dat2011$Y <- dat2011$mlat
# dat2011.Sp <- latlon2sp(dat2011, center.UTM)
# 
# segment.data.2015 <- data.frame(Effort = dat2015$dist,
#                                 Sample.Label = dat2015$segnum,
#                                 BF = dat2015$aveBF,
#                                 x = dat2015.Sp$newX,
#                                 y = dat2015.Sp$newY,
#                                 Transect.Label = dat2015$transectNum,
#                                 year = dat2015.Sp$year,
#                                 month = dat2015.Sp$month,
#                                 day = dat2015.Sp$day)
# 
# segment.data.2011 <- data.frame(Effort = dat2011$dist,
#                                 Sample.Label = dat2011$segnum,
#                                 BF = dat2011$aveBF,
#                                 x = dat2011.Sp$newX,
#                                 y = dat2011.Sp$newY,
#                                 Transect.Label = dat2011$transectNum,
#                                 year = dat2011.Sp$year,
#                                 month = dat2011.Sp$month,
#                                 day = dat2011.Sp$day)
# 
# segment.data <- rbind(segment.data.2011, segment.data.2015)
# 
# # for plotting segments:
# begin.2015 <- dat2015[, c('lat1', 'lon1', 'aveBF')]
# colnames(begin.2015) <- c('Y', 'X', 'aveBF')
# begin.2015.Sp <- latlon2sp(begin.2015, center.UTM)
# 
# end.2015 <- dat2015[, c('lat2', 'lon2', 'aveBF')]
# colnames(end.2015) <- c('Y', 'X', 'aveBF')
# end.2015.Sp <- latlon2sp(end.2015, center.UTM)
# 
# transect.segments.2015 <- data.frame(beginX = begin.2015.Sp$newX,
#                                      beginY = begin.2015.Sp$newY,
#                                      endX = end.2015.Sp$newX,
#                                      endY = end.2015.Sp$newY,
#                                      aveBF = begin.2015.Sp$aveBF)
# 
# begin.2011 <- dat2011[, c('lat1', 'lon1', 'aveBF')]
# colnames(begin.2011) <- c('Y', 'X', 'aveBF')
# begin.2011.Sp <- latlon2sp(begin.2011, center.UTM)
# 
# end.2011 <- dat2011[, c('lat2', 'lon2', 'aveBF')]
# colnames(end.2011) <- c('Y', 'X', 'aveBF')
# end.2011.Sp <- latlon2sp(end.2011, center.UTM)
# 
# transect.segments.2011 <- data.frame(beginX = begin.2011.Sp$newX,
#                                      beginY = begin.2011.Sp$newY,
#                                      endX = end.2011.Sp$newX,
#                                      endY = end.2011.Sp$newY,
#                                      aveBF = begin.2011$aveBF)
# 
# # split them into inshore and offshore sections:
# n.lines <- (sum(!is.na(all.lines.2$Lat_middle)) * 2) +
#   sum(is.na(all.lines.2$Lat_middle))
# 
# lines.strata <- matrix(data = NA, nrow = n.lines, ncol = 7)
# c <- 1
# for (k in 1:dim(all.lines.2)[1]){
#   line1 <- all.lines.2[k,]
#   if (!is.na(line1$Lat_middle)){
#     lines.strata[c, 1] <- c
#     lines.strata[c, 2] <- line1$Lat_offshore
#     lines.strata[c, 3] <- line1$Lon_offshore
#     lines.strata[c, 4] <- line1$Lat_middle
#     lines.strata[c, 5] <- line1$Lon_middle
#     lines.strata[c, 6] <- 1
#     lines.strata[c, 7] <- line1$Line
#     c <- c + 1
# 
#     lines.strata[c, 1] <- c
#     lines.strata[c, 2] <- line1$Lat_middle
#     lines.strata[c, 3] <- line1$Lon_middle
#     lines.strata[c, 4] <- line1$Lat_inshore
#     lines.strata[c, 5] <- line1$Lon_inshore
#     lines.strata[c, 6] <- 0
#     lines.strata[c, 7] <- line1$Line
#     c <- c + 1
# 
#   } else {
#     lines.strata[c, 1] <- c
#     lines.strata[c, 2] <- line1$Lat_offshore
#     lines.strata[c, 3] <- line1$Lon_offshore
#     lines.strata[c, 4] <- line1$Lat_inshore
#     lines.strata[c, 5] <- line1$Lon_inshore
#     lines.strata[c, 6] <- 1
#     lines.strata[c, 7] <- line1$Line
#     c <- c + 1
#   }
# }
# 
# lines.strata.df <- as.data.frame(lines.strata)
# colnames(lines.strata.df) <- c('line', 'lat_offshore', 'lon_offshore',
#                                'lat_inshore', 'lon_inshore', 'offshore', 'ID')
# 
# #write.csv(x = lines.strata.df,
# #          file = 'Data/transectLines_strata.csv',
# #          quote = FALSE, row.names = FALSE)
# 
# lines.strata.df.0 <- lines.strata.df
# 
# # get new x and y for offshore
# # first change column names to Y and X so latlon2sp can read it
# colnames(lines.strata.df.0) <- c('line', 'Y', 'X',
#                                  'lat_inshore', 'lon_inshore',
#                                  'offshore', 'ID')
# # convert the dataframe into a sptial object and add the
# # coordinate system using center.UTM
# lines.strata.df.Sp <- latlon2sp(lines.strata.df.0, center.UTM)
# 
# # Change the column names; Y and X were removed and newX and newY
# # were appended at the end. So, where Y and X used to be are now
# # "lat_inshore" and "lon_inshore", which need to be replaced by
# # Y and X for latlon2sp to work:
# lines.strata.df.1 <- lines.strata.df.Sp@data
# colnames(lines.strata.df.1) <- c('line', 'Y', 'X', 'offshore', 'ID',
#                                'newX_offshore', 'newY_offshore')
# 
# # send it in to latlon2sp and convert all those points to be
# # centered at center.UTM.
# lines.strata.df.Sp <- latlon2sp(lines.strata.df.1, center.UTM)
# lines.strata.df.2 <- lines.strata.df.Sp@data
# colnames(lines.strata.df.2) <- c('line', 'offshore', 'ID',
#                                  'newX_offshore', 'newY_offshore',
#                                  'newX_inshore', 'newY_inshore')

# to create shapefile, use the following:
#shapefile(lines.strata.df.Sp, filename = 'transect_lines.shp')

## End of useful code

# the following was tried using the parallel but turned out to be slower
# than expected. A faster version is found above. Saved
# for a reference
# extract.cov.large.bkgd.parallel <- function(varname, date1, center.UTM, xy){
#
#   load(paste0('RData/large_bkgd/', varname, date1, '_bkgd.RData'))
#   alltmp <- na.omit(alltmp)
#   tmp.df <- data.frame(X = alltmp$long,
#                        Y = alltmp$lat,
#                        var = alltmp$var)
#
#   var.df.Sp <- latlon2sp(tmp.df, center.UTM)
#   var.df <- data.frame(X = var.df.Sp$newX,
#                        Y = var.df.Sp$newY,
#                        var = tmp.df$var)
#
#   # 2 km boxes so subtract 1 and add 1 to the center.
#   x.min <- xy[,1] - 1
#   x.max <- xy[,1] + 1
#   y.min <- xy[,2] - 1
#   y.max <- xy[,2] + 1
#
#   xy.min.max <- cbind(x.min, x.max, y.min, y.max)
#   #xy.min.max <- xy.min.max.1[1:10000,]
#
#   var.mean <- var.SD<-var.n <- vector(mode = "numeric",
#                                     length = dim(xy.min.max)[1])
#
#   # split the dfs into chunks depending on the # workers:
#   idx <- rep(1:getDoParWorkers(),
#              times = dim(xy.min.max)[1]/getDoParWorkers())
#   tmp.dfs <- split(as.data.frame(xy.min.max), idx)
#
#   #system.time() - compared the usual subsetting and filter
#   # version... turned out the regular one is 3 times faster.
#   # , .export = c("getDoParWorkers")
#
#   zm1 <- foreach(i = 1:getDoParWorkers()) %dopar% {
#     xy <- tmp.dfs[[i]]
#     for (k in 1:dim(xy)[1]){
#       tmp <- var.df[var.df[,1] >= xy[k,1] &
#                       var.df[,1] <= xy[k,2] &
#                       var.df[,2] >= xy[k,3] &
#                       var.df[,2] <= xy[k,4], ]
#       #system.time(tmp1 <- subset.data(xy.min.max[k,], var.df))
#       var.mean[k] <- mean(tmp$var, na.rm = T)
#       var.SD[k] <- sd(tmp$var, na.rm = T)
#       var.n[k] <- dim(tmp)[1]
#     }
#     zm0 <- data.frame(mean = var.mean,
#                       sd = var.SD,
#                       n = var.n,
#                       x.min = xy[,1],
#                       x.max = xy[,2],
#                       y.min = xy[,3],
#                       y.max = xy[,4])
#
#     return(zm0)
#   }
#
#   var.out <- do.call(rbind, zm1)
#   return(var.out)
#
# }
