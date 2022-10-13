#Cc_SCB_functions



library(dsm)
library(rgdal)
library(ggplot2)
library(Distance)
library(sp)
library(xtractomatic)
library(reshape2)
library(plyr)
library(dplyr)
library(ncdf4)
library(grid)
library(RNetCDF)

library(tidyverse)
library(lubridate)
library(geosphere)

sysInfo <- Sys.info()
ifelse(sysInfo[1] == 'Linux',
       source('~/Documents/R/TomosFunctions.R'),
       source('~/R/TomosFunctions.R'))


# convert the lat/lon into northing/easting
# the study area covers zones 10 and 11. An arbitrary center point
# was created here.
approx.center <- data.frame(X=-119.967, Y=33.092)
coordinates(approx.center) <- c("X", "Y")
proj4string(approx.center) <- CRS("+proj=longlat +datum=WGS84")
center.UTM <- spTransform(approx.center,
                          CRS("+proj=utm +zone=10 ellps=WGS84"))

# function to convert lat/lon data frame into spatial data frame
latlon2sp <- function(in.df, center.UTM){
  coordinates(in.df) <- c("X", "Y")
  proj4string(in.df) <- CRS("+proj=longlat +datum=WGS84")
  out.df <- spTransform(in.df, CRS("+proj=utm +zone=10 ellps=WGS84"))
  out.df$newX <- (out.df$X - center.UTM$X)/1000
  out.df$newY <- (out.df$Y - center.UTM$Y)/1000
  return(out.df)
}

# function to convert the new coordinate system back to lat/lon
sp2latlon <- function(in.xy, center.UTM){
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
get.island <- function(filename, center.UTM){
  dat <- read.csv(paste0('data/islands/', filename), header = F)
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

## some constants are defined here:
load(paste0(dirSelector()$Rdir, 'SCB_AerialSurvey/RData/studyArea.RData'))

