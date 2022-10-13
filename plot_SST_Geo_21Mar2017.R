#plot_SST
# plots SST maps

rm(list=ls())
# sysInfo <- Sys.info()
# ifelse(sysInfo[1] == 'Linux',
#        source('~/Documents/R/TomosFunctions.R'),
#        source('~/R/TomosFunctions.R'))

source('CcSCB_functions.R')
library(viridis)

# load one prediction file to get lat/lon resolution:
#load("RData/predictions/predictionDataWithCovariatesDepth_2015-10-31.RData")
#prediction.data.geo <- prediction.data.cov2[,c("x", "y", "latitude", "longitude")]

file.names <- list.files(path = "RData/indexed_SCB/jplMURSST_studyarea",
                         pattern = 'jplMURSST',
                         include.dirs = F)

# create a new data frame for plotting
study.area.df <- data.frame(x = study.area.Sp$newX,
                            y = study.area.Sp$newY)

# these are from CcSCB_function.R
Sdata <- rbind(Sdata.2011, Sdata.2015)
# Create a date field - formatC works like the C function printf
Sdata$date <- paste(Sdata$year,
                    formatC(Sdata$month, width=2, format="d", flag="0"),
                    formatC(Sdata$day, width=2, format="d", flag="0"),
                    sep = '-')

ccData <- subset(Sdata, species == 'cc')
# convert lat/lon to x/y and extract date
latlon.df <- ccData[, c('mlon', 'mlat')]
names(latlon.df) <- c("X", "Y")
xy.df <- latlon2sp(latlon.df, center.UTM)@data
xy.df <- cbind(xy.df, ccData[, c('year', 'month', 'day')])

land.color <- '#333333'
alpha.value <- 0.8
wf <- 100
k <- 2
for (k in 1:length(file.names)){

  var.date.str <- unlist(strsplit(unlist(strsplit(file.names[k], '_'))[1],
                                  '_bkgd.RData'))
  date.str <- unlist(strsplit(var.date.str, 'jplMURSST'))[2]
  print(paste(k, 'in', length(file.names), ':', date.str))

  out.filename <- paste0('Figures/uv_SCB/uv_sst_',
                         date.str, '.png')

  if (file.exists(out.filename) == FALSE){

    sst <- extract.cov.bkgd('jplMURSST', date.str,
                            center.UTM,
                            prediction.data[, c('x', 'y')])

    sst <- na.omit(sst)
    # Geo currents are in 2 x 2 km as of 10/19/2016
    uv.geo <- extract.geo.bkgd(date.str, center.UTM,
                               prediction.data[, c('x', 'y')])
    uv.geo <- na.omit(uv.geo)
    # plot sightings if any
    sightings.1 <- sightings.data[sightings.data$date == as.Date(date.str),
                                  c('Longitude', 'Latitude')]
    sightings.2 <- ccData[ccData$date == as.Date(date.str),
                          c('mlon', 'mlat')]
    colnames(sightings.2) <- c('Longitude', 'Latitude')

    sightings.all <- rbind(sightings.1, sightings.2)
    names(sightings.all) <- c("X", "Y")

    # make a study area figure with transect lines overlaid...
    # create a new data frame for plotting
    lines.1 <- lines.df[lines.df$date == as.Date(date.str), ]

    uv.geo2 <- uv.geo[2001:2300,]
    p1 <- ggplot() +
      geom_raster(data = sst,
                  aes(x = X, y = Y, fill = mean)) +
      #scale_color_viridis(na.value = land.color,
      #                    limits = c(10, 25),
      #                    discrete = T) +
      scale_fill_gradientn(na.value = land.color,
                           limits = c(10, 25),
                           colors = viridis(n = 20)) +
      geom_polygon(fill = land.color,
                   data = all.islands.df,
                   aes(x=newX, y=newY, group = name),
                   inherit.aes = F) +
      geom_polygon(fill = land.color,
                   data = coast.line.xy,   # needs to be expanded a little
                   aes(x=newX, y=newY))  +
      geom_segment(data = uv.geo,
                   aes(x = newX, xend = newX + u * wf,
                       y = newY, yend = newY + v * wf),
                   arrow = arrow(length = unit(0.02, 'cm'))) +

      ylab("y") +
      xlab("x") +
      ggtitle(date.str)

    if (dim(lines.1)[1] >0){
      p1 <-  p1 +
        geom_segment(data = lines.1,
                     aes(x = beginX, xend = endX,
                         y = beginY, yend = endY))
    }

    if (dim(sightings.all)[1] > 0){
      sightings.all.XY <- latlon2sp(sightings.all, center.UTM)@data
      row.names(sightings.all.XY) <- NULL
      p1 <- p1 +
        geom_point(data = sightings.all.XY,
                   aes(x = newX, y = newY),
                   colour = 'black',
                   shape = 23)

    }

    # different color scheme
    #low = '#ffc500', high = '#c21500',
    #low = '#FF4E50', high = '#F9D423',

    ggsave(plot = p1,
           dpi = 600,
           width = 8.96,
           height = 5.74,
           file = out.filename)
  }
}

