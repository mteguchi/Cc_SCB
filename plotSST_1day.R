#plot_SST
# plots SST maps

rm(list=ls())

source('CcSCB_functions.R')
library(viridis)

save.fig <- T
# load one prediction file to get lat/lon resolution:
#load("RData/predictions/predictionDataWithCovariatesDepth_2015-10-31.RData")
#prediction.data.geo <- prediction.data.cov2[,c("x", "y", "latitude", "longitude")]

plot.date <- '2011-10-15'
file.name <- paste0('RData/bkgd/jplMURSST', plot.date, '_bkgd.RData')

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

#length(file.names)
var.date.str <- unlist(strsplit(unlist(strsplit(file.name, '_'))[1],
                                '_bkgd.RData'))
date.str <- unlist(strsplit(var.date.str, 'jplMURSST'))[2]

out.filename <- paste0('Figures/sst_SCB/sst_',
                       unlist(strsplit(date.str, split = '.RData')),
                       '.png')

prediction.data <- subset(prediction.data, longitude < -115)

temp.df <- extract.cov.bkgd('jplMURSST', date.str,
                            center.UTM,
                            prediction.data[, c('x', 'y')])

# getting rid of G of Ca data points:
temp.df[temp.df$X > 400 & temp.df$Y > -300, 'mean'] <- NA
temp.df <- na.omit(temp.df)
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

lines.strata.df.2$f.offshore <- as.factor(lines.strata.df.2$offshore)

coast.line <- getCoastLine('~/R/OceanDepths/coast/coast_Epac.txt',
                           lon.limits = c(-123, -115),
                           lat.limits = c(28, 36))

coast.line.df <- do.call(rbind, coast.line)
dplyr::rename(coast.line.df, X=Longitude, Y = Latitude) %>%
  latlon2sp(., center.UTM) -> coast.line.Sp

coast.line.Sp.df <- as.data.frame(coast.line.Sp@data)

# convert them back into lat/lon - may not work...
dplyr::rename(temp.df, newX = X, newY = Y) %>%
  sp2latlon(., center.UTM) -> temp.Sp.latlon
temp.df.latlon <- as.data.frame(temp.Sp.latlon@coords)
temp.df.latlon$mean <- temp.df$mean


p1 <- ggplot() +
  geom_raster(data = temp.df,
              aes(x = X, y = Y, fill = mean)) +
  scale_fill_gradientn(na.value = land.color,
                       limits = c(10, 30),
                       colors = viridis(n = 20),
                       name = 'SST (C)') +
  # scale_color_gradient(na.value = land.color,
  #                      limits = c(16, 24),
  #                      low = 'blue', high = 'red',
  #                      name = 'SST (C)') +
  geom_polygon(fill = land.color,
               data = all.islands.df,
               aes(x=newX, y=newY, group = name),
               inherit.aes = F)  +
  geom_polygon(fill = land.color,
               data = coast.line.Sp.df,
               aes(x=newX, y=newY, group=idx)) +
  # geom_segment(data = lines.strata.df.2,
  #              aes(x = newX_offshore, xend = newX_inshore,
  #                  y = newY_offshore, yend = newY_inshore),
  #              linetype = 'dotted') +
  geom_path(data = study.area.df,
            aes(x = x, y = y)) +
  geom_path(data = as.data.frame(inshore.polygon@coords),
            aes(x = newX, y = newY)) +
  geom_path(data = as.data.frame(offshore.polygon@coords),
            aes(x = newX, y = newY)) +

  ylab("y") +
  xlab("x") +
  ggtitle(plot.date) +
  theme(plot.title = element_text(hjust = 0.5))

#theme(panel.background = element_rect(fill = land.color)) +

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
if (save.fig){
  ggsave(plot = p1,
         dpi = 1200,
         width = 8.96,
         height = 5.74,
         file = out.filename)

}
