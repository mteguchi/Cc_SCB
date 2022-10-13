#plotSightingsSCB
# plots sightings around the study area.

rm(list=ls())

source('CcSCB_functions.R')

save.image <- F
land.color <- '#333333'
alpha.value <- 0.8

# Combine all sightings
sightings.data.df <- sightings.data
sightings.data.df$Year <- as.factor(unlist(lapply(strsplit(sightings.data.df$date,
                                                           split = '-'),
                                                  FUN = function(x) x[1])))
sightings.data.df <- dplyr::select(sightings.data.df, date,
                                   Latitude, Longitude, Year)
names(sightings.data.df) <- c('Date', 'Y', 'X', 'Year')

sightings.data.Sp <- latlon2sp(sightings.data.df, center.UTM)
sightings.data.xy <- sightings.data.Sp@data

xy.df.1 <- sightings.data.xy

# convert lat/lon to x/y
latlon.df <- ccData[, c('mlon', 'mlat')]
names(latlon.df) <- c("X", "Y")
survey.df <- latlon2sp(latlon.df, center.UTM)@data  # only 2015
survey.df$Year <- 2015

# xy.df.1 <- rbind(sightings.data.xy[, c('newX', 'newY', 'Year')],
#                  survey.df[, c('newX', 'newY', 'Year')])

#xy.df.1$Year <- as.factor(xy.df.1$Year)
# make a study area figure with transect lines overlaid...
# create a new data frame for plotting
study.area.df <- data.frame(x = study.area.Sp$newX,
                            y = study.area.Sp$newY)
#
xy.df <- xy.df.1[xy.df.1$newX <= max(study.area.df$x) &
                   xy.df.1$newX >= min(study.area.df$x) &
                   xy.df.1$newY <= max(study.area.df$y) &
                   xy.df.1$newY >= min(study.area.df$y), ]

xy.df.2 <- sightings.data.df[sightings.data.df$X <= -117 &
                               sightings.data.df$X >= -123 &
                               sightings.data.df$Y <= 35 &
                               sightings.data.df$Y >= 30,]

#data.2015 <- subset(lines.df, Year == 2015)

lines.df$fYear <- as.factor(lines.df$Year)
p1 <- ggplot() +
  geom_path(data = map.LTCA.Sp@data,
            aes(x = newX, y = newY),
            linetype = 2, size = 1.5) +
  geom_path(data = as.data.frame(inshore.polygon@coords),
            aes(x = newX, y = newY)) +
  geom_path(data = as.data.frame(offshore.polygon@coords),
            aes(x = newX, y = newY)) +
  geom_polygon(data = study.area.df,
               aes(x = x, y = y),
               fill = NA,
               color='black') +
  geom_polygon(fill = land.color,
               data = all.islands.df,
               aes(x=newX, y=newY, group = name),
               inherit.aes = F)  +
  geom_polygon(fill = land.color,
               data = coast.line.xy,
               aes(x=newX, y=newY))  +
  geom_segment(data = lines.df,
               aes(x = beginX, xend = endX,
                   y = beginY, yend = endY,
                   color = fYear),
               size = 1.0) +
  geom_point(data = xy.df,
             aes(x = newX, y = newY, color = Year),
             shape = 19, size = 2) +
  geom_point(data = survey.df,
             aes(x = newX, y = newY),
             shape = 4) +
  ylab("y") +
  xlab("x") +
  ggtitle("Loggerhead turtle sightings") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

        # legend.title = element_text(size = 10, hjust = 0.5),
        # legend.text = element_text(size = 8, vjust = 0))

study.area.df2 <- data.frame(lat = study.area.Sp@data$lat,
                             lon = study.area.Sp@data$lon-360)
p2 <- ggplot() +
  geom_polygon(data = study.area.df2,
               aes(x = lon, y = lat),
               fill = NA,
               color='black') +
  geom_polygon(fill = land.color,
               data = all.islands.df,
               aes(x=lon, y=lat, group = name),
               inherit.aes = F)  +
  geom_polygon(fill = land.color,
               data = coast.line.Sp@data,
               aes(x=Longitude, y=Latitude))  +
  geom_point(data = xy.df.2,
             aes(x = X, y = Y, color = Year),
             shape = 19, size = 2) +
  geom_point(data = latlon.df,
             aes(x = X, y = Y),
             shape = 4) +
  coord_map() +
  ylab("Latitude") +
  xlab("Longitude") +
  ggtitle("Loggerhead turtle sightings") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size = 10, hjust = 0.5),
        legend.text = element_text(size = 8, vjust = 0))


if (save.image == T){
  ggsave(plot = p1,
         dpi = 1200,
         file = 'Figures/sightings_SCB_km.png')
  ggsave(plot = p2,
         dpi = 1200,
         file = 'Figures/sightings_SCB_latlon.png')

}

