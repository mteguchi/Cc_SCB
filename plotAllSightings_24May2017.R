#plot_AllSightings
# plots all sightings

rm(list=ls())

save.image <- F
source('CcSCB_functions.R')
library(viridis)
#library(wesanderson)

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

study.area.Sp <- latlon2sp(study.area, center.UTM = center.UTM)
study.area.2011.Sp <- latlon2sp(study.area.2011, center.UTM = center.UTM)

study.area.df <- data.frame(x = study.area.Sp$newX,
                            y = study.area.Sp$newY)

study.area.2011.df <- data.frame(x = study.area.2011.Sp$newX,
                            y = study.area.2011.Sp$newY)

# xy.df.1 <- rbind(sightings.data.xy[, c('newX', 'newY', 'Year')],
#                  survey.df[, c('newX', 'newY', 'Year')])

#xy.df.1$Year <- as.factor(xy.df.1$Year)

# get the large coast line:
# coast.line <- getCoastLine('~/R/OceanDepths/coast/coast_Epac.txt',
#                            lon.limits = c(-125, -85),
#                            lat.limits = c(5, 38))

coast.line <- getCoastLine(paste0(dirSelector()$Rdir,
                                  'OceanDepths/coast/coast_Epac.txt'),
                           lon.limits = c(-131, -115),
                           lat.limits = c(28, 36))

coast.line.df <- do.call(rbind, coast.line)
colnames(coast.line.df) <- c('X', 'Y', 'idx')
coast.line.Sp <- latlon2sp(coast.line.df, center.UTM)

#min(study.area.df$x) max(study.area.df$y) min(study.area.df$y) max(study.area.df$x)
# xy.df <- xy.df.1[xy.df.1$newX <= 2000 &
#                    xy.df.1$newX >= -3000 &
#                    xy.df.1$newY <= 1000 &
#                    xy.df.1$newY >= -1000, ]

xy.df <- xy.df.1[xy.df.1$newX <= 500 &
                   xy.df.1$newX >= -1000 &
                   xy.df.1$newY <= 650 &
                   xy.df.1$newY >= -500, ]

dplyr::select(xy.df, newX, newY) %>%
  sp2latlon(., center.UTM) %>%
  cbind.data.frame(., xy.df$Date, xy.df$Year) -> xy.latlon

colnames(xy.latlon) <- c('X', 'Y', 'Date', 'Year')

# DGN bycatch data:
DGN_bycatch <- read.csv('Data/CC_bycatch_DGN.csv')
DGN_bycatch <- DGN_bycatch[!is.na(DGN_bycatch$LatDD1),]
# DGN_bycatch$Latitude <- DGN_bycatch$LatD1 + DGN_bycatch$LatM1/60
# DGN_bycatch$Longitude <- -1 * (DGN_bycatch$LongD1 + DGN_bycatch$LongM1/60)
DGN_bycatch$fYear <- as.factor(DGN_bycatch$Year)

# also get the ship-based survey effort:
ship.effort <- read.csv('Data/ShipEffortWithDistanceChunksBftLT4.csv')
#add spatial units:
ship.effort.XY <- ship.effort[,c('Lon', 'Lat')]
colnames(ship.effort.XY) <- c('X', 'Y')
ship.effort.Sp <- latlon2sp(ship.effort.XY,
                            center.UTM = center.UTM)
ship.effort$X <- ship.effort.Sp$newX
ship.effort$Y <- ship.effort.Sp$newY

ship.effort$fYr <- as.factor(ship.effort$Yr)

# 2006 and 2014 are the only ones with different colors:
ship.effort$cYr <- 'other'
ship.effort$cYr[ship.effort$Yr == 2006] <- '2006'
ship.effort$cYr[ship.effort$Yr == 2014] <- '2014'

#data.2015 <- subset(lines.df, Year == 2015)
lines.df$fYear <- as.factor(lines.df$Year)

# plot them in lat/lon scale
study.area.df2 <- data.frame(lat = study.area.Sp@data$lat,
                             lon = study.area.Sp@data$lon-360)

study.area.2011.df2 <- data.frame(lat = study.area.2011.Sp@data$lat,
                             lon = study.area.2011.Sp@data$lon-360)

lines.df.latlon$fYear <- as.factor(lines.df.latlon$Year)

ship.effort.2006.2014 <- ship.effort[ship.effort$Yr == 2006 | ship.effort$Yr == 2014,]
xy.latlon.2006.2014 <- xy.latlon[xy.latlon$Year == 2006 | xy.latlon$Year == 2014,]

p2 <- ggplot() +   # Ship-board effort
  geom_path(data = ship.effort,
            aes(x = Lon, y = Lat,
                group = chunk,
                color = cYr),
            size = 1.2) +
  # geom_segment(data = lines.df.latlon,
  #              aes(x = beginX, xend = endX,
  #                  y = beginY, yend = endY,
  #                  color = fYear),
  #              size = 1.2) +
  # geom_polygon(data = study.area.df2,
  #              aes(x = lon, y = lat),
  #              fill = NA, size = 1.2,
  #              alpha = 0.6,
  #              color='black') +
  geom_point(data = xy.latlon.2006.2014,
             aes(x = X, y = Y, color = Year),
             shape = 19, size = 2) +

  geom_polygon(fill = land.color,
               data = coast.line.df,
               aes(x=X, y=Y, group = idx))  +

  geom_path(data = map.LTCA,
            aes(x = X, y = Y),
            linetype = "longdash", size = 1.5,
            alpha = 0.6, color = 'black') +
  # geom_path(data = as.data.frame(inshore.polygon.latlon@coords),
  #           aes(x = X, y = Y),
  #           size = 1.1, alpha = 0.6) +
  # geom_path(data = as.data.frame(offshore.polygon.latlon@coords),
  #           aes(x = X, y = Y),
  #           size = 1.1, alpha = 0.6) +
  #
  scale_color_manual(name = 'Year',
                     values = c('darkorange2', 'darkgreen', 'gray70')) +
  # geom_point(data = latlon.df,
  #            aes(x = X, y = Y),
  #            shape = 4) +
  coord_map() +
  ylab(expression(paste("Latitude (", degree, "N)"))) +
  xlab(expression(paste("Longitude (", degree, "W)", sep=""))) +
  #ggtitle("Ship-board effort") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size = 12, hjust = 0.5),
        legend.text = element_text(size = 10, vjust = 0),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.position = c(0.1, 0.2))

ylab <- expression("Latitude ", (degree~N))

p2.2006.2014 <- ggplot() +  # 2006/2014 sightings and effort
  geom_path(data = ship.effort.2006.2014,
            aes(x = Lon, y = Lat,
                group = chunk,
                color = fYr),
            size = 1.2) +
  geom_polygon(fill = land.color,
               data = coast.line.df,
               aes(x=X, y=Y, group = idx))  +
  geom_point(data = xy.latlon.2006.2014,
             aes(x = X, y = Y, color = Year),
             shape = 19, size = 2) +
  geom_path(data = map.LTCA,
            aes(x = X, y = Y),
            linetype = "longdash", size = 1.5,
            alpha = 0.6) +

  scale_color_manual(name = 'Year',
                     values = c('darkorange2', 'darkgreen', 'gray70')) +
  coord_map() +
  ylab(expression(paste("Latitude (", degree, "N)"))) +
  xlab(expression(paste("Longitude (", degree, "W)", sep=""))) +
  #ggtitle("Loggerhead sightings") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size = 12, hjust = 0.5),
        legend.text = element_text(size = 10, vjust = 0),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.position = c(0.1, 0.2))
#
# p3 <- ggplot() +
#   geom_path(data = as.data.frame(inshore.polygon.latlon@coords),
#             aes(x = X, y = Y)) +
#   geom_path(data = as.data.frame(offshore.polygon.latlon@coords),
#             aes(x = X, y = Y)) +
#   geom_segment(data = lines.df.latlon,
#                aes(x = beginX, xend = endX,
#                    y = beginY, yend = endY,
#                    color = fYear),
#                size = 1.2) +
#   # geom_segment(data = lines.df.latlon.2015,
#   #              aes(x = beginX, xend = endX,
#   #                  y = beginY, yend = endY),
#   #              size = 1.0, color = 'darkcyan') +
#   geom_polygon(data = study.area.df2,
#                aes(x = lon, y = lat),
#                fill = NA,
#                color='black') +
#   geom_polygon(fill = land.color,
#                data = coast.line.df,
#                aes(x=X, y=Y, group = idx))  +
#   geom_path(data = map.LTCA,
#             aes(x = X, y = Y),
#             linetype = "lngdash", size = 1.5,
#             alpha = 0.6) +
#   # scale_color_manual(name = 'Year',
#   #                    values = c('darkorange2', 'darkgreen', 'gray70')) +
#
#  scale_color_viridis(discrete = TRUE,
#                        name = 'Year') +
#   coord_map() +
#   ylab(expression(paste("Latitude (", degree, "N)"))) +
#   xlab(expression(paste("Longitude (", degree, "W)", sep=""))) +
#   ggtitle("Completed aerial track lines") +
#   theme(plot.title = element_text(hjust = 0.5),
#         legend.title = element_text(size = 10, hjust = 0.5),
#         legend.text = element_text(size = 8, vjust = 0))

lines.df.latlon.2011 <- subset(lines.df.latlon, fYear == 2011)
lines.df.latlon.2015 <- subset(lines.df.latlon, fYear == 2015)

p3a <- ggplot() +
  geom_path(data = as.data.frame(inshore.polygon.latlon@coords),
            aes(x = X, y = Y)) +
  geom_path(data = as.data.frame(offshore.polygon.latlon@coords),
            aes(x = X, y = Y)) +
  geom_segment(data = lines.df.latlon.2015,
               aes(x = beginX, xend = endX,
                   y = beginY, yend = endY),
               size = 1.0,
               color = "darkcyan") +
  geom_polygon(data = study.area.df2,
               aes(x = lon, y = lat),
               fill = NA,
               color='black') +
  geom_polygon(fill = land.color,
               data = coast.line.df,
               aes(x=X, y=Y, group = idx)) +
  geom_point(data = ccData,
             aes(x = X, y = Y),
             shape = 19, size = 2,
             color = 'brown4') +
  geom_path(data = map.LTCA,
            aes(x = X, y = Y),
            linetype = "longdash", size = 1.5,
            alpha = 0.7) +
  coord_map() +
  ylab(expression(paste("Latitude (", degree, "N)"))) +
  xlab(expression(paste("Longitude (", degree, "W)", sep=""))) +
  #ggtitle("Completed track lines and loggerhead sightings in 2015") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size = 12, hjust = 0.5),
        legend.text = element_text(size = 10, vjust = 0),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12))

p3.2011 <- ggplot() +
  # geom_path(data = as.data.frame(inshore.polygon.latlon@coords),
  #           aes(x = X, y = Y)) +
  # geom_path(data = as.data.frame(offshore.polygon.latlon@coords),
  #           aes(x = X, y = Y)) +
  geom_segment(data = lines.df.latlon.2011,
               aes(x = beginX, xend = endX,
                   y = beginY, yend = endY),
               color = 'darkcyan',
               size = 1.2) +
  geom_path(data = middle.line.2011,
            aes(x = X, y = Y),
            color = 'black') +

  # geom_segment(data = lines.df.latlon.2015,
  #              aes(x = beginX, xend = endX,
  #                  y = beginY, yend = endY),
  #              size = 1.0, color = 'darkcyan') +
  geom_polygon(data = study.area.2011.df2,
               aes(x = lon, y = lat),
               fill = NA,
               color='black') +
  geom_polygon(fill = land.color,
               data = coast.line.df,
               aes(x=X, y=Y, group = idx))  +
  geom_path(data = map.LTCA,
            aes(x = X, y = Y),
            linetype = "longdash", size = 1.5,
            alpha = 0.7) +

  #scale_color_viridis(discrete = TRUE,
  #                    name = 'Year') +
  coord_map() +
  ylab(expression(paste("Latitude (", degree, "N)"))) +
  xlab(expression(paste("Longitude (", degree, "W)", sep=""))) +
  #ggtitle("Completed aerial track lines in 2011") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size = 12, hjust = 0.5),
        legend.text = element_text(size = 10, vjust = 0),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12))

p3.2015 <- ggplot() +
  geom_path(data = as.data.frame(inshore.polygon.latlon@coords),
            aes(x = X, y = Y)) +
  geom_path(data = as.data.frame(offshore.polygon.latlon@coords),
            aes(x = X, y = Y)) +
  geom_segment(data = lines.df.latlon.2015,
               aes(x = beginX, xend = endX,
                   y = beginY, yend = endY),
               color = 'darkcyan',
               size = 1.2) +
  # geom_segment(data = lines.df.latlon.2015,
  #              aes(x = beginX, xend = endX,
  #                  y = beginY, yend = endY),
  #              size = 1.0, color = 'darkcyan') +
  geom_polygon(data = study.area.df2,
               aes(x = lon, y = lat),
               fill = NA,
               color='black') +
  geom_polygon(fill = land.color,
               data = coast.line.df,
               aes(x=X, y=Y, group = idx))  +
  geom_path(data = map.LTCA,
            aes(x = X, y = Y),
            linetype = "longdash", size = 1.5,
            alpha = 0.7) +

  #scale_color_viridis(discrete = TRUE,
  #                    name = 'Year') +
  coord_map() +
  ylab(expression(paste("Latitude (", degree, "N)"))) +
  xlab(expression(paste("Longitude (", degree, "W)", sep=""))) +
  #ggtitle("Completed aerial track lines in 2015") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size = 12, hjust = 0.5),
        legend.text = element_text(size = 10, vjust = 0),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12))


xy.latlon$Date <- as.Date(xy.latlon$Date, '%Y-%m-%d')
xy.latlon$numYear <- as.numeric(format(as.Date(xy.latlon$Date), '%Y'))
xy.latlon.2005.2016 <- xy.latlon[xy.latlon$numYear >= 2005 &
                                     xy.latlon$numYear <= 2016,]

all.data <- rbind(data.frame(numYear = 2015,
                             X = ccData$X,
                             Y = ccData$Y),
                  xy.latlon.2005.2016[, c('numYear', 'X', 'Y')])
all.data$Year <- as.factor(all.data$numYear)
p.2005to2016 <- ggplot() +
  # geom_path(data = as.data.frame(inshore.polygon.latlon@coords),
  #           aes(x = X, y = Y)) +
  # geom_path(data = as.data.frame(offshore.polygon.latlon@coords),
  #           aes(x = X, y = Y)) +
  # geom_segment(data = lines.df.latlon.2015,
  #              aes(x = beginX, xend = endX,
  #                  y = beginY, yend = endY),
  #              size = 1.2,
  #              color = "#FDE725FF") +
  # geom_polygon(data = study.area.df2,
  #              aes(x = lon, y = lat),
  #              fill = NA,
  #              color='black') +
  geom_polygon(fill = land.color,
               data = coast.line.df,
               aes(x=X, y=Y, group = idx)) +
  geom_point(data = all.data,
             aes(x = X, y = Y, color = Year),
             shape = 19, size = 2) +
  geom_path(data = map.LTCA,
            aes(x = X, y = Y),
            linetype = "longdash", size = 1.5,
            alpha = 0.7) +
  scale_color_brewer(name = 'Year', palette = 'Set1') +

  # scale_color_viridis(discrete = TRUE,
  #                     name = 'Year') +
  coord_map() +
  ylab(expression(paste("Latitude (", degree, "N)"))) +
  xlab(expression(paste("Longitude (", degree, "W)", sep=""))) +
  #ggtitle("Loggerhead sightings") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size = 10, hjust = 0.5),
        legend.text = element_text(size = 8, vjust = 0),
        legend.position = c(0.85, 0.8))

p.bycatch <- ggplot() +
  geom_point(data = DGN_bycatch,
             aes(x = -LonDD1, y = LatDD1,
                 color = fYear,
                 shape = as.factor(MM),
                 size = CarpLen)) +
  geom_jitter()+
  scale_color_manual(name = 'Year',
                     values = c("1992" = 'darkgreen',
                                "1993" = 'darkred',
                                "1997" = 'darkorange',
                                '1998' = 'darkcyan',
                                '2001' = 'firebrick1',
                                '2006' = 'darkblue')) +
  scale_shape_manual(name = 'Month',
                     values = c('1' = 49,
                                '4' = 52,
                                '6' = 54,
                                '7' = 55,
                                '8' = 56,
                                '10' = 48),
                     labels = c('Jan', 'Apr', 'Jun', 'Jul',
                                'Aug', 'Oct'))+
  scale_size_area(name = "CCL (cm)") +
  guides(color = guide_legend(override.aes = list(size = 5)),
         shape = guide_legend(override.aes = list(size = 5))) +
  #scale_color_brewer(name = 'Year', palette = 'Set1') +
  #scale_color_viridis(discrete = TRUE) +
  geom_polygon(fill = land.color,
               data = coast.line.df,
               aes(x=X, y=Y, group = idx)) +
  geom_path(data = map.LTCA,
            aes(x = X, y = Y),
            linetype = 2, size = 1.5,
            alpha = 0.4) +
  coord_map() +
  ylab(expression(paste("Latitude (", degree, "N)"))) +
  xlab(expression(paste("Longitude (", degree, "W)", sep=""))) +
  #ggtitle("Loggerhead Turtle Bycatch") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size = 12, hjust = 0.5),
        legend.text = element_text(size = 12, vjust = 0),
        legend.position = c(0.8, 0.5),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))

if (save.image) {
  width.in <- 6
  ggsave(plot = p2,
       dpi = 1200,
       height = 4.2,
       #width = width.in,
       units = 'in',
       file = paste0('Figures/ShipEffort_', Sys.Date(), '.png'))
  # ggsave(plot = p3,
  #        dpi = 1200,
  #        height = 5,
  #        units = 'in',
  #        file = paste0('Figures/AerialEffort_', Sys.Date(), '.png'))
  ggsave(plot = p3a,
         dpi = 1200,
         #height = 5,
         width = width.in,
         units = 'in',
         file = paste0('Figures/AerialSightings_', Sys.Date(), '.png'))
  ggsave(plot = p2.2006.2014,
         dpi = 1200,
         height = 5,
         #width = width.in,
         units = 'in',
         file = paste0('Figures/ShipSightings_', Sys.Date(), '.png'))
  ggsave(plot = p.2005to2016,
         dpi = 1200,
         #height = 5,
         width = width.in,
         units = 'in',
         file = paste0('Figures/Sightings_2005to2016_', Sys.Date(), '.png'))
  ggsave(plot = p.bycatch,
         dpi = 1200,
         height = 8,
         units = 'in',
         file = paste0('Figures/Bycatch_', Sys.Date(), '.png'))
  ggsave(plot = p3.2011,
         dpi = 1200,
         #height = 5,
         width = width.in,
         units = 'in',
         file = paste0('Figures/AerialEffort2011_', Sys.Date(), '.png'))
  ggsave(plot = p3.2015,
         dpi = 1200,
         #height = 5,
         width = width.in,
         units = 'in',
         file = paste0('Figures/AerialEffort2015_', Sys.Date(), '.png'))
}
