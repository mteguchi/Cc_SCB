#plot_SST
# plots SST maps

rm(list=ls())
save.image <- F
source('CcSCB_functions.R')

# 2015 data that are split in 2 km chunks are here
# these were created using AirSegChopCc_2016_04_11.R on merged data files
# which were created using combineFiles.m in Matlab. AirSegChop script
# was first created by Elizabeth Becker and Karin Forney and was modified
# for turtle sightings by me.
# dat2011 <- read.csv('Data/processed/SEGDATA_SCB2011_DAS_2km_2016-06-06.csv')
# dat2015 <- read.csv('Data/processed/SEGDATA_SCB2015_DAS_2km_2016-06-06.csv')
# # dist variable in the above files is the length of each segment and NOT
# # distances of the objects from the track line.
#
# alt <- 500
#


# convert lat/lon to x/y
latlon.df <- ccData[, c('mlon', 'mlat')]
names(latlon.df) <- c("X", "Y")
xy.df <- latlon2sp(latlon.df, center.UTM)@data

# make a study area figure with transect lines overlaid...
# create a new data frame for plotting
study.area.df <- data.frame(x = study.area.Sp$newX,
                            y = study.area.Sp$newY)

data.2011 <- subset(lines.df, Year == 2011)
data.2015 <- subset(lines.df, Year == 2015)
land.color <- '#333333'
alpha.value <- 0.8
p.2011 <- ggplot() +
  geom_polygon(data = study.area.df,
              aes(x = x, y = y),
              fill = NA,
              color='black') +
  geom_polygon(fill = land.color,
               data = all.islands.df,
               aes(x=newX, y=newY, group = name),
               inherit.aes = F)  +
  geom_segment(data = data.2011,
               aes(x = beginX, xend = endX,
                   y = beginY, yend = endY)) +
  ylab("y") +
  xlab("x") +
  ggtitle("2011")

p.2015 <- ggplot() +
  geom_polygon(data = study.area.df,
               aes(x = x, y = y),
               fill = NA,
               color='black') +
  geom_polygon(fill = land.color,
               data = all.islands.df,
               aes(x=newX, y=newY, group = name),
               inherit.aes = F)  +
  geom_segment(data = data.2015,
               aes(x = beginX, xend = endX,
                   y = beginY, yend = endY)) +
  geom_point(data = xy.df,
             aes(x = newX, y = newY)) +
  ylab("y") +
  xlab("x") +
  ggtitle("2015")

  # ggsave(plot = p1,
  #        dpi = 600,
  #        file = 'Figures/sightings_2015.png')

