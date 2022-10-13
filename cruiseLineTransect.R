#cruiseLineTransect

# conducts line transect analysis on turtle sightings for 2006 and 2014

rm(list=ls())

source('Cc_SCB_functions.R')
library(mrds)

# define column types:
col_def <- cols(Cr = col_integer(),
                Yr = col_integer(),
                Mo = col_integer(),
                Dy = col_integer(),
                Time = col_time("%H:%M:%S"),
                E = col_character(),
                Lat = col_double(),
                Lon = col_double(),
                Bft = col_integer(),
                SH = col_double(),
                FR = col_integer(),
                HS = col_integer(),
                VS = col_integer(),
                Vis = col_double(),
                WnSp = col_double(),
                R = col_character(),
                Dist_km = col_double(),
                Time_sec = col_double(),
                chunk = col_integer())

effort.data <- read_csv(file = 'data/EffortWithDistanceChunksBftLT4.csv', 
                        col_types = col_def)

effort.data <- effort.data[, c('Cr', 'Yr', 'Mo', 'Dy', 'Time',
                               'E', 'Lat', 'Lon', 'Bft', 'Dist_km',
                               'chunk')]

# Then extract just 2006 and 2014:
effort.data <- dplyr::filter(effort.data, 
                             Yr == 2006 | Yr == 2014)

effort.data %>%
  dplyr::group_by(chunk, Yr) %>%
  dplyr::summarise(effort = sum(Dist_km, na.rm = T)) -> effort.by.chunk

# then remove effort == 0 - what happened to sample = 651? effort == 0? 
# how come it is in obs.table?
sample.table <- as.data.frame(dplyr::filter(effort.by.chunk, effort > 0.0))
colnames(sample.table) <- c('Sample.Label', 'Yr', 'Effort')
sample.table$Region.Label <- 'SoCal'

# get sightings data:
col_def <- cols(Cruise = col_integer(),
                Date = col_date('%d-%b-%y'),
                Time = col_time('%H:%M:%S'),
                Sight = col_character(),
                Lat = col_double(),
                Lon = col_double(),
                Species = col_character(),
                Best = col_integer(),
                Bearing = col_integer(),
                Distance = col_double(),
                Maturity = col_character(),
                Captured = col_character(),
                DetecBy = col_integer(),
                AssocJFR = col_character(),
                EffortType = col_character(),
                Beaufort = col_integer(),
                SwellHt = col_double(),
                SwellDir = col_integer(),
                WindSpd = col_double(),
                FogRain = col_character(),
                HzSun = col_integer(),
                VtSun = col_integer(),
                WindDir = col_double(),
                Visibility = col_double())

sights.data <- read_csv(file = 'data/TurtleSights_1.csv', 
                        col_types = col_def)

# extact just year
sights.data$Yr <- as.numeric(format(sights.data$Date, 
                                    format = '%Y'))
sights.data$fSpecies <- as.factor(sights.data$Species)

# change Best estimate of # of individuals == NA to == 1
sights.data[is.na(sights.data$Best), 'Best'] <- 1
sights.data.BftLT4 <- dplyr::filter(sights.data,
                                    Beaufort < 4 | is.na(Beaufort) == T)

sights.data.BftLT4 <- dplyr::filter(sights.data.BftLT4, 
                             Yr == 2006 | Yr == 2014)

# turn all sightings into the right side of the ships
sights.data.BftLT4[sights.data.BftLT4$Bearing > 180, 'Bearing'] <- 360 - sights.data.BftLT4[sights.data.BftLT4$Bearing > 180, 'Bearing']

# distance in km so no conversions needed as long as everything is in km or km^2
sights.data.BftLT4$PerpDist <- nm2km(sights.data.BftLT4$Distance * sin(deg2rad(sights.data.BftLT4$Bearing)))

#sights.data.BftLT4$fSpecies <- as.factor(sights.data.BftLT4$Species)
sights.data.BftLT4 <- sights.data.BftLT4[, c('Cruise', 'Date', 'Lat', 'Lon', 'Species',
                                             'Best', 'Bearing', 'Distance', 'Beaufort',
                                             'Yr', 'PerpDist', 'fSpecies')]

perp.data <- as.data.frame(sights.data.BftLT4[, c('PerpDist', 'Best', 'Beaufort', 'Yr')])
colnames(perp.data) <- c('distance', 'size', 'Beaufort', 'Yr')
perp.data$Region.Label <- 'SoCal'

# remove sightings with no Beaufort data:
perp.data <- na.omit(perp.data)

# create observation table, which contains all sightings and corresponding
# sample ID = transect IDs
obs.table <- data.frame(object = seq(from = 1, to = dim(sights.data.BftLT4)[1]),
                        Sample.Label = NA,
                        Region.Label = 'SoCal',
                        Yr = sights.data.BftLT4$Yr)

# find the shortest distance between each observation and chunks
min.distances <- vector(mode = 'numeric', length = dim(obs.table)[1])
k <- 1
for (k in 1:dim(sights.data.BftLT4)[1]){
  dist.all <- distGeo(sights.data.BftLT4[k, c('Lon', 'Lat')],
                      effort.data[, c('Lon', 'Lat')])/1000
  tmp <- effort.data[dist.all == min(dist.all), 'chunk']
  obs.table[k, 'Sample.Label'] <- tmp[1, 'chunk']
  obs.table[k, 'distance'] <- sights.data.BftLT4[k, 'PerpDist']
  obs.table[k, 'size'] <- sights.data.BftLT4[k, 'Best']
  min.distances[k] <- min(dist.all)  
}

# join the observation and sample tables to see if we have all positive
# effort. One sighting gets removed:
obs.table2 <- inner_join(obs.table, sample.table, by = 'Sample.Label')
obs.table3 <- right_join(obs.table, sample.table, by = 'Sample.Label')

# then create obs.table 
obs.table <- obs.table2[, c('object', 'Sample.Label', 'Region.Label.x', 'Yr.x')]
colnames(obs.table) <- c('object', 'Sample.Label', 'Region.Label', 'Yr')

# also flatfile:
flat.table <- obs.table3[, c('Sample.Label', 'distance', 'Yr.y', 'Effort', 'size')]
colnames(flat.table) <- c('Sample.Label', 'distance', 'Yr', 'Effort', 'size')
flat.table$Region.Label <- 'SoCal'

region.table <- data.frame(Region.Label = 'SoCal', 
                           Area = 1)

# Find the best detection function (using perp.data):
fit.cov.Bft.2006 <- ds(data = perp.data[perp.data$Yr == 2006,],
                       truncation="15%",
                       key = "hn",
                       formula = ~ as.factor(Beaufort))

fit.cov.Bft.2014 <- ds(data = perp.data[perp.data$Yr == 2014,],
                       truncation="15%",
                       key = "hn",
                       formula = ~ as.factor(Beaufort))

fit.cov.Bft.sepYrs.AIC <- summary(fit.cov.Bft.2006)$ds$aic + 
  summary(fit.cov.Bft.2014)$ds$aic

fit.yr.pooled.noAdj <- ds(data = perp.data,
                          truncation="15%",
                          adjustment = NULL,
                          key = "hn")

fit.yr.pooled.cosAdj <- ds(data = perp.data,
                           truncation="15%",
                           adjustment = 'cos',
                           key = "hn")

fit.cov.Bft <- ds(data = perp.data,
                  truncation="15%",
                  key = "hn",
                  formula = ~ as.factor(Beaufort))

fit.cov.Bft.yr <- ds(data = perp.data,
                     truncation="15%",
                     key = "hn",
                     formula = ~ as.factor(Beaufort) + as.factor(Yr))

fit.cov.yr <- ds(data = perp.data,
                 truncation="15%",
                 key = "hn",
                 formula = ~ as.factor(Yr))

all.AIC <- c("cov.Bft.sepYrs" = fit.cov.Bft.sepYrs.AIC,
             "yr.pooled.noAdj" = summary(fit.yr.pooled.noAdj)$ds$aic,
             "yr.pooled.cosAdj" = summary(fit.yr.pooled.cosAdj)$ds$aic,
             "cov.Bft" = summary(fit.cov.Bft)$ds$aic,
             "cov.Bft.yr" = summary(fit.cov.Bft.yr)$ds$aic,
             "cov.yr" = summary(fit.cov.yr)$ds$aic)

minAIC <- min(all.AIC)
deltaAIC <- all.AIC - min(all.AIC)

# the best model doesn't require Beaufort, so get all data in:
fit.yr.pooled.cosAdj.1 <- ds(data = obs.table2,
                           truncation="15%",
                           adjustment = 'cos',
                           key = "hn")


# Best model is fit.yr.pooled - use it to estimate density:
sample.table.2006 <- sample.table[sample.table$Yr == 2006, 
                                  c('Region.Label', 
                                    'Sample.Label', 
                                    'Effort')]

obs.table.2006 <- obs.table[obs.table$Yr == 2006,
                            c('object', 
                              'Region.Label', 
                              'Sample.Label')]

Dhat.2006 <- dht(model = fit.yr.pooled.cosAdj.1$ddf,
                 region.table = region.table,
                 sample.table = sample.table.2006,
                 obs.table = obs.table.2006)


sample.table.2014 <- sample.table[sample.table$Yr == 2014, 
                                  c('Region.Label', 
                                    'Sample.Label', 
                                    'Effort')]

obs.table.2014 <- obs.table[obs.table$Yr == 2014,
                            c('object', 
                              'Region.Label', 
                              'Sample.Label')]

Dhat.2014 <- dht(model = fit.yr.pooled.cosAdj.1$ddf,
                 region.table = region.table,
                 sample.table = sample.table.2014,
                 obs.table = obs.table.2014)


# Use the flat table to do the same analysis to see
# I got the tables correct. Seems that way. 
Dhat.flat <- ds(data = flat.table,
                truncation="15%",
                adjustment = 'cos',
                key = "hn")

save(list = ls(),
     file = paste0('RData/shipLineTransect_', 
                   Sys.Date(), '.RData'))


effort.data.2006 <- effort.data[effort.data$Yr == 2006,]
effort.data.2006.Apr <- effort.data.2006[effort.data.2006$Mo == 4,]
effort.data.2006.Dec <- effort.data.2006[effort.data.2006$Mo == 12,]

effort.data.2014 <- effort.data[effort.data$Yr == 2014,]
effort.data.2014.Aug <- effort.data.2014[effort.data.2014$Mo == 8,]
effort.data.2014.Dec <- effort.data.2014[effort.data.2014$Mo == 12,]


