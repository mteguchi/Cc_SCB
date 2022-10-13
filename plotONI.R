#plotONI

rm(list = ls())

library(ggplot2)
library(dplyr)
library(viridis)
library(reshape)

save.fig <- T
#dat.raw <- read.csv('Data/ONIall.txt')
# ONI.values <- melt(dat.raw, id.vars = 'Year', value.name = 'ONI')
#
# colnames(ONI.values) <- c('Year', 'Period', 'ONI')
#
# dt <- seq(from = 0, to = 1.0 - 1/12, by = 1/12)
# uniq.period <- c('DJF', 'JFM', 'FMA', 'MAM', 'AMJ', 'MJJ',
#                  'JJA', 'JAS', 'ASO', 'SON', 'OND', 'NDJ')
# ONI.values$dt <- NA
# for (k in 1:length(uniq.period)){
#   ONI.values[ONI.values$Period == uniq.period[k], 'dt'] <- dt[k]
# }
#
# ONI.values$time <- ONI.values$Year + ONI.values$dt
#

ONI.values <- read.csv('Data/ONI_20171128.csv')
colnames(ONI.values) <- c('Year', 'Month', 'Total', 'ClimAdj', 'ONI')

ONI.values$time <- ONI.values$Year + ONI.values$Month/12 - 1/12
ONI.values$time.end <- ONI.values$time + 1/12
ONI.values$Nino <- ifelse(ONI.values$ONI > 0, 'TRUE', 'FALSE')
ONI.values$Nino <- ifelse(ONI.values$ONI > 0, 'TRUE', 'FALSE')

ONI.values.2010 <- subset(ONI.values, Year > 2009)
ONI.values.2010$Year <- as.factor(ONI.values.2010$Year)

# p1 <- ggplot(data = ONI.values.2010,
#              aes(x = Period,
#                  y = ONI,
#                  color = Year,
#                  group = Year)) +
#   scale_color_viridis(discrete = TRUE) +
#   geom_point(size = 2) + geom_line(size = 1.5) +
#   annotate('rect', xmin = 'ASO', xmax = 'OND',
#            ymin = -Inf, ymax = Inf, alpha = 0.3) +
#   ylab("ONI") +
#   xlab("") +
#   ggtitle("ONI") +
#   theme(plot.title = element_text(hjust = 0.5),
#         legend.title = element_text(size = 10, hjust = 0.5),
#         legend.text = element_text(size = 8, vjust = 0),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12))
#
# ONI.values.2005 <- subset(ONI.values, Year > 2004)
# ONI.values.2005$Year <- as.factor(ONI.values.2005$Year)
#
# p1.2005 <- ggplot(data = ONI.values.2005,
#              aes(x = Period,
#                  y = ONI,
#                  color = Year,
#                  group = Year)) +
#   scale_color_viridis(discrete = TRUE, name = 'Year') +
#   geom_point(size = 2) + geom_line(size = 1.5) +
#   annotate('rect', xmin = 'ASO', xmax = 'OND',
#            ymin = -Inf, ymax = Inf, alpha = 0.3) +
#   ylab("ONI") +
#   xlab("") +
#   ggtitle("ONI") +
#   theme(plot.title = element_text(hjust = 0.5),
#         legend.title = element_text(size = 10, hjust = 0.5),
#         legend.text = element_text(size = 8, vjust = 0),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12))

min.yr <- 1990
max.yr <- 2017
ONI.values.1990 <- filter(ONI.values, Year <= max.yr &
                            Year >= min.yr)
DGN_bycatch_year <- c(2001, 1998, 1997, 1993, 1992)

p2 <- ggplot(data = ONI.values.1990) +
  geom_bar(stat = 'identity',
           aes(x = time, y = ONI, fill = Nino)) +
  scale_x_continuous(name = '',
                     breaks = seq(from = min.yr,
                                  to = max.yr, by = 1)) +
  scale_fill_manual(values = c('blue', 'red'), guide = FALSE) +
  #xlim(c(1990, 2016))+
  #ggtitle('Oceanic Nino Index') +
  annotate('rect', xmin = 2001, xmax = 2002,
           ymin = -Inf, ymax = Inf, alpha = 0.3) +
  annotate('rect', xmin = 1997, xmax = 1999,
           ymin = -Inf, ymax = Inf, alpha = 0.3) +
  annotate('rect', xmin = 1992, xmax = 1994,
           ymin = -Inf, ymax = Inf, alpha = 0.3) +
  annotate('rect', xmin = 2006, xmax = 2007,
           ymin = -Inf, ymax = Inf, alpha = 0.3) +
  annotate('rect', xmin = 2014, xmax = 2016,
            ymin = -Inf, ymax = Inf, alpha = 0.3) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 12, angle = 90),
        axis.text.y = element_text(size = 12),
        axis.text = element_text(size = 12))

min.yr <- 1990
max.yr <- 2007
ONI.values.1990 <- subset(ONI.values, Year < max.yr & Year > min.yr)
DGN_bycatch_year <- c(2006, 2001, 1998, 1997, 1993, 1992)

p.1991to2007 <- ggplot(data = ONI.values.1990) +
  geom_bar(stat = 'identity',
           aes(x = time, y = ONI, fill = Nino)) +
  scale_x_continuous(name = '',
                     breaks = seq(from = min.yr, to = max.yr, by = 1)) +
  scale_fill_manual(values = c('blue', 'red'), guide = FALSE) +
  ggtitle('Oceanic Nino Index') +
  annotate('rect', xmin = 2006, xmax = 2007,
           ymin = -Inf, ymax = Inf, alpha = 0.3) +
  annotate('rect', xmin = 2001, xmax = 2002,
           ymin = -Inf, ymax = Inf, alpha = 0.3) +
  annotate('rect', xmin = 1997, xmax = 1999,
           ymin = -Inf, ymax = Inf, alpha = 0.3) +
  annotate('rect', xmin = 1992, xmax = 1994,
           ymin = -Inf, ymax = Inf, alpha = 0.3) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 12, angle = 90),
        axis.text.y = element_text(size = 12))

min.yr <- 1980
max.yr <- 2017
ONI.values.1980 <- subset(ONI.values,
                          Year <= max.yr & Year >= min.yr)
ONI.values.1980$Year <- as.factor(ONI.values.1980$Year)

p4 <- ggplot(data = ONI.values.1980) +
  geom_bar(stat = 'identity',
           aes(x = time, y = ONI, fill = Nino)) +
  scale_x_continuous(name = '',
                     breaks = seq(from = min.yr,
                                  to = max.yr, by = 1)) +
  scale_fill_manual(values = c('blue', 'red'), guide = FALSE) +
  ggtitle('Oceanic Nino Index') +
  annotate('rect', xmin = 2001, xmax = 2002,
           ymin = -Inf, ymax = Inf, alpha = 0.3) +
  annotate('rect', xmin = 1997, xmax = 1999,
           ymin = -Inf, ymax = Inf, alpha = 0.3) +
  annotate('rect', xmin = 1992, xmax = 1994,
           ymin = -Inf, ymax = Inf, alpha = 0.3) +
  annotate('rect', xmin = 2006, xmax = 2007,
           ymin = -Inf, ymax = Inf, alpha = 0.3) +
  annotate('rect', xmin = 2014, xmax = 2016,
           ymin = -Inf, ymax = Inf, alpha = 0.3) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 11, angle = 90),
        axis.text.y = element_text(size = 12))

min.yr <- 2005
max.yr <- 2017
ONI.values.2001 <- subset(ONI.values,
                          Year <= max.yr & Year >= min.yr)
ONI.values.2001$Year <- as.factor(ONI.values.2001$Year)

p5 <- ggplot(data = ONI.values.2001) +
  geom_bar(stat = 'identity',
           aes(x = time, y = ONI, fill = Nino)) +
  scale_x_continuous(name = '',
                     breaks = seq(from = min.yr,
                                  to = max.yr, by = 1)) +
  scale_fill_manual(values = c('blue', 'red'), guide = FALSE) +
  ggtitle('Oceanic Nino Index') +
  annotate('rect', xmin = 2005, xmax = 2007,
           ymin = -Inf, ymax = Inf, alpha = 0.3) +
  annotate('rect', xmin = 2013, xmax = 2017,
           ymin = -Inf, ymax = Inf, alpha = 0.3) +

  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 11, angle = 90),
        axis.text.y = element_text(size = 12))

# this has to be done the last - years change into factor levels:
min.yr <- min(ONI.values$Year)
max.yr <- max(ONI.values$Year)
ONI.values.all <- ONI.values
ONI.values.all$Year <- as.factor(ONI.values$Year)

p.all <- ggplot(data = ONI.values.all) +
  geom_bar(stat = 'identity',
           aes(x = time, y = ONI, fill = Nino)) +
  scale_x_continuous(name = '',
                     breaks = seq(from = min.yr,
                                  to = max.yr, by = 1)) +
  scale_fill_manual(values = c('blue', 'red'), guide = FALSE) +
  ggtitle('Oceanic Nino Index') +
  annotate('rect', xmin = 2001, xmax = 2002,
           ymin = -Inf, ymax = Inf, alpha = 0.3) +
  annotate('rect', xmin = 1997, xmax = 1999,
           ymin = -Inf, ymax = Inf, alpha = 0.3) +
  annotate('rect', xmin = 1992, xmax = 1994,
           ymin = -Inf, ymax = Inf, alpha = 0.3) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 11, angle = 90),
        axis.text.y = element_text(size = 12))


if (save.fig){
  dpi <- 300
  # ggsave(plot = p1,
  #        dpi = 1200,
  #        file = paste0('Figures/ONI2010_month_', Sys.Date(), '.png'))
  ggsave(plot = p2,
         dpi = dpi,
         file = paste0('Figures/ONI1990_', dpi, "dpi_", Sys.Date(), '.png'))
  ggsave(plot = p.all,
         dpi = dpi,
         file = paste0('Figures/ONIall_', dpi, "dpi_", Sys.Date(), '.png'))
  ggsave(plot = p4,
         dpi = dpi,
         file = paste0('Figures/ONI1980_', dpi, "dpi_", Sys.Date(), '.png'))
  ggsave(plot = p5,
         dpi = dpi,
         file = paste0('Figures/ONI2005_', dpi, "dpi_", Sys.Date(), '.png'))
  # ggsave(plot = p1.2005,
  #        dpi = 1200,
  #        file = paste0('Figures/ONI2005_month_', Sys.Date(), '.png'),
  #        height = 4.9, width = 8)

  ggsave(plot = p.1991to2007,
         dpi = dpi,
         file = paste0('Figures/ONI1991to2007_', dpi, "dpi_", Sys.Date(), '.png'),
         height = 3.7, width = 5.9)

}
