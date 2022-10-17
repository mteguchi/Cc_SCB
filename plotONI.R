#plotONI

rm(list = ls())

library(ggplot2)
library(tidyverse)
library(viridis)
library(reshape)


# from https://stackoverflow.com/questions/19200841/consecutive-rolling-sums-in-a-vector-in-r
moving.cumsum <- function(x, n = 2){
  # computes cumulative sum over a span
  y <- rowSums(outer(1:(length(x)-n+1),
                     1:n,
                     FUN=function(i,j){x[(j - 1) + i]}))
  #y <- c(rep(NA, n-1), y)
  return(y)
}

save.fig <- T
# Get ONI Oceanic Nino Index data:
# https://origin.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/ONI_change.shtml
# look at the link at the bottom to see the most up to date data in a flat ascii file.
oceans.and.maps.dir <- paste0(Sys.getenv("HOME"), "/Oceans and Maps/")
ONI.values <- read_fwf(file = paste0(oceans.and.maps.dir, "/ONI/ONI_20221012.txt"), 
                       col_positions = fwf_widths(c(4,5,7,11,5),
                                                  col_names = c("Year", "Month", "Total", "ClimAdj", "ONI")),
                       skip = 1,
                       col_types = cols(col_integer(),
                                        col_integer(),
                                        col_double(),
                                        col_double(),
                                        col_double()))

ONI.values %>% 
  mutate(time = Year + Month/12 - 1/12,
         fYear = as.factor(Year)) %>%
  mutate(time.end = time + 1/12) %>%
  mutate(Nino = ifelse(ONI.values$ONI > 0, 'TRUE', 'FALSE')) %>%
  mutate(cumuONI = cumsum(ONI)) %>%
  mutate(cumu6moONI = c(rep(NA, 5), moving.cumsum(ONI, 6))) %>%
  mutate(cumu4moONI = c(rep(NA, 3), moving.cumsum(ONI, 4))) %>%
  mutate(cumu2moONI = c(rep(NA, 1), moving.cumsum(ONI, 2))) %>%
  mutate(lag6moONI = c(rep(NA, 6), ONI[1:(nrow(ONI.values) - 6)])) %>%
  mutate(cumulag6moONI = c(rep(NA, 5), moving.cumsum(lag6moONI, 6))) -> ONI.values

period.df <- data.frame(Month = 1:12,
                        Period = c("DJF", "JFM", "FMA", 
                                   "MAM", "AMJ", "MJJ",
                                   "JJA", "JAS", "ASO", 
                                   "SON", "OND", "NDJ"))

ONI.values %>% 
  left_join(period.df, by = "Month") -> ONI.values

ONI.values.2010 <- subset(ONI.values, Year > 2009)
ONI.values.2005 <- subset(ONI.values, Year > 2004)

min.yr <- 1990
max.yr <- 2022
ONI.values.1990 <- filter(ONI.values, 
                          Year <= max.yr &
                            Year >= min.yr)
DGN_bycatch_year <- c(2006, 2001, 1998, 1997, 1993, 1992)

# 1990s bycatch years
ONI.values %>% 
  filter(Year %in% c(1992, 1993, 1997, 1998)) %>% 
  filter(Month == 12) -> ONI.values.label.1

# Immediately before and after bycatch years in 1990s
ONI.values %>% 
  filter(Year %in% c(1991, 1994, 1996, 1999)) %>% 
  filter(Month == 12) -> ONI.values.label.1a

# 2000s bycatch years and survey years
ONI.values %>% 
  filter(Year %in% c(2001, 2006)) %>% 
  filter(Month == 12) -> ONI.values.label.2

# Immediately before and after bycatch years in 2000s
ONI.values %>% 
  filter(Year %in% c(2000, 2002, 2005, 2007)) %>% 
  filter(Month == 12) -> ONI.values.label.2a

ONI.values.2010 %>% 
  filter(Year != 2011, Year != 2015) %>% 
  filter(Month == 12) -> ONI.values.label.3a

# survey years
ONI.values %>% 
  filter(Year %in% c(2011, 2015)) %>% 
  filter(Month == 12) -> ONI.values.label.3

# 1980-1999
ONI.values %>% 
  filter(Year > 1979, Year < 2000) -> ONI.values.1980.1999

# 2000-2022
ONI.values %>% 
  filter(Year > 1999, Year < 2023) ->  ONI.values.2000.2022

p1.1 <- ggplot(data = ONI.values.1980.1999,
             aes(x = Month,
                 y = ONI,
                 color = fYear,
                 group = fYear,
                 label = fYear)) +
  scale_color_viridis(discrete = TRUE, 
                      name = "Year") +
  geom_point(size = 2) + 
  geom_line(size = 1.5) +
  annotate('rect', xmin = 9, xmax = 11,
           ymin = -Inf, ymax = Inf, 
           alpha = 0.3) +
  scale_x_continuous(breaks = 1:12) +
  geom_text(data = ONI.values.label.1,
            aes(x = Month, y = ONI, label = Year),
            color = "black",
            size = 6, fontface = "bold") +

  geom_text(data = ONI.values.label.1a,
            aes(x = Month, y = ONI, label = Year),
            color = "orange",
            size = 6, fontface = "bold") +
  ylab("ONI") +
  xlab("Month") +
  #ggtitle("ONI") +
  theme(plot.title = element_text(hjust = 0.5),
        #legend.title = element_text(size = 10, hjust = 0.5),
        #legend.text = element_text(size = 8, vjust = 0),
        legend.position = "none",
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))
#

p1.2 <- ggplot(data = ONI.values.2000.2022,
               aes(x = Month,
                   y = ONI,
                   color = fYear,
                   group = fYear,
                   label = fYear)) +
  scale_color_viridis(discrete = TRUE, 
                      name = "Year") +
  geom_point(size = 2) + 
  geom_line(size = 1.5) +
  annotate('rect', xmin = 9, xmax = 11,
           ymin = -Inf, ymax = Inf, 
           alpha = 0.3) +
  scale_x_continuous(breaks = 1:12) +
  geom_text(data = ONI.values.label.2,
            aes(x = Month, y = ONI, label = Year),
            color = "black",
            size = 6, fontface = "bold") +
  
  geom_text(data = ONI.values.label.2a,
            aes(x = Month, y = ONI, label = Year),
            color = "orange",
            size = 6, fontface = "bold") +
  geom_text(data = ONI.values.label.3,
            aes(x = Month, y = ONI, label = Year),
            color = "red",
            size = 6, fontface = "bold") +
  ylab("ONI") +
  xlab("Month") +
  #ggtitle("ONI") +
  theme(plot.title = element_text(hjust = 0.5),
        #legend.title = element_text(size = 10, hjust = 0.5),
        #legend.text = element_text(size = 8, vjust = 0),
        legend.position = "none",
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))


p1.3 <- ggplot(data = ONI.values.2010,
               aes(x = Month,
                   y = ONI,
                   color = fYear,
                   group = fYear,
                   label = fYear)) +
  scale_color_viridis(discrete = TRUE, 
                      name = "Year") +
  geom_point(size = 2) + 
  geom_line(size = 1.5) +
  annotate('rect', xmin = 9, xmax = 11,
           ymin = -Inf, ymax = Inf, 
           alpha = 0.3) +
  scale_x_continuous(breaks = 1:12) +
  geom_text(data = ONI.values.label.3a,
            aes(x = Month, y = ONI, label = Year),
            color = "black",
            size = 6, fontface = "bold") +

  # geom_text(data = ONI.values.label.2a,
  #           aes(x = Month, y = ONI, label = Year),
  #           color = "orange",
  #           size = 6, fontface = "bold") +
  geom_text(data = ONI.values.label.3,
            aes(x = Month, y = ONI, label = Year),
            color = "red",
            size = 6, fontface = "bold") +
  ylab("ONI") +
  xlab("Month") +
  #ggtitle("ONI") +
  theme(plot.title = element_text(hjust = 0.5),
        #legend.title = element_text(size = 10, hjust = 0.5),
        #legend.text = element_text(size = 8, vjust = 0),
        legend.position = "none",
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))

p2 <- ggplot(data = ONI.values.1990) +
  geom_bar(stat = 'identity',
           aes(x = time, y = ONI, fill = Nino)) +
  scale_x_continuous(name = '',
                     breaks = seq(from = min.yr,
                                  to = max.yr, by = 1)) +
  scale_fill_manual(values = c('blue', 'red'), 
                    guide = "none") +
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
  scale_fill_manual(values = c('blue', 'red'), 
                    guide = "none") +
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
  scale_fill_manual(values = c('blue', 'red'), 
                    guide = "none") +
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
  scale_fill_manual(values = c('blue', 'red'), 
                    guide = "none") +
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
  scale_fill_manual(values = c('blue', 'red'), 
                    guide = "none") +
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
  dpi <- 600
  ggsave(plot = p1.1,
         dpi = dpi,
         device = "png",
         filename = paste0('figures/ONI1980_20yr_month_', Sys.Date(), '.png'),
         width = 7.2, height = 4.32)
  ggsave(plot = p1.2,
         dpi = dpi,
         device = "png",
         filename = paste0('figures/ONI2000_23yr_month_', Sys.Date(), '.png'),
         width = 7.2, height = 4.32)
  
  ggsave(plot = p1.3,
         dpi = dpi,
         device = "png",
         filename = paste0('figures/ONI2010_13yr_month_', Sys.Date(), '.png'),
         width = 7.2, height = 4.32)

  ggsave(plot = p2,
         dpi = dpi, device = "png",
         file = paste0('figures/ONI1990_', dpi, "dpi_", Sys.Date(), '.png'),
         width = 7.2, height = 4.32)
  ggsave(plot = p.all,
         dpi = dpi, device = "png",
         file = paste0('figures/ONIall_', dpi, "dpi_", Sys.Date(), '.png'),
         width = 7.2, height = 4.32)
  ggsave(plot = p4,
         dpi = dpi, device = "png",
         file = paste0('figures/ONI1980_', dpi, "dpi_", Sys.Date(), '.png'),
         width = 7.2, height = 4.32)
  ggsave(plot = p5,
         dpi = dpi, device = "png",
         file = paste0('figures/ONI2005_', dpi, "dpi_", Sys.Date(), '.png'),
         width = 7.2, height = 4.32)
  # ggsave(plot = p1.2005,
  #        dpi = 1200,
  #        file = paste0('Figures/ONI2005_month_', Sys.Date(), '.png'),
  #        height = 4.9, width = 8)

  ggsave(plot = p.1991to2007,
         dpi = dpi, device = "png",
         file = paste0('figures/ONI1991to2007_', dpi, 
                       "dpi_", Sys.Date(), '.png'),
         height = 3.7, width = 5.9)

}
