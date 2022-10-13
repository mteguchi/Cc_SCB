#plotONI

rm(list = ls())

library(ggplot2)
#library(dplyr)
library(viridis)
library(reshape)
#library(tidyr)

save.fig <- F
dat.raw <- read.delim('Data/PDO_20171128.txt',
                      sep = "", header = T)
PDO.values <- melt(dat.raw, id.vars = 'YEAR')
colnames(PDO.values) <- c('Year', 'Month', 'PDO')

dt <- seq(from = 0, to = 1.0 - 1/12, by = 1/12)
uniq.period <- c('JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN',
                 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC')

PDO.values$dt <- NA
for (k in 1:length(uniq.period)){
  PDO.values[PDO.values$Month == uniq.period[k], 'dt'] <- dt[k]
}

PDO.values$time <- PDO.values$Year + PDO.values$dt
PDO.values$Pos <- ifelse(PDO.values$PDO > 0, 'TRUE', 'FALSE')

min.yr <- 1990
max.yr <- 2017
PDO.values.1990 <- subset(PDO.values, Year <= max.yr & Year >= min.yr)
DGN_bycatch_year <- c(2001, 1998, 1997, 1993, 1992)

p2 <- ggplot(data = PDO.values.1990) +
  geom_bar(stat = 'identity',
           aes(x = time, y = PDO, fill = Pos)) +
  scale_x_continuous(name = '',
                     breaks = seq(from = min.yr, to = max.yr, by = 1)) +
  scale_fill_manual(values = c('blue', 'red'), guide = FALSE) +
  #ggtitle('PDO index') +
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
        axis.text.y = element_text(size = 12))

min.yr <- 1980
max.yr <- max(PDO.values$Year)
PDO.values.1980 <- subset(PDO.values,
                          Year <= max.yr & Year >= min.yr)
PDO.values.1980$Year <- as.factor(PDO.values.1980$Year)

p4 <- ggplot(data = PDO.values.1980) +
  geom_bar(stat = 'identity',
           aes(x = time, y = PDO, fill = Pos)) +
  scale_x_continuous(name = '',
                     breaks = seq(from = min.yr,
                                  to = max.yr, by = 1)) +
  scale_fill_manual(values = c('blue', 'red'), guide = FALSE) +
  ggtitle('Pacific Decadal Oscillation Index') +
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

min.yr <- min(PDO.values$Year)
max.yr <- max(PDO.values$Year)
PDO.values$Year <- as.factor(PDO.values$Year)

p3 <- ggplot(data = PDO.values) +
  geom_bar(stat = 'identity',
           aes(x = time, y = PDO, fill = Pos)) +
  scale_x_continuous(name = '',
                     breaks = seq(from = min.yr,
                                  to = max.yr, by = 1)) +
  scale_fill_manual(values = c('blue', 'red'), guide = FALSE) +
  ggtitle('Pacific Decadal Oscillation Index') +
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


dpi <- 300
if (save.fig){
  ggsave(plot = p2,
         dpi = dpi,
         file = paste0('Figures/PDO1990_', dpi, "dpi_", Sys.Date(), '.png'))
  ggsave(plot = p3,
         dpi = dpi,
         file = paste0('Figures/PDOall_', dpi, "dpi_", Sys.Date(), '.png'))
  ggsave(plot = p4,
         dpi = dpi,
         file = paste0('Figures/PDO1980_', dpi, "dpi_", Sys.Date(), '.png'))


}
