# plot_sizeDistributions


rm(list=ls())

#source('Cc_SCB_functions.R')
# sysInfo <- Sys.info()
# ifelse(sysInfo[1] == 'Linux',
#        source('~/Documents/R/TomosFunctions.R'),
#        source('~/R/TomosFunctions.R'))
source('Cc_SCB_functions.R')
library(RODBC)
library(viridis)
save.fig <- F
SWFSC <- F

# data from Peckham et al (2008) ESR 5:171-183. Table 4
data.hoyt <- data.frame(year = c(1995, 1999:2007),
                        mean = c(57.5, 62.0, 67.9, 68.1, 69.1, 
                                 70.5, 69.4, 72.9, 72.9, 70.5), 
                        SD = c(13.5, 13.0, 9.8, 9.2, 8.6, 
                               9.4, 10.5, 9.4, 8.5, 9.2),
                        n = c(49, 63, 152, 184, 270, 
                              325, 144, 334, 795, 320))

# create random CCL values for Hoyt's data:
random.CCL <- vector(length = nrow(data.hoyt), mode = 'list')

for (k in 1:nrow(data.hoyt)){
  random.CCL[[k]] <- data.frame(CCL = rnorm(n = data.hoyt[k, 'n'],
                                            mean = data.hoyt[k, 'mean'],
                                            sd = data.hoyt[k, 'SD']),
                                year = data.hoyt[k, 'year'])
}

data.hoyt.CCL <- do.call('rbind', random.CCL)
data.hoyt.CCL$source = 'Peckham'

# Turner Tomaszewicz et al 2015 Biological Conservation 186:134-142.
# Cali's data; 2003 - 2011 pooled
data.cali.CCL <- read.csv(file = 'data/NPCc_Size_Dist_2015BioCon.csv')
data.cali.CCL$year <- round(runif(n = nrow(data.cali.CCL), 
                                  min = 2003, max = 2011))
data.cali.CCL$source <- 'Turner_Tomaszewicz'

if (SWFSC){
  # DGN bycatch from Allen et al. (2013) MEPS - no year...
  # get them from the source:
  fishery.data <- odbcConnect(dsn = 'OBSERVERS', uid = "", pwd = "")
  # 'Set" is a reserved word in SQL - so need to use * to get all fields
  fishery.turtle_tbl <- sqlQuery(channel = fishery.data, 
                         "select * from SeaTurt")
  
  # this one is not solved... 6/15/2017
  # turned out [dbo].[Set] works - Thanks to Yuhong!  5 minutes later.
  fishery.set_tbl <- sqlQuery(channel = fishery.data,
                      "select * from [dbo].[Set]")
  
  odbcClose(fishery.data)
  
  fishery.turtle_tbl$TripNumber <- as.character(fishery.turtle_tbl$TripNumber)
  
  fishery.set_tbl <- fishery.set_tbl[, c('TripNumber', 'Set', 
                                         'Year', 'MM', 'DD', 
                                         'LatD1', 'LatM1',
                                         'LongD1', 'LongM1')]
  
  fishery.set_tbl$Latitude <- fishery.set_tbl$LatD1 + fishery.set_tbl$LatM1/60
  fishery.set_tbl$Longitude <- fishery.set_tbl$LongD1 + fishery.set_tbl$LongM1/60
  
  fishery.set_tbl$TripNumber <- as.character(fishery.set_tbl$TripNumber)
  
  fishery.turtle_tbl <- left_join(fishery.turtle_tbl, 
                                  fishery.set_tbl, 
                                  by = c('TripNumber', 'Set'))
  
  fishery.turtle_Cc_tbl <- filter(fishery.turtle_tbl, 
                                  SpCd == 'CC')
  
  # every time this script is run, the fishery data are saved into
  # hard drive - in case the script needs to be run without the connection
  # to the server.
  save(fishery.set_tbl, fishery.turtle_tbl, fishery.turtle_Cc_tbl, 
       file = 'RData/fishery_turtle_data.RData')
  
} else {
  load('RData/fishery_turtle_data.RData')
}

data.fishery.CCL <- data.frame(CCL = fishery.turtle_Cc_tbl$CarpLen,
                               year = fishery.turtle_Cc_tbl$Year,
                               source = 'Fishery')

# stranding data:
infile <- paste0(dirSelector()$Rdir, 'Stranding/data/Turtle_Stranding_all_2017-10-30.rds')

dat1 <- read_rds(path = infile)

dat1.Cc <- filter(dat1, Genus == "Caretta") %>%
  select(., Genus, Species,
         Year_Initially_Observed,
         Stranded, Released, Latitude,
         Longitude, State,
         Sex, Curved_Carapace_Length,
         Straight_Carapace_Length) %>%
  rename(., year = Year_Initially_Observed, 
         CCL = Curved_Carapace_Length)

data.stranding.CCL <- select(dat1.Cc, CCL, year) %>%
  mutate(., source = 'Stranding') %>%
  na.omit()

all.data <- rbind(data.hoyt.CCL,
                  data.cali.CCL,
                  data.fishery.CCL,
                  data.stranding.CCL) %>%
  rename(., Source = source)


lm.source <- lm(CCL ~ Source -1, data = all.data)
sum.lm <- summary(lm.source)

ymins <- sum.lm$coefficients[, 'Estimate'] - sum.lm$coefficients[, 'Std. Error']
ymaxs <- sum.lm$coefficients[, 'Estimate'] + sum.lm$coefficients[, 'Std. Error']

colors <- viridis(n = 4)
cols <- c('Peckham' = colors[1],
          'Fishery' = colors[2],
          'Stranding' = colors[3],
          'Turner Tomaszewicz' = colors[4])

color.defs <- c('Fishery' = 'navy',
                'Peckham' = 'darkorange',
                'Stranding' = 'firebrick1',
                'Turner_Tomaszewicz' = 'darkcyan')

p2 <- ggplot() + 
  geom_density(data = all.data,
               aes(x = CCL, 
                   fill = Source,
                   color = Source),
               bw = 10, alpha = 0.2) +
  xlim(c(1, 125)) +
  xlab('CCL (cm)') + ylab('Density') +
  scale_fill_manual(values = color.defs) +
  scale_color_manual(values = color.defs) +
  
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.position = c(0.85, 0.78)) 


#p2
p1 <- ggplot() + 
  geom_point(data = all.data,
             aes(x = year, y = CCL,
                 color = Source),
             size = 2) +  
  scale_color_viridis(discrete = TRUE)  +
  # annotate('rect', xmin = 2003, xmax = 2011,
  #          ymin = -Inf, ymax = Inf, alpha = 0.3) +
  annotate('rect', xmin = -Inf, xmax = Inf,
           ymin = ymins['SourceFishery'],
           ymax = ymaxs['SourceFishery'],
           fill = colors[1], alpha = 0.2) +
  annotate('rect', xmin = -Inf, xmax = Inf,
           ymin = ymins['SourcePeckham'],
           ymax = ymaxs['SourcePeckham'],
           fill = colors[2], alpha = 0.2) +
  annotate('rect', xmin = -Inf, xmax = Inf,
           ymin = ymins['SourceStranding'],
           ymax = ymaxs['SourceStranding'],
           fill = colors[3], alpha = 0.2) +
  annotate('rect', xmin = -Inf, xmax = Inf,
           ymin = ymins['SourceTurner_Tomaszewicz'],
           ymax = ymaxs['SourceTurner_Tomaszewicz'],
           fill = colors[4], alpha = 0.2) +
  xlab('Year') + ylab('CCL (cm)') + 
  labs(color = 'Data source') + 
  theme(axis.text = element_text(size = 12),
        legend.position = c(0.1, 0.85))  
  

if (save.fig){
  ggsave(plot = p2,
         filename = 'figures/CCL_all_sources.png',
         dpi = 1200)
  
}

# stranding.SCB <- subset(stranding,
#                         Latitude < 34.45 & Longitude > -122)
