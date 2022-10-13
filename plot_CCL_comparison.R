# plot_sizeDistributions


rm(list=ls())

source('Cc_SCB_functions.R')
library(RODBC)
library(viridis)
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
data.hoyt.CCL$source = 'Baja-1'

# Turner Tomaszewicz et al 2015 Biological Conservation 186:134-142.
# Cali's data; 2003 - 2011 pooled
data.cali.CCL <- read.csv(file = 'data/NPCc_Size_Dist_2015BioCon.csv')
data.cali.CCL$year <- round(runif(n = nrow(data.cali.CCL), 
                                  min = 2003, max = 2011))
data.cali.CCL$source <- 'Baja-2'

load('RData/fishery_turtle_data.RData')

data.fishery.CCL <- data.frame(CCL = fishery.turtle_Cc_tbl$CarpLen,
                               year = fishery.turtle_Cc_tbl$Year,
                               source = 'CA DGN \nFishery')

# stranding data:
infile <- 'data/CcStrandingQuery_16March2017.csv'
stranding <- read.table(infile, sep = ",", header = TRUE)
stranding$Year <- stranding$Year_Initially_Observed
#dat.stranding$fYear <- as.factor(dat0$Year_Initially_Observed)
stranding <- stranding[, c('State', 'Year',
                           'Species_Code',
                           'Latitude', 'Longitude',
                           'Weight',
                           'Curved_Carapace_Length',
                           'Straight_Carapace_Length')]

data.stranding.CCL <- na.omit(data.frame(CCL = stranding$Curved_Carapace_Length,
                                         year = stranding$Year,
                                         source = 'US \nStranding'))

all.data <- rbind(data.hoyt.CCL,
                  data.cali.CCL,
                  data.fishery.CCL,
                  data.stranding.CCL)
all.data$source <- as.factor(all.data$source)

sample.size <-  c(nrow(data.hoyt.CCL),
                  nrow(data.cali.CCL),
                  nrow(data.fishery.CCL),
                  nrow(data.stranding.CCL))

p2 <- ggplot(data = all.data) + 
  geom_boxplot(aes(x = source, y = CCL))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 12, angle = 0),
        axis.text.y = element_text(size = 12)) + 
  
  xlab('') + ylab('CCL (cm)')

# ggsave(plot = p2,
#        height = 2.5,
#        width = 4.3,
#        units = 'in',
#        filename = 'Figures/CCL_comparison.png',
#        dpi = 1200)

lm.source <- lm(CCL ~ source -1, data = all.data)
sum.lm <- summary(lm.source)

stranding.SCB <- subset(stranding,
                        Latitude < 34.45 & Longitude > -122)
