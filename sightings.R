# hahahah it doesn't work so well  - 16 may 2017



library(ggplot2)

EPac <- read.table("data/coast_Epac.txt", sep = ",")
EPac <- EPac[EPac[,2] > 31 & EPac[,2]<36 & EPac[,1] > -126.5 & EPac[,1] <117.5,]
df_SCB <- data.frame(longitude = EPac[,1], latitude = EPac[,2])

dat <- read.table("data/Sightings Query 09July2015.txt",
                  sep = ",", header = TRUE, 
                  strip.white = TRUE)

datCC <- dat[dat$Species_Code == "CC",]

p1 <- ggplot() + geom_polygon(data = df_SCB, aes(x=longitude, y=latitude))

p1
