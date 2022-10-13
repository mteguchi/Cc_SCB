#DGN_bycatch


# get data from DGN observer program dataset:

rm(list=ls())
library(RODBC)
library(dplyr)

source('Cc_SCB_functions.R')

internet <- T
# load a couple databases through ODBC
if (internet){
  CADGN <- odbcConnect(dsn = 'OBSERVERS', uid = '', pwd = '')
  CADGN.tbls <- sqlTables(CADGN)
  CADGN.SeaTurt <- sqlQuery(CADGN, 'select * from SeaTurt')
  CADGN.Set <- sqlQuery(CADGN, 'select * from "Set"')
  
  odbcClose(CADGN)

  write.csv.rename(CADGN.SeaTurt, 
                   file = 'data/CADGN_SeaTurt.csv',
                   quote = F, row.names = F)
  write.csv.rename(CADGN.Set, 
                   file = paste0('data/CADGN_Set.csv'),
                   quote = F, row.names = F)
} else {
  CADGN.SeaTurt <- read.table(file = 'data/CADGN_SeaTurt.csv',
                              header = T, sep = ",")
  CADGN.Set <- read.table(file = 'data/CADGN_Set.csv', 
                          header = T, sep = ",")
}

data_Cc <- filter(CADGN.SeaTurt, SpCd == 'CC') %>% 
  left_join(., CADGN.Set, by = c("TripNumber", "Set")) %>%
  select(., c(TripNumber, Set, SpCd, CarpLen, CarpWth,
              Season, Year, MM, DD, LatD1, LatM1, 
              LongD1, LongM1, LatD2, LatM2,
              LongD2, LongM2))

data_Cc$LatDD1 <- data_Cc$LatD1 + data_Cc$LatM1/60
data_Cc$LonDD1 <- data_Cc$LongD1 + data_Cc$LongM1/60

data_Cc$LatDD2 <- data_Cc$LatD2 + data_Cc$LatM2/60
data_Cc$LonDD2 <- data_Cc$LongD2 + data_Cc$LongM2/60

write.csv.rename(data_Cc,
                 file = 'data/CC_bycatch_DGN.csv',
                 quote = F, row.names = F)

