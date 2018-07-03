#-------------------------------------------------------------------------------
#
# Script to process raw fisheries data to lengthclass data
#
# Coded: Perttu Rantanen
#
# Date: JUL-2018
#
# Client: Project särkikala
#-------------------------------------------------------------------------------


#install.packages("RPostgreSQL")
#install.packages("dplyr")


#- Clear workspace
rm(list=ls())

# needed libraries
library(dplyr)
library(tidyverse)


#-------------------------------------------------------------------------------
#                   0. set working directories to match folder paths                      
#-------------------------------------------------------------------------------

# Perttu:
path_data <- "C:/perttu/eu-tike/SUOMU/SCRIPTS/Suomu-scripts/orig" # folder where raw data is
path_out <- "C:/perttu/eu-tike/SUOMU/SCRIPTS/Suomu-scripts/out" # folder for results

setwd(path_data)

# import raw
raw_data <- read.csv2("2012_raw.csv", sep = ";", dec = "," )

#set >800 length to 0 (no such fish)
raw_data$Pituus_mm[raw_data$Pituus_mm >= 800 ] <- 0

#set 0 length to NA
raw_data$Pituus_mm<- raw_data$Pituus_mm %>% na_if(0) 

#Check min and max
min(raw_data$Pituus_mm, na.rm = TRUE)
max(raw_data$Pituus_mm, na.rm = TRUE)

# create 1 cm lengthclasses based on min & max ranges
raw_data$pituusluokka[raw_data$Pituus_mm >= 100 & raw_data$Pituus_mm < 110] <- 100
raw_data$pituusluokka[raw_data$Pituus_mm >= 110 & raw_data$Pituus_mm < 120] <- 110
raw_data$pituusluokka[raw_data$Pituus_mm >= 120 & raw_data$Pituus_mm < 130] <- 120
raw_data$pituusluokka[raw_data$Pituus_mm >= 130 & raw_data$Pituus_mm < 140] <- 130
raw_data$pituusluokka[raw_data$Pituus_mm >= 140 & raw_data$Pituus_mm < 150] <- 140
raw_data$pituusluokka[raw_data$Pituus_mm >= 150 & raw_data$Pituus_mm < 160] <- 150
raw_data$pituusluokka[raw_data$Pituus_mm >= 160 & raw_data$Pituus_mm < 170] <- 160
raw_data$pituusluokka[raw_data$Pituus_mm >= 170 & raw_data$Pituus_mm < 180] <- 170
raw_data$pituusluokka[raw_data$Pituus_mm >= 180 & raw_data$Pituus_mm < 190] <- 180
raw_data$pituusluokka[raw_data$Pituus_mm >= 190 & raw_data$Pituus_mm < 200] <- 190
raw_data$pituusluokka[raw_data$Pituus_mm >= 200 & raw_data$Pituus_mm < 210] <- 200
raw_data$pituusluokka[raw_data$Pituus_mm >= 210 & raw_data$Pituus_mm < 220] <- 210
raw_data$pituusluokka[raw_data$Pituus_mm >= 220 & raw_data$Pituus_mm < 230] <- 220
raw_data$pituusluokka[raw_data$Pituus_mm >= 230 & raw_data$Pituus_mm < 240] <- 230
raw_data$pituusluokka[raw_data$Pituus_mm >= 240 & raw_data$Pituus_mm < 250] <- 240
raw_data$pituusluokka[raw_data$Pituus_mm >= 250 & raw_data$Pituus_mm < 260] <- 250
raw_data$pituusluokka[raw_data$Pituus_mm >= 260 & raw_data$Pituus_mm < 270] <- 260
raw_data$pituusluokka[raw_data$Pituus_mm >= 270 & raw_data$Pituus_mm < 280] <- 270
raw_data$pituusluokka[raw_data$Pituus_mm >= 280 & raw_data$Pituus_mm < 290] <- 280
raw_data$pituusluokka[raw_data$Pituus_mm >= 290 & raw_data$Pituus_mm < 300] <- 290
raw_data$pituusluokka[raw_data$Pituus_mm >= 300 & raw_data$Pituus_mm < 310] <- 300
raw_data$pituusluokka[raw_data$Pituus_mm >= 310 & raw_data$Pituus_mm < 320] <- 310
raw_data$pituusluokka[raw_data$Pituus_mm >= 320 & raw_data$Pituus_mm < 330] <- 320
raw_data$pituusluokka[raw_data$Pituus_mm >= 330 & raw_data$Pituus_mm < 340] <- 330
raw_data$pituusluokka[raw_data$Pituus_mm >= 340 & raw_data$Pituus_mm < 350] <- 340
raw_data$pituusluokka[raw_data$Pituus_mm >= 350 & raw_data$Pituus_mm < 360] <- 350
raw_data$pituusluokka[raw_data$Pituus_mm >= 360 & raw_data$Pituus_mm < 370] <- 360
raw_data$pituusluokka[raw_data$Pituus_mm >= 370 & raw_data$Pituus_mm < 380] <- 370
raw_data$pituusluokka[raw_data$Pituus_mm >= 380 & raw_data$Pituus_mm < 390] <- 380
raw_data$pituusluokka[raw_data$Pituus_mm >= 390 & raw_data$Pituus_mm < 400] <- 390
raw_data$pituusluokka[raw_data$Pituus_mm >= 400 & raw_data$Pituus_mm < 410] <- 400
raw_data$pituusluokka[raw_data$Pituus_mm >= 410 & raw_data$Pituus_mm < 420] <- 410
raw_data$pituusluokka[raw_data$Pituus_mm >= 420 & raw_data$Pituus_mm < 430] <- 420
raw_data$pituusluokka[raw_data$Pituus_mm >= 430 & raw_data$Pituus_mm < 440] <- 430
raw_data$pituusluokka[raw_data$Pituus_mm >= 440 & raw_data$Pituus_mm < 450] <- 440
raw_data$pituusluokka[raw_data$Pituus_mm >= 450 & raw_data$Pituus_mm < 460] <- 450
raw_data$pituusluokka[raw_data$Pituus_mm >= 460 & raw_data$Pituus_mm < 470] <- 460
raw_data$pituusluokka[raw_data$Pituus_mm >= 470 & raw_data$Pituus_mm < 480] <- 470
raw_data$pituusluokka[raw_data$Pituus_mm >= 480 & raw_data$Pituus_mm < 490] <- 480
raw_data$pituusluokka[raw_data$Pituus_mm >= 490 & raw_data$Pituus_mm < 500] <- 490
raw_data$pituusluokka[raw_data$Pituus_mm >= 500 & raw_data$Pituus_mm < 510] <- 500
raw_data$pituusluokka[raw_data$Pituus_mm >= 510 & raw_data$Pituus_mm < 520] <- 510
raw_data$pituusluokka[raw_data$Pituus_mm >= 520 & raw_data$Pituus_mm < 530] <- 520
raw_data$pituusluokka[raw_data$Pituus_mm >= 530 & raw_data$Pituus_mm < 540] <- 530
raw_data$pituusluokka[raw_data$Pituus_mm >= 540 & raw_data$Pituus_mm < 550] <- 540
raw_data$pituusluokka[raw_data$Pituus_mm >= 550 & raw_data$Pituus_mm < 560] <- 550
raw_data$pituusluokka[raw_data$Pituus_mm >= 560 & raw_data$Pituus_mm < 570] <- 560
raw_data$pituusluokka[raw_data$Pituus_mm >= 570 & raw_data$Pituus_mm < 580] <- 570
raw_data$pituusluokka[raw_data$Pituus_mm >= 580 & raw_data$Pituus_mm < 590] <- 580
raw_data$pituusluokka[raw_data$Pituus_mm >= 590 & raw_data$Pituus_mm < 600] <- 590
raw_data$pituusluokka[raw_data$Pituus_mm >= 600 & raw_data$Pituus_mm < 610] <- 600
raw_data$pituusluokka[raw_data$Pituus_mm >= 610 & raw_data$Pituus_mm < 620] <- 610
raw_data$pituusluokka[raw_data$Pituus_mm >= 620 & raw_data$Pituus_mm < 630] <- 620
raw_data$pituusluokka[raw_data$Pituus_mm >= 630 & raw_data$Pituus_mm < 640] <- 630
raw_data$pituusluokka[raw_data$Pituus_mm >= 640 & raw_data$Pituus_mm < 650] <- 640
raw_data$pituusluokka[raw_data$Pituus_mm >= 650 & raw_data$Pituus_mm < 660] <- 650
raw_data$pituusluokka[raw_data$Pituus_mm >= 660 & raw_data$Pituus_mm < 670] <- 660
raw_data$pituusluokka[raw_data$Pituus_mm >= 670 & raw_data$Pituus_mm < 680] <- 670
raw_data$pituusluokka[raw_data$Pituus_mm >= 680 & raw_data$Pituus_mm < 690] <- 680
raw_data$pituusluokka[raw_data$Pituus_mm >= 690 & raw_data$Pituus_mm < 700] <- 690
raw_data$pituusluokka[raw_data$Pituus_mm >= 700 & raw_data$Pituus_mm < 710] <- 700
raw_data$pituusluokka[raw_data$Pituus_mm >= 710 & raw_data$Pituus_mm < 720] <- 710
raw_data$pituusluokka[raw_data$Pituus_mm >= 720 & raw_data$Pituus_mm < 730] <- 720
raw_data$pituusluokka[raw_data$Pituus_mm >= 730 & raw_data$Pituus_mm < 740] <- 730
raw_data$pituusluokka[raw_data$Pituus_mm >= 740 & raw_data$Pituus_mm < 750] <- 740
raw_data$pituusluokka[raw_data$Pituus_mm >= 750 & raw_data$Pituus_mm < 760] <- 750
raw_data$pituusluokka[raw_data$Pituus_mm >= 760 & raw_data$Pituus_mm < 770] <- 760
raw_data$pituusluokka[raw_data$Pituus_mm >= 770 & raw_data$Pituus_mm < 780] <- 770
raw_data$pituusluokka[raw_data$Pituus_mm >= 780 & raw_data$Pituus_mm < 790] <- 780
raw_data$pituusluokka[raw_data$Pituus_mm >= 790 & raw_data$Pituus_mm < 800] <- 790

#Check min and max
min(raw_data$pituusluokka, na.rm = TRUE)
max(raw_data$pituusluokka, na.rm = TRUE)

#delete rows with NAs in pituusluokka
nrow(raw_data)
raw_data <- raw_data %>% drop_na(pituusluokka)
nrow(raw_data)

# aggregate data to lengthclass
lc_data <- raw_data %>% group_by(Näytenumero, Laji, pituusluokka) %>% summarise(pituusluokan_kpl_maara = sum(Kpl), aliotoksenpaino_g = sum(Paino_g))

lc_data <- lc_data %>% arrange(Näytenumero, Laji, pituusluokka)
lc_data$mittausvali <- 10
lc_data <- lc_data %>% select(Näytenumero, Laji, pituusluokka, mittausvali, pituusluokan_kpl_maara, aliotoksenpaino_g)

# select species
species <- lc_data %>% group_by(Näytenumero, Laji) %>% summarise(otoksen_paino_g = sum(aliotoksenpaino_g))
species$painokokosaaliista <- ""
species$Saalisluokka <-"LANDING"
species$Kayttotarkoitus <- "Ruokakalaa"
#weight from g to kg
species$otoksen_paino_kg <- species$otoksen_paino_g/1000
species <- species %>% select(Näytenumero, Saalisluokka, Laji, Kayttotarkoitus, painokokosaaliista, otoksen_paino_kg)

speciesaggr <- species$Laji
speciesaggr<- as.character(speciesaggr)
speciesaggr <- unique(speciesaggr)

# set working directory to save data
setwd(path_out)
write.csv(lc_data, "2012_särkikalaprojektin_pituusluokat.csv", row.names = F)
write.csv(species, "2012_särkikalaprojektin_lajijakauma.csv", row.names = F)
write.csv(speciesaggr, "2012_särkikalaprojektin_lajit.csv", row.names = F)
