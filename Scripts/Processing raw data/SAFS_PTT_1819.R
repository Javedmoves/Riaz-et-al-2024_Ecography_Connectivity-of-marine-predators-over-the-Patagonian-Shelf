############################################################################################################################################################################################################################################################################################
# R script created for:
## Riaz et al 2024
### Connectivity of marine predators over the Patagonian Shelf during the highly pathogenic avian influenza (HPAI) outbreak
#### Script for processing raw SAFS PTT tracking data (2018 and 2019) using animotum

############################################################################################################################################################################################################################################################################################



library(ggplot2)
library(tidyverse)
library(aniMotum)
library(hms)
library(viridis)
library(scales)
library(readr)
library(viridis)
library(rgdal)
library(grid)
library(raster)
library(stringr)
library(lubridate)
library(maptools)
library(sp)
library(diveMove)
library(sf)
library(rnaturalearthhires)
library(data.table)
library(parallel)
library(future)
library(future.apply)
library(adehabitatLT)
library(ggOceanMaps)

Basemap1 <- basemap(limits = c(-72, -48.8, -57, -34), bathymetry = TRUE, rotate = TRUE, grid.col = NA,) + 
  theme(legend.position = "none") +
  annotation_scale(location = "br")  
Basemap1

##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
## Read in DF and some basic time and date formatting
setwd("//saeri-file/users$/jriaz/Documents/07. Data/BI201819_WCAY")

data_path <- ("//saeri-file/users$/jriaz/Documents/07. Data/BI201819_WCAY/")


SAFS_PTT <- read_rds("SAFS_PTT_BIWC.rds")

unique(SAFS_PTT$id) ## 17 ID's

SAFS_PTT <- SAFS_PTT %>%
  dplyr::select(id, date, class, lon, lat) ## Selects only the relevant columns
colnames(SAFS_PTT)[3] <- "lc"
unique(SAFS_PTT$id) ## 25 individuals


Plot <- Basemap1 + 
  geom_spatial_point(data = SAFS_PTT, aes(x = lon, y = lat), alpha = 0.5) +
  scale_colour_viridis_d() +
  theme(legend.position = "right")
Plot


##############################################################################################################################################

newd <- SAFS_PTT %>%
  group_by(id) %>%
  arrange(id, date) %>%
  filter(!lc == "Z")


##############################################################################################################################################
## Remove any duplicates or near duplicates that occured within 120 s


d1new <- newd %>%
  group_by(id) %>%
  do(distinct(., date, .keep_all = TRUE)) %>%
  do(mutate(
    .,
    dup = difftime(date, lag(date), units = "secs") < 120)) %>%
  do(arrange(., order(date))) %>%
  dplyr:: filter(.,!dup) %>%
  dplyr:: select(-dup)

d1.rep.new <- nrow(newd) - nrow(d1new)
cat(sprintf("%d duplicate time &/or near duplicate location/time records removed\n", d1.rep.new)) 
##

##############################################################################################################################################
## Remove extreme travel rate locations to be ignored at ssm filter stage

vmax <- 4

d5new <- d1new %>%
  do(mutate(., keep = grpSpeedFilter(cbind(date, lon, lat), speed.thr = vmax)))
d5.rep.new <- nrow(d1new) - sum(d5new$keep)
cat(sprintf(
  paste(
    "\n%d records with travel rates >",
    vmax,
    "m/s will be ignored by SSM filter\n"
  ),
  d5.rep.new
)) 


##############################################################################################################################################
## Remove all locations flagged to discard during the above prefiltering stage before running the SSM


NewTracks <- d5new %>%
  filter(keep == "TRUE") %>%
   dplyr :: select(id, date, lc, lon, lat) %>%
  ungroup()


##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
# Need to clean up the tracks because there are some whacky points

CleanTracks_PTT <- NewTracks
  
Fix1 <- CleanTracks_PTT %>%
  filter(id == "153626 BIWC") %>%
  filter(lon < -58) 
Basemap1 + geom_spatial_point(data = Fix1, aes(x = lon, y = lat)) + theme(legend.position = "right")

Fix2 <- CleanTracks_PTT %>%
  filter(id == "153638 BIWC") %>%
  filter(date > "2016-01-01 00:00:00") 
  # mutate(id = "153638_T2 Bird Island 2018")
Basemap1 + geom_spatial_point(data = Fix2, aes(x = lon, y = lat)) + theme(legend.position = "right")
  

Fix3 <- CleanTracks_PTT %>%
  filter(id == "153642 BIWC") %>%
  filter(lon < -55) 
Basemap1 + geom_spatial_path(data = Fix3, aes(x = lon, y = lat)) + theme(legend.position = "right")
  

Fix4 <- CleanTracks_PTT %>%
  filter(id == "153650 BIWC") %>%
  filter(lon < -55) 
Basemap1 + geom_spatial_path(data = Fix4, aes(x = lon, y = lat)) + theme(legend.position = "right")


Fix5 <- CleanTracks_PTT %>%
  filter(id == "153644 BIWC") %>%
  filter(date > "2018-01-01 00:00:00")
  # mutate(id = "153644_T1 Bird Island 2018")
Basemap1 + geom_spatial_path(data = Fix5, aes(x = lon, y = lat)) + theme(legend.position = "right")


Fix6 <- CleanTracks_PTT %>%
  filter(id == "153651 BIWC") %>%
  filter(lon < -55) 
Basemap1 + geom_spatial_path(data = Fix6, aes(x = lon, y = lat)) + theme(legend.position = "right")


Fix7 <- CleanTracks_PTT %>%
  filter(id == "153654 BIWC") %>%
  filter(date > "2018-01-01 00:00:00")
Basemap1 + geom_spatial_path(data = Fix7, aes(x = lon, y = lat)) + theme(legend.position = "right")
  
  
Fix8 <- CleanTracks_PTT %>%
  filter(id == "153657 BIWC") %>%
  filter(date > "2018-01-01 00:00:00")
Basemap1 + geom_spatial_path(data = Fix8, aes(x = lon, y = lat)) + theme(legend.position = "right")
  
  

Fix9 <- CleanTracks_PTT %>%
  filter(id == "153661 BIWC") %>%
  filter(date > "2018-08-19 12:00:00") 
Basemap1 + geom_spatial_path(data = Fix9, aes(x = lon, y = lat)) + theme(legend.position = "right")
  

Fix10 <- CleanTracks_PTT %>%
  filter(id == "153663 BIWC") %>%
  filter(lon < -55) 
Basemap1 + geom_spatial_path(data = Fix10, aes(x = lon, y = lat)) + theme(legend.position = "right")



CleanTracks_PTT <- CleanTracks_PTT %>%
  filter(! id == "153626 BIWC",
         ! id == "153638 BIWC",
         ! id == "153642 BIWC",
         ! id == "153644 BIWC",
         ! id == "153650 BIWC",
         ! id == "153651 BIWC",
         ! id == "153654 BIWC",
         ! id == "153657 BIWC",
         ! id == "153661 BIWC",
         ! id == "153663 BIWC") %>% 
  full_join(Fix1) %>%
  full_join(Fix2) %>%
  full_join(Fix3) %>%
  full_join(Fix4) %>%
  full_join(Fix5) %>%
  full_join(Fix6) %>%
  full_join(Fix7) %>%
  full_join(Fix8) %>%
  full_join(Fix9) %>%
  full_join(Fix10)


##############################################################################################################################################
##############################################################################################################################################

NewTracks <- CleanTracks_PTT

d3 <- data.frame(NewTracks) 

split_d <- split(d3, d3$id)[unique(d3$id)] ## creates ID list

fits <- aniMotum::fit_ssm(d3, model="rw", 
                          vmax = 4,
                          spdf = FALSE,
                          # control = ssm_control(optim=c("nlminb")), 
                          time.step = 1)


my.aes <- aes_lst(obs=TRUE, line=TRUE,mp=FALSE, conf = FALSE)

setwd("//saeri-file/users$/jriaz/Documents/07. Data/BI201819_WCAY/By Date/")

for(i in unique(fits$id)){
  subID <- subset(fits, id == i)
  gi <- aniMotum::map(subID, what = "predicted", aes = my.aes, by.date = TRUE)
  ggsave(filename = sprintf('%s.pdf', i), plot = gi, width = 9, height = 9)
}

fits_mp <- fit_mpm(fits, what = "predicted", model = "mpm")

PTT_Seal_DF_SSM <- grab(fits, what = "predicted", as_sf = FALSE, normalise = FALSE, group = FALSE)

PTT_SSM <- grab(fits_mp, what = "fitted", as_sf = FALSE, normalise = FALSE, group = FALSE)

PTT_SSM_Norm <- grab(fits_mp, what = "fitted", as_sf = FALSE, normalise = TRUE, group = FALSE)

PTT_SSM_Norm <- PTT_SSM_Norm %>%
  dplyr::select(id, date, g) %>%
  rename(g_norm = g)


Seal_PTT_SSM_Clean <- PTT_Seal_DF_SSM %>%
  left_join(PTT_SSM) %>%
  left_join(PTT_SSM_Norm)

Seal_PTT_SSM_Clean$ID <- Seal_PTT_SSM_Clean$id
Seal_PTT_SSM_Clean$trip <- Seal_PTT_SSM_Clean$id
Seal_PTT_SSM_Clean$col <- "BIRD_WI"
Seal_PTT_SSM_Clean$time <- Seal_PTT_SSM_Clean$date
Seal_PTT_SSM_Clean$tag <- "PTT"
Seal_PTT_SSM_Clean$stage <- "WINTER"

setwd("//saeri-file/users$/jriaz/Documents/07. Data/BI201819_WCAY")
write_rds(Seal_PTT_SSM_Clean, "Seal_PTT_BIWC_SSM_Clean.rds")



