############################################################################################################################################################################################################################################################################################
# R script created for:
## Riaz et al 2024 
### Connectivity of marine predators over the Patagonian Shelf during the highly pathogenic avian influenza (HPAI) outbreak
#### Script for processing raw SAFS GPS tracking data using animotum

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


SAFS_GPS <- read_rds("SAFS_GPS.rds")

unique(SAFS_GPS$id)

GPS <- SAFS_GPS %>%
  filter(Type == "GPS")

newd <- GPS %>%
  group_by(id) %>%
  arrange(date)


##############################################################################################################################################
## Remove any duplicates or near duplicates that occured within 10 s

d1new <- newd %>%
  group_by(id) %>%
  do(distinct(., date, .keep_all = TRUE)) %>%
  do(mutate(
    .,
    dup = difftime(date, lag(date), units = "secs") < 10)) %>%
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
  mutate(lc = "G") %>%
  dplyr :: select(id, date, lc, lon, lat) %>%
  ungroup()



##############################################################################################################################################
##############################################################################################################################################
# Run tracks through SSM. 
# Run the Predicted first and then run for the Fitted

# NewTracks <- NewTracks

d3 <- data.frame(NewTracks) 

split_d <- split(d3, d3$id)[unique(d3$id)] ## creates ID list

fits <- aniMotum::fit_ssm(d3, model="rw", 
                          vmax = 4,
                          spdf = FALSE,
                          # control = ssm_control(optim=c("nlminb")), 
                          time.step = 0.25)


my.aes <- aes_lst(obs=TRUE, line=TRUE,mp=FALSE, conf = FALSE)

setwd("//saeri-file/users$/jriaz/Documents/07. Data/BI201819_WCAY/By Date_GPS/")

for(i in unique(fits$id)){
  subID <- subset(fits, id == i)
  gi <- aniMotum::map(subID, what = "predicted", aes = my.aes, by.date = TRUE)
  ggsave(filename = sprintf('%s.pdf', i), plot = gi, width = 9, height = 9)
}

fits_mp <- fit_mpm(fits, what = "predicted", model = "mpm")

GPS_Seal_DF_SSM <- grab(fits, what = "predicted", as_sf = FALSE, normalise = FALSE, group = FALSE)

GPS_SSM <- grab(fits_mp, what = "fitted", as_sf = FALSE, normalise = FALSE, group = FALSE)

GPS_SSM_Norm <- grab(fits_mp, what = "fitted", as_sf = FALSE, normalise = TRUE, group = FALSE)

GPS_SSM_Norm <- GPS_SSM_Norm %>%
  dplyr::select(id, date, g) %>%
  rename(g_norm = g)


Seal_GPS_SSM_Clean <- GPS_Seal_DF_SSM %>%
  left_join(GPS_SSM) %>%
  left_join(GPS_SSM_Norm)

Seal_GPS_SSM_Clean$ID <- Seal_GPS_SSM_Clean$id
Seal_GPS_SSM_Clean$trip <- Seal_GPS_SSM_Clean$id
Seal_GPS_SSM_Clean$col <- "BIRD"
Seal_GPS_SSM_Clean$time <- Seal_GPS_SSM_Clean$date
Seal_GPS_SSM_Clean$tag <- "GPS"
Seal_GPS_SSM_Clean$stage <- "WINTER"

setwd("//saeri-file/users$/jriaz/Documents/07. Data/BI201819_WCAY")
write_rds(Seal_GPS_SSM_Clean, "Seal_GPS_SSM_Clean.rds")

