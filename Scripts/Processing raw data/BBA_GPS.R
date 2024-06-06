############################################################################################################################################################################################################################################################################################
# R script created for:
## Riaz et al 2024
### Connectivity of marine predators over the Patagonian Shelf during the highly pathogenic avian influenza (HPAI) outbreak
#### Script for processing raw BBA tracking data using animotum

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

Basemap1 <- basemap(limits = c(-72, -48.8, -57, -34), bathy.style = "rcb", rotate = TRUE, grid.col = NA) + 
  theme(legend.position = "none") 
Basemap1


##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
## Read in DF and some basic time and date formatting
setwd("//saeri-file/users$/jriaz/Documents/07. Data/BBA")

data_path <- ("//saeri-file/users$/jriaz/Documents/07. Data/BBA/")

Combined_GPS08_22 <- read_rds("BBA_GPS.rds")

unique(Combined_GPS08_22$id) ## 361

Dist_BBA <- Combined_GPS08_22 %>%
  dplyr::select(id, stage) %>%
  distinct(id) 


##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################

newd <- Combined_GPS08_22 %>%
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

vmax <- 20

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
)) ## 


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

d3 <- data.frame(NewTracks) 

split_d <- split(d3, d3$id)[unique(d3$id)] ## creates ID list

plan(multisession)
ts <- Sys.time()
fit_all <- future_lapply(split_d, function(z) {
  fits <- fit_ssm(z, model="rw", 
                  vmax = 20,
                  spdf = FALSE,
                  # control = ssm_control(optim=c("nlminb")), 
                  time.step = 0.1666667)
  })
plan(sequential)
difftime(Sys.time(),ts)

fit_all <- bind_rows(fit_all)
fit_new <- fit_all

write_rds(fit_new, "BBA_SSM_data.rds")


BBA_SSM <- read_rds("BBA_SSM_data.rds")

 setwd("//saeri-file/users$/jriaz/Documents/07. Data/BBA/By_Date/")
 my.aes <- aes_lst(obs=TRUE, line=TRUE,mp=FALSE, conf = FALSE)
 
 for(i in unique(BBA_SSM$id)){
   subID <- subset(BBA_SSM, id == i)
   gi <- aniMotum::map(subID, what = "predicted", aes = my.aes, by.date = TRUE)
   ggsave(filename = sprintf('%s.png', i), plot = gi, width = 9, height = 9)
 }


 # for(i in unique(BBA_SSM$id)){
 #   subID <- subset(BBA_SSM, id == i)
 #   tryCatch({
 #     gi <- aniMotum::map(subID, what = "predicted", aes = my.aes, by.date = TRUE)
 #     ggsave(filename = sprintf('%s.png', i), plot = gi, width = 9, height = 9)
 #   }, error = function(e) {
 #     cat("Error occurred for ID:", i, "\n")
 #     cat("Error message:", conditionMessage(e), "\n")
 #   })
 # }
 
 ## Remove bad data
 BBA_SSM <- BBA_SSM %>%
   filter(!id == "GPS107 NEWI",
          !id == "GPS125 NEWI")
 
 
fits_mp <- fit_mpm(BBA_SSM, what = "predicted", model = "mpm")

write_rds(fits_mp, "BBA_MPM_mod.rds")
fits_mp <- read_rds("BBA_MPM_mod.rds")


GPS_BBA_DF_SSM <- grab(BBA_SSM, what = "predicted", as_sf = FALSE, normalise = FALSE, group = FALSE)
write_rds(GPS_BBA_DF_SSM, "GPS_BBA_DF_SSM.rds")
GPS_BBA_DF_SSM <- read_rds("GPS_BBA_DF_SSM.rds")


GPS_BBA_SSM <- grab(fits_mp, what = "fitted", as_sf = FALSE, normalise = FALSE, group = FALSE)

GPS_SSM_Norm <- grab(fits_mp, what = "fitted", as_sf = FALSE, normalise = TRUE, group = FALSE)

GPS_SSM_Norm <- GPS_SSM_Norm %>%
  dplyr::select(id, date, g) %>%
  rename(g_norm = g)


BBA_GPS_SSM_Clean <- GPS_BBA_DF_SSM %>%
  left_join(GPS_BBA_SSM) %>%
  left_join(GPS_SSM_Norm)

BBA_GPS_SSM_Clean$ID <- BBA_GPS_SSM_Clean$id
BBA_GPS_SSM_Clean$trip <- BBA_GPS_SSM_Clean$id
BBA_GPS_SSM_Clean$col <- "FI"
BBA_GPS_SSM_Clean$time <- BBA_GPS_SSM_Clean$date
BBA_GPS_SSM_Clean$tag <- "GPS"
BBA_GPS_SSM_Clean$stage <- "MISC"

setwd("//saeri-file/users$/jriaz/Documents/07. Data/BBA.")

write_rds(BBA_GPS_SSM_Clean, "BBA_GPS_SSM_Clean.rds")



##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
