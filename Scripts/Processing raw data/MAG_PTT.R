############################################################################################################################################################################################################################################################################################
# R script created for:
## Riaz et al 2024 
### Connectivity of marine predators over the Patagonian Shelf during the highly pathogenic avian influenza (HPAI) outbreak
#### Script for processing raw MAG tracking data using animotum

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
library(wildlifeDI) #overlap 
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
setwd("//saeri-file/users$/jriaz/Documents/07. Data/MAG")

data_path <- ("//saeri-file/users$/jriaz/Documents/07. Data/MAG/")

MAG_PTT <- read_rds("MAG_PTT.rds")


unique(MAG_PTT$UniqueID)



Basemap1 + geom_spatial_point(data = MAG_PTT, aes(x = lon, y = lat, colour = BreedStage)) +
  # geom_polygon(data = Falklands_df, aes(x = x, y = y, group = group), fill = "grey80", colour = "black", linewidth = 0.5) +
  scale_colour_viridis_d() + theme(legend.position = "right")

Distinct_Mag <- MAG_PTT %>%
  dplyr::select(UniqueID, Age) %>%
  distinct()
names(Distinct_Mag) <- c("id","Age") ## Rename cols


MAG_PTT <- MAG_PTT %>%
  dplyr::select(UniqueID, time, class, lon, lat)

names(MAG_PTT) <- c("id","date","lc","lon","lat") ## Rename cols

unique(MAG_PTT$id)

MAG_PTT$date <-as.POSIXct(MAG_PTT$date, format = "%d/%m/%Y %H:%M:%S", tz="GMT")#format times



##############################################################################################################################################

newd <- MAG_PTT %>%
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


##############################################################################################################################################
## Remove extreme travel rate locations to be ignored at ssm filter stage

vmax <- 10

d5new <- d1new %>%
  # filter(lon > -100) %>%
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
# Run tracks through SSM. 
# Run the Predicted first and then run for the Fitted

d3 <- data.frame(NewTracks) 

split_d <- split(d3, d3$id)[unique(d3$id)] ## creates ID list

fits <- aniMotum::fit_ssm(d3, model="rw", 
                          vmax = 10,
                          spdf = FALSE,
                          # control = ssm_control(optim=c("nlminb")), 
                          time.step = 1)


my.aes <- aes_lst(obs=TRUE, line=TRUE,mp=FALSE, conf = FALSE)

setwd("//saeri-file/users$/jriaz/Documents/07. Data/MAG/By Date/")

for(i in unique(fits$id)){
  subID <- subset(fits, id == i)
  gi <- aniMotum::map(subID, what = "predicted", aes = my.aes, by.date = TRUE)
  ggsave(filename = sprintf('%s.pdf', i), plot = gi, width = 9, height = 9)
}

fits_mp <- fit_mpm(fits, what = "predicted", model = "mpm")

PTT_MAG_DF_SSM <- grab(fits, what = "predicted", as_sf = FALSE, normalise = FALSE, group = FALSE)

PTT_SSM <- grab(fits_mp, what = "fitted", as_sf = FALSE, normalise = FALSE, group = FALSE)

PTT_SSM_Norm <- grab(fits_mp, what = "fitted", as_sf = FALSE, normalise = TRUE, group = FALSE)

PTT_SSM_Norm <- PTT_SSM_Norm %>%
  dplyr::select(id, date, g) %>%
  rename(g_norm = g)


MAG_PTT_SSM_Clean <- PTT_MAG_DF_SSM %>%
  left_join(PTT_SSM) %>%
  left_join(PTT_SSM_Norm)

MAG_PTT_SSM_Clean$ID <- MAG_PTT_SSM_Clean$id
MAG_PTT_SSM_Clean$trip <- MAG_PTT_SSM_Clean$id
MAG_PTT_SSM_Clean$col <- "MISC"
MAG_PTT_SSM_Clean$time <- MAG_PTT_SSM_Clean$date
MAG_PTT_SSM_Clean$tag <- "PTT"
MAG_PTT_SSM_Clean$stage <- "MISC"

setwd("//saeri-file/users$/jriaz/Documents/07. Data/MAG")
write_rds(MAG_PTT_SSM_Clean, "MAG_PTT_SSM_Clean.rds")




