############################################################################################################################################################################################################################################################################################
# R script created for:
## Riaz et al 2024
### Connectivity of marine predators over the Patagonian Shelf during the highly pathogenic avian influenza (HPAI) outbreak
#### Script for processing raw SAFS PTT tracking data (for pups) using animotum

############################################################################################################################################################################################################################################################################################



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
#


Basemap1 <- basemap(limits = c(-72, -30, -57, -34), bathymetry = TRUE, rotate = TRUE, grid.col = NA,) + 
  theme(legend.position = "none") +
  annotation_scale(location = "br")  
# annotation_north_arrow(location = "tr", which_north = "true") 
Basemap1


##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################


SAFS_PTT_Pups <- read_rds("SAFS_PTT_Pups.rds")



##############################################################################################################################################
## Arrange DF by date and change any Z class locations to B class? Kept as K class for now

newd <- SAFS_PTT_Pups %>%
  group_by(id) %>%
  arrange(id, date) %>%
  filter(!lc == "Z")

unique(newd$id)


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
  # dplyr :: select(id, date, lc, lon, lat) %>%
  ungroup()
unique(NewTracks$id)


IDPlot1 <- Basemap1 + 
  geom_spatial_point(data = NewTracks, aes(x = lon, y = lat, colour = id)) +
  scale_colour_viridis_d() + theme(legend.position = "right")
IDPlot1

##############################################################################################################################################
##############################################################################################################################################
# Run tracks through SSM.
# Run the Predicted first and then run for the Fitted

d3 <- data.frame(NewTracks) 

split_d <- split(d3, d3$id)[unique(d3$id)] ## creates ID list

fits <- aniMotum::fit_ssm(d3, model="rw", 
                          vmax = 4, 
                          spdf = FALSE,
                          # control = ssm_control(optim=c("nlminb")), 
                          time.step = 1)


my.aes <- aes_lst(obs=TRUE, line=TRUE,mp=FALSE, conf = FALSE)

setwd("//saeri-file/users$/jriaz/Documents/07. Data/SAFS_Pups/By Date/")

for(i in unique(fits$id)){
  subID <- subset(fits, id == i)
  gi <- aniMotum::map(subID, what = "predicted", aes = my.aes, by.date = TRUE)
  ggsave(filename = sprintf('%s.png', i), plot = gi, width = 9, height = 9)
}


fits_mp <- fit_mpm(fits, what = "predicted", model = "mpm")

Pups_DF_SSM <- grab(fits, what = "predicted", as_sf = FALSE, normalise = FALSE, group = FALSE)

Pups_SSM <- grab(fits_mp, what = "fitted", as_sf = FALSE, normalise = FALSE, group = FALSE)

Pups_SSM_Norm <- grab(fits_mp, what = "fitted", as_sf = FALSE, normalise = TRUE, group = FALSE)

Pups_SSM_Norm <- Pups_SSM_Norm %>%
  dplyr::select(id, date, g) %>%
  rename(g_norm = g)


Pups_SSM_Clean <- Pups_DF_SSM %>%
  left_join(Pups_SSM) %>%
  left_join(Pups_SSM_Norm)

Pups_SSM_Clean$ID <- Pups_SSM_Clean$id
Pups_SSM_Clean$trip <- Pups_SSM$id
Pups_SSM_Clean$col <- "BIRD"
Pups_SSM_Clean$time <- Pups_SSM_Clean$date
Pups_SSM_Clean$tag <- "PTT"
Pups_SSM_Clean$stage <- "PUPS"

setwd("//saeri-file/users$/jriaz/Documents/07. Data/SAFS_Pups")
write_rds(Pups_SSM_Clean, "Seal_Pup_PTT_SSM_Clean.rds")









