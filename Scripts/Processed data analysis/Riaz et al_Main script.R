############################################################################################################################################################################################################################################################################################
# R script created for:
## Riaz et al 2024  
### Coastal connectivity of marine predators over the Patagonian Shelf during the highly pathogenic avian influenza (HPAI) outbreak
############################################################################################################################################################################################################################################################################################


## First read in a range of relevant packages

library(tidyverse)
library(aniMotum)
library(hms)
library(viridis)
library(scales)
library(readr)
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
library(ggpubr)
library(sp)
require(sf)
library(rasterVis)
library(RColorBrewer)
library(gganimate) 
require(transformr) 
library(rasterVis)
library(gganimate)
library(gifski)
library(devtools)
library(cividis)
library(wesanderson)
library(geosphere)
library(mapview)
library(ggOceanMaps)
require(devtools)
library(rgeos)

##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
## Read in shapefile and create basemap for plotting

setwd("P:/Projects/DPLUS168_pinniped bycatch/GIS shapefiles/")

proj <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")


coast_wgs<-readOGR("southamerica_adm0.shp")
proj4string(coast_wgs)
coast_wgs
plot(coast_wgs)
Falklands <- coast_wgs#
Falklands <- crop(Falklands, extent(c(-72, -48.8, -57, -33.8))) 

Falklands_df <- ggplot2::fortify(Falklands)

names(Falklands_df)[names(Falklands_df)=="long"]<-"x"
names(Falklands_df)[names(Falklands_df)=="lat"]<-"y"


Falklandsmap <- ggplot() +
  geom_polygon(data = Falklands_df, 
               aes(x = x, y = y, group = group), fill = "grey80", colour = "black") + theme_bw()
Falklandsmap


Basemap1 <- basemap(limits = c(-72, -48.8, -57, -33.8), bathymetry = TRUE, rotate = TRUE, grid.col = NA, land.col = "transparent", land.border.col = "transparent" ) + 
  theme(legend.position = "bottom")
  # annotation_scale(location = "br")  
# annotation_north_arrow(location = "tr", which_north = "true") 
Basemap1


##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
#### Read in processed data

setwd("P:/Projects/DPLUS168_pinniped bycatch/Movement data penguins etc/")

##############################################################################################################################################
## Fur seals
##############################################################################################################################################

MalesSAFS2023 <- read_rds("Males23_SSM_Clean.rds")
MalesSAFS2023$Group <- "Male"
unique(MalesSAFS2023$id) ## 19

FemalesSAFS1819 <- read_rds("Seal_GPS_SSM_Clean.rds")
FemalesSAFS1819$Group <- "Female"
unique(FemalesSAFS1819$id) ## 18


PTTSAFSBIWC <- read_rds("Seal_PTT_BIWC_SSM_Clean.rds")
PTTSAFSBIWC$Group <- "Female"
unique(PTTSAFSBIWC$id) ## 17


Pups_SAFS <- read_rds("Seal_Pup_PTT_SSM_Clean.rds")
Pups_SAFS$Group <- "Pup"
unique(Pups_SAFS$id) ## 20

SAFS_Locs <- MalesSAFS2023 %>%
  full_join(FemalesSAFS1819) %>%
  full_join(PTTSAFSBIWC) %>%
  full_join(Pups_SAFS)

SAFS_Locs_ID <- SAFS_Locs %>%
  dplyr::select(id, Group) %>%
  distinct()

SAFS_Locs$SPP <- "SAFS"


##############################################################################################################################################
## Albatrosses
##############################################################################################################################################

GPS_BBA <- read_rds("BBA_GPS_SSM_Clean.rds")
unique(GPS_BBA$id)

BBA_Locs_ID <- GPS_BBA %>%
  dplyr::select(id) %>%
  distinct()

GPS_BBA$SPP <- "BBA"


##############################################################################################################################################
## Magellanic
##############################################################################################################################################

PTT_MAG <- read_rds("MAG_PTT_SSM_Clean.rds")
unique(PTT_MAG$id)

FledgeID <- PTT_MAG %>%     #### For checking things for fledglings
  filter(id == "MAG-CD-09"|
         id == "MAG-CD-10"|
         id == "MAG-CD-11"|
         id == "MAG-CD-12"|
         id == "MAG-CD-13"|
         id == "MAG-CD-14")
nrow(PTT_MAG) - nrow(FledgeID)

MAG_Locs_ID <- PTT_MAG %>%
  dplyr::select(id) %>%
  distinct()

PTT_MAG$SPP <- "MAG"


##############################################################################################################################################
## Combine processed data for the 3 species
##############################################################################################################################################


All_Proccessed_Locs <- SAFS_Locs %>%
  full_join(GPS_BBA) %>%
  full_join(PTT_MAG)
  


FledgeID <- All_Proccessed_Locs %>% 
  filter(SPP == "MAG") %>%
  filter(id == "MAG-CD-09"|
           id == "MAG-CD-10"|
           id == "MAG-CD-11"|
           id == "MAG-CD-12"|
           id == "MAG-CD-13"|
           id == "MAG-CD-14") %>%
  mutate(Group = "Fledgling") %>%
  dplyr::select(SPP, id, Group) %>%
  distinct()


MAG_ID <- All_Proccessed_Locs %>% 
  filter(SPP == "MAG") %>%
  filter(!id == "MAG-CD-09",
         !id == "MAG-CD-10",
         !id == "MAG-CD-11",
         ! id == "MAG-CD-12",
         !id == "MAG-CD-13",
         !id == "MAG-CD-14") %>%
  mutate(Group = "Adult") %>%
  dplyr::select(SPP, id, Group) %>%
  distinct() %>%
  full_join(FledgeID) 


BBA_ID <- All_Proccessed_Locs %>% 
  filter(SPP == "BBA") %>%
  mutate(Group = "Adult") %>%
  dplyr::select(SPP, id, Group) %>%
  distinct()

IDS <- MAG_ID %>%
  full_join(SAFS_Locs_ID) %>%
  full_join(BBA_ID)
  



##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
# Calculate distance from South American coast for each animal location

setwd("P:/Projects/DPLUS168_pinniped bycatch/GIS shapefiles/")
library(units)
library(rgeos)
Shape<-read_sf("southamerica_adm0.shp")

head(Shape)

Shape <- Shape %>%
  filter(COUNTRY == "ARGENTINA" |
            COUNTRY == "URUGUAY")

Shape1 <- as(Shape, 'Spatial')
plot(Shape1)

Shape2 <- gUnaryUnion(Shape1)
plot(Shape2)
head(Shape2)

sf_polygons <- st_as_sf(Shape2)

lsf <- All_Proccessed_Locs %>% 
  dplyr::select(lon, lat) %>%
  st_as_sf(coords= c("lon","lat")) %>% 
  st_set_crs(st_crs(4326))  

head(lsf)
st_crs(lsf)  


head(sf_polygons)
st_crs(sf_polygons)  
plot(sf_polygons)

# Calculate distances using st_distance
ts <- Sys.time()
distances <- st_distance(lsf, sf_polygons, by_element = FALSE)
difftime(Sys.time(),ts)

distances_km <- set_units(distances, km)
distances_km <- set_units(distances_km, NULL) 
New_locs <-  cbind(All_Proccessed_Locs, distances_km)
New_locs <- as.data.frame(New_locs)


##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
# Now calculate distance from the Falklands coastline for each animal location

setwd("P:/Projects/DPLUS168_pinniped bycatch/GIS shapefiles/")
library(units)
library(rgeos)
UK_Shape<-read_sf("southamerica_adm0.shp")
st_crs(UK_Shape)  

UK_Shape$COUNTRY
UK_Shape <- UK_Shape %>%
  filter(COUNTRY == "U.K.")

UK_Shape1 <- as(UK_Shape, 'Spatial')
plot(UK_Shape1)

UK_sf_polygons <- st_as_sf(UK_Shape1)

lsf <- New_locs %>% ## Change this to New_locs
  dplyr::select(lon, lat) %>%
  st_as_sf(coords= c("lon","lat")) %>% # make spatial
  st_set_crs(st_crs(4326)) 

head(lsf)
st_crs(lsf)  

head(UK_sf_polygons)
st_crs(UK_sf_polygons)  
plot(UK_sf_polygons)

# Calculate distances using st_distance
ts <- Sys.time()
distances <- st_distance(lsf, UK_sf_polygons, by_element = FALSE)
difftime(Sys.time(),ts)


distances_km <- set_units(distances, km)
distances_km <- set_units(distances_km, NULL) 
distances_km
New_locs <-  cbind(New_locs, distances_km)
New_locs <- as.data.frame(New_locs)


New_locs <- New_locs %>%
  rename(distance_FI_km = 22)


## Save the dataframe
setwd("P:/Projects/DPLUS168_pinniped bycatch/Movement data penguins etc/")
write_rds(New_locs, "All_Proccessed_Locs.rds")


##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
## Summary statistics and manuscript results 

setwd("P:/Projects/DPLUS168_pinniped bycatch/Movement data penguins etc/")


##############################################################################################################################################
## ## Read in geographic shapefiles
##############################################################################################################################################

Javeds_FK_Shape<-read_sf("voluntary_no_fishing_zone.shp") ## internal waters shapefile
buffer_distance <- 22224  # in meters (22224 m = 12 nm)
Javeds_FK_Shape <- st_buffer(Javeds_FK_Shape, dist = buffer_distance) ## Adds territorial sea buffer to Falklands baseline shapefile

Javeds_SA_Shape<-read_sf("Javeds_SA_Shape.shp") ## SA martime polygon (i.e. Argentina and Uruguay internal waters + territorial sea relevant to this work) 
Javeds_SACont_Shape<-read_sf("Javeds_SACont_Shape.shp") # SA continent shapefile



##############################################################################################################################################
## ## Read in processed data with distance calculations from the abov
##############################################################################################################################################

FilterSpecies <- read_rds("All_Proccessed_Locs.rds") ## Read in processed data with distance calculations from the above


##############################################################################################################################################
## Look at the date ranges of tracked animals throughout the year and input into Table S2
##############################################################################################################################################

TableS2 <- FilterSpecies %>%
  group_by(id, SPP) %>%
  summarise(minD = min(as.Date(date)),
            maxD = max(as.Date(date)),
            days = maxD - minD) %>%
  mutate(StartMonth = month(minD),
         EndMonth = month(maxD),
         count = 1) %>%
  mutate(DateRange = paste(StartMonth, EndMonth)) %>%
  arrange(SPP) %>%
  ungroup() %>%
  group_by(SPP, DateRange) %>%
  summarise(countsum = sum(count)) %>%
  arrange(SPP)
  


##############################################################################################################################################
## ## Read in compiled list of SAFS, MAG and BBA cols along the SA Atlantic coast
##############################################################################################################################################

SA_cols <- read_csv("SA_cols.csv")
SA_cols$lon <- as.numeric(SA_cols$lon)
SA_cols$SPP <- SA_cols$Species
SA_cols$Ordered = factor(SA_cols$SPP, levels=c('BBA','SAFS','MAG'))

SA_cols1 <- SA_cols %>%
   distinct(Location, .keep_all = TRUE) %>%
  arrange(Ordered, desc(lat), desc(lon)) %>%
  mutate(ColID = row_number())
  

SA_cols1$ColID <- as.factor(SA_cols1$ColID)

SAFS_COLS <- SA_cols1 %>%
  filter(SPP == "SAFS")


Cols <- Basemap1 +    
  ggnewscale::new_scale_fill() +
  guides(fill ="none") +
  ggnewscale::new_scale_color() +
   facet_wrap(~Ordered, nrow = 1) +
  geom_polygon(data = Falklands_df, 
               aes(x = x, y = y, group = group), fill = "grey80", colour = "black", linewidth = 0.2, alpha = 0.8) +
  geom_spatial_point(data = SA_cols1, aes(x = lon, y = lat, fill = Ordered), size = 2 , shape = 17, colour = "darkgreen") +
  geom_text(data = SA_cols1, aes(x = lon - 0, y = lat - 1, label = ColID),
            color = "black", size = 3) +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank()) +
  theme(legend.position = "none") + labs(colour = "Resting") + guides(fill ="none") 
Cols



##############################################################################################################################################
## Merge SA maritime and continental shapefiles
##############################################################################################################################################

streams_merged <- Javeds_SA_Shape %>% 
  summarise()

streams_merged <- streams_merged %>%
  st_set_crs(st_crs(4326))  
streams_merged

SA_Bound_Shape <- st_union(streams_merged, Javeds_SACont_Shape) ## Need to merge the maritime and land shapefile first

Shape <- as(SA_Bound_Shape, 'Spatial')

Boundaries_df <- ggplot2::fortify(Shape)

 
 names(Boundaries_df)[names(Boundaries_df)=="lat"]<-"y"
 names(Boundaries_df)[names(Boundaries_df)=="long"]<-"x"


 
 ##############################################################################################################################################
 ## Find animal locations within SA territorial sea boundary
 ##############################################################################################################################################
 
 
# Create an sf object from the animal tracking data frame
animal_tracking_sf <- st_as_sf(FilterSpecies, coords = c("lon", "lat"), crs = st_crs(streams_merged))


# Check if points are within the polygon
animal_tracking_sf$Connectivity <- st_within(animal_tracking_sf, SA_Bound_Shape)

animal_tracking_sf$Connectivity <- as.character(animal_tracking_sf$Connectivity)

animal_tracking_sf$Connectivity <- as.numeric(animal_tracking_sf$Connectivity)


animal_tracking_sf$Connectivity[is.na(animal_tracking_sf$Connectivity)] <- 0

animal_tracking_sf$Connectivity <- as.factor(animal_tracking_sf$Connectivity)


FilterSpecies <- animal_tracking_sf %>%
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2])

FilterSpecies <- as.data.frame(FilterSpecies)



##############################################################################################################################################
## Add another column finding points within FK territorial sea boundary
##############################################################################################################################################


FK_merged <- Javeds_FK_Shape %>%
  st_transform(st_crs(4326))  
FK_merged

FK_merged <- FK_merged %>% 
  summarise()

FK_Shape <- as(FK_merged, 'Spatial')

FK_Boundaries_df <- ggplot2::fortify(FK_Shape)


names(FK_Boundaries_df)[names(FK_Boundaries_df)=="lat"]<-"y"
names(FK_Boundaries_df)[names(FK_Boundaries_df)=="long"]<-"x"


# # Create an sf object from the animal tracking data frame

animal_tracking_sf <- st_as_sf(FilterSpecies, coords = c("lon", "lat"), crs = st_crs(FK_merged))


# Check if points are within the polygon
animal_tracking_sf$FK_Connectivity <- st_within(animal_tracking_sf, FK_merged)

animal_tracking_sf$FK_Connectivity <- as.character(animal_tracking_sf$FK_Connectivity)

animal_tracking_sf$FK_Connectivity <- as.numeric(animal_tracking_sf$FK_Connectivity)


animal_tracking_sf$FK_Connectivity[is.na(animal_tracking_sf$FK_Connectivity)] <- 0

animal_tracking_sf$FK_Connectivity <- as.factor(animal_tracking_sf$FK_Connectivity)


FilterSpecies <- animal_tracking_sf %>%
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2])

FilterSpecies <- as.data.frame(FilterSpecies)



##############################################################################################################################################
## Compute coastal fixes and residency periods near/on land
##############################################################################################################################################

## Coastal points
FilterSpecies <- FilterSpecies %>%
  group_by(SPP) %>%
  mutate(Connectivity = case_when(Connectivity == "1" ~ 'TRUE',
                                  Connectivity == "0" ~ 'FALSE'))

FilterSpecies <- FilterSpecies %>%
  group_by(SPP) %>%
  mutate(FK_Connectivity = case_when(FK_Connectivity == "1" ~ 'TRUE',
                                     FK_Connectivity == "0" ~ 'FALSE'))


FilterSpecies$Ordered = factor(FilterSpecies$SPP, levels=c('BBA','SAFS','MAG'))

FilterSpecies <- FilterSpecies %>%
  group_by(SPP, id) %>%
  mutate(difftime = last(date) - first(date)) %>%
  mutate(diffmins = as.numeric(difftime, units = 'mins')) %>%
  mutate(diffhours = as.numeric(difftime, units = 'hours')) %>%
  mutate(count = 1) %>%
  mutate(totallocs = sum(count)) %>%
  ungroup()
  

## Residency points near land

FilterSpecies <- FilterSpecies %>%
  group_by(SPP) %>%
  mutate(OnLand = case_when(distances_km <= 1 & g_norm <= 0.5 ~ 'TRUE',
                            TRUE ~ 'FALSE'))


##############################################################################################################################################
## Save the clean processed info 
##############################################################################################################################################

ProcessedData <- FilterSpecies %>%
  dplyr::select(id, date, SPP, lon, lat, g, g_norm, distances_km, Connectivity, OnLand, FK_Connectivity) 

write_rds(ProcessedData, "ProcessedData.rds")

##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################


##############################################################################################################################################
## For Figure 1
##############################################################################################################################################


ConnectOnly <- FilterSpecies %>%
  filter(Connectivity == "TRUE") 

 RestOnly <- FilterSpecies %>%
  filter(OnLand == "TRUE") %>%
   ungroup()
 
 AtSeaOnly <- FilterSpecies %>%
   filter(Connectivity == "FALSE") %>%
   filter(OnLand == "FALSE")
 
 
Fig_1 <- Basemap1 + 
  guides(fill ="bottom") +
    ggnewscale::new_scale_fill() +
  guides(fill ="none") +
  ggnewscale::new_scale_color() +
   geom_spatial_point(data = AtSeaOnly, aes(x = lon, y = lat, colour = OnLand), size = 0.0001, colour = "grey20", shape =".") +
   geom_spatial_point(data = ConnectOnly, aes(x = lon, y = lat, colour = OnLand), size = 0.2, colour = "gold") +
  facet_wrap(~Ordered, nrow = 1) +
  geom_polygon(data = Falklands_df, 
               aes(x = x, y = y, group = group), fill = "grey80", colour = "black", linewidth = 0.2, alpha = 0.8) +
  geom_spatial_point(data = SA_cols1, aes(x = lon, y = lat, fill = Ordered), size = 2 , shape = 17, alpha =0.8, colour = "darkgreen") +
  # geom_text(data = SA_cols1, aes(x = lon - 1.4, y = lat, label = ColID),
  #           color = "black", size = 3) +
  xlab("Longitude") + ylab("Latitude") +
  geom_spatial_point(data = RestOnly, aes(x = lon, y = lat, colour = OnLand), size = 0.3, colour = "red") +
    theme(strip.background = element_blank(),
    strip.text.x = element_blank()) +
  theme(legend.position = "none") + labs(colour = "Resting") + guides(fill ="none") 
Fig_1

tiff("Fig_1.tiff", width = 9, height = 4.6, units = 'in', res = 400)
Fig_1
dev.off()

pdf("Fig_1.pdf", width = 9, height = 4.6)
Fig_1
dev.off()




##############################################################################################################################################
## For Figure S1
##############################################################################################################################################


SAFS_Locs_ID <- FilterSpecies %>%
  filter(SPP == "SAFS") %>%
  dplyr::select(id, Group) %>%
  distinct()

SAFS_Plot <- FilterSpecies %>%
  filter(Ordered == "SAFS") %>%
  filter(Connectivity == "TRUE") %>%
  left_join(SAFS_Locs_ID) %>%
  dplyr::select(id, Group) %>%
  distinct() %>%
  left_join(FilterSpecies)
SAFS_Plot$Seal_ID <- SAFS_Plot$id

SAFSRestOnly <- RestOnly %>%
  filter(Ordered == "SAFS") %>%
  left_join(SAFS_Locs_ID)


SAFSRestOnly$Seal_ID <- SAFSRestOnly$id

SAFSRestOnly$Group <- as.factor(SAFSRestOnly$Group)
SAFSRestOnly$Group1 = factor(SAFSRestOnly$Group, levels=c('Pup','Female','Male'))


SAFS_cols1 <- SA_cols1 %>%
  filter(Ordered == "SAFS")

SAFS_Plot$Group <- as.factor(SAFS_Plot$Group)
SAFS_Plot$Group1 = factor(SAFS_Plot$Group, levels=c('Pup','Female','Male'))



library(scico)
library(wesanderson)

Fig_S2 <- Basemap1 + 
  guides(fill ="bottom") +
  ggnewscale::new_scale_fill() +
  guides(fill ="none") +
  ggnewscale::new_scale_color() +
  geom_polygon(data = Falklands_df, 
               aes(x = x, y = y, group = group), fill = "grey80", colour = "black", linewidth = 0.2, alpha = 0.6) +
  geom_spatial_point(data = SAFS_Plot, aes(x = lon, y = lat, colour = id), size = 0.1, shape = 1, alpha = 0.7) +
  facet_wrap(~Group1) +
  scale_colour_viridis_d(direction = -1) +
  geom_spatial_point(data = SAFS_cols1, aes(x = lon, y = lat, fill = Ordered), size = 2 , shape = 17, colour = "darkgreen") +
  geom_spatial_point(data = SAFSRestOnly, aes(x = lon, y = lat), colour = "red", size = 0.6) +
   theme(strip.background = element_blank(),
         strip.text.x = element_blank()) +
   theme(legend.position = "none") + labs(colour = "Seal ID") + guides(fill ="none") +
  xlab("Longitude") + ylab("Latitude") 
Fig_S2


tiff("Fig_S2.tiff", width = 9, height = 4.5, units = "in", res = 400)
Fig_S2
dev.off()

pdf("Fig_S2.pdf", width = 9, height = 4.5)
Fig_S2
dev.off()



##############################################################################################################################################
## For Table 2
##############################################################################################################################################

BBA_Connect <- FilterSpecies %>%
  filter(SPP == "BBA") %>%
  filter(Connectivity == TRUE) %>%
  filter(lat < -54) %>%
  dplyr::select(id) %>%
  distinct()
88/109

BBA_Connect1 <- FilterSpecies %>%
  filter(SPP == "BBA") %>%
  filter(OnLand == TRUE) %>%
  filter(lat < -54) %>%
  dplyr::select(id) %>%
  distinct()

BBA_Connect2 <- FilterSpecies %>%
  filter(SPP == "BBA") %>%
  filter(OnLand == TRUE) %>%
  filter(lat > -54) %>%
  dplyr::select(id) %>%
  distinct()


SAFS_Connect <- FilterSpecies %>%
  filter(SPP == "SAFS") %>%
  filter(Connectivity == TRUE) %>%
  filter(lat < -54) %>%
  dplyr::select(id) %>%
  distinct()
14/17

SAFS_Connect1 <- FilterSpecies %>%
  filter(SPP == "SAFS") %>%
  filter(OnLand == TRUE) %>%
   filter(lat < -45) %>%
  dplyr::select(id) %>%
  distinct()

SAFS_Connect2 <- FilterSpecies %>%
  filter(SPP == "SAFS") %>%
  filter(OnLand == TRUE) %>%
  filter(lat > -54) %>%
  dplyr::select(id) %>%
  distinct()




TripSummaries <- FilterSpecies %>%
  dplyr::select(id, SPP, diffhours, totallocs) %>%
  distinct()
  

Q1A <- FilterSpecies %>%
  ungroup() %>%
  mutate(count = 1) %>%
  group_by(SPP, id, Connectivity) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  arrange(SPP, Connectivity, freq) %>%
  filter(Connectivity == FALSE) %>%
  dplyr::select(-Connectivity) %>%
  mutate(Connectivity = TRUE) %>%
  ungroup() %>%
  mutate(freq = 1 - freq) %>%
  left_join(TripSummaries) %>%
  drop_na() %>%
  mutate(HoursSpentProx = diffhours*freq)

  
Table2A <- Q1A %>%
  ungroup() %>%
  group_by(SPP) %>%
  filter(freq > 0) %>%
  mutate(freq = freq * 100) %>%
  summarise(min = min(freq),
            max = max(freq),
            mean = mean(freq),
            sd = sd(freq),
            n.ci = n()) %>%
  mutate(se = sd / sqrt(n.ci),
         lower.ci = mean - qt(1 - (0.05 / 2), n.ci - 1) * se,
         upper.ci = mean + qt(1 - (0.05 / 2), n.ci - 1) * se)
  

Table2B <- Q1A %>%
  ungroup() %>%
  group_by(SPP) %>%
  filter(HoursSpentProx > 0) %>%
  summarise(min = min(HoursSpentProx),
            max = max(HoursSpentProx),
            mean = mean(HoursSpentProx),
            sd = sd(HoursSpentProx),
            n.ci = n()) %>%
  mutate(se = sd / sqrt(n.ci),
         lower.ci = mean - qt(1 - (0.05 / 2), n.ci - 1) * se,
         upper.ci = mean + qt(1 - (0.05 / 2), n.ci - 1) * se)


##############################################################################################################################################
##############################################################################################################################################
#########################################################################################################################################

options("scipen"=-100, "digits"=4)
options("scipen"=100, "digits"=4)

Q2A <- RestOnly %>%
  ungroup() %>%
  group_by(SPP, id) %>%
  # mutate(count = 1) %>%
  mutate(RestLocs = sum(count)) %>%
  # summarise(n = ) %>%
  summarise(freq = RestLocs / totallocs) %>%
  arrange(SPP, freq) %>%
  distinct() %>%
  left_join(TripSummaries) %>%
  drop_na() %>%
  mutate(HoursSpentResting = diffhours*freq) %>%
  mutate(freq = freq * 100)



Table2C <- Q2A %>%
  ungroup() %>%
  group_by(SPP) %>%
  summarise(min = min(freq),
            max = max(freq),
            mean = mean(freq),
            sd = sd(freq),
            n.ci = n()) %>%
  mutate(se = sd / sqrt(n.ci),
         lower.ci = mean - qt(1 - (0.05 / 2), n.ci - 1) * se,
         upper.ci = mean + qt(1 - (0.05 / 2), n.ci - 1) * se)

  

Table2D <- Q2A %>%
  ungroup() %>%
  group_by(SPP) %>%
  summarise(min = min(HoursSpentResting),
            max = max(HoursSpentResting),
            mean = mean(HoursSpentResting),
            sd = sd(HoursSpentResting),
            n.ci = n()) %>%
  mutate(se = sd / sqrt(n.ci),
         lower.ci = mean - qt(1 - (0.05 / 2), n.ci - 1) * se,
         upper.ci = mean + qt(1 - (0.05 / 2), n.ci - 1) * se)


##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################



##############################################################################################################################################
## For temporal connectivity 
##############################################################################################################################################


SA_ConnectOnly <- FilterSpecies %>%
  filter(Connectivity == "TRUE") %>%
  mutate(Jurisdiction = "SA")

FK_ConnectOnly <- FilterSpecies %>%
  filter(FK_Connectivity == "TRUE") %>%
  mutate(Jurisdiction = "FK")


TemporalFrame <- SA_ConnectOnly %>%
  full_join(FK_ConnectOnly) %>%
  ungroup() %>%
  group_by(id) %>%
  arrange(id, date)


nrow(TemporalFrame)
nrow(FilterSpecies)
unique(TemporalFrame$id) 



##############################################################################################################################################
## Find time difference between the last point in SA and the first point in FK 
##############################################################################################################################################


Connect_Segements <- TemporalFrame %>%
  arrange(id,date) %>%
  group_by(id) %>%
  mutate(Jurisdiction1 = Jurisdiction) %>%
  filter((Jurisdiction == "SA" & lead(Jurisdiction) == "FK") | (Jurisdiction == "FK" & lag(Jurisdiction) == "SA")) %>%
  ungroup() %>%
  group_by(id) %>%
  mutate(Trips = sum(Jurisdiction == "SA" & lead(Jurisdiction) == "FK", na.rm = TRUE))  %>%
  mutate(Trips1 = ifelse(Jurisdiction == "SA" & lead(Jurisdiction) == "FK", row_number(), NA)) %>%
  fill(Trips1, .direction = "down") %>%
  mutate(Trips1 = ifelse(Trips1 > 1, Trips1 - 1, Trips1)) %>%
  ungroup() %>%
  group_by(id, Trips1) %>%
  mutate(ConnectTime_mins = date - lag(date)) %>%
  mutate(ConnectTime_mins = as.numeric(ConnectTime_mins, units = 'mins')) %>%
  mutate(ConnectTime_hours = ConnectTime_mins/60) %>% 
  mutate(ConnectTime_day = ConnectTime_hours/24) %>% 
  fill(ConnectTime_mins, .direction = "up") %>%
  fill(ConnectTime_hours, .direction = "up") %>%
    fill(ConnectTime_day, .direction = "up") %>%
    ungroup() %>%
  group_by(id) %>%
  mutate(Direction = "Inbound")


unique(Connect_Segements$id) 



##############################################################################################################################################
## Stuff for first part of Figure 2A  
##############################################################################################################################################


SAFS_Connect <- Connect_Segements %>%
  filter(SPP == "SAFS")
Others_Connect <- Connect_Segements %>%
  filter(!SPP == "SAFS")

Basemap2 <- basemap(limits = c(-72, -52, -57, -33.8), bathymetry = FALSE, rotate = TRUE, grid.col = NA, land.col = "transparent", land.border.col = "transparent" ) + 
  theme(legend.position = "bottom")

Fig2_A <- Basemap2 + 
   guides(fill ="none") +
   ggnewscale::new_scale_fill() +
   ggnewscale::new_scale_color() +
  geom_spatial_path(data = Others_Connect, aes(x = lon, y = lat, colour = Ordered, group = id), size = 0.5) +
    geom_spatial_path(data = SAFS_Connect, aes(x = lon, y = lat, colour = Ordered, group = id), size = 0.5) +
  scale_colour_manual(values = c("#440154FF", "orange", "#1F968BFF")) +
  labs(colour = " Species") +
  geom_polygon(data = Falklands_df, 
               aes(x = x, y = y, group = group), fill = "grey80", colour = "black", linewidth = 0.2, alpha = 0.8) +
  guides(fill ="none") +
  theme(legend.position = "none") +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank()) +
   theme(axis.text.x=element_blank(),
         axis.ticks.x=element_blank(),
         axis.title.x=element_blank(),
         axis.title.y=element_blank(),
         axis.text.y=element_blank(),
         axis.ticks.y=element_blank())
Fig2_A


Histo_A <- ggplot() + geom_histogram(data = Connect_Segements, aes(x = ConnectTime_day, fill = Ordered), colour = "black", linewidth = 0.3) +
  scale_fill_manual(values = c("#440154FF", "#1F968BFF", "orange")) +
  # scale_colour_manual(values = c("white", "white", "white"), guide = guide_legend(override.aes = list(alpha = 0) )) +
  
  labs(x = "Transit time to Falkland Islands (Day)", y = NULL, fill = "Species") +
  scale_x_continuous(expand=c(0,0.1), breaks = seq(0, 13.5, by = 1)) +
  scale_y_continuous(expand=c(0,1), breaks = seq(0, 250, by = 10)) +
  theme_classic() + theme_bw(10) + theme(strip.background = element_rect(fill="gray85"))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  panel.border = element_rect(colour = "black")) +
  theme(legend.position = c(0.8, 0.8), # c(0,0) bottom left, c(1,1) top-right.
    legend.background = element_rect()) +
  theme(legend.position = "top") +
  theme(legend.title = element_text(face = "bold")) 
Histo_A


 plot.with.inset <-
   ggdraw() +
   draw_plot(Histo_A) +
   draw_plot(Fig2_A, x = 0.25, y = 0.18, width = 0.7, height = 0.7) 
 
 plot.with.inset
 


##############################################################################################################################################
## Now need to calculate the inverse of the above first. So last point in FK and first point in SA. Then stuff for second half of Fig 2A
##############################################################################################################################################


FK_Connect_Segements <- TemporalFrame %>%
  arrange(id,date) %>%
  group_by(id) %>%
  mutate(Jurisdiction1 = Jurisdiction) %>%
  filter((Jurisdiction == "FK" & lead(Jurisdiction) == "SA") | (Jurisdiction == "SA" & lag(Jurisdiction) == "FK")) %>%
  ungroup() %>%
  group_by(id) %>%
  mutate(Trips = sum(Jurisdiction == "FK" & lead(Jurisdiction) == "SA", na.rm = TRUE))  %>%
  mutate(Trips1 = ifelse(Jurisdiction == "FK" & lead(Jurisdiction) == "SA", row_number(), NA)) %>%
  fill(Trips1, .direction = "down") %>%
  mutate(Trips1 = ifelse(Trips1 > 1, Trips1 - 1, Trips1)) %>%
  ungroup() %>%
  group_by(id, Trips1) %>%
  mutate(ConnectTime_mins = date - lag(date)) %>%
  mutate(ConnectTime_mins = as.numeric(ConnectTime_mins, units = 'mins')) %>%
  mutate(ConnectTime_hours = ConnectTime_mins/60) %>% 
  mutate(ConnectTime_day = ConnectTime_hours/24) %>% 
  fill(ConnectTime_mins, .direction = "up") %>%
  fill(ConnectTime_hours, .direction = "up") %>%
  fill(ConnectTime_day, .direction = "up") %>%
  ungroup() %>%
  group_by(id) %>%
  mutate(Direction = "Outbound")


unique(FK_Connect_Segements$id) 


SAFS_Connect1 <- FK_Connect_Segements %>%
  filter(SPP == "SAFS")
Others_Connect1 <- FK_Connect_Segements %>%
  filter(!SPP == "SAFS")

Fig2_B <- Basemap2 + 
  guides(fill ="none") +
  ggnewscale::new_scale_fill() +
  ggnewscale::new_scale_color() +
  geom_spatial_path(data = Others_Connect1, aes(x = lon, y = lat, colour = Ordered, group = id), size = 0.5) +
  geom_spatial_path(data = SAFS_Connect1, aes(x = lon, y = lat, colour = Ordered, group = id), size = 0.5) +
  scale_colour_manual(values = c("#440154FF", "orange", "#1F968BFF")) +
  labs(colour = " Species") +
  geom_polygon(data = Falklands_df, 
               aes(x = x, y = y, group = group), fill = "grey80", colour = "black", linewidth = 0.2, alpha = 0.8) +
  guides(fill ="none") +
  theme(legend.position = "none") +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank()) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
Fig2_B



Histo_B <- ggplot() + geom_histogram(data = FK_Connect_Segements, aes(x = ConnectTime_day, fill = Ordered), colour = "black", linewidth = 0.3) +
  scale_fill_manual(values = c("#440154FF", "#1F968BFF", "orange")) +
    labs(x = "Transit time to South America (Day)", y = "Number of individuals/trips (#)") +
  scale_x_continuous(expand=c(0,1), breaks = seq(0, 70, by = 10)) +
   scale_y_continuous(expand=c(0,2.5), breaks = seq(0, 250, by = 25)) +
  labs(fill = "Species") +
  theme_classic() + theme_bw(10) + theme(strip.background = element_rect(fill="gray85"))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  panel.border = element_rect(colour = "black"))  +
  theme(legend.position = "top") +
  theme(legend.title = element_text(face = "bold")) +
  scale_linetype(guide = guide_legend(override.aes = list(alpha = 0) ) )
Histo_B


plot.with.inset1 <-
  ggdraw() +
  draw_plot(Histo_B) +
  draw_plot(Fig2_B, x = 0.25, y = 0.18, width = 0.7, height = 0.7) 
plot.with.inset1


##############################################################################################################################################
## Merge panels for Fig2A
##############################################################################################################################################

 Fig2 <- ggarrange(plot.with.inset1, plot.with.inset, nrow = 1, common.legend = TRUE)
 Fig2
 
 pdf("Fig_2.pdf", width = 7, height = 5)
 Fig2
 dev.off()


##############################################################################################################################################
## Some basic temporal summary calculations
##############################################################################################################################################

CheckDirections <- Connect_Segements %>%
  full_join(FK_Connect_Segements)

Transit_Segments <- CheckDirections %>%
  dplyr::select(1,2, lon, lat, g_norm, 18:23, 32,33, 39, 40)

RowTally <- CheckDirections %>%
  full_join(FK_Connect_Segements) %>%
  dplyr::select(id, Direction, SPP) %>%
  distinct()

IncubHPAI <- CheckDirections %>%
  filter(ConnectTime_day <= 4) %>%
  mutate(Threshold = ifelse(ConnectTime_day <= 4   , 0 , ConnectTime_day)) %>%
  mutate(Threshold = ifelse(Threshold > 1   , 1 , Threshold)) %>%
  filter(Threshold < 0.9) %>%
  dplyr::select(id, Direction, SPP) %>%
  distinct()
  
nrow(IncubHPAI)/nrow(RowTally)


##############################################################################################################################################
## To create Fig 2B and Fig S1
##############################################################################################################################################
library(cowplot)
library(gridExtra)
library(MetBrewer)

locs<-ProcessedData
sp<-Transit_Segments
ids<-IDS

head(ids)

#converts locs to number of animals tracked per day ----------------------
head(locs)
locs<-locs%>% 
  dplyr:: select(id,date,SPP)
locs$D<-date(locs$date)
locs_daily<-locs%>%group_by(SPP,id,D)%>%
  summarise(n=n())
head(locs_daily)

locs_daily$mo<-month(locs_daily$D)
locs_daily$day<-day(locs_daily$D)
locs_daily$yr<-2000
locs_daily$yr[locs_daily$mo<9]<-2001
locs_daily$date<-ymd(paste(locs_daily$yr,"-",locs_daily$mo,"-",locs_daily$day))
locs_daily$species<-locs_daily$SPP
head(locs_daily)
locs_daily<-locs_daily%>%
  ungroup()%>%
  dplyr::select(-SPP)

locs_daily<-left_join(locs_daily,ids%>%
                        dplyr::select(-SPP),by="id")
locs_daily$Group[locs_daily$species=="BBA"]<-"Adult"
locs_daily$Group[locs_daily$Group=="Pup"]<-"Fledgling/Pup"
locs_daily$Group[locs_daily$Group=="Fledgling"]<-"Fledgling/Pup"

# converts arrivals into dataframe for plotting: year begins in Sept --------
con<-data.frame(id=sp$id, d_datetime=sp$date, species=sp$SPP, ConnectTime_day=sp$ConnectTime_day, direction=sp$Direction)

con <- con %>%
  filter(row_number() %% 2 != 1) ## Delete odd-rows

con$a_datetime<-con$d_date+(con$ConnectTime_day*60*60*24)
con$a_mo<-month(con$a_datetime)
con$a_day<-day(con$a_datetime)
con$d_mo<-month(con$d_datetime)
con$d_day<-day(con$d_datetime)
con$a_yr<-2000
con$d_yr<-2000
con$a_yr[con$a_mo<9]<-2001
con$d_yr[con$d_mo<9]<-2001
con$a_date00<-ymd(paste(con$a_yr,"-",con$a_mo,"-",con$a_day))
con$d_date00<-ymd(paste(con$d_yr,"-",con$d_mo,"-",con$d_day))
head(con)

con<-con%>%
  dplyr::select(-a_mo,-a_day,-d_mo,-d_day,-ConnectTime_day,-d_datetime,-a_datetime)
head(con)

con_long<-con %>%
  pivot_longer(
    cols = ends_with("_date00"),
    names_to = "date_direction",
    values_to = "date",
    values_drop_na = TRUE)


# species chronology ------------------------------------------------------

dt<-seq(as.Date("2000/9/1"), as.Date("2001/8/31"), by = "days")

a<-data_frame(date=dt,species="BBA")
head(a)
a$chron<-"Non-breeding"
a$chron[a$date<"2000-10-01"]<-"Pre-lay"
a$chron[a$date>="2000-10-01" & a$date<"2000-12-15"]<-"Incubation"
a$chron[a$date>="2000-12-01" & a$date<"2001-04-25" ]<-"Provisioning off-spring"
a$chron[a$date>"2001-04-25"]<-"Non-breeding"

p<-data_frame(date=dt,species="MAG")
head(p)
p$chron<-"Non-breeding"
p$chron[p$date<"2000-10-15"]<-"Pre-lay"
p$chron[p$date>="2000-10-15" & p$date<"2000-12-15"]<-"Incubation"
p$chron[p$date>="2000-12-15" & p$date<"2001-02-15" ]<-"Provisioning off-spring"
p$chron[p$date>="2001-02-15" & p$date<"2001-04-15" ]<-"Moult"
p$chron[p$date>"2001-04-15"]<-"Non-breeding"

s<-data_frame(date=dt,species="SAFS")
head(s)
s$chron<-"Non-breeding"
s$chron[s$date>"2000-12-15" ]<-"Provisioning off-spring"
s$chron[s$date<"2000-10-25" ]<-"Provisioning off-spring"

species_chron_a<-rbind(p,p,p,p,p,p,s,s,s,s,s,s,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a)
species_chron_a$chron<-factor(species_chron_a$chron,levels=c("Pre-lay","Incubation",
                                                             "Provisioning off-spring",
                                                             "Moult","Non-breeding"))


p_chron<-bind_rows(replicate(35, p, simplify = FALSE))
s_chron<-bind_rows(replicate(48, s, simplify = FALSE))
a_chron<-bind_rows(replicate(157, a, simplify = FALSE))
species_chron_t<-rbind(p_chron, s_chron,a_chron)

unique(species_chron_t$chron)
species_chron_t$chron<-factor(species_chron_t$chron,levels=c("Pre-lay","Incubation",
                                                             "Provisioning off-spring",
                                                             "Moult","Non-breeding"))

# plots -------------------------------------------------------------------

con_long$Ordered = factor(con_long$species, levels=c('BBA','SAFS','MAG'))
species_chron_a$Ordered = factor(species_chron_a$species, levels=c('BBA','SAFS','MAG'))


ggplot()+
  #geom_bar(data=locs_daily,aes(x=date))+
  geom_bar(data=species_chron_a,aes(x=date, group=chron, fill=chron), alpha=.2)+
  scale_x_date(date_labels = "%b",date_breaks="1 month")+
  facet_wrap(~species, nrow=3, scales="free_y")

SAarrive<-ggplot()+
  #geom_bar(data=locs_daily,aes(x=date))+
  geom_bar(data=species_chron_a,aes(x=date, group=chron, fill=chron), alpha=.4,width=1)+
  # ggnewscale::new_scale_colour() +
  geom_bar(data=con_long%>%filter(date_direction!="d_date00")%>%filter(direction=="Outbound"),
           aes(x=date, colour = "ID"),width=1, colour = "black")+
  scale_fill_manual(values=met.brewer("Java", 5))+
  scale_x_date(date_labels = "%b",date_breaks="1 month")+
  labs(y="Number of individuals/trips (#)",x="Arrival in South America (Day)", fill = "Seasonal life history")+
  theme_bw(10) + theme(strip.background = element_rect(fill="gray85"))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  panel.border = element_rect(colour = "black")) +
  theme(axis.text.x=element_text(angle=45,hjust=1),
        # legend.title = element_blank(),
        legend.position = "top")+
  facet_wrap(~Ordered, nrow=3, scales="free_y")+
  theme(legend.title = element_text(face = "bold")) +
  scale_y_continuous(breaks = seq(0, 20, 2), 
                     expand = c(0, 0))
SAarrive

FKarrive<-ggplot()+
  #geom_bar(data=locs_daily,aes(x=date))+
  geom_bar(data=species_chron_a,aes(x=date, group=chron, fill=chron), alpha=.4,width=1)+
  geom_bar(data=con_long%>%filter(date_direction!="d_date00")%>%filter(direction=="Inbound"),
           aes(x=date),width=1,show.legend=FALSE, colour = "black")+
  scale_fill_manual(values=met.brewer("Java", 5))+
  scale_x_date(date_labels = "%b",date_breaks="1 month")+
  labs(y=NULL, x="Arrival in Falkland Islands (Day)")+
  theme_bw(10) + theme(strip.background = element_rect(fill="gray85"))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  panel.border = element_rect(colour = "black")) +
  theme(axis.text.x=element_text(angle=45,hjust=1),
        legend.title = element_blank())+
  theme(legend.position = "top") +
  facet_wrap(~Ordered, nrow=3, scales="free_y")+
  scale_y_continuous(breaks = seq(0, 20, 2), expand = c(0, 0)) 
FKarrive


##############################################################################################################################################
## Merge panels for Fig2B
##############################################################################################################################################

plots1 <- ggarrange(SAarrive,FKarrive, common.legend = TRUE, legend = "top")
plots1


##############################################################################################################################################
## Merge plots from Fig2A and Fig2B
##############################################################################################################################################


Fig2_MS <- ggarrange(Fig2, plots1, nrow = 2, common.legend = TRUE,legend = "top", align = "hv", labels = "AUTO")
Fig2_MS


pdf("Fig_2.pdf", width = 7, height = 8.5)
Fig2_MS
dev.off()


##############################################################################################################################################
## Create Fig.S1
##############################################################################################################################################

FigS1A <- ggplot()+
  geom_bar(data=species_chron_t,aes(x=date, group=chron, fill=chron), alpha=.4,width=1,show.legend=FALSE)+
  scale_fill_manual(values=met.brewer("Java", 5))+
  ggnewscale::new_scale_fill() +
  
  geom_bar(data=locs_daily,aes(x=date, fill=Group), alpha = 0.9, width=1,show.legend=NA, position = "stack")+
  scale_fill_viridis_d(option = "D") +
  scale_x_date(date_labels = "%b",date_breaks="1 month")+
  labs(y="Number of individuals/trips (#)", fill = "Demographic")+
  theme_bw(10) + 
   theme(strip.background = element_rect(fill="gray85"))+ 
   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  panel.border = element_rect(colour = "black")) +
  theme(axis.title.x = element_blank(),
        axis.text.x=element_text(angle=45,hjust=1))+ 
  facet_wrap(~species, nrow=3, scales="free_y") 
FigS1A


tiff("Fig_S1.tiff", width = 6, height = 9, units = 'in', res = 300)
FigS1A
dev.off()

pdf("Fig_S1.pdf", width = 7, height = 5)
FigS1A
dev.off()


##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
#### Additional supplementary figure showing tracking data for the 6 species filtered out of the analysis

setwd("P:/Projects/DPLUS168_pinniped bycatch/Movement data penguins etc/")

AllSup <- read_rds("AllSup.rds")

Fig_New <- Basemap1 + 
  # ggnewscale::new_scale_fill() +
  guides(fill ="none") +
  ggnewscale::new_scale_color() +
  geom_spatial_point(data = AllSup, aes(x = lon, y = lat, group = Group, colour = SPP), size = 0.05, alpha = 0.5) +
  facet_wrap(~Group, nrow = 2) +
  geom_polygon(data = Falklands_df, 
               aes(x = x, y = y, group = group), fill = "grey80", colour = "black", linewidth = 0.2, alpha = 0.8) +
  xlab("Longitude") + ylab("Latitude") + labs(colour = "Species") +
  theme(legend.position = "right") +
  guides(colour = guide_legend(override.aes = list(size = 10, fill=NA))) + 
  # guides(color=guide_legend(override.aes=list(fill=NA)))
  theme(legend.position = c(1, 0), legend.justification = c(1, 0)) +
  scale_colour_manual(labels=c('Gentoo penguin', 'King penguin', "Rockhopper penguin", "Southern elephant seal", "South American sea lion", "Shooty Shearwater"), 
                      values = c("red3", "orange", "darkgreen", "purple", "steelblue4","pink3"))

Fig_New


pdf("Fig_NewSup.pdf", width = 6, height = 8)
Fig_New
dev.off()



# END OF SCRIPT
# END OF SCRIPT
# END OF SCRIPT
# END OF SCRIPT
# END OF SCRIPT
# END OF SCRIPT
# END OF SCRIPT
# END OF SCRIPT
# END OF SCRIPT
# END OF SCRIPT


##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################

