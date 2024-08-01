## Gorongosa Wild Dog AKDE Analysis 
## Meredith S Palmer
## 24-June-2024

## set R environment
rm(list=ls())
setwd("/Users/meredithspalmer/Desktop/Work/Post doc_Princeton/Mozambique/Data_Bouley_Wild Dogs")

## libraries
library(tidyverse)
library(sf)
library(mapview)
library(ctmm)
library(lubridate)
library(dplyr)
library(move2)

################
## DATA PREP ## 
###############

## load data 
# - NOTE: notes on data prep and cleaning can be found in "1. Bouley et al. 2021_GNP wild dog collar clean.R" from FigShare
wd <- read.csv("Bouley et al. 2021 Fig Share/1. GNP wild dog collars_all data clean.csv") %>% 
  mutate(across(c("GroupName", "DogName", "Month", "Year", "MonthYear", "YearSinceRelease"), as.factor)) %>% 
  mutate(LocalDateTime = as.POSIXct(LocalDateTime, format="%Y/%m/%d %H:%M", tz="Africa/Maputo"))
  
## examine data
# - records per group --> check if to combine records per group so that no pseudoreplication 
table(wd$GroupName, wd$DogName) 

# - Cheza pack: no overlap (Paula already cleaned...)
cheza <- wd[wd$GroupName == "Cheza",] %>% mutate(DogName = droplevels(DogName))
min(cheza[cheza$DogName == "Bebedo",]$LocalDateTime); max(cheza[cheza$DogName == "Bebedo",]$LocalDateTime)
min(cheza[cheza$DogName == "Nhagutua",]$LocalDateTime); max(cheza[cheza$DogName == "Nhagutua",]$LocalDateTime)

# - Gorongosa pack: no overlap (Paula already cleaned...)
gnp <- wd[wd$GroupName == "Gorongosa",] %>% mutate(DogName = droplevels(DogName))
min(gnp[gnp$DogName == "Beira",]$LocalDateTime); max(gnp[gnp$DogName == "Beira",]$LocalDateTime)
min(gnp[gnp$DogName == "Metuchira",]$LocalDateTime); max(gnp[gnp$DogName == "Metuchira",]$LocalDateTime)
min(gnp[gnp$DogName == "Nhamagaia",]$LocalDateTime); max(gnp[gnp$DogName == "Nhamagaia",]$LocalDateTime)

## examine data 
# - date range 
range(wd$LocalDateTime) #2018-2020 
wd %>% group_by(GroupName) %>% mutate(FirstDate=first(LocalDateTime),LastDate=last(LocalDateTime))
#   - NEED TO assess how much territories change on an annual basis (years 1 and years 2 since release)
#   - NEED TO ensure that when matching to CT records, time frames match (i.e., when aggregate)

## examine data
# - how may points per day
test <- wd %>% 
  mutate(Date1 = date(LocalDateTime)) %>% #issues with how this is assigned
  group_by(DogName, Date1) %>% 
  mutate(n=n())
table(test$DogName, test$n)
# -> variable; may depend on collar, when deployed, how Paula thinned data; *can account for in stats 

## examine data
# - duplicated datetimes
table(duplicated(wd$LocalDateTime), wd$DogName) #duplicates
# - explore a few dogs 
wd[wd$DogName == "Nhagutua" & duplicated(wd$LocalDateTime),] #weird, not a duplicate - date is different 
Nhagutua <- wd[wd$DogName == "Nhagutua",]
table(duplicated(Nhagutua$LocalDateTime)) #OKAY so duplicate is with another pack, not within the same dog
# - reassess
wd$DogDate <- paste(wd$DogName, wd$LocalDateTime)
table(duplicated(wd$DogDate)) #still 10 duplicates left 
wd[duplicated(wd$DogDate),]
# - fix: Nhamagaia
wd[wd$DogDate == "Nhamagaia 2020-07-31 10:03:00",] #hm, different positions and different temps 
View(wd[wd$DogName == "Nhamagaia",]) #strange, three readings at almost the same time (10:00, 10:03, 10:03) but in different locations, no consistent pattern in previous or subsequent days for when fixes should be; delete the two 10:03 rows and keep the 10:00 row
wd <- wd[!wd$DogDate == "Nhamagaia 2020-07-31 10:03:00",]
# - fix: Bebedo
wd[wd$DogDate == "Bebedo 2020-07-03 09:04:00",] #hm, different positions and different temps 
View(wd[wd$DogName == "Bebedo",]) #strange, three readings at almost the same time (9:00, 9:04, 9:04) but in different locations, no consistent pattern in previous or subsequent days for when fixes should be; delete the two 9:04 rows and keep the 9:00 row
wd <- wd[!wd$DogDate == "Bebedo 2020-07-03 09:04:00",]
# - fix: Mutiabamba
wd[wd$DogDate == "Mutiabamba 2019-12-12 18:31:00",] #hm, different positions and different temps 
View(wd[wd$DogName == "Mutiabamba",]) #unclear why duplicated timestamps; sequential, so remove second record
wd <- wd[!(wd$DogDate == "Mutiabamba 2019-12-12 18:31:00" & wd$Longitude == 34.37040),]
# - fix: Sapirandzi #1 
wd[wd$DogDate == "Sapirandzi 2019-12-12 19:00:00",] #hm, different positions and different temps 
View(wd[wd$DogName == "Sapirandzi",]) #three sequential readings all at 19:00; remove the second two
wd <- wd[!(wd$DogDate == "Sapirandzi 2019-12-12 19:00:00" & wd$Temperature == 34),]
# - fix: Sapirandzi #2 
wd[wd$DogDate == "Sapirandzi 2020-04-09 07:06:00",] #hm, different positions and different temps 
View(wd[wd$DogName == "Sapirandzi",]) #two sequential readings at 7:06; no reading at 7:00; remove the second
wd <- wd[!(wd$DogDate == "Sapirandzi 2020-04-09 07:06:00" & wd$Temperature == 31),]
# - fix: Nhambita #1
wd[wd$DogDate == "Nhambita 2019-12-12 19:00:00",] #hm, different positions and different temps 
View(wd[wd$DogName == "Nhambita",]) #two sequential readings at 19:00; remove the second
wd <- wd[!(wd$DogDate == "Nhambita 2019-12-12 19:00:00" & wd$Temperature == 34),]
# - fix: Nhambita #2
wd[wd$DogDate == "Nhambita 2019-12-28 17:03:00",] #hm, different positions and different temps 
View(wd[wd$DogName == "Nhambita",]) #three readings at almost the same time (17:00, 17:03, 17:03); delete the two 17:03 rows and keep the 17:00 row
wd <- wd[!wd$DogDate == "Nhambita 2019-12-28 17:03:00",]
# - fix: Nhambita #3
wd[wd$DogDate == "Nhambita 2020-07-03 13:02:00",]  #hm, different positions and different temps 
View(wd[wd$DogName == "Nhambita",]) #three readings at almost the same time (13:00, 13:02, 13:02); delete the two 13:02 rows and keep the 13:00 row
wd <- wd[!wd$DogDate == "Nhambita 2020-07-03 13:02:00",]
# - fix: Nhambita #4
wd[wd$DogDate == "Nhambita 2020-07-20 11:00:00",] #hm, different positions and different temps 
View(wd[wd$DogName == "Nhambita",]) #two sequential readings at 11:00; remove the second
wd <- wd[!(wd$DogDate == "Nhambita 2020-07-20 11:00:00" & wd$Latitude == -18.77852),]
# - check
wd[duplicated(wd$DogDate),] #no remaining duplicates 

## which dogs = packs in which years? 
table(wd$GroupName, wd$DogName, wd$YearSinceRelease)
# - Gorongosa: year 1 = Beira + Metuchira; year 2 = Metuchira
# - Cheza: no year 1; year 2 = Bebedo + Nhagutua 
# - Mucodza: year 1 = Ndhapiona; year 2 = Ndhapiona (split across years)
# - Mopane: no year 1, year 2 = Sapirandzi
# - Pwadzi: no year 1; year 2 = Mutiabamba 
# - Xivulo: no year 1; year 2 = Nhambita

# --> NEED TO CREATE ID variable that is data for each pack (combined) per year 
wd$PackYear <- NA
wd[wd$GroupName == "Pwadzi",]$PackYear <- "Pwadzi Year 2"
wd[wd$GroupName == "Xivulo",]$PackYear <- "Xivulo Year 2"
wd[wd$GroupName == "Mopane",]$PackYear <- "Mopane Year 2"
wd[wd$GroupName == "Cheza",]$PackYear <- "Cheza Year 2"
wd[(wd$DogName == "Ndhapiona" & wd$YearSinceRelease == "Year 1"),]$PackYear <- "Mucodza Year 1"
wd[(wd$DogName == "Ndhapiona" & wd$YearSinceRelease == "Year 2"),]$PackYear <- "Mucodza Year 2"
wd[(wd$GroupName == "Gorongosa" & wd$YearSinceRelease == "Year 1"),]$PackYear <- "Gorongosa Year 1"
wd[(wd$GroupName == "Gorongosa" & wd$YearSinceRelease == "Year 2"),]$PackYear <- "Gorongosa Year 2"
sum(is.na(wd$PackYear))

## organize dates
wd <- wd %>% 
  group_by(PackYear) %>% 
  arrange((LocalDateTime))

## formatting 
# - update data (to match Movebank column names) 
wd <- wd %>% rename(individual.local.identifier = PackYear, 
                    timestamp = LocalDateTime, 
                    location.long = Longitude, 
                    location.lat = Latitude)

write.csv(wd, "wd_cleaned.csv", row.names=F)

# - create move2 object 
wd_move <- mt_as_move2(wd,  
                  coords = c("location.long","location.lat"), 
                  crs = "EPSG:4326", 
                  time_column = "timestamp",
                  track_id_column = "individual.local.identifier", #unique separate tracks 
                  na.fail = F) # allows or not empty coordinates

# - order the data
mt_is_track_id_cleaved(wd_move) #FALSE; so group
wd_move <- dplyr::arrange(wd_move, mt_track_id(wd_move)) 

## check the data 
# - check duplicated timestamps again
mt_has_unique_location_time_records(wd_move) #TRUE (all unique)

# - check empty locations 
mt_has_no_empty_points(wd_move) #TRUE (no empty points)

# - how many locations per track
table(mt_track_id(wd_move))

# - check if boma dates removed (12 April - 14 June 2018 for Gorongosa pack, i.e., Beira)
min(wd_move$timestamp) #all good 

# - look at how "year 1" and "year 2" fall out
range(wd_move[wd_move$YearSinceRelease == "Year 1",]$timestamp) #mid June 2018 - end July 2019 
difftime(max(wd_move[wd_move$YearSinceRelease == "Year 1",]$timestamp), min(wd_move[wd_move$YearSinceRelease == "Year 1",]$timestamp)) #412 days; ~13.75 months 

range(wd_move[wd_move$YearSinceRelease == "Year 2",]$timestamp) #start Aug 2019 - end Sep 2020 
difftime(max(wd_move[wd_move$YearSinceRelease == "Year 2",]$timestamp), min(wd_move[wd_move$YearSinceRelease == "Year 2",]$timestamp)) #425 days; ~14 months 

## save manipulated data
saveRDS(wd_move, file="wilddog_movement_cleaned.rds") 


######################
## VISUALIZING DATA ##
######################

## set R environment
rm(list=ls())
setwd("/Users/meredithspalmer/Desktop/Work/Post doc_Princeton/Mozambique/Data_Bouley_Wild Dogs")

library(ggmap)
library(patchwork) 
library(ggspatial)
library(gganimate)
library(moveVis)
library(move)

## load data 
wd_move <- readRDS("wilddog_movement_cleaned.rds")

## plot data 
(bb <- st_bbox(wd_move))
exp <- 2

year1 <-  ggplot() +
  # ggspatial::annotation_map_tile(zoom = 12) + #more options in this function for dif maps 
  ggspatial::annotation_scale(aes(location="tr")) +
  theme_linedraw() +
  geom_sf(data = wd_move[wd_move$YearSinceRelease == "Year 1",], color = "darkgrey", size = 1) +
  geom_sf(data = mt_track_lines(wd_move[wd_move$YearSinceRelease == "Year 1",]), aes(color = `individual.local.identifier`)) +
  coord_sf(xlim = c(bb[1]-exp, bb[3]+exp), ylim = c(bb[2]-exp, bb[4]+exp), expand = F) + 
  guides(color = "none")

year2 <- ggplot() +
  ggspatial::annotation_map_tile(zoom = 12) + #more options in this function for dif maps 
  ggspatial::annotation_scale(aes(location="tr")) +
  theme_linedraw() +
  geom_sf(data = wd_move[wd_move$YearSinceRelease == "Year 2",], color = "darkgrey", size = 1) +
  geom_sf(data = mt_track_lines(wd_move[wd_move$YearSinceRelease == "Year 2",]), aes(color = `individual.local.identifier`)) +
  guides(color = "none")

(year1 + year2)

# request map data and then plot
# for stadia: sign up here: https://client.stadiamaps.com/signup/
# - go to profile - manage properties - API Key
# register api key
#register_stadiamaps("YOUR-API-KEY-HERE", write = FALSE)
register_stadiamaps("f6f76562-262b-4761-97d8-ae4546cfd0c9", write = FALSE)

mv_year1 <- filter(wd_move, YearSinceRelease == "Year 1")
my_year2 <- filter(wd_move, YearSinceRelease == "Year 2")

(bb <- st_bbox(wd_move))
names(bb) <- c("left","bottom","right","top") #the bounding box needs renaming
m <- ggmap::get_map(location = bb, source="stadia", maptype = "outdoors")

year1 <- ggmap(m) + theme_set(theme_bw()) + 
  geom_sf(data = mv_year1, color = "darkgrey", inherit.aes = FALSE) +
  geom_sf(data = mt_track_lines(mv_year1), aes(color = `individual.local.identifier`), inherit.aes = FALSE) +
  guides(color = "none") +  annotation_scale(aes(location="tr")) + 
  ggtitle("Years Post-Release: 1") + xlab("Longitude") + ylab("Latitude")

year2 <- ggmap(m) + theme_set(theme_bw()) + 
  geom_sf(data = my_year2, color = "darkgrey", inherit.aes = FALSE) +
  geom_sf(data = mt_track_lines(my_year2), aes(color = `individual.local.identifier`), inherit.aes = FALSE) +
  guides(color = "none") + annotation_scale(aes(location="tr")) + 
  ggtitle("Years Post-Release: 2") + xlab("Longitude") + ylab("")

(year1+year2)

## interactive plots
m_wc <- ggmap::get_stadiamap(bbox = bb, zoom=9, maptype = "stamen_watercolor")
gg_wc <- 
  ggmap(m_wc) +
  geom_sf(data = wd_move, inherit.aes = FALSE) +
  geom_sf(data = mt_track_lines(wd_move), aes(color = `individual.local.identifier`), inherit.aes = FALSE) +
  guides(color = "none")+
  annotation_scale(aes(location="br"))
plotly::ggplotly(gg_wc,tooltip = c("individual-local-identifier")) #this on OPEN STREET MAP looks great

# these also look very nice 
wdSF <- wd_move
class(wdSF) <- class(wd_move) %>% setdiff("move2") # remove class "move2" from object
mapview::mapView(wdSF, zcol="individual.local.identifier", legend=F) #as points
mapview::mapView(mt_track_lines(wd_move), zcol="individual.local.identifier", legend=F) #as lines


#################################################
## Occurrence distributions & movement models ##
#################################################

## set R environment
rm(list=ls())
setwd("/Users/meredithspalmer/Desktop/Work/Post doc_Princeton/Mozambique/Data_Bouley_Wild Dogs")

library(ctmm)
library(move)
library(move2)
library(sf)

## load & format data 
wd_move <- readRDS("wilddog_movement_cleaned.rds")
wd_telem <- to_move(wd_move)
wd_telem <- as.telemetry(wd_telem)

## examine dataset by plotting 
COL <- color(wd_telem,by='individual')
plot(wd_telem,col=COL,main="Spatial color separation")
compass() #note: does not point north 

## Movement model selection 
# - NOTE: this will need to be done per GroupYear (x8)
unique(wd_move$individual.local.identifier); length(unique(wd_move$individual.local.identifier))
#"Cheza Year 2", "Gorongosa Year 1","Gorongosa Year 2" "Mopane Year 2"    "Mucodza Year 1"  "Mucodza Year 2"   "Pwadzi Year 2"    "Xivulo Year 2"  

packyear <- unique(wd_move$individual.local.identifier)
df <- data.frame(packyear = rep(NA,8), bestfitmodel = rep(NA,8), effective.samp = rep(NA,8), abs.samp = rep(NA,8), mode.samp.int = rep(NA,8), samp.period.yrs = rep(NA,8), correct.samp.size = rep(NA,8))

# run through 1-8 manually, to assess different candidate models 
i <- 1

## Candidate model selection 

# - examine one GroupYear 
DATA <- wd_telem[[i]]
df$packyear[i] <- packyear[i]
packyear[i]
  
# - calculate a variogram object (named SVF) from the telemetry object
SVF <- variogram(DATA,CI="Gauss")
plot(SVF,main="Variogram")
  
# - fit candidate models: OUF, OUf, OUΩ, IOU, BM, IID, inactive
GUESS <- ctmm.guess(DATA,interactive=FALSE)
FITS <- ctmm.select(DATA,GUESS,trace=3,verbose=TRUE,cores=-1) #pHREML = default 
summary(FITS)
summary(FITS[[1]]) #best fit model 
df$bestfitmodel[i] <- summary(FITS[[1]])$name
plot(DATA,FITS[[1]],main="Best Fit Model") 
zoom(SVF,FITS[[1]],main="Best Fit Variogram")

# - residuals 
RES <- residuals(DATA,FITS[[1]])
plot(RES, main="Best Fit Residuals")
  
# - autocorrelation 
ACF <- correlogram(RES,res=10)
zoom(ACF,main='ACF of OUF Anisotropic Residuals')
  
## Feeding a movement model into the home range estimator
  
# - run an area-corrected AKDE (default):
#   - "AKDE_c": accounts for unmodelled correlation, oversmoothing
#   - pHREML (perturbative Hybrid REML) is default: accounts for autocorrelation estimation bias (esp in small absolute and effective sample sizes)
AKDE_pHREML <- akde(DATA, FITS[[1]], debias = TRUE)
summary(AKDE_pHREML, level.UD = 0.95)$CI
    
# - plotting AKDE: 
newEXT <- extent(AKDE_pHREML)
plot(DATA, UD = AKDE_pHREML, ext = newEXT); title(expression("AKDEc"))
  
## May need to correct for: 
  
# - (1) small sample sizes:
summary(AKDE_pHREML)$DOF["area"]; df$effective.samp[i] <- summary(AKDE_pHREML)$DOF["area"] # effective sample size of animal
nrow(DATA); df$abs.samp[i] <- nrow(DATA) # absolute sample size

# - sample sizes: expected order of pHREML bias
(bias <- (1/summary(AKDE_pHREML)$DOF["area"]^2)*100) #if <= 1%, no need to correct for small sample sizes 
(df$correct.samp.size[i] <- ifelse(as.numeric(bias) < 1, "NO", "YES")) #record did we correct FOR sample size

# --> IF AND ONLY IF need to correct for samp size 
BOOT <- ctmm.boot(DATA, FITS[[1]], error = 0.01, trace = 2, cores = -1)
summary(BOOT)

1/summary(BOOT)$DOF["area"]^3 #expected order of bootstrap bias

bbpHREML <- akde(DATA, BOOT, weights = TRUE)
summary(bbpHREML)$CI

# - plot
EXT <- extent(bbpHREML, level = 0.95)
plot(DATA, UD = bbpHREML, ext = EXT)
title(expression("Bootstrapped pHREML wAKDE"["C"]))
  
# - (2) irregular sampling intervals irregular: plot 
dt.plot(DATA) 
abline(h = 4 %#% "hours", col = "red") #most observations at four hours, but not all? 
abline(h = 2 %#% "hours", col = "blue") #first few observations at one hour 
summary(DATA)
df$mode.samp.int[i] <- summary(DATA)$`sampling interval (hours)`
df$samp.period.yrs[i] <- summary(DATA)$`sampling period (years)`
  
# - visualize high vs low frequency data 
pal <- "hr" %#% diff(DATA$t) # minimum adjacent sampling interval
pal <- pmin(c(Inf, pal), c(pal, Inf)) # sampling intervals under 2 hours
pal <- (pal < 2.5)# red (low-frequency) or yellow (high-frequency)
pal <- grDevices::rgb(1, pal, 0)
plot(DATA, col = pal, lwd = 2)
  
# - extract minimum sampling interval
"minutes" %#% min(diff(DATA$t))
  
# - calculate wAKDE (unweighted AKDE places too much density on oversampled times): 
#   - "to account for unrepresentative sampling in time" 
wAKDE_pHREML <- akde(DATA,
                     CTMM = FITS[[1]],
                     weights = TRUE) # you only need this with irregular sampling (can be slow!)
summary(wAKDE_pHREML)$CI # 95% home range area (weighted)
  
# - plotting pHREML (with and without weights) side-by-side:
EXT <- extent(list(AKDE_pHREML, wAKDE_pHREML), level = 0.95)
par(mfrow = c(1,2))
plot(DATA, UD = AKDE_pHREML, ext = EXT)
title(expression("pHREML AKDE"["C"]))
plot(DATA, UD = wAKDE_pHREML, ext = EXT)
title(expression("pHREML wAKDE"["C"]))
par(mfrow = c(1,1))
  
# - unweighted AKDE underestimates by this %; if > 1%, use wAKDE
(need.for.wakde <- (1 - summary(AKDE_pHREML)$CI[1,2] / summary(wAKDE_pHREML)$CI[1,2]) * 100)
#(need.for.wakde <- (1 - summary(bbpHREML)$CI[1,2] / summary(wAKDE_pHREML)$CI[1,2]) * 100) #if small sample size corrected 

df$wAKDE.used[i] <- ifelse(need.for.wakde > 1, "YES", "NO")
  
## Export polygons 

# - 95% (CHANGE TO wAKDE, AKDE, bbpHREML AS NECESSARY)
shapefile_polygons <- as.sf(wAKDE_pHREML, level.UD=0.95, level=0.95)
middle_polygon <- shapefile_polygons[2,]  #selects the second row which is the 95% est middle polygon
z <- crs(wd_move)
final_polygon <- st_transform(middle_polygon, crs=z) # match crs to base data so everything is aligned 
  
# -50% 
shapefile_polygons_50 <- as.sf(wAKDE_pHREML, level.UD=0.50, level=0.95)
middle_polygon_50 <- shapefile_polygons_50[2,]  #selects the second row which is the 95% est middle polygon
final_polygon_50 <- st_transform(middle_polygon_50, crs=z) # match crs to base data so everything is aligned 
  
ggplot() + geom_sf(data = final_polygon) + geom_sf(data = final_polygon_50)
  
packyear[i]
st_write(final_polygon, "wd_shapefiles/Xivulo_Year2_95.shp", append=F)
st_write(final_polygon_50, "wd_shapefiles/Xivulo_Year2_50.shp", append=F)

## do all 8


###################
## VISUALISATION ## 
###################

# plot core territories? 
library(sf)
library(ggmap)
library(patchwork) 
setwd("/Users/meredithspalmer/Desktop/Work/Post doc_Princeton/Mozambique/Data_Bouley_Wild Dogs/wd_shapefiles")

# year 1 home ranges 
gorongosa1 <- st_read("Gorongosa_Year1_50.shp")
mucodza1 <- st_read("Mucodza_Year1_50.shp")

# year 2 home ranges
cheza2 <- st_read("Cheza_Year2_50.shp")
gorongosa2 <- st_read("Gorongosa_Year2_50.shp")
mopane2 <- st_read("Mopane_Year2_50.shp")
mucodza2 <- st_read("Mucodza_Year2_50.shp")
pwadzi2 <- st_read("Pwadzi_Year2_50.shp")
xivulo2 <- st_read("Xivulo_Year2_50.shp")

# get background data 
setwd("/Users/meredithspalmer/Desktop/Work/Post doc_Princeton/Mozambique/SpatialData")
landscapes <- st_read("Landscapes.shp")
landscapes_2 <- st_read("GNP_Landscapes_New_boundary.shp")
grassland <- st_read("Grassland_only.shp")
floodplain <- st_read("Floodplain_only.shp")
rivers <- st_read("gnp_main_rivers_latlong.shp")
urema <- st_read("gnp_lakeurema_latlong.shp")

# set map parameters 
(bb <- st_bbox(wd_move))
names(bb) <- c("left","bottom","right","top") #the bounding box needs renaming
register_stadiamaps("f6f76562-262b-4761-97d8-ae4546cfd0c9", write = FALSE)
m <- ggmap::get_map(location = bb, source="stadia", maptype = "outdoors")

# Define the extent limits
xmin <- as.numeric(bb[[1]])
xmax <- as.numeric(bb[[3]])
ymin <- as.numeric(bb[[2]])
ymax <- as.numeric(bb[[4]])

#year1 
ggmap(m) + 
  theme_set(theme_bw()) + 
  #geom_sf(data = mucodza) +
  #geom_sf(data = gorongosa) + 
  ggtitle("Years Post-Release: 1") + xlab("Longitude") + ylab("Latitude")

ggmap(m) + theme_set(theme_bw()) + 
  geom_sf(data = mucodza1) +
  geom_sf(data = gorongosa1) +
  ggtitle("Years Post-Release: 2") + xlab("Longitude") + ylab("")

library(ggsci)
mypal <- pal_nejm("default")(6)

#these work but are ugly 
(year1 <- 
  ggplot() + 
    theme_set(theme_bw()) + 
    geom_sf(data = gorongosa1) + 
    geom_sf(data = landscapes) + 
    geom_sf(data = mucodza1, fill = mypal[[4]], alpha=0.5) +
    geom_sf(data = gorongosa1, fill = mypal[[5]], alpha=0.5) + 
    coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE) + 
    ggtitle("Years Post-Release: 1") + xlab("Longitude") + ylab("Latitude"))

(year2 <- ggplot() + 
    theme_set(theme_bw()) + 
    geom_sf(data = xivulo2) + 
    geom_sf(data = landscapes) +  
    geom_sf(data = xivulo2, fill = mypal[[2]], alpha=0.5) + 
    geom_sf(data = cheza2, fill = mypal[[6]], alpha=0.5) + 
    geom_sf(data = mopane2, fill = mypal[[3]], alpha=0.5) + 
    geom_sf(data = mucodza2, fill = mypal[[4]], alpha=0.5) +
    geom_sf(data = gorongosa2, fill = mypal[[5]], alpha=0.5) + 
    geom_sf(data = pwadzi2, fill = mypal[[1]], alpha=0.5) + 
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE) + 
  ggtitle("Years Post-Release: 1") + xlab("Longitude") + ylab("Latitude"))

(year1 + year2)
