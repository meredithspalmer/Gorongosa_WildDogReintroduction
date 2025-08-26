## Gorongosa Wild Dog AKDE Analysis - SEASONAL 
## Meredith S Palmer
## 01-August-2024

## set R environment
rm(list=ls())
setwd("/Users/meredithspalmer/Desktop/Work/Post doc_Princeton/Mozambique/Carnivore Data/Data_Bouley_Wild Dogs")

## libraries
library(tidyverse)
library(sf)
library(lubridate)
library(dplyr)
library(patchwork) 
library(ggmap)
library(ggspatial)
library(gganimate)
library(mapview)
library(move)
library(move2)
library(moveVis)
library(ctmm)

#######################
## READ CLEANED DATA ## 
#######################

# read data 
wd_move <- readRDS("wilddog_movement_cleaned.rds")
wd_crs <- crs(wd_move)
unique(wd_move$Month)

########################################
## DOGS THAT OVERLAP WITH CAMERA GRID ## 
########################################

# read ct coords 
ct_coords <- read.csv("../../SpatialData/Camera_coordinates.csv") %>% 
  st_as_sf(coords=c("Longitude","Latitude"), crs=wd_crs)

# plot data 
(bb <- st_bbox(wd_move))
exp <- 0.05

(p1 <- ggplot() +
  ggspatial::annotation_scale(aes(location="tr")) +
  theme_linedraw() +
  geom_sf(data = wd_move, color = "darkgrey", size = 1) +
  geom_sf(data = mt_track_lines(wd_move), aes(color = `individual.local.identifier`)) +
  coord_sf(xlim = c(bb[1]-exp, bb[3]+exp), ylim = c(bb[2]-exp, bb[4]+exp), expand = F)) 

# identify which dogs overlap with grid
p1 + geom_sf(data = ct_coords, color = "black", size = 1) 
# --> NOT Xivulo Year 2, Pwadzi Year 2, Mucodza Year 2

wd_filter <- wd_move[!wd_move$individual.local.identifier %in% c("Xivulo Year 2", "Pwadzi Year 2", "Mucodza Year 2"),]
(bb2 <- st_bbox(wd_filter))

(p2 <- ggplot() +
    ggspatial::annotation_scale(aes(location="tr")) +
    theme_linedraw() +
    geom_sf(data = wd_filter, color = "darkgrey", size = 1) +
    geom_sf(data = mt_track_lines(wd_filter), aes(color = `individual.local.identifier`)) +
    coord_sf(xlim = c(bb2[1]-exp, bb2[3]+exp), ylim = c(bb2[2]-exp, bb2[4]+exp), expand = F) + 
    geom_sf(data = ct_coords, color = "black", size = 1))
# --> Cheza Year 2, Gorongosa Year 1, Gorongosa Year 2, Mopane Year 2, Mucodza Year 1 (5) 


##########################
## SEARCH EFFORT ISSUES ## 
##########################

# define seasons 
wet <- c("Dec", "Jan", "Feb", "Mar")
edry <- c("Apr", "May", "Jun", "Jul")
ldry <- c("Aug", "Sep", "Oct", "Nov") 

# assign seasons
wd_filter$season <- ifelse(wd_filter$Month %in% wet, "WET", ifelse(wd_filter$Month %in% edry, "EARLYDRY", ifelse(wd_filter$Month %in% ldry, "LATEDRY", "ERROR")))
nrow(wd_filter[wd_filter$season == "ERROR",]) #0 

# assign season-years
wd_filter$season_year <- ifelse(wd_filter$Month %in%  c("Dec"), paste(wd_filter$season, (as.numeric(as.character(wd_filter$Year))+1), sep="_"), paste(wd_filter$season, (wd_filter$Year), sep="_"))
levels(wd_filter$season_year) <- c("EARLYDRY_2018", "LATEDRY_2018", "WET_2019", "EARLYDRY_2019", "LATEDRY_2019", "WET_2020", "EARLYDRY_2020", "LATEDRY_2020")
levels(wd_filter$season_year) #check 

## assess how much data per season per pack 

# thin down to unique dates (rather than fixes)
wd_filter$date <- as.Date(wd_filter$timestamp)
wd_filter_test <- wd_filter %>% 
  as.data.frame(wd_filter) %>% 
  group_by(GroupName) %>% 
  dplyr::select(c("GroupName", "DogName", "date", "season_year")) %>% 
  distinct(date, .keep_all=T)
head(wd_filter_test)

# confirm function did this for each pack independently: 
sum(duplicated(wd_filter_test$date))
sum(duplicated(wd_filter_test[wd_filter_test$GroupName == "Gorongosa",]$date)) 

## how much coverage per season? 
earlydry_2018 <- seq(from=as.Date("2018-04-01", "%Y-%m-%d"), to=as.Date("2018-07-31", "%Y-%m-%d"), by="days")
latedry_2018 <- seq(from=as.Date("2018-08-01", "%Y-%m-%d"), to=as.Date("2018-12-30", "%Y-%m-%d"), by="days")
wet_2019 <- seq(from=as.Date("2018-12-01", "%Y-%m-%d"), to=as.Date("2019-03-31", "%Y-%m-%d"), by="days")
earlydry_2019 <- seq(from=as.Date("2019-04-01", "%Y-%m-%d"), to=as.Date("2019-07-31", "%Y-%m-%d"), by="days")
latedry_2019 <- seq(from=as.Date("2019-08-01", "%Y-%m-%d"), to=as.Date("2019-12-30", "%Y-%m-%d"), by="days")
wet_2020 <- seq(from=as.Date("2019-12-01", "%Y-%m-%d"), to=as.Date("2020-03-31", "%Y-%m-%d"), by="days")
earlydry_2020 <- seq(from=as.Date("2020-04-01", "%Y-%m-%d"), to=as.Date("2020-07-31", "%Y-%m-%d"), by="days")
latedry_2020 <- seq(from=as.Date("2020-08-01", "%Y-%m-%d"), to=as.Date("2020-12-30", "%Y-%m-%d"), by="days")

# for each pack... 
packs <- unique(wd_filter$GroupName) #note that gorongosa pack becomes 1 
library(hash)
season_dict <- list()
season_dict[["EARLYDRY_2018"]] <- earlydry_2018
season_dict[["EARLYDRY_2019"]] <- earlydry_2019
season_dict[["EARLYDRY_2020"]] <- earlydry_2020
season_dict[["LATEDRY_2018"]] <- latedry_2018
season_dict[["LATEDRY_2019"]] <- latedry_2019
season_dict[["LATEDRY_2020"]] <- latedry_2020
season_dict[["WET_2019"]] <- wet_2019
season_dict[["WET_2020"]] <- wet_2020

wd_effort <- NULL
for(i in 1:length(packs)){
  pack_effort <- NULL
  pack <- wd_filter[wd_filter$GroupName == packs[i],]
  ssn_yrs <- unique(pack$season_year)
  for(j in 1:length(ssn_yrs)) {
    ssn_yr_date <- pack[pack$season_year == ssn_yrs[j],]$date
    ssn_yr_dict <- season_dict[ssn_yrs[j]]
    x <- table(ssn_yr_dict[[1]] %in% ssn_yr_date)
    y <- x[2]/(x[1] + x[2])*100
    pack_effort$season_year[j] <- ssn_yrs[j]
    pack_effort$start_dog[j] <- as.character(min(ssn_yr_date))
    pack_effort$end_dog[j] <- as.character(max(ssn_yr_date))
    pack_effort$start_season[j] <- as.character(min(ssn_yr_dict[[1]]))
    pack_effort$end_season[j]<- as.character(max(ssn_yr_dict[[1]]))
    pack_effort$coverage[j] <- round(y,2)
  }
  pack_effort <- as.data.frame(pack_effort)
  pack_effort$pack <- packs[i]
  wd_effort <- rbind(wd_effort, pack_effort)
}
wd_effort
# note that coverage is ACTUAL DAYS COLLARS ON, rather than overlap from start-stop

# note that Gorongosa 2019 WET is NA because 100% coverage
wd_effort[wd_effort$season_year == "WET_2019" & wd_effort$pack == "Gorongosa",]$coverage <- 100

# --> hm, consider using anything where coverage is over 50%? again, statistics can support irregular sampling. the ones I would want to think about are where the coverage started much later than the season date [concerning] or ended much earlier than the season [but if pack still around, can assume similar levels of coverage]
write.csv(wd_effort, "wd_effort_seasoncoverage.csv", row.names=F)

## for each pack, derive gap between unique days 
wd_filter_test$GroupName <- droplevels(wd_filter_test$GroupName)
wd_filter_test$timegap <- c(NA, with(wd_filter_test, date[-1] - date[-nrow(wd_filter_test)])) #dif in days
table(wd_filter_test$timegap) #neg values for each pack; replace with NAs
wd_filter_test[wd_filter_test$timegap < 0 & !is.na(wd_filter_test$timegap),]$timegap <- NA #6 NAs = 6 packs 
wd_filter_test$overgap <- ifelse(wd_filter_test$timegap > 1, "BREAK", "continuous")
table(wd_filter_test$GroupName, wd_filter_test$timegap)
# note: length of gap less important, as stats account for irregular sampling; almost all of these can be handled by statistics 
# --> rather, start/stop dates within seasons (what fraction of season covered) 

# generate start/stop effort data frame for each pack 
wd_start_stop <- data.frame()
counter <- 1
                             
for(i in 1:nrow(wd_filter_test)){
  if(is.na(wd_filter_test$overgap[i])) {
    wd_start_stop[counter,1] <- as.character(wd_filter_test$GroupName[i])
    wd_start_stop[counter,2] <- "start"
    wd_start_stop[counter,3] <- as.character(wd_filter_test$date[i])
    wd_start_stop[counter,4] <- as.character(wd_filter_test$season_year[i])
    counter <- counter + 1
  } else if (wd_filter_test$overgap[i] == "BREAK") {
    wd_start_stop[counter,1] <- as.character(wd_filter_test$GroupName[i])
    wd_start_stop[counter,2] <- "stop"
    wd_start_stop[counter,3] <- as.character(wd_filter_test$date[i-1])
    wd_start_stop[counter,4] <- as.character(wd_filter_test$season_year[i-1])
    wd_start_stop[(counter + 1),1] <- as.character(wd_filter_test$GroupName[i])
    wd_start_stop[(counter + 1),2] <- "start"
    wd_start_stop[(counter + 1),3] <- as.character(wd_filter_test$date[i])
    wd_start_stop[(counter + 1),4] <- as.character(wd_filter_test$season_year[i])
    counter <- counter + 2
  } else {NULL} 
}
names(wd_start_stop) <- c("GroupName", "Start/Stop", "date", "season_year")
head(wd_start_stop)
write.csv(wd_start_stop, "wd_effort_dates.csv", row.names=F)


##########################
## SEASONAL HOME RANGES ##
##########################

# NOTE: will generate for all, but decide with team which to use ultimately
# -> could weight by season coverage

rm(list=ls())

# define seasons 
wet <- c("Dec", "Jan", "Feb", "Mar")
edry <- c("Apr", "May", "Jun", "Jul")
ldry <- c("Aug", "Sep", "Oct", "Nov") 

# load data
wd_cleaned <- read.csv("wd_cleaned.csv")
sum(is.na(wd_cleaned$timestamp))

# filter data 
wd_filter <- wd_cleaned[!wd_cleaned$individual.local.identifier %in% c("Xivulo Year 2", "Pwadzi Year 2", "Mucodza Year 2"),]

# assign seasons
wd_filter$season <- ifelse(wd_filter$Month %in% wet, "WET", ifelse(wd_filter$Month %in% edry, "EARLYDRY", ifelse(wd_filter$Month %in% ldry, "LATEDRY", "ERROR")))
nrow(wd_filter[wd_filter$season == "ERROR",]) #0 

# assign season-years
wd_filter$season_year <- ifelse(wd_filter$Month %in%  c("Dec"), paste(wd_filter$season, (as.numeric(as.character(wd_filter$Year))+1), sep="_"), paste(wd_filter$season, (wd_filter$Year), sep="_"))
levels(wd_filter$season_year) <- c("EARLYDRY_2018", "LATEDRY_2018", "WET_2019", "EARLYDRY_2019", "LATEDRY_2019", "WET_2020", "EARLYDRY_2020", "LATEDRY_2020")
levels(wd_filter$season_year) #check 

# reassign individual identifier 
wd_filter$GroupSeasonYear <- paste(wd_filter$GroupName, wd_filter$season_year)
wd_filter$individual.local.identifier <- wd_filter$GroupSeasonYear
wd_filter$GroupSeasonYear <- NULL

# set time column
wd_filter$timestamp <- as.POSIXct(wd_filter$timestamp, format="%Y-%m-%d %H:%M:%S", tz="Africa/Maputo")
sum(is.na(wd_filter$timestamp)) #this is where we error; some are just dates without times 
# this doesn't happen if generate wd_cleaned from scratch - do that instead ): 

# convert to move2 object
wd_filter <- mt_as_move2(wd_filter,  
                       coords = c("location.long","location.lat"), 
                       crs = "EPSG:4326", 
                       time_column = "timestamp",
                       track_id_column = "individual.local.identifier", #unique separate tracks 
                       na.fail = F) # allows or not empty coordinates

# assess whether worked
mt_is_track_id_cleaved(wd_filter) #FALSE; so group
wd_filter <- dplyr::arrange(wd_filter, mt_track_id(wd_filter)) 

# check empty locations 
mt_has_no_empty_points(wd_filter) #TRUE (no empty points)

# check duplicated timestamps again
mt_has_unique_location_time_records(wd_filter) #TRUE (all unique)
#this throws issues if you reload - use the OG file generated from above script :( 

# to telemetry object 
wd_telem <- to_move(wd_filter)
wd_telem <- as.telemetry(wd_telem)

## Movement model selection 

# - NOTE: this will need to be done per GroupYear (x17) for each SEASON 
(x <- length(unique(wd_filter$individual.local.identifier))) #17
(packyear <- unique(wd_filter$individual.local.identifier))
df <- data.frame(packyear = rep(NA,x), bestfitmodel = rep(NA,x), effective.samp = rep(NA,x), abs.samp = rep(NA,x), mode.samp.int = rep(NA,x), samp.period.months = rep(NA,x), correct.samp.size = rep(NA,x))


# run through 1-17 manually, to assess different candidate models 
i <- 17

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
#df$samp.period.months[i] <- summary(DATA)$`sampling period (years)`*12
#df$samp.period.months[i] <- summary(DATA)$`sampling period (days)`/31
df$samp.period.months[i] <- summary(DATA)$`sampling period (months)`

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
(df$wAKDE.used[i] <- ifelse(need.for.wakde > 1, "YES", "NO"))

## Export polygons 

# - 95% (CHANGE TO wAKDE, AKDE, bbpHREML AS NECESSARY)
shapefile_polygons <- as.sf(AKDE_pHREML, level.UD=0.95, level=0.95)
middle_polygon <- shapefile_polygons[2,]  #selects the second row which is the 95% est middle polygon
z <- crs(wd_filter)
final_polygon <- st_transform(middle_polygon, crs=z) # match crs to base data so everything is aligned 

# -50% 
shapefile_polygons_50 <- as.sf(AKDE_pHREML, level.UD=0.50, level=0.95)
middle_polygon_50 <- shapefile_polygons_50[2,]  #selects the second row which is the 95% est middle polygon
final_polygon_50 <- st_transform(middle_polygon_50, crs=z) # match crs to base data so everything is aligned 

ggplot() + geom_sf(data = final_polygon) + geom_sf(data = final_polygon_50)

packyear[i]
st_write(final_polygon, "../../Wild Dog Reintroduction/wd_shapefiles_season/Mucodza_ED_2019_95.shp", append=F)
st_write(final_polygon_50, "../../Wild Dog Reintroduction/wd_shapefiles_season/Mucodza_ED_2019_95_50.shp", append=F)

## do all 17

write.csv(df, "wd_season_modelselection.csv", row.names=F)


###################
## VISUALISATION ## 
###################

library(sf)
library(ggmap)
library(patchwork) 
setwd("/Users/meredithspalmer/Desktop/Work/Post doc_Princeton/Mozambique/Wild Dog Reintroduction/wd_shapefiles_season")

# just as an example 
cheza_ed_2020 <- st_read("Cheza_ED_2020_95.shp")
cheza_ld_2019 <- st_read("Cheza_LD_2019_95.shp")
cheza_wt_2020 <- st_read("Cheza_WET_2020_95.shp")

# just as an example 
goro_ed_2020 <- st_read("Gorongosa_ED_2018_95.shp")
goro_ld_2019 <- st_read("Gorongosa_LD_2019_95.shp")
goro_wt_2020 <- st_read("Gorongosa_WET_2020_95.shp")

# get background data 
setwd("/Users/meredithspalmer/Desktop/Work/Post doc_Princeton/Mozambique/SpatialData")
landscapes <- st_read("Landscapes.shp")
landscapes_2 <- st_read("GNP_Landscapes_New_boundary.shp")
grassland <- st_read("Grassland_only.shp")
floodplain <- st_read("Floodplain_only.shp")
rivers <- st_read("gnp_main_rivers_latlong.shp")
urema <- st_read("gnp_lakeurema_latlong.shp")

# set map parameters 
(bb <- st_bbox(cheza_ed_2020))
names(bb) <- c("left","bottom","right","top") #the bounding box needs renaming
register_stadiamaps("f6f76562-262b-4761-97d8-ae4546cfd0c9", write = FALSE)
m <- ggmap::get_map(location = bb, source="stadia", maptype = "outdoors")

# Define the extent limits
xmin <- as.numeric(bb[[1]])
xmax <- as.numeric(bb[[3]])
ymin <- as.numeric(bb[[2]])
ymax <- as.numeric(bb[[4]])

library(ggsci)
mypal <- pal_nejm("default")(6)

#these work but are ugly 
(cheza <- 
    ggplot() + 
    theme_set(theme_bw()) + 
    geom_sf(data = cheza_ed_2020) + 
    geom_sf(data = landscapes) + 
    geom_sf(data = cheza_ed_2020, fill = mypal[[5]], alpha=0.5) + 
    geom_sf(data = cheza_ld_2019, fill = mypal[[2]], alpha=0.5) + 
    geom_sf(data = cheza_wt_2020, fill = mypal[[1]], alpha=0.5) + 
    coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE) + 
    ggtitle("Cheza: 2019-2020") + xlab("Longitude") + ylab("Latitude")) 

p1 + geom_sf(data = ct_coords, color = "black", size = 1) 

# set map parameters 
(bb <- st_bbox(goro_wt_2020))
names(bb) <- c("left","bottom","right","top") #the bounding box needs renaming
register_stadiamaps("f6f76562-262b-4761-97d8-ae4546cfd0c9", write = FALSE)
m <- ggmap::get_map(location = bb, source="stadia", maptype = "outdoors")

# Define the extent limits
xmin <- as.numeric(bb[[1]])
xmax <- as.numeric(bb[[3]])
ymin <- as.numeric(bb[[2]])
ymax <- as.numeric(bb[[4]])

(gorongosa <- 
    ggplot() + 
    theme_set(theme_bw()) + 
    geom_sf(data = goro_ed_2020) + 
    geom_sf(data = landscapes) + 
    geom_sf(data = goro_ed_2020, fill = mypal[[5]], alpha=0.5) + 
    geom_sf(data = goro_ld_2019, fill = mypal[[2]], alpha=0.5) + 
    geom_sf(data = goro_wt_2020, fill = mypal[[1]], alpha=0.5) + 
    coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE) + 
    ggtitle("Gorongosa: 2019-2020") + xlab("Longitude") + ylab("Latitude")) 




goro_wt_2020
###################
## CT RISK PROXY ## 
###################

# 1. Weight by group size
# 2. Extract value at CT 

... TBD 