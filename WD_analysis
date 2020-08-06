## Pre-/Post-Wild Dog Release Analyses ## 

# 1) activity patterns (*by season)
# - i) across entire grid pre/post wild dog release 
# - ii) in wild dog occupancy areas pre/post wild dog release 
# - iii) in areas of wild dog use and unuse post wild dog release 

# 2) distribution patterns
# - i) occupancy pre/post wild dog release 
# - ii) RAI pre/post wild dog release
# - iii) occupancy/RAI across 24hr day pre/post wild dog release 

# magnitude of change as a function of wild dog diet, diet, body size 

# load data -----------------------------------------------

# load functions and libraries 
source("../../Analyses and Scripts/MSP_WildDogAnalysisFunctions.R")

# load data
dat <- read.csv("WildCamData_Formatted_PrePostWildDog.csv")
effort <- read.csv("WildCamEffort_Formatted_PrePostWildDog.csv")

# info for formatting data-time 
tz.ct <- "Africa/Maputo"
coords <- matrix(c(34.50, -18.82), nrow=1) %>%
  sp::SpatialPoints(proj4string=sp::CRS("+proj=longlat +datum=WGS84"))

# format date-time, sunrise/sunset 
dat <- dat %>% 
  mutate(datetime = as.POSIXct(datetime, "%Y-%m-%d %H:%M:%S", tz=tz.ct), 
         Date = as.POSIXct(dat$Date, format = "%Y-%m-%d", tz = tz.ct), 
         Time.Decimal = hour(datetime) + minute(datetime)/60 + second(datetime)/3600, 
         Time.Radians = (Time.Decimal / 24) * 2 * pi, 
         Time.Sun = sunTime(Time.Radians, Date, coords)) 

# load grid cells 
grid <- st_read("../../Camera Trap Data/cameragridhexagons/CameraGridHexes2.shp") %>% 
  st_transform(crs = "+proj=longlat +datum=WGS84") %>% 
  dplyr::rename(Camera = StudySite) 
         
# data visualization: look at distribution of data through time 
test <- dat %>% 
  mutate(Date = as.Date(Date), var = 1) %>% 
  group_by(Date, season) %>% 
  dplyr::summarise(counter = sum(var)) 

ggplot(test, aes(x=Date, y=counter, group = season)) + geom_point(aes(color=season)) + 
  geom_vline(xintercept=as.Date("2018-08-01"), linetype="dashed")
# - complete year (3 seasons) of data post-release and two complete years (+1 = 7 seasons) pre-release 
# - additional post-release data even from within this first year is likely still being processed

dat <- dat %>% 
  mutate(season_year = paste(season, year(datetime)), 
         season_year = ifelse(season == "Wet" & month(datetime) == 12, 
                              paste("Wet", year(datetime)+1), season_year))
# - as wet season runs Dec-Mar, add Dec to "next years'" wet season 

test2 <- dat %>% 
  mutate(var = 1) %>% 
  group_by(season_year, season) %>% 
  dplyr::summarise(counter = sum(var))

# - order levels through time for plotting 
test2$season_year <- factor(test2$season_year, levels = c("Early Dry 2016", "Late Dry 2016", "Wet 2017", "Early Dry 2017", "Late Dry 2017", "Wet 2018", "Early Dry 2018", "Late Dry 2018", "Wet 2019", "Early Dry 2019"))

ggplot(test2, aes(x=season_year, y=counter, group=season)) + geom_bar(stat="identity", aes(fill=season)) 
# - direct comparison between seasons before/after complicated by lack of data, Cyclone Idai (wet 2018)
# - imbalance in data something to keep in mind... 

# wild dog data 
dogs <- read.csv("~/Desktop/PROJECT_GNP_WildDogs/ct_wild_dog_dist.csv") %>% 
  mutate(dog_index = distance/max(distance)) %>% 
  select(Grid, dog_index) %>% `colnames<-`(c("Camera", "Dog.Distance"))
# - these distances are in degrees; scale from 0 (closest) to 1 (furthest) to form index ("Dog.Distance")

dat <- merge(dat, dogs, all.x = T)
# - note: don't have dog data for D09, F05, K10 --> fix later, ignore for now 

# - for some analyses, look at whether in areas of dog "use" or "not use": arbitrarily divided distance data into two equal bins but will re-evaluate, hopefully if/when get better wild dog data 
hist(dogs$Dog.Distance) #where to set the break point?  
dat <- dat %>% 
  filter(!is.na(Dog.Distance)) %>% #have missing values (no data for 3 sites), will fix later 
  mutate(Dog.Distance.Cut = cut2(Dog.Distance, g=2)) %>%
  mutate(Dog.Distance.Cut = fct_recode(Dog.Distance.Cut, use = "[0.000,0.235)", less_use = "[0.235,1.000]"))

dog.poly <- st_read("~/Desktop/PROJECT_GNP_WildDogs/wild_dog_layer.shp") %>% 
  st_transform(crs = "+proj=longlat +datum=WGS84") 


# 1) activity patterns ---------------------------------------

## activity analysis

# would anticipate that increased nocturnal activity to avoid crepuscular/diurnal predators? 

# note: when running actual analyses, use glmm with regression spline instead of overlap package (see recent work by J. Fieberg [unpub] demonstrating that curves more reliable, also can include covariates on temporal activity patterns that explain differences + random effects); something similar to: 
m1 <- mixed_model(capture ~ 
                    fixed = sin(2 * pi * Time/12) * Treatment + 
                            cos(2 * pi * Time/12) * Treatment + 
                            sin(2 * pi * Time/24) * Treatment + 
                            cos(2 * pi * Time/24) * Treatment, 
                  random = ~1|Camera, 
                  family = binomial(), data = occasions) 
# - where treatment could be before/after release, could account for habitat variables, etc. ... 

# however, quickly to look at patterns: 
activity.overlap <- activity.analysis(dat)
# - this function runs activity analsyes addressing each of the three comparisons: 
#   i) across entire grid pre/post wild dog release (all.grid.prepost)
#   ii) in wild dog occupancy areas pre/post wild dog release (wd.area.prepost)
#   iii) in areas of wild dog use and unuse post wild dog release (wd.use.unuse.post)

# - this function and plotting functions below calculate delta_hat_1 for sightings < 50, delta_hat_4 for sightings >= 50, produce error message if < 20 or if no sightings recorded for a particular data subset 

(dif.activity <- activity.overlap[activity.overlap$Overlap <= 0.85,])
# - look at low-overlap (high difference) 
# - use exploratory plotting to investigate further (see below)
dif.activity[order(dif.activity$Situtation),]


## exploratory plotting 
# - look at situations highlighted in above analysis 


# i) entire grid before and after (LEGEND: red = pre-release, blue = post-release)

# - these are based on 20-30 sightings, not great: 
activity.i("Duiker_common", dat) # really need more data 
activity.i("Oribi", dat)  # really need more data 
activity.i("Hartebeest", dat) # really need more data 
activity.i("Buffalo", dat) # really need more data 

# - these species have more data: 
activity.i("Reedbuck", dat) # less dirunal 
activity.i("Bushpig", dat) # more crepuscular 
activity.i("Nyala", dat) # more diurnal 
activity.i("Wildebeest", dat) # more diurnal 
activity.i("Kudu", dat) # less diurnal 


# ii) in wild dog occupancy areas pre/post wild dog release (LEGEND: red = pre-release, blue = post-release)
# - note: bc so few early dry season post release observations, displays all data, late dry, and wet season 
# --> this group include more animals that I think of as wild dog prey
activity.ii("Hartebeest", dat) # really need more data 
activity.ii("Bushpig", dat) # less nocturnal, more crepuscular? not so many observations 

activity.ii("Warthog", dat) # seasonal patterns very similar 
activity.ii("Oribi", dat) # more crepuscular 
activity.ii("Waterbuck", dat) # seasonal patterns very similar 
activity.ii("Reedbuck", dat) # more nocturnal
activity.ii("Kudu", dat) # less diurnal 
activity.ii("Bushbuck", dat) # more dirunal 
activity.ii("Nyala", dat) # more diurnal 


# iii) in areas of wild dog use and unuse post wild dog release (**LEGEND: red = HIGH USE, blue = LOW USE**)
# --> all very food-y animals (also, not visualized but also big changes in mesopredator community activity)
activity.iii("Warthog", dat) # patterns very similar 
activity.iii("Oribi", dat) #?! 
activity.iii("Reedbuck", dat) # noon peak in high-use areas? 
activity.iii("Kudu", dat) #?!
activity.iii("Baboon", dat) # don't use high-use areas in evening 
activity.iii("Bushbuck", dat) # noon peak in high-use areas? 
activity.iii("Bushpig", dat) # patterns very similar 
activity.iii("Nyala", dat) # use high-use in mornings, low use in evenings? (see distribution iii)
activity.iii("Hartebeest", dat) # really need more data 



# 2) distribution patterns -----------------------------------


# selecting species and time period of interest --------------

#species options: "Baboon", "Buffalo", "Bushbuck", "Bushpig", "Civet", "Duiker_common", "Eland", "Elephant", "Genet", "Hartebeest", "Honey_badger", "Impala", "Kudu", "Lion", "Nyala", "Oribi", "Reedbuck", "Serval",  "Vervet", "Warthog", Waterbuck", "Wildebeest"   

#season options: Wet, Early Dry, Late Dry (takes helluva long time to run all seasons at the same itme )


## exploratory plotting --------------------------------------


# ii) RAI pre/post wild dog release 
# --> this code takes a hot second to render and sometimes works and sometimes throws an error for no reason and will fix itself if you rerun 

# distribution.ii(species, season.choice, log.choice = T, dat, effort, dog.poly)
species <- "Kudu"
season <- "Wet"
distribution.ii(species, season, log.choice = T, dat, effort, dog.poly) 
