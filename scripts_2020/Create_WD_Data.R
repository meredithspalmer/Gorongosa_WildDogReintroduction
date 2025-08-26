## Consolidate Gorongosa data pre- and post-wild dog release ## 

# set workspace 
rm(list=ls())
library(dplyr); library(Hmisc); library(lubridate)

## user input -----------------------

# set independence time 
ind.min <- 15 

# select evenness score (<= included) (0 == complete consensus; 1 == complete disagreement)
evenness_level <- 0.9

# select species of interest 
species <- c("Baboon", "Buffalo", "Bushbuck", "Bushpig", "Civet", "Elephant", "Genet", "Honey_badger", "Impala", "Kudu", "Oribi", "Reedbuck", "Vervet", "Warthog", "Waterbuck", "Wildebeest", "Nyala", "Hartebeest", "Serval", "Duiker", "Eland", "Lion", "Mongoose")

# set timezone
tz.ct <- "Africa/Maputo"

## species records ---------------- 

# load year 1 and 2 data 
wild1 <- read.csv("recordtable_year1_withbehaviorcount_CLEAN.csv") %>% 
  select(Camera, Species, DateTimeOriginal, Count, Juveniles, Moving, Eating, Resting, Standing, 
         Interacting, Horns, delta.time.mins) 
names(wild1)[11] <- "Males"

wild2 <- read.csv("recordtable_year2_withbehaviorcount_CLEAN.csv") %>% 
  select(Camera, Species, DateTimeOriginal, Count, Juveniles, Moving, Eating, Resting, Standing, 
         Interacting, Males, delta.time.mins)

wild1.2 <- rbind(wild1, wild2) %>%
  filter(!is.na(Species)) %>%
  mutate(Species = recode(Species, reedbuck = "Reedbuck", bushbuck = "Bushbuck", Duiker_red = "Duiker",
                          Duiker_common = "Duiker", `Vervet monkey` = "Vervet", 
                          Mongoose_banded = "Mongoose", Mongoose_unknown = "Mongoose", 
                          Mongoose_marsh = "Mongoose", Mongoose_slender = "Mongoose",
                          Mongoose_other = "Mongoose", Mongoose_bushy_tailed = "Mongoose",
                          Mongoose_dwarf = "Mongoose")) %>%
  filter(Species %in% species) %>% 
  filter(delta.time.mins == 0 | delta.time.mins >= ind.min) %>% 
  select(Camera:Males) 
wild1.2$DateTime <- strptime(wild1.2$DateTimeOriginal, "%m/%d/%y %H:%M", tz=tz.ct) 
rm(wild1); rm(wild2); wild1.2$DateTimeOriginal <- NULL

#NOTE: cit scis record only presence/absence of juveniles, males, so have reformatted year 1 and 2 data to be presence/absence rathar than counts 
#NOTE: keep NAs for non-dimorphic species; also, baboons lacking much data so keep NAs there too 
wild1.2[wild1.2$Males %in% seq(1:20),]$Males <- 1 
wild1.2[wild1.2$Juveniles %in% seq(1:20),]$Juveniles <- 1
wild1.2[wild1.2$Eating %in% seq(1:20),]$Eating <- 1
wild1.2[wild1.2$Resting %in% seq(1:20),]$Resting <- 1 
wild1.2[wild1.2$Standing %in% seq(1:20),]$Standing <- 1 
wild1.2[wild1.2$Interacting %in% seq(1:20),]$Interacting <- 1 

wild1.2[wild1.2$Moving %in% seq(1:20),]$Moving <- 1  #for some reason this causes issues w/o extra 
wild1.2$Moving <- droplevels(wild1.2$Moving)
wild1.2$Moving <- as.numeric(as.character(wild1.2$Moving))

# load year 3 data 
wild3 <- read.csv("WLD_Year3/WLD_S1_report_consensus_survey.csv") %>% 
  select(site, capture_date_local, capture_time_local, question__species, question__count_median, question__standing, question__resting, question__moving, question__eating, question__interacting, question__young_present, question__horns_visible, pielous_evenness_index) %>% #no horn count available
  mutate(Species = capitalize(as.character(question__species))) %>% 
  mutate(Species = recode(Species, Honeybadger = "Honey_badger", Vervetmonkey = "Vervet",
                          Lionmale = "Lion", Lionfemale = "Lion", Lioncub = "Lion")) %>% 
  filter(Species %in% species) %>% 
  select(-question__species) %>% 
  `colnames<-`(c("Camera", "Date", "Time", "Count", "Standing", "Resting", "Moving", "Eating", "Interacting", "Juveniles", "Males", "pielous_evenness_index", "Species")) 
#NOTE: in cit sci data, users record HORNS rather than MALES - male data may be less complete here 

# fix counts
wild3$Count <- as.character(wild3$Count)
wild3[wild3$Count == '11-50',]$Count <- "30"
wild3$Count <- as.numeric(wild3$Count)

# fix juvenile, male, behavior cut-off 
#NOTE: cit sci data reports proportion of users that tagged record as containing juveniles, behaviors, males: need to find and apply a cut-off to determine whether records truly contain or not ... this is a little subjective
hist(wild3$Standing); hist(wild3$Resting); hist(wild3$Moving); hist(wild3$Eating); hist(wild3$Interacting); hist(wild3$Males); hist(wild3$Juveniles)

wild3$Standing <- ifelse(wild3$Standing > 0.1, 1, 0) 
wild3$Resting <- ifelse(wild3$Resting > 0.1, 1, 0)
wild3$Moving <- ifelse(wild3$Moving > 0.5, 1, 0)
wild3$Eating <- ifelse(wild3$Eating > 0.1, 1, 0)
wild3$Interacting <- ifelse(wild3$Interacting > 0.1, 1, 0)
wild3$Males <- ifelse(wild3$Males > 0.1, 1, 0)
wild3$Juveniles <- ifelse(wild3$Juveniles > 0.1, 1, 0)

# calculate time difs 
wild3$DateTime <- strptime(paste(wild3$Date, wild3$Time), "%Y-%m-%d %H:%M:%S", tz=tz.ct)
wild3 <- wild3[order(wild3$Camera, wild3$Species, wild3$DateTime),]
wild3$index <- paste(wild3$Camera, wild3$Species); index <- unique(wild3$index)
wild3$delta.time.mins <- unlist(tapply(wild3$DateTime, INDEX = wild3$index,
                                       FUN = function(x) c(0, `units<-`(diff(x), "mins"))))

# loop to select max # species, behaviors seen within dependent sightings 
mintime <- ind.min; newdat <- NULL

for(i in 1:length(index)){
  sub <- wild3[wild3$index == index[i],]
  counter <- 0
  
  for(j in 1:nrow(sub)){
    sub$picgroup[j] <- ifelse(sub$delta.time.mins[j] > mintime, counter+1, counter)
    counter <- ifelse(sub$delta.time.mins[j] > mintime, counter+1, counter)
  }
  
  pics <- unique(sub$picgroup)
  for(k in 1:length(pics)){ 
    
    subpics <- sub[sub$picgroup == pics[k],]
    
    if(nrow(subpics) > 1){
      
      maxcount <- max(subpics$Count)
      maxyoung <- max(subpics$Juveniles); maxstand <- max(subpics$Standing)
      maxrest <- max(subpics$Resting); maxmove <- max(subpics$Moving)
      maxeat <- max(subpics$Eating); maxinter <- max(subpics$Interacting)
      maxmale <- max(subpics$Males)
      minevenness <- min(subpics$pielous_evenness_index)
      
      updat <- subpics[1,]
      updat$Count <- maxcount; updat$Juveniles <- maxyoung; updat$Standing <- maxstand
      updat$Resting <- maxrest; updat$Moving <- maxmove; updat$Eating <- maxeat
      updat$Interacting <- maxinter; updat$Males <- maxmale
      updat$pielous_evenness_index <- minevenness 
      
      newdat <- rbind(newdat, updat) 
      
    } else {
      newdat <- rbind(newdat, subpics)
    }
  }
}

wild3 <- newdat; rm(newdat)
wild3 <- wild3[year(wild3$DateTime) <= 2020,] #these records are corrected in 'additional data' files
wild3 <- wild3[wild3$pielous_evenness_index <= evenness_level,]
wild3 <- wild3 %>% select(-c(delta.time.mins, index, picgroup, pielous_evenness_index, Date, Time))

# add additional year 3 data
wild3.add <- read.csv("wildcam_year3_additionaldata.csv")
names(wild3.add) <- c("Camera", "Directory", "FileName", "DateTimeDelete", "DateTime", "Species", "Count", "Juveniles", "Moving", "Resting", "Eating", "Standing", "Interacting", "Males")

wild3.add <- wild3.add %>% select(-c(Directory, FileName, DateTimeDelete)) 
wild3.add$Species <- capitalize(as.character(wild3.add$Species))
wild3.add[wild3.add$Species == "WIldebeest",]$Species <- "Wildebeest"
wild3.add[wild3.add$Species == "Busbbuck",]$Species <- "Bushbuck"
wild3.add[wild3.add$Species == "Duiker",]$Species <- "Duiker_common"
wild3.add <- wild3.add[wild3.add$Species %in% species,]

#three baboon records where count == 0 or NA; fix at some point but remove now
wild3.add <- wild3.add[!is.na(wild3.add$Count),]
wild3.add <- wild3.add[!wild3.add$Count == 0,]

#NAs for three records where should be 0? 
wild3.add[is.na(wild3.add$Resting),]$Resting <- 0

# make juveniles and males present/absent instead of count
wild3.add$Juveniles <- ifelse(wild3.add$Juveniles > 0, 1, 0)
wild3.add$Males <- ifelse(wild3.add$Males > 0, 1, 0)

# calculate time difs 
wild3.add$DateTime <- strptime(wild3.add$DateTime, "%m/%d/%y %H:%M", tz=tz.ct)
wild3.add <- wild3.add[order(wild3.add$Camera, wild3.add$Species, wild3.add$DateTime),]
wild3.add$index <- paste(wild3.add$Camera, wild3.add$Species); index <- unique(wild3.add$index)
wild3.add$delta.time.mins <- unlist(tapply(wild3.add$DateTime, INDEX = wild3.add$index,
                                           FUN = function(x) c(0, `units<-`(diff(x), "mins"))))
mintime <- ind.min; newdat <- NULL

for(i in 1:length(index)){
  sub <- wild3.add[wild3.add$index == index[i],]
  counter <- 0
  
  for(j in 1:nrow(sub)){
    sub$picgroup[j] <- ifelse(sub$delta.time.mins[j] > mintime, counter+1, counter)
    counter <- ifelse(sub$delta.time.mins[j] > mintime, counter+1, counter)
  }
  
  pics <- unique(sub$picgroup)
  for(k in 1:length(pics)){ 
    
    subpics <- sub[sub$picgroup == pics[k],]
    
    if(nrow(subpics) > 1){
      
      maxcount <- max(subpics$Count)
      maxyoung <- max(subpics$Juveniles); maxstand <- max(subpics$Standing)
      maxrest <- max(subpics$Resting); maxmove <- max(subpics$Moving)
      maxeat <- max(subpics$Eating); maxinter <- max(subpics$Interacting)
      maxmale <- max(subpics$Males)
      
      updat <- subpics[1,]
      updat$Count <- maxcount; updat$Juveniles <- maxyoung; updat$Standing <- maxstand
      updat$Resting <- maxrest; updat$Moving <- maxmove; updat$Eating <- maxeat
      updat$Interacting <- maxinter; updat$Males <- maxmale
      
      newdat <- rbind(newdat, updat) 
      
    } else {
      newdat <- rbind(newdat, subpics)
    }
  }
}

wild3.add <- newdat; rm(newdat)
wild3.add <- wild3.add %>% select(-c(delta.time.mins, index, picgroup))
wild3 <- rbind(wild3, wild3.add)

# load year 4 data 
wild4 <- read.csv("wildcam_year4_wilddogdata.csv") %>% 
  select(-X) %>% 
  `colnames<-`(c("Camera", "Date", "Time", "Species", "Count", "Juveniles", "Moving", "Resting", "Eating",
                 "Standing", "Interacting", "Males")) 
wild4$DateTime <- strptime(paste(wild4$Date, wild4$Time), "%m/%d/%y %H:%M", tz=tz.ct)
wild4$Date <- NULL; wild4$Time <- NULL
wild4$Species <- capitalize(as.character(wild4$Species))
wild4[wild4$Species == "Mongooe",]$Species <- "Mongoose"
wild4[wild4$Species == "Bush",]$Species <- "Bushbuck"
wild4[wild4$Species == "Duiler",]$Species <- "Duiler"
wild4 <- wild4[wild4$Species %in% species,]

# make juveniles and males present/absent instead of count
wild4$Juveniles <- ifelse(wild4$Juveniles > 0, 1, 0)
wild4$Males <- ifelse(wild4$Males > 0, 1, 0)

# fix moving data 
wild4[wild4$Moving > 1,]$Moving <- 1 #check at some point whether actually juveniles? only 2 records

# add additional year 4 data 
wild4.add <- read.csv("wildcam_year4_additionaldata.csv") %>% 
  select(-c(Directory, FileName, DateTimeOriginal)) %>% 
  `colnames<-`(c("Camera", "DateTime", "Species", "Count", "Juveniles", "Moving", "Resting", "Eating",
                 "Standing", "Interacting", "Males")) 
wild4.add$Species <- capitalize(as.character(wild4.add$Species))
wild4.add <- wild4.add[wild4.add$Species %in% species,]
wild4.add$DateTime <- strptime(wild4.add$DateTime, "%m/%d/%y %H:%M")

# make juveniles and males present/absent instead of count
wild4.add$Juveniles <- ifelse(wild4.add$Juveniles > 0, 1, 0)
wild4.add$Males <- ifelse(wild4.add$Males > 0, 1, 0)

# data cleaning...
wild4.add[is.na(wild4.add$Resting),]$Resting <- 0

# calculate time difs 
wild4.add <- wild4.add[order(wild4.add$Camera, wild4.add$Species, wild4.add$DateTime),]
wild4.add$index <- paste(wild4.add$Camera, wild4.add$Species); index <- unique(wild4.add$index)
wild4.add$delta.time.mins <- unlist(tapply(wild4.add$DateTime, INDEX = wild4.add$index,
                                           FUN = function(x) c(0, `units<-`(diff(x), "mins"))))
mintime <- ind.min; newdat <- NULL

for(i in 1:length(index)){
  sub <- wild4.add[wild4.add$index == index[i],]
  counter <- 0
  
  for(j in 1:nrow(sub)){
    sub$picgroup[j] <- ifelse(sub$delta.time.mins[j] > mintime, counter+1, counter)
    counter <- ifelse(sub$delta.time.mins[j] > mintime, counter+1, counter)
  }
  
  pics <- unique(sub$picgroup)
  for(k in 1:length(pics)){ 
    
    subpics <- sub[sub$picgroup == pics[k],]
    
    if(nrow(subpics) > 1){
      
      maxcount <- max(subpics$Count)
      maxyoung <- max(subpics$Juveniles); maxstand <- max(subpics$Standing)
      maxrest <- max(subpics$Resting); maxmove <- max(subpics$Moving)
      maxeat <- max(subpics$Eating); maxinter <- max(subpics$Interacting)
      maxmale <- max(subpics$Males)
      
      updat <- subpics[1,]
      updat$Count <- maxcount; updat$Juveniles <- maxyoung; updat$Standing <- maxstand
      updat$Resting <- maxrest; updat$Moving <- maxmove; updat$Eating <- maxeat
      updat$Interacting <- maxinter; updat$Males <- maxmale
      
      newdat <- rbind(newdat, updat) 
      
    } else {
      newdat <- rbind(newdat, subpics)
    }
  }
}

wild4.add <- newdat; rm(newdat)
wild4.add <- wild4.add %>% select(-c(delta.time.mins, index, picgroup))

wild4 <- rbind(wild4, wild4.add)
wild3.4 <- rbind(wild3, wild4)

# combine two species data sets 
wild3.4$Camera <- as.character(wild3.4$Camera); wild1.2$Camera <- as.character(wild1.2$Camera)
wild1.2$Species <- as.character(wild1.2$Species)
wild1.2$DateTime <- as.character(wild1.2$DateTime); wild3.4$DateTime <- as.character(wild3.4$DateTime)

wild.dat <-  dplyr::bind_rows(wild1.2, wild3.4)

# add metadata
wild.dat$season <- ifelse(month(wild.dat$DateTime) %in% c(4:7), "Early Dry", 
                          ifelse(month(wild.dat$DateTime) %in% c(8:11), "Late Dry", 
                                 ifelse(month(wild.dat$DateTime) %in% c(12,1:3), "Wet", "ERROR")))

covs <- read.csv("../../WildCam Platform Materials/Data for WildCam Classroom/cam_metadata_fromfield_and_raw_raster.csv")
names(covs)[1] <- "Camera"
covs <- covs[c("Camera", "Latitude", "Longitude", "height.cm", "angle", "detect.obscured", "cover.ground", "termite.count.100m", "habitat.number", "habitat.type", "boundary_dist", "chitengo_dist", "fire_frequency", "pans_dist", "river_dist", "road_dist", "settlement_dist", "tree_1km", "urema_dist")]
wild.dat <- merge(wild.dat, covs, all.x=T)

write.csv(wild.dat, "WildCamData_WildDog_Formatted.csv", row.names=F)


## search effort ----------------------

# load year 1, 2, and 3 search effort --> reate sequences of operational dates 
effort1 <- read.csv("Camera_operation_years1and2.csv")
effort2 <- read.csv("Camera_operation_year3.csv") %>% 
  select(-c(Notes, X, X.1, X.2)) %>% 
  mutate(Problem2_from = "", Problem2_to = "", Problem3_from = "", Problem3_to = "")
effort <- rbind(effort1, effort2)

datify <- function(x){strptime(x, "%m/%d/%y", tz=tz.ct)}

effort123 <- NULL 
for(i in 1:nrow(effort)){
  sub <- effort[i,]; sub <- sub[!sapply(sub, function(x) all(x == ""))]

  if(length(sub) == 3){
    sub.seq <- seq(datify(sub$Start), datify(sub$End), 'days')
    } else if(length(sub) == 5){
      sub.seq <- c(seq(datify(sub$Start), datify(sub$Problem1_from),'days'), 
                   seq(datify(sub$Problem1_to), datify(sub$End),'days'))
      } else if(length(sub) == 7){ 
        sub.seq <- c(seq(datify(sub$Start), datify(sub$Problem1_from), 'days'), 
                     seq(datify(sub$Problem1_to), datify(sub$Problem2_from), 'days'), 
                     seq(datify(sub$Problem2_to), datify(sub$End),'days'))
        } else if(length(sub) == 9){
          sub.seq <- c(seq(datify(sub$Start), datify(sub$Problem1_from), 'days'), 
                       seq(datify(sub$Problem1_to), datify(sub$Problem2_from), 'days'), 
                       seq(datify(sub$Problem2_to), datify(sub$Problem3_from), 'days'),
                       seq(datify(sub$Problem3_to), datify(sub$End), 'days'))
        }
  
  effort.frame <- data.frame(site = as.character(sub$Camera), date = sub.seq)
  effort123 <- rbind(effort123, effort.frame)
}
rm(effort); rm(effort1); rm(effort2)

# year 4 search effort (hack for now)
effort1 <- read.csv("wildcam_year4_wilddogdata.csv") %>% 
  select(Camera, capture_date)
effort1 <- effort1[!duplicated(effort1),]
effort2 <- read.csv("wildcam_year4_additionaldata.csv") %>% 
  select(site, DateTimeCorrected) %>% 
  `colnames<-`(c("Camera", "capture_date")) 
effort2$capture_date <- gsub( " .*$", "", effort2$capture_date)
effort2 <- effort2[!duplicated(effort2),]
effort <- rbind(effort1, effort2); rm(effort1); rm(effort2)
effort$capture_date <- strptime(effort$capture_date, "%m/%d/%y", tz=tz.ct)
effort[year(effort$capture_date) > 2020,] #none 

# if camera goes >14 days without capture, consider camera "off" during that period
effort <- effort[order(effort$Camera, effort$capture_date),]
effort$delta.time.days <- unlist(tapply(effort$capture_date, INDEX = effort$Camera,
                                        FUN = function(x) c(0, `units<-`(diff(x), "days"))))
effort[effort$delta.time.days > 14,] #J03

# J03 does have a gap in camera trap effort of ~6 mo. --> manually classify as J03A and J03B

sites <- unique(effort$Camera)
effort.4 <- NULL
for(i in 1:length(sites)){
  sub <- effort[effort$Camera == sites[i],]
  min.date <- min(sub$capture_date)
  max.date <- max(sub$capture_date)
  sub.df <- data.frame(site = as.character(sites[i]), min.date = min.date, max.date = max.date)
  effort.4 <- rbind(effort.4, sub.df)
}

# expand 
effort4 <- NULL
for(i in 1:nrow(effort.4)){
  subseq <- seq(effort.4$min.date[i], effort.4$max.date[i], by='days')
  sub.df <- data.frame(site = as.character(effort.4$site[i]), 
                       date = subseq)
  effort4 <- rbind(effort4, sub.df)
}
effort4$site <- as.character(effort4$site)
effort4[effort4$site %in% c("J03A", "JO3B"),]$site <- "J03"

# combine two effort data sets 
effort <- rbind(effort123, effort4)
max(effort$date); min(effort$date) 

# fix effort 
effort$season <- ifelse(month(effort$date) %in% c(4:7), "Early Dry", 
                        ifelse(month(effort$date) %in% c(8:11), "Late Dry", 
                               ifelse(month(effort$date) %in% c(12,1:3), "Wet", "ERROR")))

write.csv(effort, "WildCamEffort_WildDog_Formatted.csv", row.names=F)
