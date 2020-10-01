#############################################
## Calculating circadian activity patterns ## 
#############################################

# Analysis of Gorongosa Park camera trap data looking to see whether wild dog 
# release affected the daily activity patterns of ungulate species across 
# different areas of wild dog use

# M.S.Palmer 29 Sep 2020

# set workspace 
rm(list=ls())
setwd("~/Desktop/Grad School/Post doc_Princeton/Mozambique/Data_Kaitlyn/Camera Trap Data/Processed camera data")

# load libraries 
library(overlap); library(sp); library(dplyr); library(lubridate); library(sf); library(activity)
library(pryr); library(ggplot2); library(Hmisc); library(tidyverse); library(Rmisc); library(mgcv)
library(MuMIn); library(stringr); library(DHARMa); library(plyr); library(errors); library(glmmTMB)
library(MASS)
#library(mgcViz) <-- cannot get this package to load, but think would have cool plotting features if worked

# load data
dat <- read.csv("WildCamData_WildDog_Formatted.csv") 
names(dat)[c(12, 15:19, 27, 29, 30)] <- c("Season", "Height.cm", "Angle", "Detect.Obsc", "Ground.Cov", "Term.Count", "Road.Dist", "Tree.Density.1km", "Urema.Dist")
dat <- dat[c("Camera", "Species", "DateTime", "Season", "Tree.Density.1km", "Road.Dist", "Urema.Dist", "Height.cm", "Angle", "Detect.Obsc", "Ground.Cov", "Term.Count")]

# select species
species.list <- c("Bushbuck", "Reedbuck", "Nyala", "Oribi", "Impala", "Waterbuck", "Warthog", "Kudu", "Buffalo", "Bushpig", "Elephant", "Wildebeest", "Duiker", "Hartebeest", "Eland")

# info for formatting data-time 
tz.ct <- "Africa/Maputo"
coords <- matrix(c(34.50, -18.82), nrow=1) %>%
  sp::SpatialPoints(proj4string=sp::CRS("+proj=longlat +datum=WGS84"))

# wild dog release date 
wd.release <- strptime("2018-06-16", "%Y-%m-%d", tz=tz.ct)

# areas of wild dog activity 
high.use <- c("C08", "D03", "D05", "D07", "D09", "E04", "E06", "E08", "E10", "F03", "F05", "F07", "F09", "F11", "G04", "G06", "G08", "G10")
low.use <- c("A06", "A08", "A10", "B05", "B07", "B09", "C06", "H05", "H07", "H09")
no.use <- c("E02", "E12", "F01", "G02", "G12", "H03", "H11", "H13", "I04", "I06", "I08", "I10", "I12", "I14", "J03", "J05", "J07", "J09", "J11", "J13", "K04", "K06", "K08", "K10", "K12", "L05", "L07", "L09", "L11", "L13", "M08", "M10") 

# format date-time, sunrise/sunset, pre-/post-release designation 
dat <- dat %>% 
  mutate(DateTime = as.POSIXct(DateTime, "%m/%d/%y %H:%M", tz=tz.ct), 
         Time.Decimal = hour(DateTime) + minute(DateTime)/60 + second(DateTime)/3600, 
         Time.Radians = (Time.Decimal / 24) * 2 * pi, 
         Time.Sun = sunTime(Time.Radians, DateTime, coords), 
         Sun.Hour = (Time.Sun * 24) / (2 * pi),
         Sun.Hour = floor(Sun.Hour),
         Year = year(DateTime),
         TimePeriod = factor(ifelse(DateTime < wd.release, "PreRelease", "PostRelease")), 
         WildDogUse = factor(ifelse(Camera %in% high.use, "high.use", 
                                    ifelse(Camera %in% low.use,"low.use", "no.use"))), 
         Season.Year = paste(Season, Year), 
         s.Tree.Density.1km = scale(Tree.Density.1km), 
         s.Urema.Dist = scale(Urema.Dist), 
         s.Road.Dist = scale(Road.Dist), 
         s.Termite = scale(Term.Count),
         s.Height = scale(Height.cm),
         s.Angle = scale(Angle),
         Detect.Obsc = factor(Detect.Obsc),
         s.Ground.Cov = scale(Ground.Cov)) %>%
  filter(Species %in% species.list)
dat$Date <- gsub( " .*$", "", dat$DateTime) #doesn't run in pipe for some reason
dat$Sun.Hour <- floor(dat$Sun.Hour) #doesn't run in pipe for some reason

# re-order levels
dat$TimePeriod <- factor(dat$TimePeriod, levels = c("PreRelease", "PostRelease"))
dat$WildDogUse <- factor(dat$WildDogUse, levels = c("no.use", "low.use", "high.use"))

# as wet season runs Dec-Mar, add Dec to "next years'" wet season 
dat$Season.Year <- ifelse((dat$Season == "Wet" & month(dat$DateTime) == 12), 
                          paste("Wet", (year(dat$DateTime)+1)), dat$Season.Year)

# disgard 2018 early dry (transition period), 2016 early dry (incomplete season) 
dat <- dat[!(dat$Season == "Early Dry" & year(dat$DateTime) %in% c("2016", "2018")),] 



### analyses ------------------

## 1) calculating circadian activity distributions and evaluating whether daily patterns 
## of activity change after wild dog release 

# this function requires that both before and after data include at least 10 observations 
act.diel.difs <- function(dat, species, wild.dog.use, nboots){
    
    # calculate activity 
    before <- dat[(dat$Species == species & dat$WildDogUse == wild.dog.use & dat$TimePeriod == "PreRelease"),]
    after <- dat[(dat$Species == species & dat$WildDogUse == wild.dog.use & dat$TimePeriod == "PostRelease"),]
    
    if(nrow(before) & nrow(after) >= 10){
      wts.before <- 1/ifelse(before$Time.Sun>pi/2 & before$Time.Sun<pi*3/2, 1.5, 1)
      wts.after <- 1/ifelse(after$Time.Sun>pi/2 & after$Time.Sun<pi*3/2, 1.5, 1)
      
      before.fit <- fitact(before$Time.Sun, sample="model", reps=nboots, wt=wts.before)
      after.fit <- fitact(after$Time.Sun, sample="model", reps=nboots, wt=wts.after)
      
      # differences in diel patterning before and after 
      diel.dif <- as.data.frame(t(compareCkern(before.fit, after.fit, reps=nboots)))
      diel.dif$Species <- as.character(species)
      diel.dif$WildDogUse <- wild.dog.use
      diel.dif <- diel.dif[c(5:6,1:4)]
      names(diel.dif)[3:6] <- c("Obs. Overlap Index", "Null Mean Overlap Index", "SE Null", 
                                "Prob Obs. Index Chance")
      diel.dif[,c(3:6)] <- round(diel.dif[,c(3:6)], 3)
      diel.dif[c(1:6)] <- sapply(diel.dif[c(1:6)], as.character)
      
    } else {
      diel.dif <- data.frame(Species = as.character(species), WildDogUse = as.character(wild.dog.use), 
                             `Obs. Overlap Index` = "Not Enough Data", `Null Mean Overlap Index` = "NA",
                             `SE Null`= 'NA', `Prob Obs. Index Chance` = "NA")
      names(diel.dif)[c(3:6)] <- c("Obs. Overlap Index", "Null Mean Overlap Index", "SE Null",
                                   "Prob Obs. Index Chance")
    }
    return(diel.dif) 
}

# loop through species
nboots <- 1000 
diel.diffs <- data.frame()
seasons <- unique(dat$Season)

for(i in 1:length(species.list)){
  
  for(j in 1:length(unique(seasons))){
    sub <- dat[dat$Season == seasons[j],]
    
    # no use, low use, high use 
    act.list.no <- act.diel.difs(sub, species.list[i], "no.use", nboots)
    act.list.no$Season <- as.character(seasons[j])
    act.list.low <- act.diel.difs(sub, species.list[i], "low.use", nboots)
    act.list.low$Season <- as.character(seasons[j])
    act.list.high <- act.diel.difs(sub, species.list[i], "high.use", nboots)
    act.list.high$Season <- as.character(seasons[j])
    
    diel.diffs <- rbind(diel.diffs, act.list.no, act.list.low, act.list.high)
  }
}


## 2) evaluating whether shifts were specifically driven by wild dog activity (GAMs)

dat.gam <- ddply(dat, .(Species, Season, Year, TimePeriod, WildDogUse, Sun.Hour), summarise, amt.activity=sum(tag)) 
dat.gam.all <- ddply(dat.gam, .(Species,Season,Year,TimePeriod,WildDogUse), summarise, daily.act=sum(amt.activity))
dat.gam <- merge(dat.gam, dat.gam.all, all.x=T)
dat.gam$prop.act <- dat.gam$amt.activity/dat.gam$daily.act
dat.gam$day.night <- ifelse(dat.gam$Sun.Hour >= 6 & dat.gam$Sun.Hour <= 18, "day", "night") #ct visibility 
dat.gam$TimePeriod <- factor(dat.gam$TimePeriod, levels = c("PreRelease", "PostRelease")) #reoder levels 
dat.gam$WildDogUse <- factor(dat.gam$WildDogUse, levels = c("no.use", "low.use", "high.use")) #reorder levels 
dat.gam <- dat.gam %>% mutate(WD = interaction(WildDogUse, TimePeriod)) #specify interaction term 

# look through species 
gam.mods <- NULL 
for(i in 1:length(species.list)){
  
  m3c <- gam(prop.act ~ s(Sun.Hour, by=WD, bs="cc") +
               WildDogUse * TimePeriod + Season + day.night, 
             family = quasibinomial, 
             data = dat.3.3[dat.3.3$Species == species,])
  
  #look at residuals 
  gam.check(m3c); acf(residuals(m3c))
  
  # coerce model output into nice data frame 
  mod.out <- data.frame(summary(m3c)[6]$coefficients$cond)
  mod.out$Coefficient <- row.names(mod.out)
  row.names(mod.out) <- c()
  mod.out[,c(1:2,4)] <- round(mod.out[,c(1:2,4)], 3)
  mod.out[,3] <- round(mod.out[,3],2)
  names(mod.out)[2:4] <- c("SE", "z value", "p value")
  mod.out$Species <- as.character(species.list[i])
  mod.out <- mod.out[,c(6,5,1:4)]
  
  gam.mods <- rbind(gam.mods, mod.out)
}


## 3) evaluating whether diel activity  shifts minimized temporal overlap with wild dogs

mod.out <- NULL
for(i in 1:length(species.list)){
  m.morn <- glm(prop.act ~ WildDogUse * TimePeriod + Season, family = "quasibinomial", 
                data = dat.3.3[dat.3.3$Sun.Hour %in% c(5:8) & dat.gam$Species == species,])
  
  m.even <- glm(prop.act ~ WildDogUse * TimePeriod + Season, family = "quasibinomial", 
                data = dat.3.3[dat.3.3$Sun.Hour %in% c(16:19) & dat.gam$Species == species,])
  
  # save results 
  mod.m <- data.frame(summary(m.morn)[12])
  mod.m$Coefficient <- row.names(mod.m)
  row.names(mod.m) <- c()
  mod.m[,c(1:2,4)] <- round(mod.m[,c(1:2,4)], 3)
  mod.m[,3] <- round(mod.m[,3],2)
  names(mod.m)[1:4] <- c("Estimate", "SE", "t value", "p value")
  mod.m$Species <- as.character(species.list[i])
  mod.m$TimePeriod <- "Morning"
  mod.m <- mod.m[,c(6,7,5,1:4)]
  
  mod.e <- data.frame(summary(m.even)[12])
  mod.e$Coefficient <- row.names(mod.e)
  row.names(mod.e) <- c()
  mod.e[,c(1:2,4)] <- round(mod.e[,c(1:2,4)], 3)
  mod.e[,3] <- round(mod.e[,3],2)
  names(mod.e)[1:4] <- c("Estimate", "SE", "t value", "p value")
  mod.e$Species <- as.character(species.list[i])
  mod.e$TimePeriod <- "Evening"
  mod.e <- mod.e[,c(6,7,5,1:4)]
  
  mod.out <- rbind(mod.out, mod.m, mod.e)
}


## Q: do species traits predict magnitude of diel shifts in activity pattern? 

# load species traits 
species.traits <- read.csv("../../Analyses and Scripts/PROJECT_GNP_WildDogs/Gorongosa_Species_Traits.csv")
names(species.traits)[1] <- "Species"
species.traits$Digestive.System <- ifelse(species.traits$Digestive.System == "Ruminant", "Ruminant", "Non-ruminant")
species.traits$s.Avg.Body.Mass <- scale(species.traits$Avg.Body.Mass)
species.traits$WD.Index <- as.numeric(gsub( " .*$", "", species.traits$WD.Preference.Index))
species.traits$s.Group.Size <- scale(species.traits$Social.Group.Size)
species.traits$s.Interbirth <- scale(species.traits$Interbirth.Interval..d.)
species.traits$sLitter <- scale(species.traits$Litter.Size)
species.traits <- species.traits[c("Species", "Order", "Family", "Digestive.System", "Feeding.Guild", "WD.Index",
                                   "s.Avg.Body.Mass", "s.Group.Size", "s.Interbirth", "sLitter")]

# calculate index of overlap between circular distributions and associated error 
nboots <- 1000 
full.df <- data.frame()
seasons <- unique(dat$Season)

for(i in 1:length(species.list)){
  
  max.sub.df <- NULL
  
  for(j in 1:length(unique(seasons))){
    
    sub <- dat[dat$Season == seasons[j] & dat$Species == species.list[i],]
    sub.before <- sub[sub$TimePeriod == "PreRelease",]
    sub.after <- sub[sub$TimePeriod == "PostRelease",]
    amt.data <- data.frame(table(sub$TimePeriod, sub$WildDogUse))
    
    # no use
    if(amt.data[1,3] <= 10 | amt.data[2,3] <= 10){
      mean.no <- "not enough data"; sd.no <- "not enough data"; se.no <- "not enough data"
      
    } else if(amt.data[1,3] <= 75 | amt.data[1,3] <= 75){
      boots.no <- bootstrap(sub.before[sub.before$WildDogUse == "no.use",]$Time.Sun, 
                            sub.after[sub.after$WildDogUse == "no.use",]$Time.Sun, 
                            nboots, type="Dhat1") 
      mean.no <- as.character(mean(boots.no)); sd.no <- as.character(sd(boots.no))
      se.no <- as.character(sd(boots.no)/sqrt(length(boots.no)))
      
    } else {
      boots.no <- bootstrap(sub.before[sub.before$WildDogUse == "no.use",]$Time.Sun, 
                            sub.after[sub.after$WildDogUse == "no.use",]$Time.Sun, 
                            nboots, type="Dhat4") 
      mean.no <- as.character(mean(boots.no)); sd.no <- as.character(sd(boots.no))
      se.no <- as.character(sd(boots.no)/sqrt(length(boots.no)))
    }
    
    # low use 
    if(amt.data[3,3] <= 10 | amt.data[4,3] <= 10){
      mean.low <- "not enough data"; sd.low <- "not enough data"; se.low <- "not enough data"
      
    } else if(amt.data[3,3] <= 75 | amt.data[4,3] <= 75){
      
      boots.low <- bootstrap(sub.before[sub.before$WildDogUse == "low.use",]$Time.Sun, 
                             sub.after[sub.after$WildDogUse == "low.use",]$Time.Sun, 
                             nboots, type="Dhat1") 
      mean.low <- as.character(mean(boots.low)); sd.low <- as.character(sd(boots.low))
      se.low <- as.character(sd(boots.low)/sqrt(length(boots.low)))
      
    } else {
      boots.low <- bootstrap(sub.before[sub.before$WildDogUse == "low.use",]$Time.Sun, 
                             sub.after[sub.after$WildDogUse == "low.use",]$Time.Sun, 
                             nboots, type="Dhat4") 
      mean.low <- as.character(mean(boots.low)); sd.low <- as.character(sd(boots.low))
      se.low <- as.character(sd(boots.low)/sqrt(length(boots.low)))
    }
    
    # high use 
    if(amt.data[5,3] <= 10 | amt.data[6,3] <= 10){
      mean.high <- "not enough data"; sd.high <- "not enough data"; se.high <- "not enough data"
      
    } else if(amt.data[5,3] <= 75 | amt.data[6,3] <= 75){
      boots.high <- bootstrap(sub.before[sub.before$WildDogUse == "high.use",]$Time.Sun, 
                              sub.after[sub.after$WildDogUse == "high.use",]$Time.Sun, 
                              nboots, type="Dhat1") 
      mean.high <- as.character(mean(boots.high)); sd.high <- as.character(sd(boots.high))
      se.high <- as.character(sd(boots.high)/sqrt(length(boots.high)))
      
    } else {
      boots.high <- bootstrap(sub.before[sub.before$WildDogUse == "high.use",]$Time.Sun, 
                              sub.after[sub.after$WildDogUse == "high.use",]$Time.Sun, 
                              nboots, type="Dhat4") 
      mean.high <- as.character(mean(boots.high)); sd.high <- as.character(sd(boots.high))
      se.high <- as.character(sd(boots.high)/sqrt(length(boots.high)))
    }
    
    sub.df <- data.frame(species = as.character(species.list[i]),
                         season = as.character(seasons[j]), 
                         wilddoguse = c("no.use", "low.use", "high.use"),
                         overlap = c(mean.no, mean.low, mean.high), 
                         sd = c(sd.no, sd.low, sd.high), 
                         se = c(se.no, se.low, se.high))
    
    max.sub.df <- rbind(max.sub.df, sub.df)
  }
  
  full.df <- rbind(full.df, max.sub.df)
}

# save files 
write.csv(full.df, "bootstrap_overlap.csv", row.names=F)
overlap.dat <- read.csv("bootstrap_overlap.csv")
names(overlap.dat)[1] <- "Species"

# merge with species traits 
overlap.dat <- merge(overlap.dat, species.traits, all.x=T)

overlap.dat$wilddoguse <- factor(overlap.dat$wilddoguse, levels = c("no.use", "low.use", "high.use"))

# regress overlap with traits 
overlap.dat <- overlap.dat[!overlap.dat$overlap == "not enough data",]
overlap.dat[4:6] <- sapply(overlap.dat[4:6], as.character)
overlap.dat[4:6] <- sapply(overlap.dat[4:6], as.numeric)

# add errors back to response variable 
errors(overlap.dat$overlap) <- overlap.dat$se

m1 <- glm(overlap ~ wilddoguse + season + Feeding.Guild + s.Avg.Body.Mass + 
            I(s.Avg.Body.Mass^2) + WD.Index + s.Group.Size + s.Interbirth + sLitter, 
          family = quasibinomial, 
          data = overlap.dat)

# save output
mod.out <- data.frame(summary(m1)[12])
mod.out$Coefficient <- row.names(mod.out)
row.names(mod.out) <- c()
mod.out[,c(1:2,4)] <- round(mod.out[,c(1:2,4)], 3)
mod.out[,3] <- round(mod.out[,3],2)
names(mod.out)[1:4] <- c("Estimate", "SE", "t value", "p value")
mod.out <- mod.out[,c(5,1:4)]
write.csv(mod.out, "../../Analyses and Scripts/PROJECT_GNP_WildDogs/Traits_Overlap.csv", row.names=F)
