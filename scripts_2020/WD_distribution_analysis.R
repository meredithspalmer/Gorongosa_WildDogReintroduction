########################################################################
## Calculating landscape use (relative activity across the landscape) ## 
## in GNP before and after wild dog release ############################
########################################################################

## M.S.Palmer 29-Sep-2020

# set workspace 
rm(list=ls())
setwd("~/Desktop/Grad School/Post doc_Princeton/Mozambique/Data_Kaitlyn/Camera Trap Data/Processed camera data")

# load functions and libraries 
library(ggplot2); library(unmarked); library(Hmisc); library(dplyr); library(effects); library(plyr)
library(sp); library(rgdal); library (rgeos); library(maptools); library(viridis); library(lubridate)
library(glmmTMB); library(ggpubr)

# species of interest 
species.list <- c("Bushbuck", "Reedbuck", "Nyala", "Oribi", "Impala", "Waterbuck", "Warthog", "Kudu", "Buffalo", "Bushpig", "Elephant", "Wildebeest", "Eland", "Duiker", "Hartebeest")

# wild dog release date 
tz.ct <- "Africa/Maputo"
wd.release <- strptime("2018-06-16", "%Y-%m-%d", tz=tz.ct)

# areas of wild dog activity 
high.use <- c("C08", "D03", "D05", "D07", "D09", "E04", "E06", "E08", "E10", "F03", "F05", "F07", "F09", "F11", "G04", "G06", "G08", "G10")
low.use <- c("A06", "A08", "A10", "B05", "B07", "B09", "C06", "H05", "H07", "H09")
no.use <- c("E02", "E12", "F01", "G02", "G12", "H03", "H11", "H13", "I04", "I06", "I08", "I10", "I12", "I14", "J03", "J05", "J07", "J09", "J11", "J13", "K04", "K06", "K08", "K10", "K12", "L05", "L07", "L09", "L11", "L13", "M08", "M10") 

## records data and search effort 

# load and format data 
dat <- read.csv("WildCamData_WildDog_Formatted.csv") %>% 
  mutate(Date = as.POSIXct(strptime(DateTime, "%m/%d/%y")), 
         Week = week(Date), 
         Week.Year = as.numeric(paste(year(Date), Week, sep="")), 
         Season.Year = paste(season, year(Date)), 
         TimePeriod = ifelse(Date < wd.release, "PreRelease", "PostRelease"), 
         WildDogUse = ifelse(Camera %in% high.use, "high.use", ifelse(Camera %in% low.use, "low.use", "no.use")), 
         occu = 1)
dat$Season.Year <- ifelse((dat$season == "Wet" & month(dat$Date) == 12), paste("Wet", (year(dat$Date)+1)), dat$Season.Year)

effort <- read.csv("WildCamEffort_WildDog_Formatted.csv") %>% 
  mutate(date = as.POSIXct(strptime(date, "%m/%d/%y")), 
         Week = week(date), 
         Week.Year = as.numeric(paste(year(date), week(date), sep="")), 
         Season.Year = paste(season, year(date)),
         TimePeriod = ifelse(date < wd.release, "PreRelease", "PostRelease"))
effort$Season.Year <- ifelse((effort$season == "Wet" & month(effort$date) == 12), paste("Wet", (year(effort$date)+1)), effort$Season.Year)
names(effort)[1:2] <- c("Camera","Date")

# disgard 2018 early dry (transition period), 2016 early dry (incomplete season) 
dat <- dat[!(dat$season == "Early Dry" & year(dat$Date) %in% c("2016", "2018")),] 
effort <- effort[!(effort$season == "Early Dry" & year(effort$Date) %in% c("2016", "2018")),]

## covariates 
covs <- dat[c("Camera", "height.cm", "angle", "detect.obscured", "cover.ground", "termite.count.100m",
              "boundary_dist", "chitengo_dist", "fire_frequency", "pans_dist", "river_dist", "road_dist",
              "settlement_dist", "tree_1km", "urema_dist", "WildDogUse")]
covs <- covs[!duplicated(covs),]

covs <- covs %>%
  mutate(s.height = scale(height.cm), s.angle = scale(angle), s.termite = scale(termite.count.100m),
         s.boundary = scale(boundary_dist), s.chitengo = scale(chitengo_dist), s.pans = scale(pans_dist),
         s.fire = scale(fire_frequency), s.river = scale(river_dist), s.road = scale(road_dist), 
         s.settlement = scale(settlement_dist), s.tree = scale(tree_1km), s.urema = scale(urema_dist), 
         detect.obscured = factor(detect.obscured), cover.ground = scale(cover.ground), 
         WildDogUse = factor(WildDogUse)) 

# check correlations 
res <- rcorr(as.matrix(covs[,c(17:28)]), type="pearson")
round(res$r,3)
round(res$P,3)
plot(covs$WildDogUse, covs$s.road)
plot(covs$WildDogUse, covs$s.urema)
# --> many correlated variables: select only those with no significant correlation or where slope of significant correlation is less than abs(0.1)
# remove 'boundary', 'chitengo', 'fire', 'settlement', 'pans', 'river' in favor of 'urema', 'road', 'tree' 

covs <- covs[c("Camera", "s.height", "s.angle", "detect.obscured", "WildDogUse", "cover.ground", "s.termite",
               "s.urema", "s.tree", "s.road")]

## shapefiles 

#full grid 
grid <- readOGR("../../Analyses and Scripts/PROJECT_GNP_WildDogs/CameraGridHexes2.shp")
names(grid)[6] <- "Camera"

#wild dog use high
wd_hi <- readOGR("../../Analyses and Scripts/PROJECT_GNP_WildDogs/GNP wild dog shapefiles/wd_highuse_hexes.shp")
wd_hi <- fortify(wd_hi)

#wild dog use low
wd_lo_1 <- readOGR("../../Analyses and Scripts/PROJECT_GNP_WildDogs/GNP wild dog shapefiles/wd_lowuse1_hexes.shp")
wd_lo_1 <- fortify(wd_lo_1)
wd_lo_2 <- readOGR("../../Analyses and Scripts/PROJECT_GNP_WildDogs/GNP wild dog shapefiles/wd_lowuse2_hexes.shp")
wd_lo_2 <- fortify(wd_lo_2)

#wild do no use
wd_no_1 <- readOGR("../../Analyses and Scripts/PROJECT_GNP_WildDogs/GNP wild dog shapefiles/wd_nouse1_hexes.shp")
wd_no_1 <- fortify(wd_no_1)
wd_no_2 <- readOGR("../../Analyses and Scripts/PROJECT_GNP_WildDogs/GNP wild dog shapefiles/wd_nouse2_hexes.shp")
wd_no_2 <- fortify(wd_no_2)


## Relative activity index ----------------

# While hierarchical occupancy modeling accounts for imperfect detection of species, it only addresses presence or absence and thus fails to account for differences in species abundance. For common species that are widespread in space, a comparison of relative activity across camera sites may provide greater insight into seasonal dynamics of space use.

# When summing data across week-long windows, 3 possible approaches: 
# - 1/0 seen in the week (traditional OM)
# - total number of detections per week
# - total count of animals seen per week (count individuals within detections)
# --> here, focus on later two 

# Use week-long windows; assume seasons are closed 

# format data 
dat$Date <- as.character(dat$Date)
RAI.dat <- ddply(dat, .(Species, Camera, Week.Year, season, Season.Year, TimePeriod), summarise, 
                 tot.occu = sum(occu), 
                 tot.count = sum(Count))

effort$Date <- as.character(effort$Date); effort$on <- 1
RAI.effort <- ddply(effort, .(Camera, Week.Year, season, Season.Year), summarise,
                  Days.On = sum(on))

full.RAI <- NULL
for(i in 1:length(species.list)){
  sub <- RAI.dat[RAI.dat$Species == species.list[i],]
  sub <- merge(RAI.effort, sub, all.x=T)
  sub$Species <- as.character(species.list[i])
  sub$TimePeriod <- NULL
  sub[is.na(sub)] <- 0
  full.RAI <- rbind(full.RAI, sub)
}
RAI.dat <- full.RAI; rm(full.RAI)

# add covs back in 
RAI.dat <- merge(RAI.dat, covs, all.x=T) %>% 
  mutate(TimePeriod = ifelse(Season.Year %in% c("Early Dry 2017", "Late Dry 2016", "Late Dry 2017", "Wet 2017", "Wet 2018"), "PreRelease", "PostRelease"))

# re-order levels 
RAI.dat$TimePeriod <- factor(RAI.dat$TimePeriod, levels = c("PreRelease", "PostRelease"))
RAI.dat$WildDogUse <- factor(RAI.dat$WildDogUse, levels = c("no.use", "low.use", "high.use"))


## Calculate hierarchical models for each camera using DETECTIONS and COUNTS 

hist(RAI.dat$tot.count); hist(RAI.dat$tot.occu) # likely zero-inflated and/or negative binomial
# --> did model comparison with AIC for combinations of zi/no zi and poisson, nb1, nb2; best fit is nbinom1 with zi 

## detections: 
detect.output <- NULL
detect.interactions <- NULL
detect.pred <- NULL

for(i in 1:length(species.list)){
  
  m1 <- glmmTMB(tot.occu ~ WildDogUse*TimePeriod + # wild dog covariates 
                  s.termite + s.urema + s.road + s.tree + # other spatial covariates
                  season + # other temporal covariates 
                  s.height + s.angle + detect.obscured + # detection parameters 
                  offset(log(Days.On)) + # offset for number of days each camera active 
                  (1|Camera), # random effect for camera trap site
                ziformula = ~1, family = nbinom1, 
                data = RAI.dat[RAI.dat$Species == species.list[i],])
  
  # coerce model output into nice data frame 
  mod.out <- data.frame(summary(m1)[6]$coefficients$cond)
  mod.out$Coefficient <- row.names(mod.out)
  row.names(mod.out) <- c()
  mod.out[,c(1:2,4)] <- round(mod.out[,c(1:2,4)], 3)
  mod.out[,3] <- round(mod.out[,3],2)
  names(mod.out)[2:4] <- c("SE", "z value", "p value")
  mod.out$Species <- as.character(species.list[i])
  mod.out <- mod.out[,c(6,5,1:4)]
  
  # generate confidence intervals 
  cis <- data.frame(confint(m1)) #wald/delta-method CI
  cis$Coefficient <- row.names(cis); row.names(cis) <- c()
  cis$Coefficient <- gsub("cond.", "", cis$Coefficient)
  cis <- cis[!cis$Coefficient %in% c("Camera.Std.Dev.(Intercept)", "zi.zi~(Intercept)", "sigma"),]
  cis$Estimate <- NULL
  names(cis)[1:2] <- c("lci", "uci")
  
  mod.out <- merge(mod.out, cis)
  detect.output <- rbind(detect.output, mod.out)
  
  # for interaction plots
  ef <- effect("WildDogUse:TimePeriod", m1)
  x <- as.data.frame(ef)
  x$Species <- as.character(species.list[i])
  detect.interactions <- rbind(detect.interactions, x)
  
  # predict for 'occupancy' plotting 
  newdat <- RAI.dat[RAI.dat$Species == species.list[i],] %>% select(-c(tot.occu, tot.count))
  temp <- predict(m1, newdat, se.fit=T, type="response")
  y <- cbind(newdat, temp)
  detect.pred <- rbind(detect.pred, y)
}

write.csv(detect.output,"../../Analyses and Scripts/PROJECT_GNP_WildDogs/Spatial_Detection_Output.csv",row.names=F)
write.csv(detect.interactions,"../../Analyses and Scripts/PROJECT_GNP_WildDogs/Spatial_Detection_Interactions.csv", row.names=F)
write.csv(detect.pred,"../../Analyses and Scripts/PROJECT_GNP_WildDogs/Spatial_Detection_Predictions.csv", row.names=F)


## Q: do species traits predict magnitude of diel shifts in activity pattern? 

# load species traits 
species.traits <- read.csv("../../Analyses and Scripts/PROJECT_GNP_WildDogs/Gorongosa_Species_Traits.csv")
names(species.traits)[c(1,9)] <- c("Species", "% Diet")
species.traits$Digestive.System <- ifelse(species.traits$Digestive.System == "Ruminant", "Ruminant", "Non-ruminant")
species.traits$s.Avg.Body.Mass <- scale(species.traits$Avg.Body.Mass)
species.traits$WD.Index <- as.numeric(gsub( " .*$", "", species.traits$WD.Preference.Index))
species.traits$s.Group.Size <- scale(species.traits$Social.Group.Size)
species.traits$s.Interbirth <- scale(species.traits$Interbirth.Interval..d.)
species.traits$sLitter <- scale(species.traits$Litter.Size)
species.traits <- species.traits[c("Species", "Order", "Family", "Digestive.System", "Feeding.Guild", "% Diet",
                                   "WD.Index", "s.Avg.Body.Mass", "s.Group.Size", "s.Interbirth", "sLitter")]

# calculate difference in overall activity levels and associated error 
act.dif <- read.csv("../../Analyses and Scripts/PROJECT_GNP_WildDogs/Activity_Difs_3season_3use.csv")
act.dif <- act.dif[!act.dif$Difference == "Not Enough Data",]

# merge with species traits 
act.dif <- merge(act.dif, species.traits, all.x=T)

# format data 
act.dif$WildDogUse <- factor(act.dif$WildDogUse, levels = c("no.use", "low.use", "high.use"))
act.dif$Difference <- as.numeric(as.character(act.dif$Difference))

# add errors back to response variable 
errors(act.dif$Difference) <- act.dif$SE

m2 <- glm(Difference ~ WildDogUse + Season + Feeding.Guild + s.Avg.Body.Mass + 
            I(s.Avg.Body.Mass^2) + WD.Index + s.Group.Size + s.Interbirth + sLitter, 
          data = act.dif)

summary(m2)

# save output
mod.out <- data.frame(summary(m2)[12])
mod.out$Coefficient <- row.names(mod.out)
row.names(mod.out) <- c()
mod.out[,c(1:2,4)] <- round(mod.out[,c(1:2,4)], 3)
mod.out[,3] <- round(mod.out[,3],2)
names(mod.out)[1:4] <- c("Estimate", "SE", "t value", "p value")
mod.out <- mod.out[,c(5,1:4)]
write.csv(mod.out, "../../Analyses and Scripts/PROJECT_GNP_WildDogs/Traits_Difference.csv", row.names=F)
