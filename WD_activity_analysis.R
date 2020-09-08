## Calculating activity, overlap, and difference in daily activity patterns ## 

# load functions and libraries 
library(overlap); library(sp); library(dplyr); library(lubridate); library(sf); library(activity)
library(pryr); library(ggplot2); library(Hmisc); library(tidyverse); library(Rmisc); library(glmmTMB)
library(MuMIn); library(stringr); library(DHARMa)

source("WildDog_Activity_Level_Functions.R")

# load data
dat <- read.csv("WildCamData_WildDog_Formatted.csv") 

# select species
species.list <- c("Bushbuck", "Reedbuck", "Nyala", "Oribi", "Impala", "Waterbuck", "Warthog", "Kudu", "Buffalo", "Bushpig", "Elephant", "Wildebeest", "Duiker", "Hartebeest")

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
  mutate(DateTime = as.POSIXct(DateTime, "%Y-%m-%d %H:%M:%S", tz=tz.ct), 
         Date = as.POSIXct(dat$DateTime, format = "%Y-%m-%d", tz = tz.ct), 
         Time.Decimal = hour(DateTime) + minute(DateTime)/60 + second(DateTime)/3600, 
         Time.Radians = (Time.Decimal / 24) * 2 * pi, 
         Time.Sun = sunTime(Time.Radians, Date, coords), 
         Sun.Hour = (Time.Sun * 24) / (2 * pi),
         Sun.Hour = floor(Sun.Hour),
         Year = year(DateTime),
         TimePeriod = ifelse(DateTime < wd.release, "PreRelease", "PostRelease"), 
         WildDogUse = ifelse(Camera %in% high.use, "high.use", ifelse(Camera %in% low.use, "low.use", "no.use")), 
         Season.Year = paste(Season, Year), 
         s.Tree.Density.1km = scale(Tree.Density.1km), 
         s.Urema.Dist = scale(Urema.Dist), 
         s.Road.Dist = scale(Road.Dist)) %>%
  filter(Species %in% species.list)

# as wet season runs Dec-Mar, add Dec to "next years'" wet season 
dat$Season.Year <- ifelse((dat$Season == "Wet" & month(dat$DateTime) == 12), paste("Wet", (year(dat$DateTime)+1)), dat$Season.Year)

# disgard 2018 early dry (transition period), 2016 early dry (incomplete season) 
dat <- dat[!(dat$Season == "Early Dry" & year(dat$DateTime) %in% c("2016", "2018")),] 


## modelling ------------------

## 1) activity ----------------

# does overall activity level change depending on time period x wild dog use area + spatial and temporal environmental covariates? 

# for iterating through species, save model rankings and model output here:
final.sel.table <- NULL
final.mod.table <- NULL

# iterate through species (manually, so can check models/residuals) 
# species: Impala, Kudu, Warthog, Waterbuck, Oribi, Reedbuck, Buffalo, Bushbuck, Bushpig, Elephant, Nyala, Duiker, Hartebeest, Eland, Wildebeest

species <- "Warthog"
act.data <- dat[dat$Species == species,]

m1 <- glmmTMB(Time.Sun ~ TimePeriod + WildDogUse + Season + (1|Camera), data = act.data)
m2 <- glmmTMB(Time.Sun ~ TimePeriod*WildDogUse + Season + (1|Camera), data = act.data) 
m3 <- glmmTMB(Time.Sun ~ TimePeriod*WildDogUse + Season +  s.Tree.Density.1km + (1|Camera), data = act.data)
m4 <- glmmTMB(Time.Sun ~ TimePeriod*WildDogUse + Season + s.Road.Dist + (1|Camera), data = act.data) 
m5 <- glmmTMB(Time.Sun ~ TimePeriod*WildDogUse + Season + s.Urema.Dist + (1|Camera), data = act.data) 
m6 <- glmmTMB(Time.Sun ~ TimePeriod*WildDogUse + Season + s.Tree.Density.1km + s.Urema.Dist + (1|Camera), data = act.data)
m7 <- glmmTMB(Time.Sun ~ TimePeriod*WildDogUse + Season + s.Tree.Density.1km + s.Road.Dist + (1|Camera), data = act.data) 
m8 <- glmmTMB(Time.Sun ~ TimePeriod*WildDogUse + Season + s.Urema.Dist + s.Road.Dist + (1|Camera), data = act.data)
m9 <- glmmTMB(Time.Sun ~ TimePeriod*WildDogUse + Season + s.Tree.Density.1km + s.Road.Dist + s.Urema.Dist + (1|Camera), data = act.data)

# model ranking
(mod.list <- model.sel(m1, m2, m3, m4, m5, m6, m7, m8, m9))

# evaluate model residuals (fill in best-fit model)
acf(resid(m5)); pacf(resid(m5))
simres <- simulateResiduals(m5); plot(simres)

# coerce model rankings into neat dataframe and save
sel.table <- as.data.frame(mod.list)[(length(mod.list)-4):length(mod.list)] 
sel.table[,2:3] <- round(sel.table[,2:3],2)
sel.table[,4:5] <- round(sel.table[,4:5],3)
sel.table$Model <- rownames(sel.table)
for(i in 1:nrow(sel.table)) sel.table$Model[i]<- as.character(formula(paste(sel.table$Model[i])))[3]
sel.table$Species <- species
sel.table <- sel.table[,c(7,6,1,2,3,4,5)]
final.sel.table <- rbind(final.sel.table, sel.table)

# coerce model output into nice dataframe and save
subset(mod.list, delta < 2) #delta AICc for small sample bias

# --> if one top model (fill in best-fit model): 
mod.out <- data.frame(summary(m1)[6]$coefficients$cond)
mod.out$Coefficient <- row.names(mod.out)
row.names(mod.out) <- c()
mod.out[,c(1:2,4)] <- round(mod.out[,c(1:2,4)], 3)
mod.out[,3] <- round(mod.out[,3],2)
names(mod.out)[2:4] <- c("SE", "z value", "p value")
mod.out$Species <- species
mod.out <- mod.out[,c(6,5,1:4)]
final.mod.table <- rbind(final.mod.table, mod.out)

# --> if need to model average: (note: use conditional model average; SE in table is the adjusted SE)
mod.out <- data.frame(summary(model.avg(mod.list, subset = delta < 2))[10])
mod.out$coefmat.subset.Std..Error <- NULL
mod.out$Coefficient <- row.names(mod.out)
row.names(mod.out) <- c()
re <- "\\(([^()]+)\\)"
mod.out$Coefficient <- gsub(re, "\\1", str_extract_all(mod.out$Coefficient, re))
mod.out[mod.out$Coefficient == "Int",]$Coefficient <- c("(Intercept)")
names(mod.out)[1:4] <- c("Estimate", "SE", "z value", "p value")
mod.out[,c(1,2,4)] <- round(mod.out[,c(1,2,4)],3)
mod.out[,3] <- round(mod.out[,3],2)
mod.out$Species <- species
mod.out <- mod.out[,c(6,5,1:4)]
final.mod.table <- rbind(final.mod.table, mod.out)

# save results 
final.mod.table$sig <- ifelse(final.mod.table$`p value` <= 0.05, "*", "")
write.csv(final.mod.table, "Activity_Models.csv", row.names=F)
write.csv(final.sel.table, "Activity_Models_AIC.csv", row.names=F)

## --> predator covariates correlate with activity level of: 
# - waterbuck (before/after x wild dog area)
# - reedbuck (before/after)
# - buffalo (before/after x wild dog area)
# - bushbuck (before/after x wild dog area)

## --> plotting results 
# note: I ran these on a supercomputer with nboots=1000 and uploaded those data to make these graphs; for quick laptop visualization purposes, run nboots=10
nboots <- 10

# - recalculate activity levels
dat$tag <- paste(dat$TimePeriod, dat$WildDogUse)

# - 1) waterbuck (before/after x wild dog area)
wb.act <- activity.level.plot(dat, species="Waterbuck", nboots=nboots, WildDogUse=T) 
names(wb.act)[8] <- "Time Period"

(p1 <- ggplot(wb.act, aes(x = x, y = y, group = tag)) + 
  scale_x_continuous(name="Time (Hrs)", breaks = seq(from=0, to=24, by=4), limits=c(0,24), 
                     expand = c(0,0)) +
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="blue", alpha=0.2) +
  geom_line(aes(lty=`Time Period`)) + 
  annotate("rect",xmin=0,xmax=6,ymin=-Inf,ymax=Inf, alpha=0.1, fill="black") + 
  annotate("rect",xmin=19,xmax=24,ymin=-Inf,ymax=Inf, alpha=0.1, fill="black") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  ggtitle("Waterbuck Diel Activity") + ylab("Density") +
  facet_wrap(~wild.dog))

# - 2) reedbuck (before/after)
rb.act <- activity.level.plot(dat, species="Reedbuck", nboots=nboots, WildDogUse=F) 
names(rb.act)[6] <- "Time Period"

(p2 <- ggplot(rb.act, aes(x = x, y = y, group = `Time Period`)) + 
  scale_x_continuous(name="Time (Hrs)", breaks = seq(from=0, to=24, by=4), limits=c(0,24), 
                     expand = c(0,0)) +
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="blue", alpha=0.2) +
  geom_line(aes(lty=`Time Period`)) + 
  annotate("rect",xmin=0,xmax=6,ymin=-Inf,ymax=Inf, alpha=0.1, fill="black") + 
  annotate("rect",xmin=19,xmax=24,ymin=-Inf,ymax=Inf, alpha=0.1, fill="black") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  ggtitle("Reedbuck Diel Activity") + ylab("Density"))


# - 3) buffalo (before/after x wild dog area)
(bf.act <- activity.level.plot(dat, species="Buffalo", nboots=nboots, WildDogUse=T) 
names(bf.act)[8] <- "Time Period"

p3 <- ggplot(bf.act, aes(x = x, y = y, group = tag)) + 
  scale_x_continuous(name="Time (Hrs)", breaks = seq(from=0, to=24, by=4), limits=c(0,24), 
                     expand = c(0,0)) +
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="blue", alpha=0.2) +
  geom_line(aes(lty=`Time Period`)) + 
  annotate("rect",xmin=0,xmax=6,ymin=-Inf,ymax=Inf, alpha=0.1, fill="black") + 
  annotate("rect",xmin=19,xmax=24,ymin=-Inf,ymax=Inf, alpha=0.1, fill="black") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  ggtitle("Buffalo Diel Activity") + ylab("Density") +
  facet_wrap(~wild.dog))


# - 4) bushbuck (before/after x wild dog area)
(bb.act <- activity.level.plot(dat, species="Bushbuck", nboots=10, WildDogUse=T) 
names(bb.act)[8] <- "Time Period"

p4 <- ggplot(bb.act, aes(x = x, y = y, group = tag)) + 
  scale_x_continuous(name="Time (Hrs)", breaks = seq(from=0, to=24, by=4), limits=c(0,24), 
                     expand = c(0,0)) +
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="blue", alpha=0.2) +
  geom_line(aes(lty=`Time Period`)) + 
  annotate("rect",xmin=0,xmax=6,ymin=-Inf,ymax=Inf, alpha=0.1, fill="black") + 
  annotate("rect",xmin=19,xmax=24,ymin=-Inf,ymax=Inf, alpha=0.1, fill="black") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  ggtitle("Bushbuck Diel Activity") + ylab("Density") +
  facet_wrap(~wild.dog))


## 2) activity ----------------

# does a) activity level and/or b) patterning of diel activity change after wild dog release i) across whole grid, ii) in areas of high/med/low use? 

# calculates overlap index Dhat4 for the two fitted distributions, then generates a null distribution of overlap indices using data sampled randomly
# with replacement from the combined data. This randomised distribution is then used to define an empirical probability distribution against which the 
# probability that the observed overlap arose by chance is judged.
# obs = observed overlap index; null = mean null overlap index; seNull = SE of null distribution; pNull = probability observed index arose by chance

# loop through species (again, fewer boots for laptop visualization; real data run on supercomputer with nboots=1000)
nboots <- 10 
activity.levels <- data.frame(); activity.diffs <- data.frame(); diel.diffs <- data.frame()

for(i in 1:length(species.list)){
  
  # no use, low use, high use 
  act.list.all <- act.diel.difs(dat, species.list[i], "all", nboots)
  act.list.no <- act.diel.difs(dat, species.list[i], "no.use", nboots)
  act.list.low <- act.diel.difs(dat, species.list[i], "low.use", nboots)
  act.list.high <- act.diel.difs(dat, species.list[i], "high.use", nboots)
  
  # activity levels before and after 
  activity.levels <- rbind(activity.levels, act.list.all[[1]], act.list.no[[1]], act.list.low[[1]], act.list.high[[1]])
  
  # differences in activity levels before and after 
  activity.diffs <- rbind(activity.diffs, act.list.all[[2]], act.list.no[[2]], act.list.low[[2]], act.list.high[[2]])
  
  # differences in diel patterning before and after 
  diel.diffs <- rbind(diel.diffs, act.list.all[[3]], act.list.no[[3]], act.list.low[[3]], act.list.high[[3]])
}

# save results 
write.csv(activity.levels, "Activity_Levels.csv", row.names=F)
write.csv(activity.diffs, "Activity_Difs.csv", row.names=F)
write.csv(diel.diffs, "Diel_Difs.csv", row.names=F)


## 3) activity overlap ----------------

# is magnitude of overlap between before and after (i.e., lots overlap = not much change, little overlap = shift in activity) predicted by species traits? 

# load species traits 
species.traits <- read.csv("Gorongosa_Species_Traits.csv")
names(species.traits)[1] <- "Species"
species.traits$s.Avg.Body.Mass <- scale(species.traits$Avg.Body.Mass)

# calculate index of overlap between circular distributions (Dhat4 overlap index)
overlap.dat <- data.frame()
nboots <- 10 #more for real data 

for (i in 1:length(species.list)){
  
  # all 
  all <- rbind(
    activity.overlap(dat, species.list[i], "Wet", "all", nboots), #wet season
    activity.overlap(dat, species.list[i], "Early Dry", "all", nboots), #early dry season
    activity.overlap(dat, species.list[i], "Late Dry", "all", nboots)) #late dry season 
  
  # no use 
  no.use <- rbind(
    activity.overlap(dat, species.list[i], "Wet", "no.use", nboots), #wet season
    activity.overlap(dat, species.list[i], "Early Dry", "no.use", nboots), #early dry season
    activity.overlap(dat, species.list[i], "Late Dry", "no.use", nboots)) #late dry season 
  
  # low use 
  low.use <- rbind(
    activity.overlap(dat, species.list[i], "Wet", "low.use", nboots), #wet season
    activity.overlap(dat, species.list[i], "Early Dry", "low.use", nboots), #early dry season
    activity.overlap(dat, species.list[i], "Late Dry", "low.use", nboots)) #late dry season 
  
  # high use 
  high.use <- rbind(
    activity.overlap(dat, species.list[i], "Wet", "high.use", nboots), #wet season
    activity.overlap(dat, species.list[i], "Early Dry", "high.use", nboots), #early dry season
    activity.overlap(dat, species.list[i], "Late Dry", "high.use", nboots)) #late dry season 
  
  overlap.dat <- rbind(overlap.dat, no.use, low.use, high.use)
}
  
# save files 
write.csv(overlap.dat, "Overlap_Data.csv", row.names=F)

# merge with species traits 
overlap.dat <- merge(overlap.dat, species.traits, all.x=T)

# regress overlap with traits 
overlap.dat <- overlap.dat[!overlap.dat$Overlap == "Error - too few sightings",]
overlap.dat <- overlap.dat[!overlap.dat$WildDogUse == "All",] #figure out what to do with these data later... 
overlap.dat$Overlap <- as.numeric(as.character(overlap.dat$Overlap))

m1 <- glmmTMB(Overlap ~ WildDogUse + Season + Feeding.Guild + s.Avg.Body.Mass + I(s.Avg.Body.Mass^2),
              data = overlap.dat)

# save output
mod.out <- data.frame(summary(m1)[6]$coefficients$cond)
mod.out$Coefficient <- row.names(mod.out)
row.names(mod.out) <- c()
mod.out[,c(1:2,4)] <- round(mod.out[,c(1:2,4)], 3)
mod.out[,3] <- round(mod.out[,3],2)
names(mod.out)[2:4] <- c("SE", "z value", "p value")
mod.out <- mod.out[,c(5,1:4)]
write.csv(mod.out, "Overlap_Results.csv", row.names=F)

# make predict dataframes and plot: 
... do this! ... 
