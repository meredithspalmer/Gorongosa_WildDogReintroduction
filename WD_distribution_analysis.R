## Calculating changes in distribution/activity across study site pre/post wild dog release ## 

## set workspace 

# load functions and libraries 
library(ggplot2); library(unmarked); library(Hmisc); library(dplyr); library(effects);
library(sp); library(rgdal); library (rgeos); library(maptools); library(ggpubr)

# species of interest 
species.list <- c("Bushbuck", "Reedbuck", "Nyala", "Oribi", "Impala", "Waterbuck", "Warthog", "Kudu", "Buffalo", "Bushpig", "Elephant", "Wildebeest", "Duiker", "Hartebeest")

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
  mutate(Date = strptime(DateTime, "%m/%d/%y"), 
         Week = week(Date), 
         Week.Year = as.numeric(paste(year(Date), Week, sep="")), 
         Season.Year = paste(season, year(Date)), 
         TimePeriod = ifelse(Date < wd.release, "PreRelease", "PostRelease"), 
         WildDogUse = ifelse(Camera %in% high.use, "high.use", ifelse(Camera %in% low.use, "low.use", "no.use")), 
         occu = 1)
dat$Season.Year <- ifelse((dat$season == "Wet" & month(dat$Date) == 12), paste("Wet", (year(dat$Date)+1)), dat$Season.Year)

effort <- read.csv("WildCamEffort_WildDog_Formatted.csv") %>% 
  mutate(date = strptime(date, "%Y-%m-%d"), 
         Week - week(date), 
         Week.Year = as.numeric(paste(year(date), week(date), sep="")), 
         Season.Year = paste(season, year(date)),
         TimePeriod = ifelse(date < wd.release, "PreRelease", "PostRelease"))
effort$Season.Year <- ifelse((effort$season == "Wet" & month(effort$date) == 12), paste("Wet", (year(effort$date)+1)), effort$Season.Year)
names(effort)[1:2] <- c("Camera","Date")

# disgard 2018 early dry (transition period during which wild dogs release) and 2016 early dry (v. start of study - incomplete season) 
dat <- dat[!(dat$season == "Early Dry" & year(dat$Date) %in% c("2016", "2018")),] 
effort <- effort[!(effort$season == "Early Dry" & year(effort$date) %in% c("2016", "2018")),]

## covariates 

# select covariates
covs <- dat[c("Camera", "height.cm", "angle", "detect.obscured", "cover.ground", "termite.count.100m",
              "boundary_dist", "chitengo_dist", "fire_frequency", "pans_dist", "river_dist", "road_dist",
              "settlement_dist", "tree_1km", "urema_dist", "WildDogUse")]
covs <- covs[!duplicated(covs),]

# center and scale continuous covariates, set categorical covariates to factor 
covs <- covs %>%
  mutate(s.height = scale(height.cm), s.angle = scale(angle), s.termite = scale(termite.count.100m),
         s.boundary = scale(boundary_dist), s.chitengo = scale(chitengo_dist), s.pans = scale(pans_dist),
         s.fire = scale(fire_frequency), s.river = scale(river_dist), s.road = scale(road_dist), 
         s.settlement = scale(settlement_dist), s.tree = scale(tree_1km), s.urema = scale(urema_dist), 
         detect.obscured = factor(detect.obscured), cover.ground = cover.ground/100, 
         WildDogUse = factor(WildDogUse)) 

# check for correlated variables (Pearson Correlation Coefficient) 
res <- rcorr(as.matrix(covs[,c(17:28)]), type="pearson")
round(res$r,3)
round(res$P,3)
plot(covs$WildDogUse, covs$s.road)
plot(covs$WildDogUse, covs$s.urema)
# --> many correlated variables: select those with no significant correlation or where slope of significant correlation is less than abs(0.1)

# remove 'boundary', 'chitengo', 'fire', 'settlement', 'pans', 'river' in favor of 'urema', 'road', 'tree' 
covs <- covs %>% select(Camera, s.height, s.angle, detect.obscured, WildDogUse, cover.ground, s.termite, s.urema, s.tree, s.road)

## shapefiles/spatial data 

#full grid 
grid <- readOGR("CameraGridHexes2.shp")
names(grid)[6] <- "Camera"

#wild dog use high
wd_hi <- readOGR("wd_highuse_hexes.shp")
wd_hi <- fortify(wd_hi)

#wild dog use low
wd_lo_1 <- readOGR("wd_lowuse1_hexes.shp")
wd_lo_1 <- fortify(wd_lo_1)
wd_lo_2 <- readOGR("wd_lowuse2_hexes.shp")
wd_lo_2 <- fortify(wd_lo_2)

#wild do no use
wd_no_1 <- readOGR("wd_nouse1_hexes.shp")
wd_no_1 <- fortify(wd_no_1)
wd_no_2 <- readOGR("wd_nouse2_hexes.shp")
wd_no_2 <- fortify(wd_no_2)


## "Relative activity index" ----------------

# Know from K.G. thesis that occupancy modeling doesn't work well for species that are distributed across entire study area (all grid cells = 1): 
# "While hierarchical occupancy modeling accounts for imperfect detection of species, it only addresses presence or absence and thus fails to account for 
# differences in species abundance. For common species that are widespread in space, a comparison of relative activity across camera sites may provide greater 
# insight into seasonal dynamics of space use."

# Instead, run a hierarchical model that includes spatial and temporal covariates AND camera trap detection covariates that looks at changes in animal captures 
# (could be interpreted as relative abundance or activity) across the study site, correcting for days camera active, random effects, etc. 

# When summing data across week-long windows, 3 possible approaches: 
# - 1/0 seen in the week (traditional OM)
# - total number of detections per week (traditional RAI) 
# - total count of animals seen per week (count individuals within detections; demonstrated by my recent work to produce more accurate and precise estimates of 
# animal abundance; Palmer et al. 2018) 

# In this work, I create dataframes that focus on the later two; however, as many animals in GNP travel alone or are captured individually by the camera traps, 
# the metrics end up being identical for many species. Thus, for the analysis section, I focus on total counts only. 

# Use week-long windows; assume seasons (wet, early dry, late dry) are closed to changes in occupancy 

# format data 
dat$Date <- as.character(dat$Date)
RAI.dat <- ddply(dat, .(Species, Camera, Week.Year, season, Season.Year, TimePeriod), summarise, tot.occu = sum(occu), tot.count = sum(Count))

effort$Date <- as.character(effort$Date); effort$on <- 1
RAI.effort <- ddply(effort, .(Camera, Week.Year, season, Season.Year), summarise, Days.On = sum(on))

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

# re-order factor levels 
RAI.dat$TimePeriod <- factor(RAI.dat$TimePeriod, levels = c("PreRelease", "PostRelease"))
RAI.dat$WildDogUse <- factor(RAI.dat$WildDogUse, levels = c("no.use", "low.use", "high.use"))


## Calculate hierarchical models for each camera using COUNTS 

hist(RAI.dat$tot.count) # likely zero-inflated and/or negative binomial
# --> did model comparison with AIC for combinations of zi/no zi and poisson, nb1, nb2; best fit is nbinom1 with zi 

# modeling total counts: 
abund.output <- NULL
abund.interactions <- NULL
abund.pred <- NULL

for(i in 1:length(species.list)){
  
  m1 <- glmmTMB(tot.count ~ 
                  WildDogUse*TimePeriod + # wild dog covariates 
                  cover.ground + s.termite + s.urema + s.road + s.tree +  # other spatial "occupancy" covariates
                  season + # other temporal "occupancy" covariates 
                  s.height + s.angle + detect.obscured + # block on "detection" covariates  
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
  
  cis <- data.frame(confint(m1)) #wald/delta-method CI
  cis$Coefficient <- row.names(cis); row.names(cis) <- c()
  cis$Coefficient <- gsub("cond.", "", cis$Coefficient)
  cis <- cis[!cis$Coefficient %in% c("Camera.Std.Dev.(Intercept)", "zi.zi~(Intercept)", "sigma"),]
  cis$Estimate <- NULL
  names(cis)[1:2] <- c("lci", "uci")
  
  mod.out <- merge(mod.out, cis)
  abund.output <- rbind(abund.output, mod.out)
  
  # for interaction plots
  ef <- effect("WildDogUse:TimePeriod", m1)
  x <- as.data.frame(ef)
  x$Species <- as.character(species.list[i])
  abund.interactions <- rbind(abund.interactions, x)
  
  # predict for 'occupancy' plotting 
  newdat <- RAI.dat[RAI.dat$Species == species.list[i],] %>% select(-c(tot.occu, tot.count))
  temp <- predict(m1, newdat, se.fit=T, type="response")
  y <- cbind(newdat, temp)
  abund.pred <- rbind(abund.pred, y)
}

write.csv(abund.output, "Spatial_Abundance_Output.csv", row.names=F)
write.csv(abund.interactions, "Spatial_Abundance_Interactions.csv", row.names=F)
write.csv(abund.pred, "Spatial_Abundance_Interactions.csv", row.names=F)

## plot these outputs 
# - A) magnitude of covs (**this might be better as a table) 
# - B) interaction effects of sign
# - C) spatial distribution (get from shiny app!!!) 

## A) magnitude of effect (forest plots) (**again, may just make table) 
# for plotting purposes, include only spatial and temporal occupancy covariates (aka leave OUT detection covariates - blocked on) 

# format data 
plot.dat <- read.csv("Spatial_Abundance_Output.csv")
plot.dat$Response <- "Total Counts"
plot.dat$sig <- ifelse(plot.dat$`p value` <= 0.05, 1, 0)
plot.dat <- plot.dat[!plot.dat$Coefficient %in% c("(Intercept)", "detect.obscured1", "s.angle", "s.height"),]
plot.dat$Coefficient <- factor(plot.dat$Coefficient)
plot.dat$Coefficient <- factor(plot.dat$Coefficient, levels = c("WildDogUselow.use", "WildDogUsehigh.use", "TimePeriodPostRelease", "WildDogUselow.use:TimePeriodPostRelease", "WildDogUsehigh.use:TimePeriodPostRelease", "seasonWet", "seasonLate Dry", "cover.ground", "s.termite", "s.tree", "s.urema", "s.road"))
levels(plot.dat$Coefficient) <- c("WD Use [Low]", "WD Use [High]", "Time Period [Post-WD Release]", "WD Use [Low] x Time Period [Post-WD Release]", "WD Use [High] x Time Period [Post-WD Release]", "Season [Wet]", "Season [Late Dry]", "Ground Cover", "Termite Mounds", "Tree Density", "Lake Distance", "Road Distance") 
plot.dat$WDtag <- factor(ifelse(plot.dat$Coefficient %in% c("WD Use [Low]", "WD Use [High]", "Time Period [Post-WD Release]", "WD Use [Low] x Time Period [Post-WD Release]", "WD Use [High] x Time Period [Post-WD Release]"), 1, 0))
sig.dat <- plot.dat[plot.dat$sig == 1,]

# log odds for coefficients
# note: tried odds ratio plot, but made results more difficult to read. here, plotting log odds (aka model estimates)
ggplot(plot.dat, aes(x=reorder(Coefficient, desc(Coefficient)), y=Estimate), group=WDtag) + 
  coord_flip() + #scale_x_discrete(position = "top") + #looks awkward with uneven rows
  geom_point(aes(alpha=0.1, color=WDtag), position=position_dodge(width=0.75), shape = 1) + 
  geom_errorbar(aes(ymin=lci, ymax=uci, alpha=0.1, color=WDtag), width=.2, position=position_dodge(width=0.75)) + 
  geom_point(data = sig.dat, aes(x=reorder(Coefficient, desc(Coefficient)), y=Estimate, color=WDtag),shape=16, position=position_dodge(width=0.75)) + 
  geom_errorbar(data=sig.dat, aes(ymin=lci, ymax=uci, color=WDtag), width=.2, position = position_dodge(width=0.75)) +
  scale_color_manual(values=c("black", "red")) + 
  geom_hline(yintercept = 0, lty = "dashed") + 
  facet_wrap(~Species) + 
  xlab("") + ylab("Estimate") + theme_bw() + theme(legend.position = "none")


## B) interactions plots: 

# - species for which there were significant interactions between wild dog space use and time period before/after release: 
interact.spp <- c("Bushbuck", "Bushpig", "Duiker", "Eland", "Hartebeest", "Impala", "Nyala", "Oribi", "Reedbuck", "Waterbuck")

# format data 
levels(abund.interactions$WildDogUse) <- c("High", "Low", "None")
names(abund.interactions)[2] <- "Period"
levels(abund.interactions$Period) <- c("Post-Wild Dog Release", "Pre-Wild Dog Release")

# plot interaction effects 
ggplot(abund.interactions[abund.interactions$Species %in% interact.spp,], 
       aes(WildDogUse, fit, color=Period, group=Period)) + 
  geom_point(position=position_dodge(width=0.5), aes(shape=Period)) +
  geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=0.3, position=position_dodge(width=0.5)) + 
  scale_color_manual(values=c("red", "black")) +
  theme_bw(base_size=12) + xlab("Intensity of Wild Dog Use") + 
  facet_wrap(~Species, scales="free")


## C) distribution map 

# calculate percent difference between use of areas before/after wild dog release for each species, each season 

perc.diff.ed <- NULL; perc.diff.ld <- NULL; perc.diff.w <- NULL
for(i in 1:length(species.list)){
  sub <- abund.pred[abund.pred$Species == species.list[i],] %>% 
    select(Camera, Week.Year, season, Season.Year, TimePeriod, WildDogUse, fit)
 
  #model predicted weekly occupancy; average across seasons: 
  sub.before <- ddply(sub[sub$TimePeriod == "PreRelease",], .(Camera, season, WildDogUse), summarise, 
                      mean.fit.before = mean(fit)) 
  sub.after <- ddply(sub[sub$TimePeriod == "PostRelease",], .(Camera, season, WildDogUse), summarise, 
                     mean.fit.after = mean(fit)) 
    
  #calculate percent change from before
  sub <- merge(sub.before, sub.after, all=T)
  sub <- sub[!is.na(sub$mean.fit.before),]; sub <- sub[!is.na(sub$mean.fit.after),]
  sub$`Percent Change` <- (sub$mean.fit.after - sub$mean.fit.before)/sub$mean.fit.before
  
  #merge with grid FOR EACH SEASON
  ed.grid <- merge(grid, sub[sub$season == "Early Dry",], by="Camera")
  ed.grid@data$id <- rownames(ed.grid@data)
  ed.grid.points <- fortify(ed.grid, region="id")
  ed.grid.df <- join(ed.grid.points, ed.grid@data, by="id")
  ed.grid.df$Species <- as.character(species.list[i])
  
  ld.grid <- merge(grid, sub[sub$season == "Late Dry",], by="Camera")
  ld.grid@data$id <- rownames(ld.grid@data)
  ld.grid.points <- fortify(ld.grid, region="id")
  ld.grid.df <- join(ld.grid.points, ld.grid@data, by="id")
  ld.grid.df$Species <- as.character(species.list[i])
  
  w.grid <- merge(grid, sub[sub$season == "Wet",], by="Camera")
  w.grid@data$id <- rownames(w.grid@data)
  w.grid.points <- fortify(w.grid, region="id")
  w.grid.df <- join(w.grid.points, w.grid@data, by="id")
  w.grid.df$Species <- as.character(species.list[i])
  
  perc.diff.ed <- rbind(perc.diff.ed, ed.grid.df)
  perc.diff.ld <- rbind(perc.diff.ld, ld.grid.df)
  perc.diff.w <- rbind(perc.diff.w, w.grid.df)
}

# create plot crosshatching (optional)
source("https://raw.githubusercontent.com/imaddowzimet/drawcrosshatch/master/draw_crosshatch.R") 
wd_hi_estimated <- wd_hi %>% group_by(group) %>% nest()
lines_hi <- map_df(wd_hi_estimated$data, draw.crosshatch, width =500, pattern= "crosshatch")
wd_lo_estimated_1 <- wd_lo_1 %>% group_by(group) %>% nest()
lines_lo_1 <- map_df(wd_lo_estimated_1$data, draw.crosshatch, width=500, pattern= "horizontal")
wd_lo_estimated_2 <- wd_lo_2 %>% group_by(group) %>% nest()
lines_lo_2 <- map_df(wd_lo_estimated_2$data, draw.crosshatch, width=500, pattern= "horizontal")
wd_no_estimated_1 <- wd_no_1 %>% group_by(group) %>% nest()
lines_no_1 <- map_df(wd_no_estimated_1$data, draw.crosshatch, width=500, pattern= "vertical")
wd_no_estimated_2 <- wd_no_2 %>% group_by(group) %>% nest()
lines_no_2 <- map_df(wd_no_estimated_2$data, draw.crosshatch, width=500, pattern= "vertical")

# --> to add to ggplot, include: 
#geom_segment(data=lines_no_1, aes(x= x, y = y , xend = xend, yend = yend), alpha=0.2, color="green3", inherit.aes = F) + geom_segment(data=lines_no_2, aes(x= x, y = y , xend = xend, yend = yend), alpha=0.2, color="green3", inherit.aes = F) + geom_segment(data=lines_lo_1, aes(x= x, y = y , xend = xend, yend = yend), alpha=0.2, color="orange", inherit.aes = F) + geom_segment(data=lines_lo_2, aes(x= x, y = y , xend = xend, yend = yend), alpha=0.2, color="orange", inherit.aes = F) + geom_segment(data=lines_hi, aes(x= x, y = y , xend = xend, yend = yend), alpha=0.2, color="red", inherit.aes = F) 

# note: legends vary so widely that facet_wrapping distorts patterns; rather, cycle through and paste individual graphs together 

map.plot <- function(species, data){ 
  p1 <- ggplot() + 
    geom_polygon(data=data[data$Species == species,], aes(long, lat, group = group, fill = `Percent Change`), 
                 colour = alpha("white", 1/2), size = 0.7) + 
    geom_polygon(data=wd_no_1, aes(long, lat), color="green3", fill=NA, size=1) + 
    geom_polygon(data=wd_no_2, aes(long, lat), color="green3", fill=NA, size=1) + 
    geom_polygon(data=wd_lo_1, aes(long, lat), color="orange", fill=NA, size=1) + 
    geom_polygon(data=wd_lo_2, aes(long, lat), color="orange", fill=NA, size=1) + 
    geom_polygon(data=wd_hi, aes(long, lat), color="red", fill=NA, size=1) + 
    scale_fill_gradient(low="gray20", high="papayawhip", na.value="white") + 
    theme_bw() + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), 
          legend.position="none") + 
    xlab("") + ylab("") + facet_wrap(~Species)
  
  return(p1)
}

# early dry 
ed.plots <- NULL
for(i in 1:length(species.list)){
  p1 <- map.plot(species.list[i], perc.diff.ed)
  ed.plots[[i]] <- p1
}

# add in legends -- all on different scales 
(early.dry.figure <- ggarrange(ed.plots[[1]], ed.plots[[2]], ed.plots[[3]], ed.plots[[4]], ed.plots[[5]], 
                               ed.plots[[6]], ed.plots[[7]], ed.plots[[8]], ed.plots[[9]], ed.plots[[10]], 
                               ed.plots[[11]], ed.plots[[12]], ed.plots[[13]], ed.plots[[14]], 
                    ncol = 4, nrow = 4))

# late dry 
ld.plots <- NULL
for(i in 1:length(species.list)){
  p1 <- map.plot(species.list[i], perc.diff.ld)
  ld.plots[[i]] <- p1
}

# add in legends -- all on different scales 
(late.dry.figure <- ggarrange(ld.plots[[1]], ld.plots[[2]], ld.plots[[3]], ld.plots[[4]], ld.plots[[5]], 
                              ld.plots[[6]], ld.plots[[7]], ld.plots[[8]], ld.plots[[9]], ld.plots[[10]], 
                              ld.plots[[11]], ld.plots[[12]], ld.plots[[13]], ld.plots[[14]], 
                              ncol = 4, nrow = 4))

# wet 
w.plots <- NULL
for(i in 1:length(species.list)){
  p1 <- map.plot(species.list[i], perc.diff.w)
  w.plots[[i]] <- p1
}

# add in legends -- all on different scales 
(wet.figure <- ggarrange(w.plots[[1]], w.plots[[2]], w.plots[[3]], w.plots[[4]], w.plots[[5]], 
                         w.plots[[6]], w.plots[[7]], w.plots[[8]], w.plots[[9]], w.plots[[10]], 
                         w.plots[[11]], w.plots[[12]], w.plots[[13]], w.plots[[14]], 
                         ncol = 4, nrow = 4))


    

    
    
## real occupancy modelling ----------------------
# did start messing around with real OMs, but don't think will follow up much further given difficulty interpreting changes in animal behavior... 
# anyhow, this is formatting code for a VERY BASIC OM; if did pursue, would consider multi-season OM or similar.... 

species <- "Bushbuck"
sub <- dat[dat$Species == species,]

# for now, select wet season 2019 (after), wet season 2018 (before)
sub.before <- sub[sub$Season.Year == "Early Dry 2017",] #19 weeks 
effort.before <- effort[effort$Season.Year == "Early Dry 2017",]
sub.after <- sub[sub$Season.Year == "Early Dry 2019",] #19 weeks 
effort.after <- effort[effort$Season.Year == "Early Dry 2019",]

sub.before <- sub.before[c("Camera", "Week", "occu")]; sub.after <- sub.after[c("Camera", "Week", "occu")]
effort.before <- effort.before[c("Camera", "Week")]; effort.after <- effort.after[c("Camera", "Week")]

sub.before <- sub.before[!duplicated(sub.before),]; sub.after <- sub.after[!duplicated(sub.after),]
effort.before <- effort.before[!duplicated(effort.before),]; effort.after <- effort.after[!duplicated(effort.after),]

# combine records and search effort 
before <- merge(effort.before, sub.before, all.x=T)
after <- merge(effort.after, sub.after, all.x=T)
before[is.na(before)] <- 0; after[is.na(after)] <- 0

# convert long to wide format 
before <- spread(before, Week, occu)
after <- spread(after, Week, occu)

# extract sites 
sites.before <- unique(before$Camera); (n.before <- length(sites.before))
sites.after <- unique(after$Camera); (n.after <- length(sites.after))
before$Camera <- NULL; after$Camera <- NULL

# covariates spatial 
covs.before <- covs %>% filter(Camera %in% sites.before) %>% select(-Camera) 
covs.after <- covs %>% filter(Camera %in% sites.after) %>% select(-Camera) 

# covariates temporal
x.before <- length(before)
x.after <- length(after)
time.before <- as.factor(rep(c(1:x.before), n.before))
time.after <- as.factor(rep(c(1:x.after), n.after)) 
covs.obs.before <- data.frame(time.before)
covs.obs.after <- data.frame(time.after)

# create unmarked dataframe 
camtrap.before <- unmarkedFrameOccu(y = before, siteCovs = covs.before, obsCovs = covs.obs.before)
camtrap.after <- unmarkedFrameOccu(y = after, siteCovs = covs.after, obsCovs = covs.obs.after)

# naive occupancy estimate
(naive_occ.before <- sum(ifelse(rowSums(before, na.rm=T)>0,1,0))/nrow(before[1])) #100% sites
(naive_occ.after <- sum(ifelse(rowSums(after, na.rm=T)>0,1,0))/nrow(after[1])) #97.7% sites

# basic occupancy model: homogenous detection and occupancy 
fm1.b <- occu(~1 ~1, camtrap.before)
fm1.a <- occu(~1 ~1, camtrap.after)

# detection, occupancy modelled using logit transformations - backtranform to original scale
backTransform(fm1.b, 'det')  #detection prob of 59.9% per week given bushbuck presence 
backTransform(fm1.a, 'det')  #detection prob of 58.5% per week given bushbuck presence 

backTransform(fm1.b, 'state')  #100% of sites estimated to be occupied 
backTransform(fm1.a, 'state')  #97.8% of sites estimated to be occupied

# adding covariates to before
fm2.b <- occu(~time.before ~1, camtrap.before) #time-specific detection, constant occupancy
fm3.b <- occu(~s.height ~1, camtrap.before) #heigh-varying detection, constant occupancy
fm4.b <- occu(~s.angle ~1, camtrap.before)
fm5.b <- occu(~detect.obscured ~1, camtrap.before)
fm6.b <- occu(~s.height+s.angle ~ 1, camtrap.before)
fm7.b <- occu(~s.height+s.angle+detect.obscured ~ 1, camtrap.before)
fm8.b <- occu(~s.height+s.angle+detect.obscured+time.before ~ 1, camtrap.before)

fmList <- fitList(
  "p(.)psi(.)" = fm1.b, "p(time)psi(.)" = fm2.b, "p(cam.height)psi(.)" = fm3.b, 
  "p(cam.angle)psi(.)" = fm4.b, "p(detect.obs)psi(.)" = fm5.b, "p(height+angle)psi(.)" = fm6.b,
  "p(height+angle+detect)psi(.)" = fm7.b, "p(height+angle+detect+time)psi(.)" = fm8.b)
modSel(fmList)

# need to do some kind of model averaging???? 

fm9.b <- occu(~detect.obscured ~s.cover, camtrap.before) 
fm10.b <- occu(~detect.obscured ~habitat.number, camtrap.before) 
fm11.b <- occu(~detect.obscured ~s.urema, camtrap.before) 
fm12.b <- occu(~detect.obscured ~s.road, camtrap.before) 

fmList <- fitList(
  "p(.)psi(.)" = fm1.b, "p(time)psi(.)" = fm2.b, "p(cam.height)psi(.)" = fm3.b, 
  "p(cam.angle)psi(.)" = fm4.b, "p(detect.obs)psi(.)" = fm5.b, "p(height+angle)psi(.)" = fm6.b,
  "p(height+angle+detect)psi(.)" = fm7.b, "p(height+angle+detect+time)psi(.)" = fm8.b, 
  "p(detect.obs)psi(cover)" = fm9.b, "p(detect.obs)psi(habitat)" = fm10.b, 
  "p(detect.obs)psi(urema)" = fm11.b, "p(detect.obs)psi(road)" = fm12.b)
modSel(fmList)

# no covariates on detection?? 

# calculate confidence intervals for estimates
confint(fm6, type = "state")
