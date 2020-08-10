## Wild Dog Analysis Libraries and Functions ## 

## source this file when running the primary analysis script 


## libraries to load 
library(overlap); library(sp); library(dplyr); library(lubridate); library(sf)
library(pryr); library(ggplot2); library(Hmisc); library(tidyverse); library(Rmisc)


## timeplot function (from K. Gaynor)
# - calculates diel activity patterns relative to sunrise/sunset 

timeplot <-function (A, n.grid = 128, kmax = 3, linecol = "#00BFC4",  ...) 
{
  bwA <- getBandWidth(A, kmax = kmax)
  
  xsc <- 24/(2 * pi)
  xxRad <- seq(0, 2 * pi, length = n.grid)
  xx <- xxRad * xsc
  densA <- densityFit(A, xxRad, bwA)/xsc
  
  ylim <- c(0, max(densA))
  plot(0, 0, type = "n", ylim = ylim, xlim = range(xx), xlab = "Time", 
       ylab = "Activity", xaxt = "n", ...)
  axis(1, at = c(0, 6, 12, 18, 24), labels = c("Midnight", 
                                               "Sunrise", "Noon", "Sunset", "Midnight"))
  lines(xx, densA, lty = 1, col = linecol, lwd = 2)
  return(invisible(list(x = xx, densityA = densA)))
}


## two-species timeplot function (from K. Gaynor)
# - calculates diel activity patterns relative to sunrise/sunset 

overlapPlot2 <- function (A, B, xscale = 24, linetype = c(1, 1), linecol = c("#F8766D", "#00BFC4"),  linewidth = c(2, 2),
                          n.grid = 128, kmax = 3, adjust = 1, ...) 
{
  bwA <- getBandWidth(A, kmax = kmax)/adjust
  bwB <- getBandWidth(B, kmax = kmax)/adjust
  if (is.na(bwA) || is.na(bwB)) 
    stop("Bandwidth estimation failed.")
  xsc <- if (is.na(xscale))
    1
  else xscale/(2 * pi)
  xxRad <- seq(0, 2 * pi, length = n.grid)
  xx <- xxRad * xsc
  densA <- densityFit(A, xxRad, bwA)/xsc
  densB <- densityFit(B, xxRad, bwB)/xsc
  densOL <- pmin(densA, densB)
  ylim <- c(0, max(densA, densB))
  plot(0, 0, type = "n", ylim = ylim, xlim = range(xx), xlab = "Time", 
       ylab = "Density", xaxt = "n", ...)
  if (is.na(xscale)) {
    axis(1, at = c(0, pi/2, pi, 3 * pi/2, 2 * pi), labels = c("0", 
                                                              expression(pi/2), expression(pi), expression(3 * 
                                                                                                             pi/2), expression(2 * pi)))
  }
  else if (xscale == 24) {
    axis(1, at = c(0, 6, 12, 18, 24), labels = c("Midnight", 
                                                 "Sunrise", "Noon", "Sunset", "Midnight"))
  }
  else {
    axis(1)
  }
  lines(xx, densA, lty = linetype[1], col = linecol[1], lwd = linewidth[[1]])
  lines(xx, densB, lty = linetype[2], col = linecol[2], lwd = linewidth[[2]])
  return(invisible(list(x = xx, densityA = densA, densityB = densB)))
}


## generating activity csv 
# - generates a table and prints a csv of activity overlap values
# - adjusts delta-hat overlap calculator for large and small sample sizes 

# - compares overlap for each species in dataset:
# - i) between species activity before and after wild dog release across entire survey area 
# - ii) between species activity before and after wild dog release in wild dog use area
# - iii) between species activity in areas of high and low wild dog use post-wild dog release 

activity.analysis <- function(dat){ 
  species <- unique(dat$Species)
  season <- c("All", "Wet", "Early Dry", "Late Dry") 
  activity.df <- NULL
  
  for(i in 1:length(species)){
    sub.season <- NULL 
    
    for(j in 1:length(season)){
 
      if(season[j] == "All"){
        # subset data 
        pre.dat.1 <- dat[dat$Species == species[i] & dat$TimePeriod == "PreRelease",]
        post.dat.1 <- dat[dat$Species == species[i] & dat$TimePeriod == "PostRelease",]
        
        use.dat <- post.dat.1[post.dat.1$Dog.Distance.Cut=="use",]
        luse.dat <- post.dat.1[post.dat.1$Dog.Distance.Cut=="less_use",]
      
        } else {
          # subset data 
          pre.dat.1 <- dat[dat$Species==species[i] & dat$TimePeriod=="PreRelease" & dat$season==season[j],]
          post.dat.1 <- dat[dat$Species==species[i] & dat$TimePeriod=="PostRelease" & dat$season==season[j],]

          use.dat <- post.dat.1[post.dat.1$Dog.Distance.Cut=="use",]
          luse.dat <- dat[post.dat.1$Dog.Distance.Cut=="less_use",]
          }
       
      # calculate overlap 1
      x <- min(nrow(pre.dat.1), nrow(post.dat.1))
      
      if(x < 20){
        activity_overlap_1 <- "ERROR: too few sightings"
      } else if(x >= 20 & x < 50) {
        activity_overlap_1 <- round(overlapEst(pre.dat.1$Time.Sun, post.dat.1$Time.Sun, type="Dhat1"), digits=3)
      } else if(x >= 50){ 
        activity_overlap_1 <- round(overlapEst(pre.dat.1$Time.Sun, post.dat.1$Time.Sun, type="Dhat4"), digits=3)
      }
      
      #calculate overlap 2 
      pre.dat.2 <- pre.dat.1[pre.dat.1$Dog.Distance.Cut=="use",]
      post.dat.2 <- post.dat.1[post.dat.1$Dog.Distance.Cut=="use",]
      y <- min(nrow(pre.dat.2), nrow(post.dat.2))
      
      if(y < 20){
        activity_overlap_2 <- "ERROR: too few sightings"
      } else if(y >= 20 & y < 50) {
        activity_overlap_2 <- round(overlapEst(pre.dat.2$Time.Sun, post.dat.2$Time.Sun, type="Dhat1"), digits=3)
      } else if(y >= 50){ 
        activity_overlap_2 <- round(overlapEst(pre.dat.2$Time.Sun, post.dat.2$Time.Sun, type="Dhat4"), digits=3)
      } 
      
      #calculate overlap 3
      z <- min(nrow(use.dat), nrow(luse.dat))
      
      if(z < 20){
        activity_overlap_3 <- "ERROR: too few sightings"
      } else if(z >= 20 & z < 50) {
        activity_overlap_3 <- round(overlapEst(use.dat$Time.Sun, luse.dat$Time.Sun, type = "Dhat1"), digits = 3)
      } else if( z >= 50){ 
        activity_overlap_3 <- round(overlapEst(use.dat$Time.Sun, luse.dat$Time.Sun, type = "Dhat4"), digits = 3)
      }
      
      sub.df <- data.frame(
        Species = as.character(species[i]),
        Season = season[j],
        Situtation = c("all.grid.prepost", "wd.area.prepost", "wd.use.unuse.post"),
        Overlap = c(activity_overlap_1, activity_overlap_2, activity_overlap_3))
      sub.season <- rbind(sub.season, sub.df)
      }
    activity.df <- rbind(activity.df, sub.season)
    }
  return(activity.df)
  }
  

## plotting pre-/post-release activity for each species, each season
# - creates a series of three plots 

activity.i <- function(species, dat){ 
  # frustratingly, can't loop because affects how plots are saved/returned 
  
  #wet season 
  pre.dat.w <- dat[dat$Species == species & dat$TimePeriod == "PreRelease" & dat$season == "Wet",]
  post.dat.w <- dat[dat$Species == species & dat$TimePeriod == "PostRelease" & dat$season == "Wet",]
  x = min(nrow(pre.dat.w), nrow(post.dat.w))
  
  #calculate overlap 
  if(x < 20){
    activity_overlap.w <- "ERROR: too few sightings"
  } else if(x >= 20 & x < 50) {
    activity_overlap.w <- paste("Overlap: ", round(overlapEst(pre.dat.w$Time.Sun, post.dat.w$Time.Sun, type = "Dhat1"), digits = 3), "; # Pre Obs: ", nrow(pre.dat.w), "; #Post Obs: ", nrow(post.dat.w), sep="")
  } else if( x >= 50){ 
    activity_overlap.w <- paste("Overlap: ", round(overlapEst(pre.dat.w$Time.Sun, post.dat.w$Time.Sun, type = "Dhat4"), digits = 3), "; # Pre Obs: ", nrow(pre.dat.w), "; #Post Obs: ", nrow(post.dat.w), sep="")
  }
  
  #plot 
  wet.plot %<a-% {
    overlapPlot2(pre.dat.w$Time.Sun, post.dat.w$Time.Sun) 
    title(paste(as.character(species), ": Wet Season", sep=""), adj=0)
    mtext(activity_overlap.w, adj=0)
  } 
 
  #early dry season 
  pre.dat.ed <- dat[dat$Species == species & dat$TimePeriod == "PreRelease" & dat$season == "Early Dry",]
  post.dat.ed <- dat[dat$Species == species & dat$TimePeriod == "PostRelease" & dat$season == "Early Dry",]
  x = min(nrow(pre.dat.ed), nrow(post.dat.ed))
  
  #calculate overlap 
  if(x < 20){
    activity_overlap.ed <- "ERROR: too few sightings"
  } else if(x >= 20 & x < 50) {
    activity_overlap.ed <- paste("Overlap: ", round(overlapEst(pre.dat.ed$Time.Sun, post.dat.ed$Time.Sun, type="Dhat1"), digits=3), "; # Pre Obs: ", nrow(pre.dat.ed), "; # Post Obs: ", nrow(post.dat.ed), sep="")
  } else if( x >= 50){ 
    activity_overlap.ed <- paste("Overlap: ", round(overlapEst(pre.dat.ed$Time.Sun, post.dat.ed$Time.Sun, type="Dhat4"), digits=3), "; # Pre Obs: ", nrow(pre.dat.ed), "; # Post Obs: ", nrow(post.dat.ed), sep="")
  } 
  
  #plot 
  earlydry.plot %<a-% {
    overlapPlot2(pre.dat.ed$Time.Sun, post.dat.ed$Time.Sun) 
    title(paste(as.character(species), ": Early Dry Season", sep=""), adj=0)
    mtext(activity_overlap.ed, adj=0)
  }     
    
  #late dry season 
  pre.dat.ld <- dat[dat$Species == species & dat$TimePeriod == "PreRelease" & dat$season == "Late Dry",]
  post.dat.ld <- dat[dat$Species == species & dat$TimePeriod == "PostRelease" & dat$season == "Late Dry",]
  x = min(nrow(pre.dat.ld), nrow(post.dat.ld))
  
  #calculate overlap 
  if(x < 20){
    activity_overlap.ld <- "ERROR: too few sightings"
  } else if(x >= 20 & x < 50) {
    activity_overlap.ld <- paste("Overlap: ", round(overlapEst(pre.dat.ld$Time.Sun, post.dat.ld$Time.Sun, type="Dhat1"), digits=3), "; # Pre Obs: ", nrow(pre.dat.ld), "; # Post Obs: ", nrow(post.dat.ld), sep="")
  } else if( x >= 50){ 
    activity_overlap.ld <- paste("Overlap: ", round(overlapEst(pre.dat.ld$Time.Sun, post.dat.ld$Time.Sun, type="Dhat4"), digits=3), "; # Pre Obs: ", nrow(pre.dat.ld), "; # Post Obs: ", nrow(post.dat.ld), sep="")
  }
  
  #plot 
  latedry.plot %<a-% {
    overlapPlot2(pre.dat.ld$Time.Sun, post.dat.ld$Time.Sun) 
    title(paste(as.character(species), ": Late Dry Season", sep=""), adj=0)
    mtext(activity_overlap.ld, adj=0)
  }    
  
  par(mfrow=c(1,3))
  return(list(wet.plot, earlydry.plot, latedry.plot))
}
  
  
## plotting pre-/post-release activity for each species, each season in wild dog use areas 
# - creates a series of three plots 

activity.ii <- function(species, dat){ 
  # frustratingly, can't loop because affects how plots are saved/returned 
  
  #all data 
  pre.dat.a <- dat[dat$Species==species & dat$TimePeriod=="PreRelease" & dat$Dog.Distance.Cut=="use",]
  post.dat.a <- dat[dat$Species==species & dat$TimePeriod=="PostRelease" & dat$Dog.Distance.Cut=="use",]
  x = min(nrow(pre.dat.a), nrow(post.dat.a))
  
  #calculate overlap 
  if(x < 20){
    activity_overlap.a <- "ERROR: too few sightings"
  } else if(x >= 20 & x < 50) {
    activity_overlap.a <- paste("Overlap: ", round(overlapEst(pre.dat.a$Time.Sun, post.dat.a$Time.Sun, type="Dhat1"), digits=3), "; # Pre Obs: ", nrow(pre.dat.a), "; # Post Obs: ", nrow(post.dat.a), sep="")
  } else if( x >= 50){ 
    activity_overlap.a <- paste("Overlap: ", round(overlapEst(pre.dat.a$Time.Sun, post.dat.a$Time.Sun, type="Dhat4"), digits=3), "; # Pre Obs: ", nrow(pre.dat.a), "; # Post Obs: ", nrow(post.dat.a), sep="")
  }
  
  #plot 
  all.plot %<a-% {
    overlapPlot2(pre.dat.a$Time.Sun, post.dat.a$Time.Sun) 
    title(as.character(species), adj=0)
    mtext(activity_overlap.a, adj=0)
  } 
  
  #late dry data 
  pre.dat.ld <- dat[dat$Species==species & dat$TimePeriod=="PreRelease" & dat$Dog.Distance.Cut=="use" & 
                      dat$season == "Late Dry",]
  post.dat.ld <- dat[dat$Species==species & dat$TimePeriod=="PostRelease" & dat$Dog.Distance.Cut=="use" & 
                       dat$season == "Late Dry",]
  x = min(nrow(pre.dat.ld), nrow(post.dat.ld))
  
  #calculate overlap 
  if(x < 20){
    activity_overlap.ld <- "ERROR: too few sightings"
  } else if(x >= 20 & x < 50) {
    activity_overlap.ld <- paste("Overlap: ", round(overlapEst(pre.dat.ld$Time.Sun, post.dat.ld$Time.Sun, type="Dhat1"), digits=3), "; # Pre Obs: ", nrow(pre.dat.ld), "; # Post Obs: ", nrow(post.dat.ld), sep="")
  } else if( x >= 50){ 
    activity_overlap.ld <- paste("Overlap: ", round(overlapEst(pre.dat.ld$Time.Sun, post.dat.ld$Time.Sun, type="Dhat4"), digits=3), "; # Pre Obs: ", nrow(pre.dat.ld), "; # Post Obs: ", nrow(post.dat.ld), sep="")
  }
  
  #plot 
  latedry.plot %<a-% {
    overlapPlot2(pre.dat.ld$Time.Sun, post.dat.ld$Time.Sun) 
    title(paste(as.character(species), ": Late Dry Season", sep=""), adj=0)
    mtext(activity_overlap.ld, adj=0)
  } 
  
  #wet data 
  pre.dat.w <- dat[dat$Species==species & dat$TimePeriod=="PreRelease" & dat$Dog.Distance.Cut=="use" & 
                      dat$season == "Wet",]
  post.dat.w <- dat[dat$Species==species & dat$TimePeriod=="PostRelease" & dat$Dog.Distance.Cut=="use" & 
                       dat$season == "Wet",]
  x = min(nrow(pre.dat.w), nrow(post.dat.w))
  
  #calculate overlap 
  if(x < 20){
    activity_overlap.w <- "ERROR: too few sightings"
  } else if(x >= 20 & x < 50) {
    activity_overlap.w <- paste("Overlap: ", round(overlapEst(pre.dat.w$Time.Sun, post.dat.w$Time.Sun, type="Dhat1"), digits=3), "; # Pre-Obs: ", nrow(pre.dat.w), "; # Post Obs: ", nrow(post.dat.w), sep="")
  } else if( x >= 50){ 
    activity_overlap.w <- paste("Activity overlap: ", round(overlapEst(pre.dat.w$Time.Sun, post.dat.w$Time.Sun, type="Dhat4"), digits=3), "; # Pre Obs: ", nrow(pre.dat.w), "; # Post Obs: ", nrow(post.dat.w), sep="")
  }
  
  #plot 
  wet.plot %<a-% {
    overlapPlot2(pre.dat.w$Time.Sun, post.dat.w$Time.Sun) 
    title(paste(as.character(species), ": Wet Season", sep=""), adj=0)
    mtext(activity_overlap.w, adj=0)
  } 
  
  par(mfrow=c(1,3))
  return(list(all.plot, latedry.plot, wet.plot))
}

## plotting post-release activity for each species in/out of wild dog areas
# - creates a series of three plots 

activity.iii <- function(species, dat){ 
  # frustratingly, can't loop because affects how plots are saved/returned 
  
  #all data 
  use.dat.a <- dat[dat$Species==species & dat$TimePeriod=="PostRelease" & dat$Dog.Distance.Cut=="use",]
  luse.dat.a <- dat[dat$Species==species & dat$TimePeriod=="PostRelease" & dat$Dog.Distance.Cut=="less_use",]
  x = min(nrow(use.dat.a), nrow(luse.dat.a))

  #calculate overlap 
  if(x < 20){
    activity_overlap.a <- "ERROR: too few sightings"
  } else if(x >= 20 & x < 50) {
    activity_overlap.a<- paste("Overlap: ", round(overlapEst(use.dat.a$Time.Sun, luse.dat.a$Time.Sun, type = "Dhat1"), digits = 3), "; # Near WD: ", nrow(use.dat.a), "; # Far WD: ", nrow(luse.dat.a), sep="")
  } else if( x >= 50){ 
    activity_overlap.a <- paste("Overlap: ", round(overlapEst(use.dat.a$Time.Sun, luse.dat.a$Time.Sun, type = "Dhat4"), digits = 3), "; # Near WD: ", nrow(use.dat.a), "; # Far WD: ", nrow(luse.dat.a), sep="")
  }
  
  #plot 
  all.plot %<a-% {
    overlapPlot2(use.dat.a$Time.Sun, luse.dat.a$Time.Sun) 
    title(paste(as.character(species)), adj=0)
    mtext(activity_overlap.a, adj=0)
  } 
  
  #late dry data 
  use.dat.ld <- dat[dat$Species==species & dat$TimePeriod=="PostRelease" & dat$Dog.Distance.Cut=="use" & 
                     dat$season == "Late Dry",]
  luse.dat.ld <- dat[dat$Species==species & dat$TimePeriod=="PostRelease" & dat$Dog.Distance.Cut=="less_use" & 
                      dat$season == "Late Dry",]
  x = min(nrow(use.dat.ld), nrow(luse.dat.ld))
  
  #calculate overlap 
  if(x < 20){
    activity_overlap.ld <- "ERROR: too few sightings"
  } else if(x >= 20 & x < 50) {
    activity_overlap.ld<- paste("Overlap: ", round(overlapEst(use.dat.ld$Time.Sun, luse.dat.ld$Time.Sun, type = "Dhat1"), digits=3), "; # Near WD: ", nrow(use.dat.ld), "; # Far WD: ", nrow(luse.dat.ld), sep="")
  } else if( x >= 50){ 
    activity_overlap.ld <- paste("Overlap: ", round(overlapEst(use.dat.ld$Time.Sun, luse.dat.ld$Time.Sun, type = "Dhat4"), digits=3), "; # Near WD: ", nrow(use.dat.ld), "; # Far WD: ", nrow(luse.dat.ld), sep="")
  }
  
  #plot 
  latedry.plot %<a-% {
    overlapPlot2(use.dat.ld$Time.Sun, luse.dat.ld$Time.Sun) 
    title(paste(as.character(species), ": Late Dry Season", sep=""), adj=0)
    mtext(activity_overlap.ld, adj=0)
  } 
  
  #wet data 
  use.dat.w<- dat[dat$Species==species & dat$TimePeriod=="PostRelease" & dat$Dog.Distance.Cut=="use" & 
                      dat$season == "Wet",]
  luse.dat.w <- dat[dat$Species==species & dat$TimePeriod=="PostRelease" & dat$Dog.Distance.Cut=="less_use" & 
                       dat$season == "Wet",]
  x = min(nrow(use.dat.w), nrow(luse.dat.w))
  
  #calculate overlap 
  if(x < 20){
    activity_overlap.w <- "ERROR: too few sightings"
  } else if(x >= 20 & x < 50) {
    activity_overlap.w <- paste("Overlap: ", round(overlapEst(use.dat.w$Time.Sun, luse.dat.w$Time.Sun, type = "Dhat1"), digits=3), "; # Near WD: ", nrow(use.dat.w), "; # Far WD: ", nrow(luse.dat.w), sep="")
  } else if( x >= 50){ 
    activity_overlap.w <- paste("Overlap: ", round(overlapEst(use.dat.w$Time.Sun, luse.dat.w$Time.Sun, type = "Dhat4"), digits=3), "; # Near WD: ", nrow(use.dat.w), "; # Far WD: ", nrow(luse.dat.w), sep="")
  }
  
  #plot 
  wet.plot %<a-% {
    overlapPlot2(use.dat.w$Time.Sun, luse.dat.w$Time.Sun) 
    title(paste(as.character(species), ": Wet Season", sep=""), adj=0)
    mtext(activity_overlap.w, adj=0)
  } 
  
  par(mfrow=c(1,3))
  return(list(all.plot,  latedry.plot, wet.plot))
}


## RAI plotting function 
# - plots RAI values across camera trap grid relative to wild dog use areas
# - can choose whether log or regular RAI
# - need to select what species and season want to plot 

distribution.ii <- function(species, season.choice, log.choice = T, dat, effort, dog.poly){
  
  # calculate days operational per camera 
  camop.df <- effort %>% 
    group_by(site, season, TimePeriod) %>% 
    dplyr::summarise(Operation = n()) %>%
    filter(season == season.choice)
  names(camop.df)[1] <- "Camera"
  
  camop <- data.frame(
    Camera = rep(unique(dat$Camera), each=2), 
    season = as.character(season.choice), 
    TimePeriod = rep(c("PreRelease", "PostRelease"), 57))
  
  camop <- merge(camop, camop.df, all.x=T); rm(camop.df)
  camop[is.na(camop)] <- 0 #sites off 
  
  # calculate sightings per camera 
  record_count <- dat %>% 
    filter(Species == species, season == season.choice) %>% 
    group_by(Camera, season, TimePeriod) %>% 
    dplyr::summarise(Detections = n()) # when have counts, TotalCounts = sum(counts)) 
  
  # join and format 
  RAI.table <- merge(camop, record_count, all.x=T)
  RAI.table[is.na(RAI.table)] <- 0
  
  # calculate RAI
  RAI.table$RAI.det <- RAI.table$Detections / RAI.table$Operation  
  #RAI.table$RAI.count <- RAI.table$TotalCounts / RAI.table$Operation 
  
  RAI.table$tag <- paste(RAI.table$TimePeriod, RAI.table$season)
  RAI.table <- spread(RAI.table[c("Camera", "tag", "RAI.det")], tag, RAI.det)
  names(RAI.table)[c(2:3)] <- c("Post", "Pre")
  RAI.table$Dif <- RAI.table$Post - RAI.table$Pre
  
  # log 
  if(log.choice == T){
    RAI.table$Post <- ifelse(RAI.table$Post == 0, log(RAI.table$Post+0.001), log(RAI.table$Post))
    RAI.table$Pre <- ifelse(RAI.table$Pre == 0, log(RAI.table$Pre+0.001), log(RAI.table$Pre))
    RAI.table$Dif <- ifelse(RAI.table$Dif == 0, log(RAI.table$Dif+0.001), log(RAI.table$Dif))
  }
  
  RAI.table <- merge(grid, RAI.table)
  
  plot.pre <-  ggplot() + 
    geom_sf(data = RAI.table$geometry, fill = "grey", color="white") + 
    geom_sf(data = RAI.table$geometry, aes(fill = RAI.table$Pre)) + 
    geom_sf(data = dog.poly, fill=NA, color="red") +
    ggtitle(paste(species, ": PreRelease ", season, sep="")) +
    labs(fill = "RAI")
  
  plot.post <- ggplot() + 
    geom_sf(data = RAI.table$geometry, fill = "grey", color="white") + 
    geom_sf(data = RAI.table$geometry, aes(fill = RAI.table$Post)) + 
    geom_sf(data = dog.poly, fill=NA, color="red") + 
    ggtitle(paste(species, ": PostRelease ", season, sep="")) +
    labs(fill = "RAI")
  
  plot.dif <- ggplot() + 
    geom_sf(data = RAI.table$geometry, fill = "grey", color="white") + 
    geom_sf(data = RAI.table$geometry, aes(fill = RAI.table$Dif)) + 
    geom_sf(data = dog.poly, fill=NA, color="red")+ 
    ggtitle(paste(species, ": Difference ", season, sep="")) +
    labs(fill = "delta_RAI")
  
  return(multiplot(plot.pre, plot.post, plot.dif, cols=3))
 
}

# Diel RAI plotting function 
distribution.ii <- function(species, season.choice, log.choice =T, dat, effort, dog.poly){
  
  library(animation); library(tweenr)
  
  # calculate days operational per camera 
  camop.df <- effort %>% 
    group_by(site, season, TimePeriod) %>% 
    dplyr::summarise(Operation = n()) %>%
    filter(season == season.choice)
  names(camop.df)[1] <- "Camera"
  
  camop <- data.frame(
    Camera = rep(unique(dat$Camera), each=48), 
    Hour = c(0:23),  
    TimePeriod = rep(c(rep("PreRelease",24), rep("PostRelease",24)),57), 
    season = as.character(season.choice)) 
  
  camop <- merge(camop, camop.df, all.x=T)
  camop[is.na(camop)] <- 0 #sites off 
  
  # calculate sightings per camera 
  record_count <- dat %>% 
    filter(Species == species, season == season.choice) %>% 
    mutate(Hour = hour(datetime)) %>% 
    group_by(Camera, season, TimePeriod, Hour) %>% 
    dplyr::summarise(Detections = n()) # when have counts, TotalCounts = sum(counts)) 
  
  # join and format 
  RAI.table <- merge(camop, record_count, all.x=T)
  RAI.table[is.na(RAI.table)] <- 0
  
  # calculate RAI
  RAI.table$RAI.det <- RAI.table$Detections / RAI.table$Operation  
  #RAI.table$RAI.count <- RAI.table$TotalCounts / RAI.table$Operation 
  
  # log 
  if(log.choice == T){
    RAI.table$RAI.det <- ifelse(RAI.table$RAI.det == 0, log(RAI.table$RAI.det+0.001), log(RAI.table$RAI.det))
  }
