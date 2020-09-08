## Function for MSP_WildDog_Activity_Level.R

# calculates data for plotting model results where differences in activty patterns before/after wild dog release is significant
# - if before/after interacts significantly with wild dog use areas, set WildDogUse = T, otherwise, set WildDogUse = F
activity.level.plot <- function(dat, species, nboots, WildDogUse=T){
  
  if(WildDogUse == T){
    
    tm.pre.no <- 2*pi*dat[dat$Species == species & dat$tag == "PreRelease no.use",]$Sun.Hour/24
    wts.pre.no <- 1/ifelse(tm.pre.no>pi/2 & tm.pre.no<pi*3/2, 1.5, 1)
    mod.pre.no <- fitact(tm.pre.no, sample="model", reps=nboots, wt=wts.pre.no)
    pre.no <- as.data.frame(mod.pre.no@pdf)
    
    pre.no$x <- pre.no$x / 6.283*24
    pre.no$y <- pre.no$y / 4
    pre.no$lcl <- pre.no$lcl / 4
    pre.no$ucl <- pre.no$ucl / 4
    pre.no$tag <- "Pre-Release / No Wild Dog Use"
    pre.no$wild.dog <- "No Wild Dog Use"
    pre.no$TimePeriod <- "Pre-Release"
    
    tm.post.no <- 2*pi*dat[dat$Species == species & dat$tag == "PostRelease no.use",]$Sun.Hour/24
    wts.post.no <- 1/ifelse(tm.post.no>pi/2 & tm.post.no<pi*3/2, 1.5, 1)
    mod.post.no <- fitact(tm.post.no, sample="model", reps=nboots, wt=wts.post.no)
    post.no <- as.data.frame(mod.post.no@pdf)
    post.no$x <- post.no$x / 6.283*24
    post.no$y <- post.no$y / 4
    post.no$lcl <- post.no$lcl / 4
    post.no$ucl <- post.no$ucl / 4
    post.no$tag <- "Post-Release / No Wild Dog Use"
    post.no$wild.dog <- "No Wild Dog Use"
    post.no$TimePeriod <- "Post-Release"
    
    tm.pre.low <- 2*pi*dat[dat$Species == species & dat$tag == "PreRelease low.use",]$Sun.Hour/24
    wts.pre.low <- 1/ifelse(tm.pre.low>pi/2 & tm.pre.low<pi*3/2, 1.5, 1)
    mod.pre.low <- fitact(tm.pre.low, sample="model", reps=nboots, wt=wts.pre.low)
    pre.low <- as.data.frame(mod.pre.low@pdf)
    pre.low$x <- pre.low$x / 6.283*24
    pre.low$y <- pre.low$y / 4
    pre.low$lcl <- pre.low$lcl / 4
    pre.low$ucl <- pre.low$ucl / 4
    pre.low$tag <- "Pre-Release / Low Wild Dog Use"
    pre.low$wild.dog <- "Low Wild Dog Use"
    pre.low$TimePeriod <- "Pre-Release"
    
    tm.post.low <- 2*pi*dat[dat$Species == species & dat$tag == "PostRelease low.use",]$Sun.Hour/24
    wts.post.low <- 1/ifelse(tm.post.low>pi/2 & tm.post.low<pi*3/2, 1.5, 1)
    mod.post.low <- fitact(tm.post.low, sample="model", reps=nboots, wt=wts.post.low)
    post.low <- as.data.frame(mod.post.low@pdf)
    post.low$x <- post.low$x / 6.283*24
    post.low$y <- post.low$y / 4
    post.low$lcl <- post.low$lcl / 4
    post.low$ucl <- post.low$ucl / 4
    post.low$tag <- "Post-Release / Low Wild Dog Use"
    post.low$wild.dog <- "Low Wild Dog Use"
    post.low$TimePeriod <- "Post-Release"
    
    tm.pre.high <- 2*pi*dat[dat$Species == species & dat$tag == "PreRelease high.use",]$Sun.Hour/24
    wts.pre.high <- 1/ifelse(tm.pre.high>pi/2 & tm.pre.high<pi*3/2, 1.5, 1)
    mod.pre.high <- fitact(tm.pre.high, sample="model", reps=nboots, wt=wts.pre.high)
    pre.high <- as.data.frame(mod.pre.high@pdf)
    pre.high$x <- pre.high$x / 6.283*24
    pre.high$y <- pre.high$y / 4
    pre.high$lcl <- pre.high$lcl / 4
    pre.high$ucl <- pre.high$ucl / 4
    pre.high$tag <- "Pre-Release / High Wild Dog Use"
    pre.high$wild.dog <- "High Wild Dog Use"
    pre.high$TimePeriod <- "Pre-Release"
    
    tm.post.high <- 2*pi*dat[dat$Species == species & dat$tag == "PostRelease high.use",]$Sun.Hour/24
    wts.post.high <- 1/ifelse(tm.post.high>pi/2 & tm.post.high<pi*3/2, 1.5, 1)
    mod.post.high <- fitact(tm.post.high, sample="model", reps=nboots, wt=wts.post.high)
    post.high <- as.data.frame(mod.post.high@pdf)
    post.high$x <- post.high$x / 6.283*24
    post.high$y <- post.high$y / 4
    post.high$lcl <- post.high$lcl / 4
    post.high$ucl <- post.high$ucl / 4
    post.high$tag <- "Post-Release / High Wild Dog Use"
    post.high$wild.dog <- "High Wild Dog Use"
    post.high$TimePeriod <- "Post-Release"
    
    final.dat <- rbind(pre.no, post.no, pre.low, post.low, pre.high, post.high)
  } 
  
  else if(WildDogUse == F) {
    
    tm.pre <- 2*pi*dat[dat$Species == species & dat$TimePeriod == "PreRelease",]$Sun.Hour/24
    wts.pre <- 1/ifelse(tm.pre>pi/2 & tm.pre<pi*3/2, 1.5, 1)
    mod.pre <- fitact(tm.pre, sample="model", reps=nboots, wt=wts.pre)
    pre <- as.data.frame(mod.pre@pdf)
    pre$x <- pre$x / 6.283*24
    pre$y <- pre$y / 4
    pre$lcl <- pre$lcl / 4
    pre$ucl <- pre$ucl / 4
    pre$tag <- "Pre-Release"
    
    tm.post <- 2*pi*dat[dat$Species == species & dat$TimePeriod == "PostRelease",]$Sun.Hour/24
    wts.post <- 1/ifelse(tm.post>pi/2 & tm.post<pi*3/2, 1.5, 1)
    mod.post <- fitact(tm.post, sample="model", reps=nboots, wt=wts.post)
    post <- as.data.frame(mod.post@pdf)
    post$x <- post$x / 6.283*24
    post$y <- post$y / 4
    post$lcl <- post$lcl / 4
    post$ucl <- post$ucl / 4
    post$tag <- "Post-Release"
    
    final.dat <- rbind(pre, post)
  }
  
  return(final.dat)
}

# calculates Dhat4 overlap index between two circular distributions 
# - requires at least 10 sightings to calculate, otherwise flags error 
activity.overlap <- function(dat, species, season, wild.dog.use, nboots){
  
  if(wild.dog.use == "all"){
    
    before <- dat[dat$Species == species & dat$Season == season & dat$TimePeriod == "PreRelease",]
    after <- dat[dat$Species == species & dat$Season == season & dat$TimePeriod == "PostRelease",]
    
    if(nrow(before) < 10 | nrow(after) < 10){ 
      overlap <- data.frame(Species = as.character(species.list[i]),
                            Season = as.character(season), 
                            WildDogUse = as.character("All"), 
                            Overlap = as.character("Error - too few sightings"))
    } else {
      wts.before <- 1/ifelse(before$Time.Sun>pi/2 & before$Time.Sun<pi*3/2, 1.5, 1)
      wts.after <- 1/ifelse(after$Time.Sun>pi/2 & after$Time.Sun<pi*3/2, 1.5, 1)
      before.fit <- fitact(before$Time.Sun, sample="model", reps=nboots, wt=wts.before)
      after.fit <- fitact(after$Time.Sun, sample="model", reps=nboots, wt=wts.after)
      overlap <- data.frame(Species = as.character(species), 
                            Season = as.character(season),
                            WildDogUse = as.character("All"),
                            Overlap = as.character(round(ovl4(before.fit, after.fit),3)))
    }
    
  } else {
    
    before <- dat[dat$Species == species & dat$Season == season & dat$WildDogUse == wild.dog.use &
                    dat$TimePeriod == "PreRelease",]
    after <- dat[dat$Species == species & dat$Season == season & dat$WildDogUse == wild.dog.use &
                   dat$TimePeriod == "PostRelease",]
    
    if(nrow(before) < 10 | nrow(after) < 10){ 
      overlap <- data.frame(Species = as.character(species.list[i]),
                            Season = as.character(season), 
                            WildDogUse = as.character(wild.dog.use), 
                            Overlap = as.character("Error - too few sightings"))
    } else {
      wts.before <- 1/ifelse(before$Time.Sun>pi/2 & before$Time.Sun<pi*3/2, 1.5, 1)
      wts.after <- 1/ifelse(after$Time.Sun>pi/2 & after$Time.Sun<pi*3/2, 1.5, 1)
      before.fit <- fitact(before$Time.Sun, sample="model", reps=nboots, wt=wts.before)
      after.fit <- fitact(after$Time.Sun, sample="model", reps=nboots, wt=wts.after)
      overlap <- data.frame(Species = as.character(species), 
                            Season = as.character(season),
                            WildDogUse = as.character(wild.dog.use),
                            Overlap = as.character(round(ovl4(before.fit, after.fit),3)))
    }
  }
  return(overlap)
}
  
# calculates activity levels, differences in activity pre/post, and differences in timing of activity pre/post
# - stores three resultant data fraemes in a list (unlist afterwards to examine)
act.diel.difs <- function(dat, species, wild.dog.use, nboots){
  
  if(wild.dog.use == "all"){
    
    # calculate activity 
    before <- dat[(dat$Species == species & dat$TimePeriod == "PreRelease"),]
    after <- dat[(dat$Species == species & dat$TimePeriod == "PostRelease"),]
    
    wts.before <- 1/ifelse(before$Time.Sun>pi/2 & before$Time.Sun<pi*3/2, 1.5, 1)
    wts.after <- 1/ifelse(after$Time.Sun>pi/2 & after$Time.Sun<pi*3/2, 1.5, 1)
    
    before.fit <- fitact(before$Time.Sun, sample="model", reps=nboots, wt=wts.before)
    after.fit <- fitact(after$Time.Sun, sample="model", reps=nboots, wt=wts.after)
    
    # save activity levels before and after 
    act.lev <- data.frame(rbind(t(as.data.frame(before.fit@act)), t(as.data.frame(after.fit@act))))
    act.lev$Species <- as.character(species)
    act.lev$Period <- c("Pre-Release", "Post-Release")
    act.lev$WildDogUse <- as.character("all")
    names(act.lev)[c(1:4)] <- c("Activity Level", "SE", "LCI.95%", "UCI.95%")
    row.names(act.lev) <- c(); act.lev <- act.lev[,c(5:7, 1:4)]
    act.lev[c(4:7)] <- round(act.lev[c(4:7)], 3)
    
    # differences in activity levels before and after 
    act.dif <- as.data.frame(compareAct(list(before.fit,after.fit)))
    act.dif$Species <- as.character(species)
    act.dif$WildDogUse <- "All"; row.names(act.dif) <- c()
    act.dif <- act.dif[c(5:6, 1:4)]
    act.dif[c(3:6)] <- round(act.dif[c(3:6)], 3); act.dif$sig <- ifelse(act.dif$p <= 0.05, "*","")
    
    # differences in diel patterning before and after 
    diel.dif <- as.data.frame(t(compareCkern(before.fit, after.fit, reps=nboots)))
    diel.dif$Species <- as.character(species)
    diel.dif$WildDogUse <- "All"
    diel.dif <- diel.dif[c(5:6,1:4)]
    names(diel.dif)[3:6] <- c("Obs. Overlap Index", "Null Mean Overlap Index", "SE Null", "Prob Obs. Index Chance")
    diel.dif[,c(3:6)] <- round(diel.dif[,c(3:6)], 3)
    
  } else {
    
    # calculate activity 
    before <- dat[(dat$Species == species & dat$WildDogUse == wild.dog.use & dat$TimePeriod == "PreRelease"),]
    after <- dat[(dat$Species == species & dat$WildDogUse == wild.dog.use & dat$TimePeriod == "PostRelease"),]
    
    wts.before <- 1/ifelse(before$Time.Sun>pi/2 & before$Time.Sun<pi*3/2, 1.5, 1)
    wts.after <- 1/ifelse(after$Time.Sun>pi/2 & after$Time.Sun<pi*3/2, 1.5, 1)
    
    before.fit <- fitact(before$Time.Sun, sample="model", reps=nboots, wt=wts.before)
    after.fit <- fitact(after$Time.Sun, sample="model", reps=nboots, wt=wts.after)
    
    # save activity levels before and after 
    act.lev <- data.frame(rbind(t(as.data.frame(before.fit@act)), t(as.data.frame(after.fit@act))))
    act.lev$Species <- as.character(species)
    act.lev$Period <- c("Pre-Release", "Post-Release")
    act.lev$WildDogUse <- as.character(wild.dog.use)
    names(act.lev)[c(1:4)] <- c("Activity Level", "SE", "LCI.95%", "UCI.95%")
    row.names(act.lev) <- c(); act.lev <- act.lev[,c(5:7, 1:4)]
    act.lev[c(4:7)] <- round(act.lev[c(4:7)], 3)
    
    # differences in activity levels before and after 
    act.dif <- as.data.frame(compareAct(list(before.fit,after.fit)))
    act.dif$Species <- as.character(species)
    act.dif$WildDogUse <- "No Use"; row.names(act.dif) <- c()
    act.dif <- act.dif[c(5:6, 1:4)]
    act.dif[c(3:6)] <- round(act.dif[c(3:6)], 3); act.dif$sig <- ifelse(act.dif$p <= 0.05, "*","")
    
    # differences in diel patterning before and after 
    diel.dif <- as.data.frame(t(compareCkern(before.fit, after.fit, reps=nboots)))
    diel.dif$Species <- as.character(species)
    diel.dif$WildDogUse <- "No Use"
    diel.dif <- diel.dif[c(5:6,1:4)]
    names(diel.dif)[3:6] <- c("Obs. Overlap Index", "Null Mean Overlap Index", "SE Null", "Prob Obs. Index Chance")
    diel.dif[,c(3:6)] <- round(diel.dif[,c(3:6)], 3)
    
  }
  
  # bundle and return (unlist dataframes afterwards)
  total.dat <- list(act.lev, act.dif, diel.dif)
  return(total.dat) 
}
