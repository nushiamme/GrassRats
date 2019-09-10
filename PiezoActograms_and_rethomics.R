## Trying out CATkit package for circadian rhythm and activity data
require(tidyverse)
require(reshape2)
#require(wmtsa)
#require(waveslim)
#require(biwavelet)
#require(stringr) ## For padding with leading 0's

require(ggplot2)
library(behavr)
library(ggetho)
library(zeitgebr) ## For periodogram and spectrogram
library(sleepr) ## For sleep analyses
library(dplyr) ## for missing times 
library(DataCombine) # To insert rows at specific locations in dataframe
library(viridis) # for beautiful colors!

## Set working directory
setwd("E:\\Ex_Google_Drive\\Piezo_data\\For_rethomics")

source("Spectrogram.R")

## Metadata file for rethomics behavr table
meta_full <- read.csv("E:\\Ex_Google_Drive\\Piezo_data\\Meta_ExptPhase1.csv")

meta_activ <- meta_full[,c("Indiv", "Sex", "Photoperiod", "Sugar", "Chamber")]

m.Act <- read.csv(".\\Melted\\Melted_PiezoActivity_Phase1_new.csv") 

## General functions
my_theme <- theme_classic(base_size = 15) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))

my_theme2 <- theme_classic(base_size = 30) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))

#my_colors <- c("#23988aff", "#F38BA8", "#440558ff", "#9ed93aff")

#### Ignore if reading in Melted activity csv; only run to re-process raw data ####
## Get all the required csv's together
paths <- dir(pattern = "\\.csv$")
names(paths) <- basename(paths)
paths
Activ <- lapply(paths, read.csv, header=T)

#Activsumm <- data.frame(matrix(NA, nrow=length(Activ), ncol=25))
#names(Activsumm) <- c("Min", "Mean", "Max", "File") #,"sd"
m.Activ <- list()
Activ$File <- noquote(names(Activ))
for(i in 1:(length(Activ)-1)) { 
  ## Splitting the file name to get IndivID, date, and time
  Activ[[i]]$Treatment <- 
    unlist(lapply(strsplit(as.character(Activ$File[i]), "_"), "[", 1))
  Activ[[i]]$FileDate <- 
    unlist(lapply(strsplit(as.character(Activ$File[i]), "_"), "[", 2))
  #Activ[[i]][[1]] <- gsub('-', '/', Activ[[i]][[1]])
  Activ[[i]]$Month <-
    unlist(lapply(strsplit(as.character(Activ[[i]][[1]]), "/"), "[", 1))
  Activ[[i]]$Day <-
    unlist(lapply(strsplit(as.character(Activ[[i]][[1]]), "/"), "[", 2))
  Activ[[i]]$Year <-
    unlist(lapply(strsplit(as.character(Activ[[i]][[1]]), "[/ ]+"), "[", 3))
  Activ[[i]]$Time <-
    unlist(lapply(strsplit(as.character(Activ[[i]][[1]]), " +"), "[", 2))
  Activ[[i]]$Hour <-
    unlist(lapply(strsplit(as.character(Activ[[i]]$Time), ":"), "[", 1))
  Activ[[i]]$Minute <-
    unlist(lapply(strsplit(as.character(Activ[[i]]$Time), ":"), "[", 2))
  m.Activ[[i]] <- melt(Activ[[i]], id.vars= c("Day", "Month", "Year", "Time", "Hour", "Minute", "Treatment"), 
                  measure.vars=c("G9", "G5", "G14", "G16", "G18", "G10", "G8", "G2", "T10", "T5", "T17", "T12", "G4", "G1","G13",
                                 "G17", "G19", "G11", "G12",  "G3", "T9", "T11", "T19", "T13"))
  names(m.Activ[[i]]) <- c("Day", "Month", "Year", "Time", "Hour", "Minute", "Treatment", "Indiv", "PiezoAct")
}
m.Act <- do.call(rbind, m.Activ)

m.Act$Hour2 <- as.numeric(m.Act$Hour)
m.Act$Day2 <- as.numeric(m.Act$Day)
m.Act$Minute <- as.numeric(m.Act$Minute)

## Add date column, without missing rows/times
m.Act$Date <- as.POSIXct(paste(paste(m.Act$Day2, m.Act$Month, m.Act$Year, sep="/"),
                                    paste(m.Act$Hour2, m.Act$Minute, sep=":")), 
                              format = "%d/%m/%Y %H:%M", tz="America/Anchorage")

## Order by individual and date
m.Act <- m.Act[order(m.Act$Indiv, m.Act$Date),]
head(m.Act)
tail(m.Act)
#m.Act <- m.Act[is.na(m.Act$Date),]

write.csv(m.Act,"Melted\\Melted_PiezoActivity_Phase1.csv", row.names=F)


#### Organizing time and date column ####
## Do this again so that factor is converted back to POSIXct
## Add date column, without missing rows/times
m.Act$Date <- as.POSIXct(paste(paste(m.Act$Day2, m.Act$Month, m.Act$Year, sep="/"),
                               paste(m.Act$Hour2, m.Act$Minute, sep=":")), 
                         format = "%d/%m/%Y %H:%M", tz="America/Anchorage")

## Calculate time difference between rows
n <- length(m.Act$Date)
#m.Act <- InsertRow(m.Act, rep(NA,ncol(m.Act)), 1)
#m.Act$Date[1] <- as.POSIXct(paste(paste(m.Act$Day2[2], m.Act$Month[2], m.Act$Year[2], sep="/"), 
                         # "00:00:01"), format = "%d/%m/%Y %H:%M:%S", tz="America/Anchorage")
head(m.Act)
m.Act$Time_diff <- NA
m.Act$Time_diff[2:n] <- m.Act$Date[2:n]-m.Act$Date[1]
m.Act$Time_diff[1] <- 0
head(m.Act)

## To make Time_diff column think 0 = midnight of the start date
Day_start <- as.POSIXct(paste(paste(m.Act$Day2[1], m.Act$Month[1], m.Act$Year[1], sep="/"), 
                                   "00:00:00"), format = "%d/%m/%Y %H:%M:%S", tz="America/Anchorage")
time_diff_from_start <- as.numeric(60*60*(m.Act$Date[1]- Day_start)) ## Convert hours to seconds

m.Act$Time_diff2 <- m.Act$Time_diff+time_diff_from_start
## For checking which rows of Time_diff are > 5
#m.Act$sno <- seq(1:length(m.Act$Date))

#m.Activity <- data_with_missing_times[order(data_with_missing_times$Indiv, data_with_missing_times$Date),]

m.Activity <- m.Act
m.Activity$Treatment <- factor(m.Activity$Treatment, 
                               levels = c("2WeekAcclimation", "4WeekPhotoperiod", "LowSucrose", "HighSucrose"))
anim_cham <- meta_full[,c("Indiv", "Chamber")]
m.Activity <- merge(m.Activity,anim_cham,by="Indiv")
m.Activity <- m.Activity[order(m.Activity$Chamber, m.Activity$Date),]
head(m.Activity)


#m.Activity <- m.Activity[complete.cases(m.Activity),]

## for behavr processing
dt.act <- data.table::data.table(m.Activity, key='Chamber')
names(dt.act)[names(dt.act) == 'Chamber'] <- 'id'

#dt.act <- dt.act[order(dt.act$Chamber,dt.act$Date)]
dt.meta <- data.table::data.table(meta_activ, key="Chamber")

names(dt.meta)[names(dt.meta) == 'Chamber'] <- 'id'

beh.act <-behavr(dt.act,metadata = dt.meta)
beh.act$t <- beh.act$Time_diff2
#beh.act$t <- rep(seq(1,300*length(beh.act$Time[beh.act$id=="T13"]),by=300), times=length(unique(beh.act$id)))
summary(beh.act, detailed=T)
#beh.act <- beh.act[order(beh.act$Chamber,beh.act$Date)]
head(beh.act)
tail(beh.act)


## IGNORE Summarizing mean and max activity levels
#stat_dt <- beh.act[,
 #             .(mean_acti = mean(PiezoAct),
  #              max_acti = max(PiezoAct)
   #           ),
    #          by='id']
#stat_dt


## Sorting individuals by mean activity levels
# the average time spent moving per 1000 (rounded)
mean_piezo_beh <- beh.act[, .(mean_activ = round(mean(PiezoAct, na.rm=T) * 1000)), by=id]
# join curent meta and the summary table
new_meta <- beh.act[mean_piezo_beh, meta=T]
# set new metadata
setmeta(beh.act, new_meta)
head(beh.act[meta=T])
head(beh.act)

#beh.act <- beh.act[beh.act$PiezoAct != "NA",]
## Trying out a ggetho plot
ggetho(beh.act, aes(x=t, y=id, z=PiezoAct)) + stat_tile_etho()

## Plotting indivs by mean activity levels
ggetho(beh.act, aes(x=t, y=interaction(id, mean_activ, sep = " : "), z=PiezoAct)) +
  stat_tile_etho()

## Sorting by treatment and activity levels now
ggetho(beh.act, aes(x=t, y=interaction(id, mean_activ, Photoperiod, sep = " : "), z=PiezoAct)) +
  stat_tile_etho() + my_theme

## Use bars instead of tiles
ggetho(beh.act, aes(x=t, y=interaction(id, mean_activ, Photoperiod, sep = " : "), z=PiezoAct)) +
  stat_bar_tile_etho() + my_theme

## Population graphs
ggetho(beh.act, aes(x=t, y=PiezoAct, color=Photoperiod)) + stat_pop_etho() + facet_grid(Sex~.) + my_theme

##Population level, all indivs; excluding 2 week acclim; faceted by photoperiod
ggetho(beh.act[Treatment != "2WeekAcclimation",], 
       aes(x=t, y=PiezoAct, col=Photoperiod), time_wrap = hours(24)) + stat_pop_etho() + facet_grid(Photoperiod~.) + my_theme


##Population level, all indivs; just the "expt" values; faceted by photoperiod, just 2wk
p.pop_2wk <- ggetho(beh.act[beh.act$Prep_expt=="expt" & beh.act$Treatment=="2WeekAcclimation",], 
       aes(x=t, y=PiezoAct, col=Photoperiod), time_wrap = hours(24)) + stat_pop_etho() + #facet_grid(Photoperiod~.) + 
  my_theme + ggtitle("2 week Acclimation") + ylim(0,3) + 
  theme(axis.text.x = element_blank(), legend.position = "none") + xlab("")

##Population level, all indivs; just the "expt" values; faceted by photoperiod, just 4wk
p.pop_4wk <- ggetho(beh.act[beh.act$Prep_expt=="expt" & beh.act$Treatment=="4WeekPhotoperiod",], 
       aes(x=t, y=PiezoAct, col=Photoperiod), time_wrap = hours(24)) + stat_pop_etho() + #facet_grid(Photoperiod~.) + 
  my_theme + ggtitle("4 week photoperiod") + ylim(0,3) + theme(axis.text.x = element_blank()) + xlab("")

##Population level, all indivs; just the "expt" values; faceted by photoperiod, just high sucrose
p.pop_HCS <- ggetho(beh.act[beh.act$Prep_expt=="expt" & beh.act$Treatment=="HighSucrose",], 
       aes(x=t, y=PiezoAct, col=Photoperiod), time_wrap = hours(24)) + stat_pop_etho() + #facet_grid(Photoperiod~.) + 
  my_theme  + ggtitle("High Sucrose") + ylim(0,3)

grid.arrange(p.pop_2wk, p.pop_4wk,p.pop_HCS, ncol=1, nrow=3)

##Population level, all indivs; just the "expt" values; colored by photoperiod, just high sucrose
# Viridis colors c("#23988aff", "#F38BA8", "#440558ff", "#9ed93aff")
ggetho(beh.act[beh.act$Prep_expt=="expt" & beh.act$Treatment=="HighSucrose",], 
                    aes(x=t, y=PiezoAct, col=Photoperiod, fill=Photoperiod), time_wrap = hours(24)) + 
  stat_pop_etho() + #facet_grid(Photoperiod~.) + 
  my_theme  + ggtitle("High Sucrose") + ylim(0,3) +
  scale_color_manual(values = c("#F38BA8", "#23988aff")) +
  scale_fill_manual(values = c("#F38BA8", "#23988aff"))

##Population level, all short photoperiod indivs; just last week of high sucrose
pop_short_HCS <- ggetho(beh.act[beh.act$Prep_expt=="expt" & beh.act$Treatment=="HighSucrose" & beh.act$id<13,], 
       aes(x=t, y=PiezoAct, col=Photoperiod, fill=Photoperiod), time_wrap = hours(24), 
       time_offset = hours(4)) + 
  stat_pop_etho() + #facet_grid(Photoperiod~.) + 
  my_theme  + ggtitle("High Sucrose") + ylim(0,3) + 
  scale_color_manual(values = "#23988aff") +
  scale_fill_manual(values = "#23988aff")


##Population level, all short photoperiod indivs; just last week of high sucrose
pop_long_HCS <- ggetho(beh.act[beh.act$Prep_expt=="expt" & beh.act$Treatment=="HighSucrose" & beh.act$id>12,], 
       aes(x=t, y=PiezoAct, col=Photoperiod, fill=Photoperiod), time_wrap = hours(24)) + 
  stat_pop_etho() + #facet_grid(Photoperiod~.) + 
  my_theme  + ggtitle("High Sucrose") + ylim(0,3) +
  scale_color_manual(values = "#F38BA8") +
  scale_fill_manual(values = "#F38BA8")

grid.arrange(pop_short_HCS, pop_long_HCS)

## Population graph single individual, short
pop_short_indiv <- ggetho(beh.act[Treatment != "2WeekAcclimation" & beh.act$id=="2"], 
       aes(x=t, y=PiezoAct, col=Photoperiod), time_wrap = hours(24)) + stat_pop_etho()  + my_theme +
  scale_color_manual(values="blue")

## Population graph, single individual, long
pop_long_indiv <- ggetho(beh.act[Treatment != "2WeekAcclimation" & beh.act$id=="24"], 
       aes(x=t, y=PiezoAct, col=Photoperiod), time_wrap = hours(24)) + stat_pop_etho()  + my_theme

grid.arrange(pop_short_indiv, pop_long_indiv)

#Now phase shifted to activity onset
pop_short_indiv_shift <- ggetho(beh.act[Treatment != "2WeekAcclimation" & beh.act$id=="2"], 
                          aes(x=t, y=PiezoAct, col=Photoperiod), time_wrap = hours(24),
                          time_offset = hours(4)) + stat_pop_etho()  + my_theme +
  scale_color_manual(values="blue")

grid.arrange(pop_short_indiv_shift, pop_long_indiv)

## Same behavior over multiple days, population-level, as polar coordinates
## DO THIS WITH JUST LAST WEEK of 4-week and of high sucrose
ggetho(beh.act, aes(x=t, y=PiezoAct, col=Photoperiod), time_wrap = days(1)) + stat_pop_etho(geom='polygon', fill=NA)  + 
 my_theme + coord_polar() +
  stat_ld_annotations(height=.5,
                      alpha=.2,
                      x_limits = c(0, days(1)),
                      outline = NA)

## For just 4 week, expt phase
ggetho(beh.act[beh.act$Prep_expt=="expt" & beh.act$Treatment=="4WeekPhotoperiod",], aes(x=t, y=PiezoAct, col=Photoperiod), time_wrap = days(1)) + stat_pop_etho(geom='polygon', fill=NA)  + 
  my_theme + coord_polar() +
  stat_ld_annotations(height=.5, alpha=.2, x_limits = c(0, days(1)), outline = NA)

## For just high sucrose, expt phase
ggetho(beh.act[beh.act$Prep_expt=="expt" & beh.act$Treatment=="HighSucrose",], aes(x=t, y=PiezoAct, col=Photoperiod), time_wrap = days(1)) + stat_pop_etho(geom='polygon', fill=NA)  + 
  my_theme + coord_polar() +
  stat_ld_annotations(height=.5, alpha=.2, x_limits = c(0, days(1)), outline = NA)


## Double-plotted actograms
ggetho(beh.act, aes(x=t, z=PiezoAct), multiplot = 2) + stat_bar_tile_etho() + my_theme


## Double-plotted actograms by individual
ggetho(beh.act, aes(x=t, z=PiezoAct), multiplot = 2) + stat_bar_tile_etho() + my_theme + 
  facet_wrap(~id) + scale_x_continuous(breaks =seq(0,(48*60*60),(3*60*60)), labels = seq(0,48,3))

## VERY NICE For all individuals
## Double-plotted actograms by individual, tiles, viridis
## Only x-labels every 6 hours; 3 is too crammed
ggetho(beh.act, aes(x=t, z=PiezoAct), multiplot = 2) + stat_tile_etho() + my_theme + 
  facet_wrap(~id) + scale_x_continuous(breaks =seq(0,(48*60*60),(6*60*60)), labels = seq(0,48,6)) +
  scale_fill_viridis()

## VERY NICE For all short photoperiod indivs
## Double-plotted actograms by individual, tiles, viridis
## Only x-labels every 6 hours; 3 is too crammed
ggetho(beh.act[beh.act$id<13,], aes(x=t, z=PiezoAct), multiplot = 2) + stat_tile_etho() + my_theme + 
  facet_wrap(~id) + scale_x_continuous(breaks =seq(0,(48*60*60),(6*60*60)), labels = seq(0,48,6)) +
  scale_fill_viridis()

## VERY NICE For all long photoperiod indivs
## Double-plotted actograms by individual, tiles, viridis
## Only x-labels every 6 hours; 3 is too crammed
ggetho(beh.act[beh.act$id>12,], aes(x=t, z=PiezoAct), multiplot = 2) + stat_tile_etho() + my_theme + 
  facet_wrap(~id) + scale_x_continuous(breaks =seq(0,(48*60*60),(6*60*60)), labels = seq(0,48,6)) +
  scale_fill_viridis()


## For just one individual, T13; long photoperiod
ggetho(beh.act[beh.act$id=="24",],
       aes(x=t, z=PiezoAct), multiplot = 2) + stat_bar_tile_etho(fill="red") + my_theme +
  scale_x_continuous(breaks =seq(0,172800,10800), labels = seq(0,48,3))

## LD in the background, long photoperiod T13
ggetho(beh.act[beh.act$id=="24" & beh.act$Treatment!="2WeekAcclimation",], aes(x=t, z=PiezoAct), multiplot=2) +
  stat_ld_annotations(height=1, ld_colours = c('white', 'grey70'), outline = NA,l_duration = hours(12),phase = hours(6)) +
  stat_bar_tile_etho() + my_theme2 + theme(axis.text.y = element_text(size=15))

## OVERALL viridis, Tiled, long photoperiod T13
ggetho(beh.act[beh.act$id=="24" & beh.act$Treatment!="2WeekAcclimation",], aes(x=t, z=PiezoAct), multiplot=2) +
  stat_ld_annotations(height=1, ld_colours = c('white', 'grey70'),outline = NA,l_duration = hours(12),phase = hours(6)) +
  stat_tile_etho() + my_theme2 + theme(axis.text.y = element_text(size=15)) +
  #scale_fill_gradientn(colours = rev(terrain.colors(10))) +
  scale_fill_viridis() + ylab("Period") + xlab("Time (hours)") +
  scale_x_continuous(breaks =seq(0,172800,10800), labels = seq(0,48,3))

## Just the "expt" section of 2wk, Tiled, long photoperiod T13
ggetho(beh.act[beh.act$id=="24" & beh.act$Prep_expt=="expt" & beh.act$Treatment=="2WeekAcclimation",], aes(x=t, z=PiezoAct), multiplot=2) +
  stat_ld_annotations(height=1, ld_colours = c('white', 'grey70'),outline = NA,l_duration = hours(12),phase = hours(6)) +
  stat_tile_etho() + my_theme2 + theme(axis.text.y = element_text(size=15)) +
  #scale_fill_gradientn(colours = rev(terrain.colors(10))) +
  scale_fill_viridis() + ylab("Period") + xlab("Time (hours)") +
  scale_x_continuous(breaks =seq(0,172800,10800), labels = seq(0,48,3)) 

## Just the "expt" section of 4wk, Tiled, long photoperiod T13
ggetho(beh.act[beh.act$id=="24" & beh.act$Prep_expt=="expt" & beh.act$Treatment=="4WeekPhotoperiod",], aes(x=t, z=PiezoAct), multiplot=2) +
  #stat_ld_annotations(height=1, ld_colours = c('white', 'grey70'),outline = NA,l_duration = hours(12),phase = hours(6)) +
  stat_tile_etho() + my_theme2 + theme(axis.text.y = element_text(size=15)) +
  #scale_fill_gradientn(colours = rev(terrain.colors(10))) +
  scale_fill_viridis() + ylab("Period") + xlab("Time (hours)") +
  scale_x_continuous(breaks =seq(0,172800,10800), labels = seq(0,48,3)) 

## Just the "expt" section of low sucrose, Tiled, long photoperiod T13
ggetho(beh.act[beh.act$id=="24" & beh.act$Prep_expt=="expt" & beh.act$Treatment=="LowSucrose",], aes(x=t, z=PiezoAct), multiplot=2) +
  #stat_ld_annotations(height=1, ld_colours = c('white', 'grey70'),outline = NA,l_duration = hours(12),phase = hours(6)) +
  stat_tile_etho() + my_theme2 + theme(axis.text.y = element_text(size=15)) +
  #scale_fill_gradientn(colours = rev(terrain.colors(10))) +
  scale_fill_viridis() + ylab("Period") + xlab("Time (hours)") +
  scale_x_continuous(breaks =seq(0,172800,10800), labels = seq(0,48,3))

## Just the "expt" section of high sucrose, Tiled, long photoperiod T13
ggetho(beh.act[beh.act$id=="24" & beh.act$Prep_expt=="expt" & beh.act$Treatment=="HighSucrose",], aes(x=t, z=PiezoAct), multiplot=2) +
  #stat_ld_annotations(height=1, ld_colours = c('white', 'grey70'),outline = NA,l_duration = hours(12),phase = hours(6)) +
  stat_tile_etho() + my_theme2 + theme(axis.text.y = element_text(size=15)) +
  #scale_fill_gradientn(colours = rev(terrain.colors(10))) +
  scale_fill_viridis() + ylab("Period") + xlab("Time (hours)") +
  scale_x_continuous(breaks =seq(0,172800,10800), labels = seq(0,48,3))

## Short photoperiod individual G9
ggetho(beh.act[beh.act$id=="1",],aes(x=t, z=PiezoAct), multiplot = 2) + 
  stat_bar_tile_etho() + my_theme +
  scale_x_continuous(breaks =seq(0,172800,10800), labels = seq(0,48,3))

## Short photoperiod individual G9, tiles, viridis
ggetho(beh.act[beh.act$id=="1",],aes(x=t, z=PiezoAct), multiplot = 2) + 
  stat_tile_etho() + my_theme +
  scale_fill_viridis() + ylab("Period") + xlab("Time (hours)") +
  scale_x_continuous(breaks =seq(0,172800,10800), labels = seq(0,48,3))

## OVERALL Tiled, short photoperiod G5
ggetho(beh.act[beh.act$id=="2",], aes(x=t, z=PiezoAct), multiplot=2) +
  stat_ld_annotations(height=1, ld_colours = c('white', 'grey70'),outline = NA,l_duration = hours(12),phase = hours(6)) +
  stat_tile_etho() + my_theme2 + theme(axis.text.y = element_text(size=15)) +
  #scale_fill_gradientn(colours = rev(terrain.colors(10))) +
  scale_fill_viridis() + ylab("Period") + xlab("Time (hours)") +
  scale_x_continuous(breaks =seq(0,172800,10800), labels = seq(0,48,3))

## Just the "expt" section of 2wk, Tiled, short photoperiod G2
ggetho(beh.act[beh.act$id=="2" & beh.act$Prep_expt=="expt" & beh.act$Treatment=="2WeekAcclimation",], aes(x=t, z=PiezoAct), multiplot=2) +
  stat_ld_annotations(height=1, ld_colours = c('white', 'grey70'),outline = NA,l_duration = hours(12),phase = hours(6)) +
  stat_tile_etho() + my_theme2 + theme(axis.text.y = element_text(size=15)) +
  #scale_fill_gradientn(colours = rev(terrain.colors(10))) +
  scale_fill_viridis() + ylab("Period") + xlab("Time (hours)") +
  scale_x_continuous(breaks =seq(0,172800,10800), labels = seq(0,48,3)) 

## Just the "expt" section of 4wk, Tiled, short photoperiod G2
ggetho(beh.act[beh.act$id=="2" & beh.act$Prep_expt=="expt" & beh.act$Treatment=="4WeekPhotoperiod",], aes(x=t, z=PiezoAct), multiplot=2) +
  #stat_ld_annotations(height=1, ld_colours = c('white', 'grey70'),outline = NA,l_duration = hours(12),phase = hours(6)) +
  stat_tile_etho() + my_theme2 + theme(axis.text.y = element_text(size=15)) +
  #scale_fill_gradientn(colours = rev(terrain.colors(10))) +
  scale_fill_viridis() + ylab("Period") + xlab("Time (hours)") +
  scale_x_continuous(breaks =seq(0,172800,10800), labels = seq(0,48,3)) 

## Just the "expt" section of high sucrose, Tiled, short photoperiod G2
ggetho(beh.act[beh.act$id=="2" & beh.act$Treatment=="HighSucrose",], aes(x=t, z=PiezoAct), multiplot=2) +
  #stat_ld_annotations(height=1, ld_colours = c('white', 'grey70'),outline = NA,l_duration = hours(12),phase = hours(6)) +
  stat_tile_etho() + my_theme2 + theme(axis.text.y = element_text(size=15)) +
  #scale_fill_gradientn(colours = rev(terrain.colors(10))) +
  scale_fill_viridis() + ylab("Period") + xlab("Time (hours)") +
  scale_x_continuous(breaks =seq(0,172800,10800), labels = seq(0,48,3))

## Short photoperiod individual G5
ggetho(beh.act[beh.act$id=="2",],
       aes(x=t, z=PiezoAct), multiplot = 2) + stat_bar_tile_etho() + my_theme

## Short photoperiod individual G5, tiles, viridis
ggetho(beh.act[beh.act$id=="2",],aes(x=t, z=PiezoAct), multiplot = 2) + 
  stat_tile_etho() + my_theme +
  scale_fill_viridis() + ylab("Period") + xlab("Time (hours)") +
  scale_x_continuous(breaks =seq(0,172800,10800), labels = seq(0,48,3))

## LD in the background
ggetho(beh.act[beh.act$id=="1" & beh.act$Treatment!="2WeekAcclimation",], aes(x=t, z=PiezoAct), multiplot=2) +
  stat_ld_annotations(height=1, ld_colours = c('white', 'grey70'), outline = NA,l_duration = hours(4),phase = hours(10)) +
  stat_bar_tile_etho() + my_theme

ggetho(dt, aes(x=t, y=moving)) +
  # the default annotation layer
  stat_ld_annotations() +
  # on top of it, a second layer that
  # starts at day 2 thoughout day 5,
  # and where L colour is grey
  stat_ld_annotations(x_limits = days(c(2,5)),
                      ld_colours = c("grey", "black" )) +
  stat_pop_etho()


## To phase-shift graph
ggetho(beh.act, aes(x=t, z=PiezoAct),  
       time_wrap = hours(24),
       time_offset = hours(16.31)) + stat_pop_etho() + my_theme


## Generate periodogram
per_dt <- periodogram(PiezoAct, beh.act[Treatment!='2WeekAcclimation',], FUN = chi_sq_periodogram, resample_rate = 1/mins(5))
per_dt
## PERIODOGRAM
ggperio(per_dt, aes(period, power, colour=Photoperiod, fill=Photoperiod)) + 
  stat_pop_etho() + my_theme2 + theme(legend.key.height = unit(3, 'lines')) + 
  scale_color_manual(values = c("#F38BA8", "#23988aff")) +
  scale_fill_manual(values = c("#F38BA8", "#23988aff"))

## PERIODOGRAM by individual
ggperio(per_dt, aes(period, power, colour=Photoperiod)) + 
  stat_pop_etho() + facet_wrap(~id) + my_theme

## Identify peaks
peaks_dt <-find_peaks(per_dt)

## Plot with peaks identified
ggperio(peaks_dt, aes(period, power, colour=Photoperiod)) + 
  geom_line() +  geom_peak(colour="blue") + my_theme

## By individual
ggperio(peaks_dt, aes(period, power, colour=Photoperiod)) + 
  geom_line() +  geom_peak(colour="blue") +  facet_wrap( ~ id)  + my_theme

## With lines
ggperio(peaks_dt) + 
  geom_line(aes(group = id, colour = Photoperiod)) +
  geom_peak(col = "black") +
  geom_line(aes(y = signif_threshold)) +
  facet_wrap(~ id, ncol = 8)

## Generating spectrogram to analyse power NOT WORKING. No 'spectrogram' function seems to exist in zeitgebr
spect_dt <- spectrogram(PiezoAct,
                        beh.act,
                        period_range = c(hours(6), hours(24)))

ggspectro(spect_dt) + 
  stat_tile_etho() + 
  scale_y_hours(name= "Period", log=T) + # log axis for period 
  facet_wrap(~ condition) +
  stat_ld_annotations()

## Sleep bout analyses
bout_dt <- bout_analysis(PiezoAct, beh.act[Treatment!="2WeekAcclimation"])
ggetho(bout_dt, aes(y=duration / 60, colour=Photoperiod), time_wrap = hours(24)) + 
  stat_pop_etho() + 
  facet_grid(Photoperiod ~ .) +
  scale_y_continuous(name= "Bout length (min)") + my_theme


### Old

#Actogram faceted by expt trial
ggplot(m.Activity[m.Activity$Indiv=="G9",], aes(Hour2, Day2)) + my_theme + scale_y_reverse() +
  facet_grid(Treatment+Month~., scales = "free", space="free_y") + geom_tile(aes(fill=PiezoAct)) +
  scale_fill_gradient(name = 'value of\ninterest', low = 'white', high = 'red')

ggplot(m.Activity[m.Activity$Indiv=="G9",], aes(Hour2, PiezoAct)) + my_theme +
  facet_grid(Treatment~Month, space="free_y") + geom_boxplot(aes(group=Hour2)) + geom_smooth(method="loess")


#Test <- m.Activity[m.Activity$Indiv=="G9" & m.Activity$Treatment=="LowSucrose",]


#### OLD DATE FORMATTING IGNORE ####
## Create a vector with missing times
imputed <- as.POSIXct("2019-03-20 00:01:00", format = "%y/%m/%d %H:%M:%S", tz="America/Anchorage")

for(i in 1:length(m.Activity$Time_diff)){
  if(m.Activity$Time_diff[i]>5){
    imp <- seq(min(m.Activity$Date[i]), max(m.Activity$Date[i+1]), 
               by = "5 min")
    imputed <- c(imputed,imp)
  }
}
head(imputed) ## Check to see if first value is NA
imputed <- imputed[-1]

## Create data frame repeating the missing times by the number of individuals, to then merge.
df.time <- data.frame(Date=rep(imputed, length(unique(m.Activity$Indiv))), 
                      Indiv=rep(unique(m.Activity$Indiv), each=length(imputed)))
head(df.time) # Should be a repeating individual column and distinct date column, separated by 5 min

### IGNORE - old - Trying to make a complete time column, including missing times
#ts <- seq.POSIXt(as.POSIXct(paste(paste(m.Activity$Day2, m.Activity$Month, m.Activity$Year, sep="/"),
#                      paste(m.Activity$Hour2, m.Activity$Minute, sep=":")), format = "%d/%m/%Y %H:%M", by="min"))

## Final time format
#ts <- seq.POSIXt(as.POSIXlt("2019/03/20 00:00:00", tz="America/Anchorage"), 
#                as.POSIXlt("2019/06/08 14:48:00",tz="America/Anchorage"), by="5 min")

## To align starting dates to midnight
ts <- seq.POSIXt(as.POSIXlt("2019/03/20 00:01:00", tz="America/Anchorage"), 
                 as.POSIXlt("2019/03/20 16:26:00",tz="America/Anchorage"), by="5 min")
#ts <- as.POSIXct(format.POSIXct(ts,'%y/%m/%d %H:%M:%S', tz="America/Anchorage"))

df_start_time <- data.frame(Date=rep(ts, length(unique(m.Activity$Indiv))), Indiv=rep(unique(m.Activity$Indiv), each=length(ts)))
head(df_start_time)

df.bound_time <- rbind(df_start_time, df.time)
head(df.bound_time)
m.Activity <- full_join(df.bound_time,m.Act, by=c("Date","Indiv"))
head(m.Activity)
