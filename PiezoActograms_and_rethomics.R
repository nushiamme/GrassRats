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
library(signal) ## for spectrogram function

## Set working directory
setwd("E:\\Ex_Google_Drive\\Piezo_data\\For_rethomics")

source("Spectrogram.R")

## Metadata file for rethomics behavr table
meta_full <- read.csv("E:\\Ex_Google_Drive\\Piezo_data\\Meta_Expt.csv")

meta_activ1 <- meta_full[meta_full$Phase==1,c("Indiv", "Sex", "Photoperiod", "Sugar", "Chamber", "Phase")]
meta_activ2 <- meta_full[meta_full$Phase==2,c("Indiv", "Sex", "Photoperiod", "Sugar", "Chamber", "Phase")]


m.Act <- read.csv(".\\Melted\\Melted_PiezoActivity_bothPhases_new.csv") 

## Try to plot mean SB lengths per hour, like the rethomics population plot


## General functions
my_theme <- theme_classic(base_size = 15) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))

my_theme2 <- theme_classic(base_size = 30) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))

#my_colors <- c("#23988aff", "#F38BA8", "#440558ff", "#9ed93aff")

#### Ignore if reading in Melted activity csv; only run to re-process raw data ####
## Get all the required csv's together
## MAKE SURE to change first cell [1,1] in all csv's to "Date" in Excel before reading in here
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
  Activ[[i]]$Phase <- 
    as.numeric(unlist(lapply(strsplit(as.character(Activ$File[i]), "_"), "[", 2)))
  Activ[[i]]$FileDate <- 
    unlist(lapply(strsplit(as.character(Activ$File[i]), "_"), "[", 3))
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
  if(Activ[[i]]$Phase[1]==1) {
  m.Activ[[i]] <- melt(Activ[[i]], id.vars= c("Day", "Month", "Year", "Time", "Hour", "Minute", "Treatment", "Phase"), 
                  measure.vars=c("G9", "G5", "G14", "G16", "G18", "G10", "G8", "G2", "T10", "T5", "T17", "T12", "G4", "G1","G13",
                                 "G17", "G19", "G11", "G12",  "G3", "T9", "T11", "T19", "T13"))
  names(m.Activ[[i]]) <- c("Day", "Month", "Year", "Time", "Hour", "Minute", "Treatment", "Phase", "Indiv", "PiezoAct")
  }
  if(Activ[[i]]$Phase[1]==2) {
    m.Activ[[i]] <- melt(Activ[[i]], id.vars= c("Day", "Month", "Year", "Time", "Hour", "Minute", "Treatment",  "Phase"), 
                         measure.vars=c("G35", "G23", "G24", "G28", "G29", "G22", "T1", "G36", "G27", "G40", "G33", "G20", "G21", "G26",
                                        "T32", "G31", "G39", "G34", "G37",  "G25", "G38", "G30"))
    names(m.Activ[[i]]) <- c("Day", "Month", "Year", "Time", "Hour", "Minute", "Treatment", "Phase", "Indiv", "PiezoAct")
  }
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

#### Write to csv ####
write.csv(m.Act,"Melted\\Melted_PiezoActivity_bothPhases_new.csv", row.names=F)


#### If loops to add Prep_expt column
#### Takes too long. Avoid for now. Try to optimize? For now do in Excel ####
m.Act$Prep_expt <- 0
for(i in 1:length(m.Act$PiezoAct)) {
  if(m.Act$Phase[i]==1) {
    if(m.Act$Date[i] %within% 
       interval(ymd("2019-03-27"), ymd("2019-04-03"))) { # last week of 2wk acclim
      m.Act$Prep_expt[i] <- "expt"
    } else if (m.Act$Date[i] %within% 
               interval(ymd("2019-04-24"), ymd("2019-04-30"))) { # last wk of 4wk
      m.Act$Prep_expt[i] <- "expt"
    } else if (m.Act$Date[i] %within% 
               interval(ymd("2019-05-07"), ymd("2019-05-08"))) { #low sucrose test
      m.Act$Prep_expt[i] <- "expt"
    } else if (m.Act$Date[i] %within% 
               interval(ymd("2019-05-27"), ymd("2019-06-03"))) { #last week of expt HCS
      m.Act$Prep_expt[i] <- "expt"
    } else {
      m.Act$Prep_expt[i] <- "prep"
    }
  }
  if(m.Act$Phase[i]==2) {
    if(m.Act$Date[i] %within% 
           interval(ymd("2019-06-23"), ymd("2019-06-30"))) {
      m.Act$Prep_expt[i] <- "expt"
    } else if(m.Act$Date[i] %within% 
       interval(ymd("2019-07-21"), ymd("2019-07-28"))) {
      m.Act$Prep_expt[i] <- "expt"
    } else if (m.Act$Date[i] %within% 
               interval(ymd("2019-08-01"), ymd("2019-08-05"))) {
      m.Act$Prep_expt[i] <- "expt"
    } else if (m.Act$Date[i] %within% 
               interval(ymd("2019-08-21"), ymd("2019-08-29"))) {
      m.Act$Prep_expt[i] <- "expt"
    } else {
      m.Act$Prep_expt[i] <- "prep"
    }
  }
}



#### Organizing time and date column ####
## Do this again so that factor is converted back to POSIXct if reading in from csv
## If reading in from newly written csv, 
##... make sure to add Prep_expt column in Excel
## Add date column, without missing rows/times
m.Act$Date <- as.POSIXct(paste(paste(m.Act$Day2, m.Act$Month, m.Act$Year, sep="/"),
                               paste(m.Act$Hour2, m.Act$Minute, sep=":")), 
                         format = "%d/%m/%Y %H:%M", tz="America/Anchorage")

## Make two separate data frames for Phase 1 and Phase 2
m.Act1 <- m.Act[m.Act$Phase==1,]
#m.Act1 <- m.Act1[order(m.Act1$Chamber, m.Act1$Date),]
m.Act2 <- m.Act[m.Act$Phase==2,]
#m.Act2 <- m.Act2[order(m.Act2$Chamber, m.Act2$Date),]

## Process phase 1 and phase 2 files
processAct <- function(genAct){
## Calculate time difference between rows
n <- length(genAct$Date)
#genAct <- InsertRow(genAct, rep(NA,ncol(genAct)), 1)
#genAct$Date[1] <- as.POSIXct(paste(paste(genAct$Day2[2], genAct$Month[2], genAct$Year[2], sep="/"), 
                         # "00:00:01"), format = "%d/%m/%Y %H:%M:%S", tz="America/Anchorage")
head(genAct)
genAct$Time_diff <- NA
genAct$Time_diff[2:n] <- genAct$Date[2:n]-genAct$Date[1]
genAct$Time_diff[1] <- 0
head(genAct)

## To make Time_diff column think 0 = midnight of the start date
Day_start <- as.POSIXct(paste(paste(genAct$Day2[1], genAct$Month[1], genAct$Year[1], sep="/"), 
                                   "00:00:00"), format = "%d/%m/%Y %H:%M:%S", tz="America/Anchorage")
time_diff_from_start <- as.numeric(60*60*(genAct$Date[1]- Day_start)) ## Convert hours to seconds

genAct$Time_diff2 <- genAct$Time_diff+time_diff_from_start

genAct$Time_diff_Treatment <- genAct$Time_diff2 ## will shift this later


genAct$Treatment <- factor(genAct$Treatment, 
                               levels = c("2WeekAcclimation", "4WeekPhotoperiod", "LowSucrose", "HighSucrose"))
anim_cham <- meta_full[meta_full$Phase==1, c("Indiv", "Chamber")] ## Comment out ****
#anim_cham <- meta_full[meta_full$Phase==2,c("Indiv", "Chamber")] ## Comment in ****
m.Activity <- merge(genAct,anim_cham,by="Indiv")
m.Activity <- m.Activity[order(m.Activity$Chamber, m.Activity$Date),]
return(m.Activity)
}

## Before running line switch out the starred lines above, and rerun "processAct"
m.Activity1 <- processAct(m.Act1)
head(m.Activity1)
## Before running line switch out the starred lines above, and rerun "processAct"
m.Activity2 <- processAct(m.Act2) 
head(m.Activity2)
tail(m.Activity2)

## Looking at non-shifted graphs, creating new 'shifted' 'time_diff' column
## shifting 4 week short photoperiod animals 3 hours earlier; shifting HCS animals 3.5 hours earlier.
m.Activity1$Time_diff_Treatment[m.Activity1$Treatment=="4WeekPhotoperiod" & m.Activity1$Chamber<12] <- 
  m.Activity1$Time_diff_Treatment[m.Activity1$Treatment=="4WeekPhotoperiod" & m.Activity1$Chamber<12]+(3*3600)

m.Activity1$Time_diff_Treatment[m.Activity1$Treatment=="HighSucrose" & m.Activity1$Chamber<12] <- 
  m.Activity1$Time_diff_Treatment[m.Activity1$Treatment=="HighSucrose" & m.Activity1$Chamber<12]+(3.5*3600)

### CHANGE THIS AFTER LOOKING AT PHASE SHIFT
m.Activity2$Time_diff_Treatment[m.Activity2$Treatment=="4WeekPhotoperiod" & m.Activity2$Chamber<12] <- 
  m.Activity2$Time_diff_Treatment[m.Activity2$Treatment=="4WeekPhotoperiod" & m.Activity2$Chamber<12]+(3*3600)

m.Activity2$Time_diff_Treatment[m.Activity2$Treatment=="HighSucrose" & m.Activity2$Chamber<12] <- 
  m.Activity2$Time_diff_Treatment[m.Activity2$Treatment=="HighSucrose" & m.Activity2$Chamber<12]+(3.5*3600)


#m.Activity <- m.Activity[complete.cases(m.Activity),]

## for behavr processing
processBehvr <- function(genAct){
dt.act <- data.table::data.table(genAct, key='Chamber')
names(dt.act)[names(dt.act) == 'Chamber'] <- 'id'

#dt.act <- dt.act[order(dt.act$Chamber,dt.act$Date)]
#dt.meta <- data.table::data.table(meta_activ1, key="Chamber") ## Comment out ****
dt.meta <- data.table::data.table(meta_activ2, key="Chamber") ## Comment in ****

names(dt.meta)[names(dt.meta) == 'Chamber'] <- 'id'

beh.act <-behavr(dt.act,metadata = dt.meta)
beh.act$t <- beh.act$Time_diff_Treatment
#beh.act$t <- rep(seq(1,300*length(beh.act$Time[beh.act$id=="T13"]),by=300), times=length(unique(beh.act$id)))
#summary(beh.act, detailed=T)
#beh.act <- beh.act[order(beh.act$Chamber,beh.act$Date)]
return(beh.act)
}

## Before running the next two lines switch starred comment out and in lines above & rerun processBehvr
beh.act1 <- processBehvr(m.Activity1)
head(beh.act1)
## Before running the next two lines switch starred comment out and in lines above & rerun processBehvr
beh.act2 <- processBehvr(m.Activity2) 
tail(beh.act2)

## Shifted 't' column earlier by 4 hours,
#just for !2wkacclim short photoperiod individuals, like subjective day
beh.act_shift <-behavr(dt.act,metadata = dt.meta)
beh.act_shift$t <- beh.act$Time_diff_Treatment
#beh.act$t <- rep(seq(1,300*length(beh.act$Time[beh.act$id=="T13"]),by=300), times=length(unique(beh.act$id)))
summary(beh.act_shift, detailed=T)
#beh.act <- beh.act[order(beh.act$Chamber,beh.act$Date)]
head(beh.act_shift)
tail(beh.act_shift)

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
ggetho(beh.act1, aes(x=t, y=id, z=PiezoAct)) + stat_tile_etho()

## Plotting indivs by mean activity levels
ggetho(beh.act, aes(x=t, y=interaction(id, mean_activ, sep = " : "), z=PiezoAct)) +
  stat_tile_etho()

## Sorting by treatment and activity levels now
ggetho(beh.act, aes(x=t, y=interaction(id, mean_activ, Photoperiod, sep = " : "), z=PiezoAct)) +
  stat_tile_etho() + my_theme

## Use bars instead of tiles
ggetho(beh.act2, aes(x=t, y=interaction(id, Photoperiod, sep = " : "), z=PiezoAct)) +
  stat_bar_tile_etho() + my_theme

## Population graphs
ggetho(beh.act, aes(x=t, y=PiezoAct, color=Photoperiod)) + stat_pop_etho() + facet_grid(Sex~.) + my_theme

##Population level, all indivs; excluding 2 week acclim; faceted by photoperiod
ggetho(beh.act2[Treatment != "2WeekAcclimation",], 
       aes(x=t, y=PiezoAct, col=Photoperiod), time_wrap = hours(24)) + stat_pop_etho() + facet_grid(Photoperiod~.) + my_theme


##Population level, all indivs; just the "expt" values; faceted by photoperiod, just 2wk
p.pop_2wk <- ggetho(beh.act2[beh.act2$Prep_expt=="expt" & beh.act2$Treatment=="2WeekAcclimation",], 
       aes(x=t, y=PiezoAct, col=Photoperiod), time_wrap = hours(24)) + stat_pop_etho() + #facet_grid(Photoperiod~.) + 
  my_theme2 + ggtitle("2 week Acclimation") + ylim(0,3) +
  scale_color_manual(values = c("#F38BA8", "#23988aff")) +
  scale_fill_manual(values = c("#F38BA8", "#23988aff")) +
  theme(legend.key.height = unit(3, 'lines')) + xlab("Time (hours)") +
  scale_x_continuous(breaks =seq(0,(48*60*60),(3*60*60)), labels = seq(0,48,3))


##Population level, all indivs; just the "expt" values; faceted by photoperiod, just 4wk
p.pop_4wk <- ggetho(beh.act2[beh.act2$Prep_expt=="expt" & beh.act2$Treatment=="4WeekPhotoperiod",], 
       aes(x=t, y=PiezoAct, col=Photoperiod), time_wrap = hours(24)) + stat_pop_etho() + #facet_grid(Photoperiod~.) + 
  my_theme2 + ggtitle("4 week photoperiod") + ylim(0,3) +
  scale_color_manual(values = c("#F38BA8", "#23988aff")) +
  scale_fill_manual(values = c("#F38BA8", "#23988aff")) +
  theme(legend.key.height = unit(3, 'lines')) + xlab("Time (hours)") +
  scale_x_continuous(breaks =seq(0,(48*60*60),(3*60*60)), labels = seq(0,48,3))

##Population level, all indivs; just the "expt" values; faceted by photoperiod, just low sucrose
ggetho(beh.act[beh.act$Prep_expt=="expt" & beh.act$Treatment=="LowSucrose",], 
       aes(x=t, y=PiezoAct, col=Photoperiod), time_wrap = hours(24)) + stat_pop_etho() + #facet_grid(Photoperiod~.) + 
  my_theme2 + ggtitle("Low Sucrose") + ylim(0,3) +
  scale_color_manual(values = c("#F38BA8", "#23988aff")) +
  scale_fill_manual(values = c("#F38BA8", "#23988aff")) +
  theme(legend.key.height = unit(3, 'lines')) + xlab("Time (hours)") +
  scale_x_continuous(breaks =seq(0,(48*60*60),(1*60*60)), labels = seq(0,48,1))

##Population level, all indivs; just the "expt" values; faceted by photoperiod, just high sucrose
sugar.labs <- c("No sugar", "8% sugar")
names(sugar.labs) <- c("N", "Y")
ggetho(beh.act2[beh.act2$Prep_expt=="expt" & beh.act2$Treatment=="HighSucrose",], 
                    aes(x=t, y=PiezoAct, col=Sugar), time_wrap = hours(24)) + stat_pop_etho() +
  #stat_summary(fun.data = "mean_cl_boot", colour = "grey30", alpha=0.3, size = 1) +
  facet_grid(Photoperiod~., labeller = labeller(Sugar=sugar.labs)) + 
  my_theme2 + ggtitle("High Sucrose") + ylim(0,3) +
  scale_color_manual(values = c("#F38BA8", "#23988aff")) +
  scale_fill_manual(values = c("#F38BA8", "#23988aff")) +
  theme(legend.key.height = unit(3, 'lines'), 
        panel.grid.major.x = element_line(colour = "grey90", size=1)) + 
  xlab("Time (hours)") +
  scale_x_continuous(breaks =seq(0,(48*60*60),(1*60*60)), labels = seq(0,48,1))

grid.arrange(p.pop_2wk, p.pop_4wk,p.pop_HCS, ncol=1, nrow=3)

## With Shifted beh.act data frame
##Population level, all indivs; just the "expt" values; colored by photoperiod, just high sucrose
# Viridis colors c("#23988aff", "#F38BA8", "#440558ff", "#9ed93aff")
## Used to use beh.act_shift dataframe
ggetho(beh.act1[beh.act1$Prep_expt=="expt" & beh.act1$Treatment=="HighSucrose",], 
                    aes(x=t, y=PiezoAct, col=Photoperiod, fill=Photoperiod), time_wrap = hours(24)) + 
  stat_pop_etho() + facet_grid(Photoperiod~.) + 
  my_theme2 + ggtitle("High Sucrose") + ylim(0,3) +
  scale_color_manual(values = c("#F38BA8", "#23988aff")) +
  scale_fill_manual(values = c("#F38BA8", "#23988aff")) +
  theme(legend.key.height = unit(3, 'lines')) + xlab("Time (hours)") +
  scale_x_continuous(breaks =seq(0,(48*60*60),(3*60*60)), labels = seq(0,48,3))

##Population level, all short photoperiod indivs; just last week of high sucrose
pop_short_HCS <- ggetho(beh.act_shift[beh.act_shift$Prep_expt=="expt" & beh.act_shift$Treatment=="HighSucrose" & beh.act$id<13,], 
       aes(x=t, y=PiezoAct, col=Photoperiod, fill=Photoperiod), time_wrap = hours(24), 
       time_offset = hours(4)) + 
  stat_pop_etho() + #facet_grid(Photoperiod~.) + 
  my_theme2 + ggtitle("High Sucrose") + ylim(0,3) + 
  scale_color_manual(values = "#23988aff") +
  scale_fill_manual(values = "#23988aff") +
  scale_x_continuous(breaks =seq(0,(48*60*60),(3*60*60)), labels = seq(0,48,3))


##Population level, all long photoperiod indivs; just last week of high sucrose
pop_long_HCS <- ggetho(beh.act[beh.act$Prep_expt=="expt" & beh.act$Treatment=="HighSucrose" & beh.act$id>12,], 
       aes(x=t, y=PiezoAct, col=Photoperiod, fill=Photoperiod), time_wrap = hours(24)) + 
  stat_pop_etho() + #facet_grid(Photoperiod~.) + 
  my_theme  + ggtitle("High Sucrose") + ylim(0,3) +
  scale_color_manual(values = "#F38BA8") +
  scale_fill_manual(values = "#F38BA8")

## With Shifted beh.act data frame
##Population level, all indivs; just the "expt" values; colored by photoperiod, just last week of 4week
# Viridis colors c("#23988aff", "#F38BA8", "#440558ff", "#9ed93aff")
ggetho(beh.act_shift[beh.act_shift$Prep_expt=="expt" & beh.act_shift$Treatment=="4WeekPhotoperiod",], 
       aes(x=t, y=PiezoAct, col=Photoperiod, fill=Photoperiod), time_wrap = hours(24)) + 
  stat_pop_etho() + #facet_grid(Photoperiod~.) + 
  my_theme2  + ggtitle("4 Week Photoperiod") + ylim(0,3) +
  scale_color_manual(values = c("#F38BA8", "#23988aff")) +
  scale_fill_manual(values = c("#F38BA8", "#23988aff")) +
  theme(legend.key.height = unit(3, 'lines')) +
  scale_x_continuous(breaks =seq(0,(48*60*60),(3*60*60)), labels = seq(0,48,3)) + xlab("Time (hours)")

##Population level, all short photoperiod indivs; just last week of 4 week
ggetho(beh.act_shift[beh.act_shift$Prep_expt=="expt" & 
                                        beh.act_shift$Treatment=="4WeekPhotoperiod" & beh.act_shift$id<13,], 
                        aes(x=t, y=PiezoAct, col=Photoperiod, fill=Photoperiod), time_wrap = hours(24), 
                        time_offset = hours(4)) + 
  stat_pop_etho() + #facet_grid(Photoperiod~.) + 
  my_theme  + ggtitle("4 Week Photoperiod") + ylim(0,3) + 
  scale_color_manual(values = "#23988aff") +
  scale_fill_manual(values = "#23988aff") +
  scale_x_continuous(breaks =seq(0,(48*60*60),(3*60*60)), labels = seq(0,48,3))

## Synced
grid.arrange(pop_short_HCS, pop_long_HCS)

## Non-shifted, trying out phase 2 animals
##Population level, all short photoperiod indivs; just last week of 4 week
ggetho(beh.act1[beh.act1$Prep_expt=="expt" & 
                       beh.act1$Treatment=="4WeekPhotoperiod" & beh.act1$id<13,], 
       aes(x=t, y=PiezoAct, col=Photoperiod, fill=Photoperiod)) +#, time_wrap = hours(24)) +#, 
       #time_offset = hours(4)) + 
  stat_pop_etho() + #facet_grid(Photoperiod~.) + 
  my_theme  + ggtitle("4 Week Photoperiod") + #ylim(0,3) + 
  scale_color_manual(values = c("#F38BA8", "#23988aff")) +
  scale_fill_manual(values = c("#F38BA8", "#23988aff")) #+
  scale_x_continuous(breaks =seq(0,(48*60*60),(3*60*60)), labels = seq(0,48,3))

peri_dt <- periodogram(PiezoAct, beh.act1, period_range = C(5*60*60,34*60*60),
                       FUN = ls_periodogram, resample_rate = 1/mins(5))

ggperio(peri_dt, aes(period, power, colour=Photoperiod)) + 
  stat_pop_etho()
  
spect_dt <- spectrogram_fun(beh.act1$PiezoAct, beh.act1,
                        period_range = c(hours(6), hours(24)))
  
ggspectro(spect_dt) + 
    stat_tile_etho() + 
    scale_y_hours(name= "Period", log=T) + # log axis for period 
    facet_wrap(~ condition) +
    stat_ld_annotations()
  

## Population graph single individual, short
pop_short_indiv <- ggetho(beh.act[Treatment != "2WeekAcclimation" & beh.act$id=="2"], 
       aes(x=t, y=PiezoAct, col=Photoperiod), time_wrap = hours(24)) + stat_pop_etho()  + my_theme +
  scale_color_manual(values="blue")

## Population graph single individual, short
pop_short_indiv <- ggetho(beh.act2[Treatment != "2WeekAcclimation" & beh.act2$id=="2"], 
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
ggetho(beh.act1, aes(x=t, z=PiezoAct), multiplot = 2) + stat_bar_tile_etho() + my_theme


## Double-plotted actograms by individual
ggetho(beh.act, aes(x=t, z=PiezoAct), multiplot = 2) + stat_bar_tile_etho() + my_theme + 
  facet_wrap(~id) + scale_x_continuous(breaks =seq(0,(48*60*60),(3*60*60)), labels = seq(0,48,3))

## VERY NICE For all individuals
## Double-plotted actograms by individual, tiles, viridis
## Only x-labels every 6 hours; 3 is too crammed
ggetho(beh.act1, aes(x=t, z=PiezoAct), multiplot = 2) + stat_tile_etho() + my_theme + 
  facet_wrap(~id) + #scale_x_continuous(breaks =seq(0,(48*60*60),(6*60*60)), labels = seq(0,48,6)) +
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
ggetho(beh.act1[beh.act1$id=="24",],
       aes(x=t, z=PiezoAct), multiplot = 2) + stat_bar_tile_etho(fill="red") + my_theme +
  scale_x_continuous(breaks =seq(0,172800,10800), labels = seq(0,48,3))

## For just one individual, phase 2; long photoperiod
ggetho(beh.act2[beh.act2$id=="20",],
       aes(x=t, z=PiezoAct), multiplot = 2) + stat_tile_etho() + my_theme +
  scale_x_continuous(breaks =seq(0,172800,10800), labels = seq(0,48,3)) +
  scale_fill_viridis() +
  theme(axis.text.y = element_text(angle=0, size=8))

## LD in the background, long photoperiod T13
ggetho(beh.act[beh.act$id=="24" & beh.act$Treatment!="2WeekAcclimation",], aes(x=t, z=PiezoAct), multiplot=2) +
  stat_ld_annotations(height=1, ld_colours = c('white', 'grey70'), outline = NA,l_duration = hours(12),phase = hours(6)) +
  stat_bar_tile_etho() + my_theme2 + theme(axis.text.y = element_text(size=15))

## For Cory
## OVERALL including 2 week acclim, viridis, Tiled, long photoperiod T13
ggetho(beh.act1[beh.act1$id=="24" & beh.act1$Treatment!="2WeekAcclimation",], aes(x=t, z=PiezoAct), multiplot=2) +
  stat_ld_annotations(height=1, ld_colours = c('white', 'grey70'),outline = NA,l_duration = hours(12),phase = hours(6)) +
  stat_tile_etho() + my_theme2 + theme(axis.text.y = element_blank()) +
  #scale_fill_gradientn(colours = rev(terrain.colors(10))) +
  scale_fill_viridis() + ylab("Time (48h intervals)") + xlab("Time (hours)") +
  scale_x_continuous(breaks =seq(0,172800,10800), labels = seq(0,48,3))

## OVERALL including 2 week acclim, viridis, Tiled, long photoperiod T13
ggetho(beh.act1[beh.act1$id=="24" & beh.act1$Treatment!="2WeekAcclimation",], aes(x=t, z=PiezoAct), multiplot=2) +
  stat_ld_annotations(height=1, ld_colours = c('white', 'grey70'),outline = NA,l_duration = hours(12),phase = hours(6)) +
  stat_tile_etho() + my_theme2 + theme(axis.text.y = element_blank()) +
  #scale_fill_gradientn(colours = rev(terrain.colors(10))) +
  scale_fill_viridis() + ylab("Time (48h intervals)") + xlab("Time (hours)") +
  scale_x_continuous(breaks =seq(0,172800,10800), labels = seq(0,48,3))

## OVERALL including 2 week acclim, viridis, Tiled, short photoperiod 
ggetho(beh.act1[beh.act1$id=="11" & beh.act1$Treatment!="2WeekAcclimation",], aes(x=t, z=PiezoAct), multiplot=2) +
  stat_ld_annotations(height=1, ld_colours = c('white', 'grey70'),outline = NA,l_duration = hours(12),phase = hours(6)) +
  stat_tile_etho() + my_theme2 + theme(axis.text.y = element_blank()) +
  #scale_fill_gradientn(colours = rev(terrain.colors(10))) +
  scale_fill_viridis() + ylab("Time (48h intervals)") + xlab("Time (hours)") +
  scale_x_continuous(breaks =seq(0,172800,10800), labels = seq(0,48,3))

## For Cory
## OVERALL excluding 2 week acclim, viridis, Tiled, long photoperiod T13
ggetho(beh.act[beh.act$id=="24" & beh.act$Treatment!="2WeekAcclimation",], aes(x=t, z=PiezoAct), multiplot=2) +
  stat_ld_annotations(height=1, ld_colours = c('white', 'grey70'),outline = NA,l_duration = hours(12),phase = hours(6)) +
  stat_tile_etho() + my_theme2 + 
  theme(axis.text.y = element_text(size=15)) +
  #theme(axis.text.y = element_blank()) +
  #scale_fill_gradientn(colours = rev(terrain.colors(10))) +
  scale_fill_viridis() + ylab("") + xlab("Time (hours)") +
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

## For Cory
## OVERALL Incl 2 week  Tiled, short photoperiod G5
ggetho(beh.act[beh.act$id=="2",], aes(x=t, z=PiezoAct), multiplot=2) +
  stat_ld_annotations(height=1, ld_colours = c('white', 'grey70'),outline = NA,l_duration = hours(12),phase = hours(6)) +
  stat_tile_etho() + my_theme2 + theme(axis.text.y = element_blank()) +
  #scale_fill_gradientn(colours = rev(terrain.colors(10))) +
  scale_fill_viridis() + ylab("") + xlab("Time (hours)") +
  scale_x_continuous(breaks =seq(0,172800,10800), labels = seq(0,48,3))

## For Cory
## OVERALL excl 2 week  Tiled, short photoperiod G5
ggetho(beh.act[beh.act$id=="2" & beh.act$Treatment!="2WeekAcclimation",], aes(x=t, z=PiezoAct), multiplot=2) +
  stat_ld_annotations(height=1, ld_colours = c('white', 'grey70'),outline = NA,l_duration = hours(12),phase = hours(6)) +
  stat_tile_etho() + my_theme2 + theme(axis.text.y = element_blank()) +
  #scale_fill_gradientn(colours = rev(terrain.colors(10))) +
  scale_fill_viridis() + ylab("") + xlab("Time (hours)") +
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
