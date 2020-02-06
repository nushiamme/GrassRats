## Rethomics for dopamine pump animals
## Script started Feb 6, 2020
## Contact: Anusha Shankar, nushiamme<at>gmail<dot>com

require(reshape2)
require(ggplot2)
library(behavr)
library(ggetho)
library(zeitgebr) ## For periodogram and spectrogram
library(sleepr) ## For sleep analyses
library(dplyr) ## for missing times 
library(DataCombine) # To insert rows at specific locations in dataframe
library(viridis) # for beautiful colors!
library(signal) ## for spectrogram function

## Set  wd
setwd("E:\\Ex_Google_Drive\\Piezo_data\\Dopamine_for_rethomics")

## Metadata file for rethomics behavr table
meta_full <- read.csv("E:\\Ex_Google_Drive\\Piezo_data\\Meta_Expt.csv")

meta_dop <- meta_full[meta_full$Phase==3,c("Indiv", "Sex", "Photoperiod", "Sugar", "ChamberF",
                                           "ChamberMF", "ChamberM", "Phase")]
m.Act <- read.csv(".\\Melted\\Melted_Dopamine_Activity.csv")

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
  Activ[[i]]$Treatment <- 
    unlist(lapply(strsplit(as.character(Activ$File[i]), "_"), "[", 3))
  ## Splitting the file name to get IndivID, date, and time
  Activ[[i]]$PhaseMF <- 
    unlist(lapply(strsplit(as.character(Activ$File[i]), "_"), "[", 2))
  Activ[[i]]$FileDate <- 
    unlist(lapply(strsplit(as.character(Activ$File[i]), "_"), "[", 4))
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
  if(Activ[[i]]$PhaseMF[1]=="F") {
    m.Activ[[i]] <- melt(Activ[[i]], id.vars= c("Day", "Month", "Year", "Time", "Hour", "Minute","Treatment", "PhaseMF"), 
                         measure.vars=c("G6", "T35", "T37", "T38", "R31", "G7"))
    names(m.Activ[[i]]) <- c("Day", "Month", "Year", "Time", "Hour", "Minute", "Treatment", "PhaseMF", "Indiv", "PiezoAct")
  }
  if(Activ[[i]]$PhaseMF[1]=="MF") {
    m.Activ[[i]] <- melt(Activ[[i]], id.vars= c("Day", "Month", "Year", "Time", "Hour", "Minute", "Treatment", "PhaseMF"), 
                         measure.vars=c("G6", "T35", "T37", "T38", "R31", "G7", "R32", "GR43", "T39", "R33", "G32", "GR44"))
    names(m.Activ[[i]]) <- c("Day", "Month", "Year", "Time", "Hour", "Minute", "Treatment", "PhaseMF", "Indiv", "PiezoAct")
  }
  if(Activ[[i]]$PhaseMF[1]=="M") {
    m.Activ[[i]] <- melt(Activ[[i]], id.vars= c("Day", "Month", "Year", "Time", "Hour", "Minute", "Treatment", "PhaseMF"), 
                         measure.vars=c("R32", "GR43", "T39", "R33", "G32", "GR44"))
    names(m.Activ[[i]]) <- c("Day", "Month", "Year", "Time", "Hour", "Minute", "Treatment", "PhaseMF", "Indiv", "PiezoAct")
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
write.csv(m.Act,"Melted\\Melted_Dopamine_Activity.csv", row.names=F)

#### Start here if reading in csv ####
## Do this again so that factor is converted back to POSIXct if reading in from csv
## If reading in from newly written csv, 
##... make sure to add Prep_expt column in Excel
## Add date column, without missing rows/times
m.Act$Date <- as.POSIXct(paste(paste(m.Act$Day2, m.Act$Month, m.Act$Year, sep="/"),
                               paste(m.Act$Hour2, m.Act$Minute, sep=":")), 
                         format = "%d/%m/%Y %H:%M", tz="America/Anchorage")


## Make two separate data frames by sex
## Add sex and chamber number columns to data from metadata file
anim_cham <- meta_dop[,c("Indiv", "ChamberMF", "Sex")]
m.Act <- merge(m.Act,anim_cham,by="Indiv")
m.ActM <- m.Act[m.Act$Sex=="M",]
#m.Act1 <- m.Act1[order(m.Act1$Chamber, m.Act1$Date),]
m.ActF <- m.Act[m.Act$Sex=="F",]


## Process files to line up with metadata and add time difference column for 't'
processAct <- function(genAct){
  ## Calculate time difference between rows
  n <- length(genAct$Date)
  head(genAct)
  genAct$Time_diff <- NA
  genAct$Time_diff[2:n] <- genAct$Date[2:n]-genAct$Date[1]
  genAct$Time_diff[1] <- 0
  head(genAct)
  
  ## To make Time_diff column think 0 = midnight of the start date
  Day_start <- as.POSIXct(paste(paste(genAct$Day2[1], genAct$Month[1], genAct$Year[1], sep="/"), 
                                "00:00:00"), format = "%d/%m/%Y %H:%M:%S", tz="America/Anchorage")
  time_diff_from_start <- as.numeric(60*60*(genAct$Date[1]-Day_start)) ## Convert hours to seconds
  
  genAct$Time_diff2 <- genAct$Time_diff+time_diff_from_start
  
  genAct$Time_diff_Treatment <- genAct$Time_diff2 ## will shift this later
  m.Activity <- genAct
  
  ## Order by indiv and date, and return
  m.Activity <- m.Activity[order(m.Activity$Chamber, m.Activity$Date),]
  return(m.Activity)
}

m.ActivityM <- processAct(m.ActM) 
m.ActivityF <- processAct(m.ActF) 

m.Activity <- rbind(m.ActivityF, m.ActivityM)

## for behavr processing
processBehvr <- function(genAct){
  dt.act <- data.table::data.table(genAct, key='ChamberMF')
  names(dt.act)[names(dt.act) == 'ChamberMF'] <- 'id'
  
  dt.meta <- data.table::data.table(meta_dop, key="ChamberMF")
  
  names(dt.meta)[names(dt.meta) == 'ChamberMF'] <- 'id'
  
  beh.act <-behavr(dt.act,metadata = dt.meta)
  beh.act$t <- beh.act$Time_diff_Treatment
  return(beh.act)
}

## Run on m.Activity object
beh.act <- processBehvr(m.Activity)
head(beh.act)

## Population plot
ggetho(beh.act[beh.act$id==2,], 
       aes(x=t, y=PiezoAct, col=Photoperiod, fill=Photoperiod), time_wrap = hours(24)) +#, 
  #time_offset = hours(4)) + 
  stat_pop_etho() + #facet_grid(Treatment~.) + 
  my_theme  + #ggtitle("4 Week Photoperiod") + #ylim(0,3) + 
  scale_color_manual(values = c("#F38BA8", "#23988aff")) +
  scale_fill_manual(values = c("#F38BA8", "#23988aff")) #+
  #scale_x_continuous(breaks =seq(0,(48*60*60),(3*60*60)), labels = seq(0,48,3))


## Actograms

##Not working
ggetho(beh.act, aes(x=t, z=PiezoAct)) + stat_tile_etho() + my_theme + 
  facet_wrap(~id) + 
  #scale_x_continuous(breaks =seq(0,(48*60*60),(6*60*60)), labels = seq(0,48,6)) +
  scale_fill_viridis()

ggetho(beh.act[beh.act$id=="5",], aes(x=t, z=PiezoAct)) +
 # stat_ld_annotations(height=1, ld_colours = c('white', 'grey70'),
  #                    outline = NA,l_duration = hours(12),phase = hours(6)) +
  stat_tile_etho() + #my_theme2 + theme(axis.text.y = element_text(size=15)) +
  #scale_fill_gradientn(colours = rev(terrain.colors(10))) +
  scale_fill_viridis() + ylab("Period") + xlab("Time (hours)") #+
  #scale_x_continuous(breaks =seq(0,172800,10800), labels = seq(0,48,3)) 

ggetho(beh.act, aes(x=t, y=as.factor(id), z=PiezoAct)) + stat_tile_etho() + scale_fill_viridis()

