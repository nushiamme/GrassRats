## Trying out CATkit package for circadian rhythm and activity data
require(tidyverse)
require(reshape2)
#require(wmtsa)
#require(waveslim)
#require(biwavelet)
require(stringr) ## For padding with leading 0's

require(ggplot2)
library(behavr)
library(ggetho)
library(zeitgebr) ## For periodogram and spectrogram
library(sleepr) ## For sleep analyses

## Set working directory
setwd("E:\\Ex_Google_Drive\\Piezo_data\\For_rethomics")

## Metadata file for rethomics behavr table
meta_full <- read.csv("E:\\Ex_Google_Drive\\Piezo_data\\Meta_ExptPhase1.csv")

meta_activ <- meta_full[,c("Indiv", "Sex", "Photoperiod", "Sugar", "Chamber")]

## General functions
my_theme <- theme_classic(base_size = 15) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))

my_theme2 <- theme_classic(base_size = 30) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))

my_colors <- c("#23988aff", "#F38BA8", "#440558ff", "#9ed93aff")

## Get all the required csv's together
paths <- dir(pattern = "\\.csv$")
names(paths) <- basename(paths)

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
m.Activity <- do.call(rbind, m.Activ)

m.Activity$Hour2 <- as.numeric(m.Activity$Hour)
m.Activity$Day2 <- as.numeric(m.Activity$Day)
m.Activity$Minute <- as.numeric(m.Activity$Minute)

m.Activity$Date <- as.POSIXct(paste(paste(m.Activity$Day2, m.Activity$Month, m.Activity$Year, sep="/"),
                                    paste(m.Activity$Hour2, m.Activity$Minute, sep=":")), format = "%d/%m/%Y %H:%M")

#dplyr::arrange(m.Activity, Date) # Sort just by date, not my individual

m.Activity <- m.Activity[order(m.Activity$Indiv, m.Activity$Date),]
head(m.Activity)
tail(m.Activity)

m.Activity$Treatment <- factor(m.Activity$Treatment, 
                               levels = c("2WeekAcclimation", "4WeekPhotoperiod", "LowSucrose", "HighSucrose"))

## for behavr processing
dt.act <- data.table::data.table(m.Activity, key="Indiv")
dt.meta <- data.table::data.table(meta_activ, key="Indiv")
names(dt.act)[names(dt.act) == 'Indiv'] <- 'id'
names(dt.meta)[names(dt.meta) == 'Indiv'] <- 'id'

beh.act <-behavr(dt.act,metadata = dt.meta)
beh.act$t <- rep(seq(1,300*length(beh.act$Time[beh.act$id=="T13"]),by=300), times=length(unique(beh.act$id)))
head(beh.act)
summary(beh.act, detailed=T)

stat_dt <- beh.act[,
              .(mean_acti = mean(PiezoAct),
                max_acti = max(PiezoAct)
              ),
              by='id']
stat_dt


## Sorting individuals by mean activity levels
# the average time spent moving per 1000 (rounded)
mean_piezo_beh <- beh.act[, .(mean_activ = round(mean(PiezoAct) * 1000)), by=id]
# join curent meta and the summary table
new_meta <- beh.act[mean_piezo_beh, meta=T]
# set new metadata
setmeta(beh.act, new_meta)
head(beh.act[meta=T])
head(beh.act)


## Trying out a ggetho plot
ggetho(beh.act, aes(x=t, y=id, z=PiezoAct)) + stat_bar_tile_etho()

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

##Same behavior over consecutive days, all indivs; excluding 2 week acclim; faceted by photoperiod
ggetho(beh.act[Treatment != "2WeekAcclimation",], 
       aes(x=t, y=PiezoAct, col=Photoperiod), time_wrap = hours(24)) + stat_pop_etho() + facet_grid(Photoperiod~.) + my_theme

## Same behavior over multiple days, single individual, short
ggetho(beh.act[Treatment != "2WeekAcclimation" & beh.act$id=="G9"], 
       aes(x=t, y=PiezoAct, col=Photoperiod), time_wrap = hours(24)) + stat_pop_etho()  + my_theme

## Same behavior over multiple days, single individual, long
ggetho(beh.act[beh.act$id=="T13"], 
       aes(x=t, y=PiezoAct, col=Photoperiod), time_wrap = hours(24)) + stat_pop_etho()  + my_theme

## Same behavior over multiple days, population-level, as polar coordinates
ggetho(beh.act, aes(x=t, y=PiezoAct, col=Photoperiod), time_wrap = days(1)) + stat_pop_etho(geom='polygon', fill=NA)  + 
 my_theme + coord_polar()

## Double-plotted actograms
ggetho(beh.act, aes(x=t, z=PiezoAct), multiplot = 2) + stat_bar_tile_etho() + my_theme

## Double-plotted actograms by individual
ggetho(beh.act, aes(x=t, z=PiezoAct), multiplot = 2) + stat_bar_tile_etho() + my_theme + facet_wrap(~id)

## For just one individual; long photoperiod
ggetho(beh.act[beh.act$id=="T13",],
       aes(x=t, z=PiezoAct), multiplot = 2) + stat_bar_tile_etho() + my_theme

## Short photoperiod individual G9 colored by photoperiod FIXXXXX
ggetho(beh.act[beh.act$id=="G9",],
       aes(x=t, z=PiezoAct, fill=Photoperiod), multiplot = 2) + stat_bar_tile_etho() + my_theme +
  scale_fill_manual(values = c('red', 'black'))

## Short photoperiod individual G9
ggetho(beh.act[beh.act$id=="G9",],
       aes(x=t, z=PiezoAct), multiplot = 2) + stat_bar_tile_etho() + my_theme

## Short photoperiod individual G5
ggetho(beh.act[beh.act$id=="G5",],
       aes(x=t, z=PiezoAct), multiplot = 2) + stat_bar_tile_etho() + my_theme

## LD in the background
ggetho(beh.act[beh.act$id=="G9" & beh.act$Treatment!="2WeekAcclimation",], aes(x=t, z=PiezoAct), multiplot=2) +
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
ggetho(beh.act, aes(x=t, y=PiezoAct), 
       time_wrap = hours(24),
       time_offset = hours(6)) + stat_pop_etho() + my_theme


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
                        period_range = c(hours(6), hours(28)))

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



breaks_min <- seq(from = 0,to = 60, by = 10)
## Now aggregated in 10-minute bins; can change to 5
ag <- aggregate(m.Activity$PiezoAct, FUN=mean,
                by=list(Indiv=m.Activity$Indiv, Treatment = m.Activity$Treatment, Day= m.Activity$Day2, 
                        Month= m.Activity$Month, Year= m.Activity$Year, Hour = m.Activity$Hour2, 
                        Minute=cut(m.Activity$Minute, breaks_min, include.lowest=T)))
names(ag)[names(ag) == 'x'] <- 'PiezoAct'

ag$Minute2 <- ag$Minute
levels(ag$Minute2) <- c(seq(0,50,10))
ag$Minute2<- str_pad(ag$Minute2, 2, pad = "0")
ag$Hour<- str_pad(ag$Hour, 2, pad = "0")
ag$Hour_min <- paste0(ag[,"Hour"],":",ag[,"Minute2"])
ag$Minute2 <- as.numeric(as.character(ag$Minute2))


#ag_or <- ag[with(ag, order(as.numeric(Month), Day, Hour, Minute2, Indiv)),]
#ag_or$Hour_min2<- factor(ag_or$Hour_min, levels = unique(ag_or$Hour_min), ordered=T)

ggplot(ag_or[ag_or$Indiv=="G17" & ag$Treatment=="4WeekPhotoperiod",], aes(Hour_min, PiezoAct)) + my_theme + #scale_y_reverse() +
  facet_grid(Month+Day~., scales = "free") + 
  geom_bar(stat = "identity") +
  scale_fill_gradient(name = 'value of\ninterest', low = 'white', high = 'red') +
  theme(axis.text.x = element_text(angle=90, hjust=0.5), panel.spacing.y = unit(0.1, "lines"))




tomelt <- read.csv("4WeekPhotoperiod_05May2019Actlev.csv")


melted <- melt(tomelt, id.vars= "Date", 
               measure.vars=c("G9", "G5", "G14", "G16", "G18", "G10", "G8", "G2", "T10", "T5", "T17", "T12", "G4", "G1","G13",
                              "G17", "G19", "G11", "G12",  "G3", "T9", "T11", "T19", "T13"))

for(i in unique(melted$variable)) {
  tosave <- melted[melted$variable==i,]
  names(tosave) <- c("Date", "Indiv", "Piezoact")
  write.csv(tosave,paste0("E:\\Ex_Google_Drive\\Piezo_data\\Acto_indivs\\","Acto_4Wk_05May2019_",i,".csv"),
            row.names=F)
}

G1$seq <- seq(1,1479,1)
G1 <- read.csv("Acto_indivs\\Acto_4wk_05May2019_G1.csv")
ts.G1 <- ts(G1$Piezoact,start=G1$seq[1],end = rev(G1$seq)[1])

behavr(G1,)