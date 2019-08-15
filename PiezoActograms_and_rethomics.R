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

## General functions
my_theme <- theme_classic(base_size = 10) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))

setwd("E:\\Ex_Google_Drive\\Piezo_data\\For_rethomics")

## Metadata file for rethomics behavr table
meta_full <- read.csv("E:\\Ex_Google_Drive\\Piezo_data\\Meta_ExptPhase1.csv")

meta_activ <- meta_full[,c("Indiv", "Sex", "Photoperiod", "Sugar", "Chamber")]

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

m.Activity$Treatment <- factor(m.Activity$Treatment, 
                               levels = c("2WeekAcclimation", "4WeekPhotoperiod", "LowSucrose", "HighSucrose"))
ggplot(m.Activity[m.Activity$Indiv=="G9",], aes(Hour2, Day2)) + my_theme + scale_y_reverse() +
  facet_grid(Treatment+Month~., scales = "free", space="free_y") + geom_tile(aes(fill=PiezoAct)) +
  scale_fill_gradient(name = 'value of\ninterest', low = 'white', high = 'red')

ggplot(m.Activity[m.Activity$Indiv=="G9",], aes(Hour2, PiezoAct)) + my_theme +
  facet_grid(Treatment~Month, space="free_y") + geom_boxplot(aes(group=Hour2)) + geom_smooth(method="loess")


#Test <- m.Activity[m.Activity$Indiv=="G9" & m.Activity$Treatment=="LowSucrose",]



m.Activity$Minute <- as.numeric(m.Activity$Minute)
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

dt.act <- data.table::data.table(m.Activity, key="Indiv")
dt.meta <- data.table::data.table(meta_activ, key="Indiv")
names(dt.act)[names(dt.act) == 'Indiv'] <- 'id'
names(dt.meta)[names(dt.meta) == 'Indiv'] <- 'id'

## for behavr processing
beh.act <-behavr(dt.act,metadata = dt.meta)

stat_dt <- beh.act[,
              .(mean_acti = mean(PiezoAct),
                max_acti = max(PiezoAct)
              ),
              by='id']
stat_dt


## Trying out a ggetho plot
ggetho(beh.act, aes(x=Minute, y=id, z=PiezoAct)) + stat_tile_etho()



### Old

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