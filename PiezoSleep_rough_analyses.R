## Analyzing piezosleep data for grass rats
## Code by Anusha Shankar
## Contact: nushiamme<at>gmail<dot>com
## Started April 15, 2019

## Libraries
library(ggplot2)
library(reshape2)
library(plyr)
library(gridExtra)

## Set wd
setwd("E:\\Google Drive\\Toshiba_desktop\\Fairbanks\\Research\\GrassRats\\Piezo_data")


## Read in files
anim_ID <- read.csv("Chamber_Animal_IDs.csv")
anim_sex <- read.csv("Animal_sex.csv")
piezoday_short <- read.csv("4WeekPhotoperiod_15Apr2019_combined.csv")
piezoday_long <- read.csv("2WeekAcclimation_20Mar2019.csv")

piezoday_presugar_bouts <- read.csv("4WeekPhotoperiod_15Apr2019_SleepBout_MeanSB.csv")
piezoday_postsugar_bouts <- read.csv("HighSucrose_22May2019SleepBout_MeanSB.csv")

## General functions
my_theme <- theme_classic(base_size = 25) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))

## Melt to form long dataframes
m.piezoday_short <- melt(piezoday_short, id.vars= c("MOUSE_ID", "TESTDATE"), measure.vars=c("PERCENTSLEEPTOT1", "PERCENTSLEEPNIGHT1",
                                                                             "PERCENTSLEEPDAY1", "PERCENTSLEEPTOT2", 
                                                                             "PERCENTSLEEPNIGHT2", "PERCENTSLEEPDAY2",
                                                                             "PERCENTSLEEPTOT3", "PERCENTSLEEPNIGHT3",
                                                                             "PERCENTSLEEPDAY3", "PERCENTSLEEPTOT4", 
                                                                             "PERCENTSLEEPNIGHT4", "PERCENTSLEEPDAY4",
                                                                             "PERCENTSLEEPTOT5", "PERCENTSLEEPNIGHT5",
                                                                             "PERCENTSLEEPDAY5", "PERCENTSLEEPTOT6", 
                                                                             "PERCENTSLEEPNIGHT6", "PERCENTSLEEPDAY6",
                                                                             "PERCENTSLEEPTOT7", "PERCENTSLEEPNIGHT7", 
                                                                             "PERCENTSLEEPDAY7", "PERCENTSLEEPTOT8", 
                                                                             "PERCENTSLEEPNIGHT8", "PERCENTSLEEPDAY8"))

m.piezobout_short <- melt(piezoday_short, id.vars= c("MOUSE_ID", "TESTDATE"), measure.vars=c("BOUTLENGTHTOT1", "BOUTLENGTHNITE1",
                                                                                            "BOUTLENGTHDAY1", "BOUTLENGTHTOT2", 
                                                                                            "BOUTLENGTHNITE2", "BOUTLENGTHDAY2",
                                                                                            "BOUTLENGTHTOT3", "BOUTLENGTHNITE3",
                                                                                            "BOUTLENGTHDAY3", "BOUTLENGTHTOT4", 
                                                                                            "BOUTLENGTHNITE4", "BOUTLENGTHDAY4",
                                                                                            "BOUTLENGTHTOT5", "BOUTLENGTHNITE5",
                                                                                            "BOUTLENGTHDAY5", "BOUTLENGTHTOT6", 
                                                                                            "BOUTLENGTHNITE6", "BOUTLENGTHDAY6",
                                                                                            "BOUTLENGTHTOT7", "BOUTLENGTHNITE7", 
                                                                                            "BOUTLENGTHDAY7", "BOUTLENGTHTOT8", 
                                                                                            "BOUTLENGTHNITE8", "BOUTLENGTHDAY8"))


m.piezoday_long <- melt(piezoday_long, id.vars= c("MOUSE_ID", "TESTDATE"), measure.vars=c("PERCENTSLEEPTOT1", "PERCENTSLEEPNIGHT1",
                                                                                            "PERCENTSLEEPDAY1", "PERCENTSLEEPTOT2", 
                                                                                            "PERCENTSLEEPNIGHT2", "PERCENTSLEEPDAY2",
                                                                                            "PERCENTSLEEPTOT3", "PERCENTSLEEPNIGHT3",
                                                                                            "PERCENTSLEEPDAY3", "PERCENTSLEEPTOT4", 
                                                                                            "PERCENTSLEEPNIGHT4", "PERCENTSLEEPDAY4",
                                                                                            "PERCENTSLEEPTOT5", "PERCENTSLEEPNIGHT5",
                                                                                            "PERCENTSLEEPDAY5", "PERCENTSLEEPTOT6", 
                                                                                            "PERCENTSLEEPNIGHT6", "PERCENTSLEEPDAY6"))

m.piezobout_short <- rename(m.piezobout_short, replace = c("MOUSE_ID"="GrassRat_ID"))


### Processing sleep bout length changes before and after sugar
m.presugar <- melt(piezoday_presugar_bouts, id.vars="Mouse.IDs.", 
                   measure.vars=c("G9", "G5", "G14", "G16", "G18", "G10", "G8", "G2", "T10", "T5", "T17", "T12", "G4", "G1",
                                  "G13", "G17", "G19", "G11", "G12",  "G3", "T9", "T11", "T19", "T13"))
m.postsugar <- melt(piezoday_postsugar_bouts, id.vars="Mouse.IDs.", 
                    measure.vars=c("G9", "G5", "G14", "G16", "G18", "G10", "G8", "G2", "T10", "T5", "T17", "T12", "G4", "G1",
                                   "G13", "G17", "G19", "G11", "G12",  "G3", "T9", "T11", "T19", "T13"))

names(m.presugar) <- c("DateTime", "ID", "Sleep_bout")
names(m.postsugar) <- c("DateTime", "ID", "Sleep_bout")
m.presugar$datetime.posix <- as.POSIXct(m.presugar$DateTime, format="%m/%d/%Y %H:%M", tz="GMT")
m.presugar$date <- as.POSIXct(strptime(m.presugar$DateTime, format="%m/%d/%Y", tz="GMT"))

m.presugar$time <- strftime(m.presugar$datetime.posix, format="%H:%M", tz = "GMT")
m.presugar$time <- as.POSIXct(m.presugar$time, format="%H:%M", tz = "GMT")

m.presugar$Treatment <- 0
m.presugar$Treatment[m.presugar$ID %in% c("G9", "G5", "G14", "G16", "G18", "G10", 
                                        "G8", "G2", "T10", "T5", "T17", "T12")] <- "Short"
m.presugar$Treatment[m.presugar$ID %in% c("G4", "G1", "G13", "G17", "G19", "G11", "G12", "G3", "T9", 
                                        "T11", "T19", "T13")] <- "Long"

m.postsugar$Treatment <- 0
m.postsugar$Treatment[m.postsugar$ID %in% c("G9", "G5", "G14", "G16", "G18", "G10", 
                                          "G8", "G2", "T10", "T5", "T17", "T12")] <- "Short"
m.postsugar$Treatment[m.postsugar$ID %in% c("G4", "G1", "G13", "G17", "G19", "G11", "G12", "G3", "T9", 
                                          "T11", "T19", "T13")] <- "Long"

m.presugar$Sugar <- "Pre"
m.postsugar$Sugar <- "Post"

bound_prepost_sugar <- rbind(m.presugar, m.postsugar)

## Sleep bout plot
ggplot(m.presugar, aes(ID, Sleep_bout)) + geom_boxplot() + facet_grid(.~Treatment, scales="free_x") + my_theme +
  ylab("Sleep bout (s)")


ggplot(m.postsugar, aes(ID, Sleep_bout)) + geom_boxplot() + facet_grid(.~Treatment, scales="free_x") + my_theme +
  ylab("Sleep bout (s)")

ggplot(bound_prepost_sugar, aes(ID, Sleep_bout)) + geom_boxplot() + facet_grid(Sugar~Treatment, scales="free_x") + my_theme +
  ylab("Sleep bout (s)")

mod_sleepbout_SugarTreatmentIndiv <- lmer(Sleep_bout~Treatment+Sugar+(1|ID), data=bound_prepost_sugar)
summary(mod_sleepbout_SugarTreatmentIndiv)
plot(mod_sleepbout_SugarTreatmentIndiv)

mod_sleepbout_SugarIndiv <- lmer(Sleep_bout~Sugar+(1|ID), data=bound_prepost_sugar)
summary(mod_sleepbout_SugarIndiv)
plot(mod_sleepbout_SugarIndiv)

mod_sleepbout_Sugar <- lm(Sleep_bout~Sugar, data=bound_prepost_sugar)
summary(mod_sleepbout_Sugar)

anova(mod_sleepbout_Sugar, mod_sleepbout_SugarIndiv, mod_sleepbout_SugarTreatmentIndiv)

ggplot(bound_prepost_sugar, aes(Sleep_bout)) + geom_histogram() + facet_grid(.~Treatment)
ggplot(bound_prepost_sugar, aes(Sugar, Sleep_bout)) + geom_boxplot() + facet_grid(.~Treatment) + my_theme

## Function to pull the experiment day out of the "variable" column and make it a new one
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
m.piezoday_short$ExptDay <- substrRight(as.character(m.piezoday_short$variable), 1)
m.piezobout_short$ExptDay <- substrRight(as.character(m.piezobout_short$variable), 1)
m.piezoday_long$ExptDay <- substrRight(as.character(m.piezoday_long$variable), 1)


## To extract what the measure is
substrLeft <- function(x, n){
  substr(x,1,nchar(x)-1)
}
m.piezoday_short$Measure <- substrLeft(as.character(m.piezoday_short$variable), 1)
m.piezobout_short$Measure <- substrLeft(as.character(m.piezobout_short$variable), 1)
m.piezoday_long$Measure <- substrLeft(as.character(m.piezoday_long$variable), 1)

## Day or night column
substrMid <- function(x, n){
  substr(x, nchar(x)-2, nchar(x))
}
m.piezoday_short$DayNightTot <- substrMid(as.character(m.piezoday_short$Measure))
m.piezoday_short$DayNightTot<- revalue(as.factor(m.piezoday_short$DayNightTot),c("GHT"="Night", "TOT" = "Total", "DAY" = "Day"))

m.piezobout_short$DayNightTot <- substrMid(as.character(m.piezobout_short$Measure))
m.piezobout_short$DayNightTot<- revalue(as.factor(m.piezobout_short$DayNightTot),c("ITE"="Night", "TOT" = "Total", "DAY" = "Day"))

m.piezoday_long$DayNightTot <- substrMid(as.character(m.piezoday_long$Measure))
m.piezoday_long$DayNightTot<- revalue(as.factor(m.piezoday_long$DayNightTot),c("GHT"="Night", "TOT" = "Total", "DAY" = "Day"))



## Add chamber numbers to data frame to add treatment levels
m.piezoday_short <- merge(m.piezoday_short, anim_ID, by=c("MOUSE_ID"))
m.piezoday_short <- merge(m.piezoday_short, anim_sex, by=c("MOUSE_ID"))

m.piezobout_short <- merge(m.piezobout_short, anim_ID, by=c("MOUSE_ID"))
m.piezobout_short <- merge(m.piezobout_short, anim_sex, by=c("MOUSE_ID"))

m.piezoday_long <- merge(m.piezoday_long, anim_ID, by=c("MOUSE_ID"))
m.piezoday_long <- merge(m.piezoday_long, anim_sex, by=c("MOUSE_ID"))


## Add column for Room/Treatment
m.piezoday_short$ShortLong <- 0
m.piezoday_short$ShortLong[m.piezoday_short$Chamber<13] <- "Short"
m.piezoday_short$ShortLong[m.piezoday_short$Chamber>12] <- "Long"

m.piezobout_short$ShortLong <- 0
m.piezobout_short$ShortLong[m.piezobout_short$Chamber<13] <- "Short"
m.piezobout_short$ShortLong[m.piezobout_short$Chamber>12] <- "Long"

m.piezoday_long$ShortLong <- 0
m.piezoday_long$ShortLong[m.piezoday_long$Chamber<13] <- "Short"
m.piezoday_long$ShortLong[m.piezoday_long$Chamber>12] <- "Long"

## Make some test sleep-wake histograms per animal
sleep_wake_4wk <- ggplot(m.piezoday_short[m.piezoday_short$DayNightTot!="Total",], aes(DayNightTot, value)) + 
  facet_grid(.~MOUSE_ID) + my_theme +  geom_boxplot(aes(fill=DayNightTot)) +
  scale_fill_manual(values = c("grey70", "black")) +
  theme(axis.text.x = element_text(angle=90)) +
  ylab("Percent Sleep")
  
sleep_wake_4wk

## Sleep-wake boxplots overall
sleep_wake_4wk_all <- ggplot(m.piezoday_short[m.piezoday_short$DayNightTot!="Total",], aes(DayNightTot, value)) + 
  facet_grid(.~ShortLong) + my_theme +  geom_boxplot(aes(fill=DayNightTot)) +
  scale_fill_manual(values = c("grey70", "black")) + geom_point(aes(col=MOUSE_ID)) +
  theme(axis.text.x = element_text(angle=90)) + ylim(0,100) +
  ylab("Percent Sleep") + ggtitle("4 Week Test")

sleep_wake_4wk_all

## Bout
sleep_bout_expt_all <- ggplot(m.piezobout_short[m.piezobout_short$DayNightTot!="Total",], aes(DayNightTot, value)) + 
  facet_grid(.~ShortLong) + my_theme +  geom_boxplot(aes(fill=DayNightTot)) +
  scale_fill_manual(values = c("grey70", "black")) + geom_point(aes(col=GrassRat_ID), size=2) +
  theme(axis.text.x = element_text(angle=90)) +
  ylab("Mean sleep bout length (seconds)") + xlab("Day/Night")

sleep_bout_expt_all

## Now same graphs for 2-week acclimation period
## Make some test sleep-wake histograms per animal
sleep_wake_2wk <- ggplot(m.piezoday_long[m.piezoday_long$DayNightTot!="Total",], aes(DayNightTot, value)) + 
  facet_grid(.~MOUSE_ID) + my_theme +  geom_boxplot(aes(fill=DayNightTot)) +
  scale_fill_manual(values = c("grey70", "black")) +
  theme(axis.text.x = element_text(angle=90)) +
  ylab("Percent Sleep")

sleep_wake_2wk

## Sleep-wake boxplots overall
sleep_wake_2wk_all <- ggplot(m.piezoday_long[m.piezoday_long$DayNightTot!="Total",], aes(DayNightTot, value)) + 
  facet_grid(.~ShortLong) + my_theme +  geom_boxplot(aes(fill=DayNightTot)) +
  scale_fill_manual(values = c("grey70", "black")) + geom_point(aes(col=MOUSE_ID)) +
  theme(axis.text.x = element_text(angle=90)) + ylim(0,100) +
  ylab("Percent Sleep") + ggtitle("2 Week Acclimation")

sleep_wake_2wk_all
  
grid.arrange(sleep_wake_2wk_all, sleep_wake_4wk_all, ncol=2)
