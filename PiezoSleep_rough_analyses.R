## Analyzing piezosleep data for grass rats
## Code by Anusha Shankar
## Contact: nushiamme<at>gmail<dot>com
## Started April 15, 2019

## Libraries
library(ggplot2)
library(reshape2)
library(plyr)
library(gridExtra)
require(lmerTest)
require(multcomp) ## for glht for plotting lmer result
require(CATkit) ## For circadian analyses

## Set wd
setwd("E:\\Ex_Google_Drive\\Piezo_data\\SleepBout_histogram_files/")


## Read in files
anim_ID <- read.csv("Chamber_Animal_IDs.csv")
anim_sex <- read.csv("Animal_sex.csv")
piezoday_4wk <- read.csv("4WeekPhotoperiod_15Apr2019_dailySS_combinedTime.csv")
piezoday_4wk_2 <- read.csv("4WeekPhotoperiod_24Apr2019_dailySS_combinedTime.csv")
piezoday_4wk_3 <- read.csv("4WeekPhotoperiod_08Apr2019_dailySS_combinedTime.csv")
piezoday_2wk <- read.csv("2WeekAcclimation_26Mar2019_longTime.csv")
piezoday_LowSugar <- read.csv("LowSucrose_07May2019_dailySS_2PercentSucrose_combinedTime.csv")
piezoday_HighSugar <- read.csv("HighSucrose_29May2019_dailySS_combinedTime.csv")
piezoday_presugar_bouts <- read.csv("4WeekPhotoperiod_15Apr2019SleepBout_MeanSB.csv")
piezoday_postsugar_bouts <- read.csv("HighSucrose_29May2019SleepBout_MeanSB.csv")

bout_histogram_2wk <- read.csv("2WeekAcclimation_26Mar2019SleepBout__histogramSB.csv")
bout_histogram_4wk <- read.csv("4WeekPhotoperiod_15Apr2019SleepBout__histogramSB.csv")
bout_histogram_LowSugar <- read.csv("LowSucrose_07May2019SleepBout_2PercentSucrose__histogramSB.csv")

sugar <- read.csv("Sugar_conc.csv")


## General functions
my_theme <- theme_classic(base_size = 25) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))

## Melt to form long dataframes
m.piezoday_short2 <- melt(piezoday_4wk, id.vars= c("MOUSE_ID", "TESTDATE"), measure.vars=c("PERCENTSLEEPTOT1", "PERCENTSLEEPNIGHT1",
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

m.piezobout_short2 <- melt(piezoday_4wk, id.vars= c("MOUSE_ID", "TESTDATE"), measure.vars=c("BOUTLENGTHTOT1", "BOUTLENGTHNITE1",
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

m.piezoday_short3 <- melt(piezoday_4wk_2, id.vars= c("MOUSE_ID", "TESTDATE"), measure.vars=c("PERCENTSLEEPTOT1", "PERCENTSLEEPNIGHT1",
                                                                                          "PERCENTSLEEPDAY1", "PERCENTSLEEPTOT2", 
                                                                                          "PERCENTSLEEPNIGHT2", "PERCENTSLEEPDAY2",
                                                                                          "PERCENTSLEEPTOT3", "PERCENTSLEEPNIGHT3",
                                                                                          "PERCENTSLEEPDAY3", "PERCENTSLEEPTOT4", 
                                                                                          "PERCENTSLEEPNIGHT4", "PERCENTSLEEPDAY4",
                                                                                          "PERCENTSLEEPTOT5", "PERCENTSLEEPNIGHT5",
                                                                                          "PERCENTSLEEPDAY5", "PERCENTSLEEPTOT6", 
                                                                                          "PERCENTSLEEPNIGHT6", "PERCENTSLEEPDAY6"))

m.piezobout_short3 <- melt(piezoday_4wk_2, id.vars= c("MOUSE_ID", "TESTDATE"), measure.vars=c("BOUTLENGTHTOT1", "BOUTLENGTHNITE1",
                                                                                           "BOUTLENGTHDAY1", "BOUTLENGTHTOT2", 
                                                                                           "BOUTLENGTHNITE2", "BOUTLENGTHDAY2",
                                                                                           "BOUTLENGTHTOT3", "BOUTLENGTHNITE3",
                                                                                           "BOUTLENGTHDAY3", "BOUTLENGTHTOT4", 
                                                                                           "BOUTLENGTHNITE4", "BOUTLENGTHDAY4",
                                                                                           "BOUTLENGTHTOT5", "BOUTLENGTHNITE5",
                                                                                           "BOUTLENGTHDAY5", "BOUTLENGTHTOT6", 
                                                                                           "BOUTLENGTHNITE6", "BOUTLENGTHDAY6"))

m.piezoday_short1 <- melt(piezoday_4wk_3, id.vars= c("MOUSE_ID", "TESTDATE"), measure.vars=c("PERCENTSLEEPTOT1", "PERCENTSLEEPNIGHT1",
                                                                                             "PERCENTSLEEPDAY1", "PERCENTSLEEPTOT2", 
                                                                                             "PERCENTSLEEPNIGHT2", "PERCENTSLEEPDAY2",
                                                                                             "PERCENTSLEEPTOT3", "PERCENTSLEEPNIGHT3",
                                                                                             "PERCENTSLEEPDAY3", "PERCENTSLEEPTOT4", 
                                                                                             "PERCENTSLEEPNIGHT4", "PERCENTSLEEPDAY4",
                                                                                             "PERCENTSLEEPTOT5", "PERCENTSLEEPNIGHT5",
                                                                                             "PERCENTSLEEPDAY5", "PERCENTSLEEPTOT6", 
                                                                                             "PERCENTSLEEPNIGHT6", "PERCENTSLEEPDAY6"))

m.piezobout_short1 <- melt(piezoday_4wk_3, id.vars= c("MOUSE_ID", "TESTDATE"), measure.vars=c("BOUTLENGTHTOT1", "BOUTLENGTHNITE1",
                                                                                              "BOUTLENGTHDAY1", "BOUTLENGTHTOT2", 
                                                                                              "BOUTLENGTHNITE2", "BOUTLENGTHDAY2",
                                                                                              "BOUTLENGTHTOT3", "BOUTLENGTHNITE3",
                                                                                              "BOUTLENGTHDAY3", "BOUTLENGTHTOT4", 
                                                                                              "BOUTLENGTHNITE4", "BOUTLENGTHDAY4",
                                                                                              "BOUTLENGTHTOT5", "BOUTLENGTHNITE5",
                                                                                              "BOUTLENGTHDAY5", "BOUTLENGTHTOT6", 
                                                                                              "BOUTLENGTHNITE6", "BOUTLENGTHDAY6"))


m.piezoday_long <- melt(piezoday_2wk, id.vars= c("MOUSE_ID", "TESTDATE"), measure.vars=c("PERCENTSLEEPTOT1", "PERCENTSLEEPNIGHT1",
                                                                                            "PERCENTSLEEPDAY1", "PERCENTSLEEPTOT2", 
                                                                                            "PERCENTSLEEPNIGHT2", "PERCENTSLEEPDAY2",
                                                                                            "PERCENTSLEEPTOT3", "PERCENTSLEEPNIGHT3",
                                                                                            "PERCENTSLEEPDAY3", "PERCENTSLEEPTOT4", 
                                                                                            "PERCENTSLEEPNIGHT4", "PERCENTSLEEPDAY4",
                                                                                            "PERCENTSLEEPTOT5", "PERCENTSLEEPNIGHT5",
                                                                                            "PERCENTSLEEPDAY5"))

m.piezobout_long <- melt(piezoday_2wk, id.vars= c("MOUSE_ID", "TESTDATE"), measure.vars=c("BOUTLENGTHTOT1", "BOUTLENGTHNITE1",
                                                                                           "BOUTLENGTHDAY1", "BOUTLENGTHTOT2", 
                                                                                           "BOUTLENGTHNITE2", "BOUTLENGTHDAY2",
                                                                                           "BOUTLENGTHTOT3", "BOUTLENGTHNITE3",
                                                                                           "BOUTLENGTHDAY3", "BOUTLENGTHTOT4", 
                                                                                           "BOUTLENGTHNITE4", "BOUTLENGTHDAY4",
                                                                                           "BOUTLENGTHTOT5", "BOUTLENGTHNITE5",
                                                                                           "BOUTLENGTHDAY5"))

m.piezoday_LowSugar <- melt(piezoday_LowSugar, id.vars= c("MOUSE_ID", "TESTDATE"), measure.vars=c("PERCENTSLEEPTOT1", "PERCENTSLEEPNIGHT1",
                                                                                         "PERCENTSLEEPDAY1", "PERCENTSLEEPTOT2", 
                                                                                         "PERCENTSLEEPNIGHT2", "PERCENTSLEEPDAY2"))

m.piezobout_LowSugar <- melt(piezoday_LowSugar, id.vars= c("MOUSE_ID", "TESTDATE"), measure.vars=c("BOUTLENGTHTOT1", "BOUTLENGTHNITE1",
                                                                                          "BOUTLENGTHDAY1", "BOUTLENGTHTOT2", 
                                                                                          "BOUTLENGTHNITE2", "BOUTLENGTHDAY2"))

m.piezoday_HighSugar <- melt(piezoday_HighSugar, id.vars= c("MOUSE_ID", "TESTDATE"), measure.vars=c("PERCENTSLEEPTOT1", "PERCENTSLEEPNIGHT1",
                                                                                         "PERCENTSLEEPDAY1", "PERCENTSLEEPTOT2", 
                                                                                         "PERCENTSLEEPNIGHT2", "PERCENTSLEEPDAY2",
                                                                                         "PERCENTSLEEPTOT3", "PERCENTSLEEPNIGHT3",
                                                                                         "PERCENTSLEEPDAY3", "PERCENTSLEEPTOT4", 
                                                                                         "PERCENTSLEEPNIGHT4", "PERCENTSLEEPDAY4",
                                                                                         "PERCENTSLEEPTOT5", "PERCENTSLEEPNIGHT5",
                                                                                         "PERCENTSLEEPDAY5"))

m.piezobout_HighSugar <- melt(piezoday_HighSugar, id.vars= c("MOUSE_ID", "TESTDATE"), measure.vars=c("BOUTLENGTHTOT1", "BOUTLENGTHNITE1",
                                                                                          "BOUTLENGTHDAY1", "BOUTLENGTHTOT2", 
                                                                                          "BOUTLENGTHNITE2", "BOUTLENGTHDAY2",
                                                                                          "BOUTLENGTHTOT3", "BOUTLENGTHNITE3",
                                                                                          "BOUTLENGTHDAY3", "BOUTLENGTHTOT4", 
                                                                                          "BOUTLENGTHNITE4", "BOUTLENGTHDAY4",
                                                                                          "BOUTLENGTHTOT5", "BOUTLENGTHNITE5",
                                                                                          "BOUTLENGTHDAY5"))


m.piezobout_long$Treatment <- "2wk"
m.piezobout_short1$Treatment <- "4wk"
m.piezobout_short2$Treatment <- "4wk2"
m.piezobout_short3$Treatment <- "4wk3"
m.piezobout_LowSugar$Treatment <- "LowSugar"
m.piezobout_HighSugar$Treatment <- "HighSugar"

m.piezoday_long$Treatment <- "2wk"
m.piezoday_short1$Treatment <- "4wk"
m.piezoday_short2$Treatment <- "4wk2"
m.piezoday_short3$Treatment <- "4wk3"
m.piezoday_LowSugar$Treatment <- "LowSugar"
m.piezoday_HighSugar$Treatment <- "HighSugar"



m.piezobout_short1 <- rename(m.piezobout_short1, replace = c("MOUSE_ID"="GrassRat_ID"))
m.piezobout_short2 <- rename(m.piezobout_short2, replace = c("MOUSE_ID"="GrassRat_ID"))
m.piezobout_short3 <- rename(m.piezobout_short3, replace = c("MOUSE_ID"="GrassRat_ID"))
m.piezobout_long <- rename(m.piezobout_long, replace = c("MOUSE_ID"="GrassRat_ID"))
m.piezoday_short1 <- rename(m.piezoday_short1, replace = c("MOUSE_ID"="GrassRat_ID"))
m.piezoday_short2 <- rename(m.piezoday_short2, replace = c("MOUSE_ID"="GrassRat_ID"))
m.piezoday_short3 <- rename(m.piezoday_short3, replace = c("MOUSE_ID"="GrassRat_ID"))
m.piezoday_long <- rename(m.piezoday_long, replace = c("MOUSE_ID"="GrassRat_ID"))
m.piezoday_LowSugar <- rename(m.piezoday_LowSugar, replace = c("MOUSE_ID"="GrassRat_ID"))
m.piezoday_HighSugar <- rename(m.piezoday_HighSugar, replace = c("MOUSE_ID"="GrassRat_ID"))
m.piezobout_LowSugar <- rename(m.piezobout_LowSugar, replace = c("MOUSE_ID"="GrassRat_ID"))
m.piezobout_HighSugar <- rename(m.piezobout_HighSugar, replace = c("MOUSE_ID"="GrassRat_ID"))


m.piezobout_short <- rbind(m.piezobout_short1, m.piezobout_short2, m.piezobout_short3)
m.piezoday_short <- rbind(m.piezoday_short1, m.piezoday_short2, m.piezoday_short3)

m.piezobout_4wksugars <- rbind(m.piezobout_short, m.piezobout_LowSugar, m.piezobout_HighSugar)
m.piezoday_4wksugars <- rbind(m.piezoday_short, m.piezoday_LowSugar, m.piezoday_HighSugar)

## Processing sleep bout histograms
## 2 week acclimation
m.bout_histo_2wk <- melt(bout_histogram_2wk, id.vars="Mouse.IDs.", 
                         measure.vars=c("G9", "G5", "G14", "G16", "G18", "G10", "G8", "G2", "T10", "T5", "T17", "T12", "G4", "G1",
                                        "G13", "G17", "G19", "G11", "G12",  "G3", "T9", "T11", "T19", "T13"))
names(m.bout_histo_2wk) <- c("BoutLength_sec", "GrassRat_ID", "PercentTime")
m.bout_histo_2wk$Treatment <- "2wk"

ggplot(m.bout_histo_2wk, aes(BoutLength_sec/60, PercentTime)) + geom_line(aes(col=GrassRat_ID)) +
  my_theme + xlab("Bout length (min)")

## 4 week photoperiod
m.bout_histo_4wk <- melt(bout_histogram_4wk, id.vars="Mouse.IDs.", 
                         measure.vars=c("G9", "G5", "G14", "G16", "G18", "G10", "G8", "G2", "T10", "T5", "T17", "T12", "G4", "G1",
                                        "G13", "G17", "G19", "G11", "G12",  "G3", "T9", "T11", "T19", "T13"))
names(m.bout_histo_4wk) <- c("BoutLength_sec", "GrassRat_ID", "PercentTime")

m.bout_histo_4wk$Treatment <- 0
m.bout_histo_4wk$Treatment[m.bout_histo_4wk$GrassRat_ID %in% c("G9", "G5", "G14", "G16", "G18", "G10", 
                                          "G8", "G2", "T10", "T5", "T17", "T12")] <- "Short_4wk"
m.bout_histo_4wk$Treatment[m.bout_histo_4wk$GrassRat_ID %in% c("G4", "G1", "G13", "G17", "G19", "G11", "G12", "G3", "T9", 
                                          "T11", "T19", "T13")] <- "Neutral_4wk"

ggplot(m.bout_histo_4wk, aes(BoutLength_sec/60, PercentTime)) + geom_line(aes(col=GrassRat_ID)) + facet_grid(Treatment~.) +
  my_theme + xlab("Bout length (min)")

## Low sugar
m.bout_histo_LowSugar <- melt(bout_histogram_LowSugar, id.vars="Mouse.IDs.", 
                         measure.vars=c("G9", "G5", "G14", "G16", "G18", "G10", "G8", "G2", "T10", "T5", "T17", "T12", "G4", "G1",
                                        "G13", "G17", "G19", "G11", "G12",  "G3", "T9", "T11", "T19", "T13"))
names(m.bout_histo_LowSugar) <- c("BoutLength_sec", "GrassRat_ID", "PercentTime")

m.bout_histo_LowSugar$Treatment <- 0
m.bout_histo_LowSugar$Treatment[m.bout_histo_4wk$GrassRat_ID %in% c("G9", "G5", "G14", "G16", "G18", "G10", 
                                                               "G8", "G2", "T10", "T5", "T17", "T12")] <- "Short_lowSugar"
m.bout_histo_LowSugar$Treatment[m.bout_histo_4wk$GrassRat_ID %in% c("G4", "G1", "G13", "G17", "G19", "G11", "G12", "G3", "T9", 
                                                               "T11", "T19", "T13")] <- "Neutral_lowSugar"

ggplot(m.bout_histo_LowSugar, aes(BoutLength_sec/60, PercentTime)) + geom_line(aes(col=GrassRat_ID)) + facet_grid(Treatment~.) +
  my_theme + xlab("Bout length (min)")

#### Sugar files ####

### Processing sleep bout length changes before and after sugar
m.presugar <- melt(piezoday_presugar_bouts, id.vars="Mouse.IDs.", 
                   measure.vars=c("G9", "G5", "G14", "G16", "G18", "G10", "G8", "G2", "T10", "T5", "T17", "T12", "G4", "G1",
                                  "G13", "G17", "G19", "G11", "G12",  "G3", "T9", "T11", "T19", "T13"))
m.postsugar <- melt(piezoday_postsugar_bouts, id.vars="Mouse.IDs.", 
                    measure.vars=c("G9", "G5", "G14", "G16", "G18", "G10", "G8", "G2", "T10", "T5", "T17", "T12", "G4", "G1",
                                   "G13", "G17", "G19", "G11", "G12",  "G3", "T9", "T11", "T19", "T13"))


names(m.presugar) <- c("DateTime", "ID", "Sleep_bout")
names(m.postsugar) <- c("DateTime", "ID", "Sleep_bout")
#m.presugar$datetime.posix <- as.POSIXct(m.presugar$DateTime, format="%m/%d/%Y %H:%M", tz="GMT")
#m.presugar$date <- as.POSIXct(strptime(m.presugar$DateTime, format="%m/%d/%Y", tz="GMT"))

#m.presugar$time <- strftime(m.presugar$datetime.posix, format="%H:%M", tz = "GMT")
#m.presugar$time <- as.POSIXct(m.presugar$time, format="%H:%M", tz = "GMT")

m.presugar$Treatment <- 0
m.presugar$Treatment[m.presugar$ID %in% c("G9", "G5", "G14", "G16", "G18", "G10", 
                                        "G8", "G2", "T10", "T5", "T17", "T12")] <- "Short"
m.presugar$Treatment[m.presugar$ID %in% c("G4", "G1", "G13", "G17", "G19", "G11", "G12", "G3", "T9", 
                                        "T11", "T19", "T13")] <- "Neutral"

m.postsugar$Treatment <- 0
m.postsugar$Treatment[m.postsugar$ID %in% c("G9", "G5", "G14", "G16", "G18", "G10", 
                                          "G8", "G2", "T10", "T5", "T17", "T12")] <- "Short"
m.postsugar$Treatment[m.postsugar$ID %in% c("G4", "G1", "G13", "G17", "G19", "G11", "G12", "G3", "T9", 
                                          "T11", "T19", "T13")] <- "Neutral"

m.presugar$Sugar <- "Pre"
m.postsugar$Sugar <- "Post"

bound_prepost_sugar <- rbind(m.presugar, m.postsugar)
bound_prepost_sugar <- bound_prepost_sugar[!is.na(bound_prepost_sugar$Sleep_bout),]
bound_prepost_sugar$TreatmentSugar <- paste0(bound_prepost_sugar$Treatment, bound_prepost_sugar$Sugar)

bound_prepost_sugar$Time <- 
    sapply(strsplit(as.character(bound_prepost_sugar$DateTime), "[/ ]+"), "[", 2)

bound_prepost_sugar$Hour <- 
  sapply(strsplit(as.character(bound_prepost_sugar$Time), ":"), "[", 1)
bound_prepost_sugar$Hour <- as.numeric(bound_prepost_sugar$Hour)
bound_prepost_sugar$Hour2 <- bound_prepost_sugar$Hour
bound_prepost_sugar$Hour2[bound_prepost_sugar$Hour2>12] <- bound_prepost_sugar$Hour2[bound_prepost_sugar$Hour2>12]-24

#Subtract 24 from anything >12

## Sleep bout plot
ggplot(m.presugar, aes(ID, Sleep_bout)) + geom_boxplot() + facet_grid(.~Treatment, scales="free_x") + my_theme +
  ylab("Sleep bout (s)")

ggplot(m.postsugar, aes(ID, Sleep_bout)) + geom_boxplot() + facet_grid(.~Treatment, scales="free_x") + my_theme +
  ylab("Sleep bout (s)")

ggplot(bound_prepost_sugar, aes(ID, Sleep_bout)) + geom_boxplot() + facet_grid(Sugar~Treatment, scales="free_x") + 
  my_theme + ylab("Sleep bout (s)")

mod_sleepbout_SugarTreatmentIndiv <- lmer(Sleep_bout~-1+(Hour)^2+TreatmentSugar+(1|ID), 
                                          data=bound_prepost_sugar[!is.na(bound_prepost_sugar$Sleep_bout),])
summary(mod_sleepbout_SugarTreatmentIndiv)
plot(mod_sleepbout_SugarTreatmentIndiv)

mod_sleepbout_SugarIndiv <- lmer(Sleep_bout~Sugar+(1|ID), data=bound_prepost_sugar)
summary(mod_sleepbout_SugarIndiv)
plot(mod_sleepbout_SugarIndiv)

mod_sleepbout_Sugar <- lm(Sleep_bout~Sugar, data=bound_prepost_sugar)
summary(mod_sleepbout_Sugar)

anova(mod_sleepbout_SugarIndiv, mod_sleepbout_SugarTreatmentIndiv)
anova(mod_sleepbout_Sugar)

mod_daily_bout_sugar <- lmer(value~Treatment+Sex+ShortLong+Sugar+(1|Chamber),data=m.piezobout_combined[m.piezobout_combined$DayNightTot=="Total",])
anova(mod_daily_bout_sugar)

ggplot(bound_prepost_sugar, aes(Sleep_bout)) + geom_histogram() + facet_grid(.~Treatment)
ggplot(bound_prepost_sugar, aes(Sugar, Sleep_bout)) + geom_boxplot() + facet_grid(.~Treatment) + my_theme

#### Piezo bout and/or day ####
## Function to pull the experiment day out of the "variable" column and make it a new one
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
m.piezoday_4wksugars$ExptDay <- substrRight(as.character(m.piezoday_4wksugars$variable), 1)
m.piezobout_4wksugars$ExptDay <- substrRight(as.character(m.piezobout_4wksugars$variable), 1)
m.piezoday_long$ExptDay <- substrRight(as.character(m.piezoday_long$variable), 1)
m.piezobout_long$ExptDay <- substrRight(as.character(m.piezobout_long$variable), 1)
m.piezoday_short2$ExptDay <- substrRight(as.character(m.piezoday_short2$variable), 1)

## To extract what the measure is
substrLeft <- function(x, n){
  substr(x,1,nchar(x)-1)
}
m.piezoday_4wksugars$Measure <- substrLeft(as.character(m.piezoday_4wksugars$variable), 1)
m.piezobout_4wksugars$Measure <- substrLeft(as.character(m.piezobout_4wksugars$variable), 1)
m.piezoday_long$Measure <- substrLeft(as.character(m.piezoday_long$variable), 1)
m.piezobout_long$Measure <- substrLeft(as.character(m.piezobout_long$variable), 1)
m.piezoday_short2$Measure <- substrLeft(as.character(m.piezoday_short2$variable), 1)


## Day or night column
substrMid <- function(x, n){
  substr(x, nchar(x)-2, nchar(x))
}
m.piezoday_4wksugars$DayNightTot <- substrMid(as.character(m.piezoday_4wksugars$Measure))
m.piezoday_4wksugars$DayNightTot<- revalue(as.factor(m.piezoday_4wksugars$DayNightTot),c("GHT"="Night", "TOT" = "Total", "DAY" = "Day"))

m.piezobout_4wksugars$DayNightTot <- substrMid(as.character(m.piezobout_4wksugars$Measure))
m.piezobout_4wksugars$DayNightTot<- revalue(as.factor(m.piezobout_4wksugars$DayNightTot),c("ITE"="Night", "TOT" = "Total", "DAY" = "Day"))

m.piezoday_long$DayNightTot <- substrMid(as.character(m.piezoday_long$Measure))
m.piezoday_long$DayNightTot<- revalue(as.factor(m.piezoday_long$DayNightTot),c("GHT"="Night", "TOT" = "Total", "DAY" = "Day"))

m.piezobout_long$DayNightTot <- substrMid(as.character(m.piezobout_long$Measure))
m.piezobout_long$DayNightTot<- revalue(as.factor(m.piezobout_long$DayNightTot),c("ITE"="Night", "TOT" = "Total", "DAY" = "Day"))

m.piezoday_short2$DayNightTot <- substrMid(as.character(m.piezoday_short2$Measure))
m.piezoday_short2$DayNightTot<- revalue(as.factor(m.piezoday_short2$DayNightTot),c("GHT"="Night", "TOT" = "Total", "DAY" = "Day"))



## Add chamber numbers to data frame to add treatment levels
m.piezoday_4wksugars <- merge(m.piezoday_4wksugars, anim_ID, by=c("GrassRat_ID"))
m.piezoday_4wksugars <- merge(m.piezoday_4wksugars, anim_sex, by=c("GrassRat_ID"))

m.piezobout_4wksugars <- merge(m.piezobout_4wksugars, anim_ID, by=c("GrassRat_ID"))
m.piezobout_4wksugars <- merge(m.piezobout_4wksugars, anim_sex, by=c("GrassRat_ID"))

m.piezoday_long <- merge(m.piezoday_long, anim_ID, by=c("GrassRat_ID"))
m.piezoday_long <- merge(m.piezoday_long, anim_sex, by=c("GrassRat_ID"))

m.piezobout_long <- merge(m.piezobout_long, anim_ID, by=c("GrassRat_ID"))
m.piezobout_long <- merge(m.piezobout_long, anim_sex, by=c("GrassRat_ID"))


## Add column for Room/Treatment
m.piezoday_4wksugars$ShortLong <- 0
m.piezoday_4wksugars$ShortLong[m.piezoday_4wksugars$Chamber<13] <- "Short"
m.piezoday_4wksugars$ShortLong[m.piezoday_4wksugars$Chamber>12] <- "Neutral"

m.piezobout_4wksugars$ShortLong <- 0
m.piezobout_4wksugars$ShortLong[m.piezobout_4wksugars$Chamber<13] <- "Short"
m.piezobout_4wksugars$ShortLong[m.piezobout_4wksugars$Chamber>12] <- "Neutral"

m.piezoday_long$ShortLong <- 0
m.piezoday_long$ShortLong[m.piezoday_long$Chamber<13] <- "Short"
m.piezoday_long$ShortLong[m.piezoday_long$Chamber>12] <- "Neutral"

m.piezobout_long$ShortLong <- 0
m.piezobout_long$ShortLong[m.piezobout_long$Chamber<13] <- "Short"
m.piezobout_long$ShortLong[m.piezobout_long$Chamber>12] <- "Neutral"

m.piezobout_combined <- rbind(m.piezobout_4wksugars, m.piezobout_long)
m.piezobout_combined$Treatment <- as.factor(as.character(m.piezobout_combined$Treatment))
m.piezobout_combined$Treatment <- factor(m.piezobout_combined$Treatment, levels = c("2wk", "4wk", "4wk2", "4wk3", "LowSugar", "HighSugar"))


m.piezobout_combined$Sugar <- 0
m.piezobout_combined$Sugar[m.piezobout_combined$Treatment %in% c("2wk", "4wk", "4wk2", "4wk3")] <- "None"
m.piezobout_combined$Sugar[m.piezobout_combined$Treatment %in% c("LowSugar")] <- "2%"
m.piezobout_combined_conc<- merge(m.piezobout_combined, sugar, by = "GrassRat_ID")
m.piezobout_combined_conc$Sugar[m.piezobout_combined_conc$Treatment=="HighSugar" & 
                                  m.piezobout_combined_conc$Sugar==0] <- m.piezobout_combined_conc$Sugar_conc[m.piezobout_combined_conc$Treatment=="HighSugar" & 
                                                                                                                m.piezobout_combined_conc$Sugar==0]
m.piezobout_combined_conc$Sugar <- as.factor(m.piezobout_combined_conc$Sugar)

mod_sleepbout_full <- lmer(value~Sex+ShortLong+Sugar+DayNightTot+(1|GrassRat_ID),
                          data=m.piezobout_combined_conc[m.piezobout_combined_conc$DayNightTot!="Total",])
mod_sleepbout_noSex <- lmer(value~ShortLong+Sugar+DayNightTot+(1|GrassRat_ID),
                               data=m.piezobout_combined_conc[m.piezobout_combined_conc$DayNightTot!="Total",])
mod_sleepbout_noSex_noPhotoperiod <- lmer(value~Sugar+DayNightTot+(1|GrassRat_ID),
                               data=m.piezobout_combined_conc[m.piezobout_combined_conc$DayNightTot!="Total",])
mod_sleepbout_noSex_noDaynight_noPhotoperiod <- lmer(value~Sugar+(1|GrassRat_ID),
                                          data=m.piezobout_combined_conc[m.piezobout_combined_conc$DayNightTot!="Total",])
mod_sleepbout_onlySugar <- lm(value~Sugar, data=m.piezobout_combined_conc[m.piezobout_combined_conc$DayNightTot!="Total",])

mod_sleepbout_SugarDayNight_interac <- lmer(value~Sugar*DayNightTot+(1|GrassRat_ID), ## BEST MODEL
                                          data=m.piezobout_combined_conc[m.piezobout_combined_conc$DayNightTot!="Total",])

mod_sleepbout_SugarTreatment_DayNight_interac <- lmer(value~Sugar*ShortLong + DayNightTot +(1|GrassRat_ID), ## BEST MODEL
                                             data=m.piezobout_combined_conc[m.piezobout_combined_conc$DayNightTot!="Total",])


anova(mod_sleepbout_full, mod_sleepbout_noSex, mod_sleepbout_noSex_noPhotoperiod, 
      mod_sleepbout_noSex_noDaynight_noPhotoperiod, mod_sleepbout_onlySugar, mod_sleepbout_SugarDayNight_interac,
      mod_sleepbout_SugarTreatment_DayNight_interac)

summary(mod_sleepbout_SugarDayNight_interac)
anova(mod_sleepbout_SugarDayNight_interac)
qqnorm(resid(mod_sleepbout_SugarDayNight_interac))
qqline(resid(mod_sleepbout_SugarDayNight_interac))
coef(mod_sleepbout_SugarDayNight_interac)

tmp <- as.data.frame(confint(glht(mod_sleepbout_SugarDayNight_interac))$confint)
tmp$Comparison <- rownames(tmp)
tmp$Comparison <- revalue(as.factor(tmp$Comparison),c("(Intercept)" = "Intercept", "DayNightTotNight" = "Night",
                                                      "Sugar0.08" = "8% sucrose", "Sugar0.08:DayNightTotNight" = "Night 8%",
                                                      "Sugar2%" = "2% sucrose", "Sugar2%:DayNightTotNight" = "Night 2%",
                                                      "SugarNone" = "No sucrose", "SugarNone:DayNightTotNight" = "Night No sucrose"))

ggplot(tmp, aes(x = Comparison, y = Estimate, ymin = lwr, ymax = upr)) + my_theme +
  #theme(axis.text.x = element_text(angle=30, vjust=0.7)) +
  geom_errorbar() + geom_point()

mod_Photoperiod <- lmer(value~ShortLong+DayNightTot + (1|GrassRat_ID),
                        data=m.piezobout_4wksugars[m.piezobout_4wksugars$DayNightTot!="Total" & 
                                                     m.piezobout_4wksugars$Treatment %in% c("4wk", "4wk2", "4wk3"),])

mod_Photoperiod_perc <- lmer(value~ShortLong+DayNightTot + (1|GrassRat_ID),
                        data=m.piezobout_4wksugars[m.piezobout_4wksugars$DayNightTot!="Total" & 
                                                     m.piezobout_4wksugars$Treatment %in% c("4wk", "4wk2", "4wk3"),])


## Make some test sleep percentages per animal
sleep_wake_4wk <- ggplot(m.piezoday_short2[m.piezoday_short2$DayNightTot!="Total",], aes(DayNightTot, value)) + 
  facet_grid(.~GrassRat_ID) + my_theme +  geom_boxplot(aes(fill=DayNightTot)) +
  scale_fill_manual(values = c("grey70", "black")) +
  theme(axis.text.x = element_text(angle=90)) +
  ylab("Percent Sleep")
  
sleep_wake_4wk

## Sleep bouts per animal
sleep_bout_indiv_4wk <- ggplot(m.piezobout_short[m.piezobout_short$DayNightTot!="Total",], aes(DayNightTot, value)) + 
  facet_grid(.~GrassRat_ID) + my_theme +  geom_boxplot(aes(fill=DayNightTot)) +
  scale_fill_manual(values = c("grey70", "black")) +
  theme(axis.text.x = element_text(angle=90)) +
  ylab("Sleep bout length (s)")

sleep_bout_indiv_4wk

## Sleep-wake boxplots overall
sleep_wake_4wk_all <- ggplot(m.piezoday_short[m.piezoday_short$DayNightTot!="Total",], aes(DayNightTot, value)) + 
  facet_grid(.~ShortLong) + my_theme +  geom_boxplot(aes(fill=DayNightTot)) +
  scale_fill_manual(values = c("grey70", "black")) + geom_point(aes(col=GrassRat_ID)) +
  theme(axis.text.x = element_text(angle=90)) + ylim(0,100) +
  ylab("Percent Sleep") + ggtitle("4 Week Test")

sleep_wake_4wk_all

## USE THIS - percent sleep in each phase of the expt, day vs night, long vs short
sleep_wake_4wk_all <- ggplot(m.piezoday_4wksugars[m.piezoday_4wksugars$DayNightTot!="Total",], aes(DayNightTot, value)) + 
  facet_grid(ShortLong~Treatment) + my_theme +  geom_boxplot(aes(fill=DayNightTot)) +
  scale_fill_manual(values = c("grey90", "black")) + geom_point(col="grey70") +
  theme(axis.text.x = element_text(angle=90)) + ylim(0,100) +
  ylab("Percent Sleep") + ggtitle("4 Week Test")

sleep_wake_4wk_all

## Bout
sleep_bout_expt_4wk <- ggplot(m.piezobout_4wksugars[m.piezobout_4wksugars$DayNightTot!="Total",], aes(DayNightTot, value)) + 
  facet_grid(ShortLong~Treatment) + my_theme +  geom_boxplot(aes(fill=DayNightTot)) +
  scale_fill_manual(values = c("grey70", "black")) + geom_point(aes(col=GrassRat_ID), size=2) +
  theme(axis.text.x = element_text(angle=90), legend.position = "none")  + ggtitle("4 week") + ylim(0,1100) +
  ylab("Mean sleep bout length (seconds)") + xlab("Day/Night")

sleep_bout_expt_4wk

sleep_bout_expt_2wk <- ggplot(m.piezobout_long[m.piezobout_long$DayNightTot!="Total",], aes(DayNightTot, value)) + 
  facet_grid(ShortLong~.) + my_theme +  geom_boxplot(aes(fill=DayNightTot)) +
  scale_fill_manual(values = c("grey70", "black")) + geom_point(aes(col=GrassRat_ID), size=2) +
  theme(axis.text.x = element_text(angle=90), legend.position = "none") + ggtitle("2 week") + ylim(0,1100) + 
  ylab("Mean sleep bout length (seconds)") + xlab("Day/Night")

sleep_bout_expt_2wk

m.piezobout_combined <- rbind(m.piezobout_4wksugars, m.piezobout_long)
m.piezobout_combined$Treatment <- as.factor(as.character(m.piezobout_combined$Treatment))
m.piezobout_combined$Treatment <- factor(m.piezobout_combined$Treatment, levels = c("2wk", "4wk", "4wk2", "4wk3", "LowSugar", "HighSugar"))
sleep_bout_expt_combined <- ggplot(m.piezobout_combined[m.piezobout_combined$DayNightTot!="Total",], aes(DayNightTot, value)) + 
  facet_grid(ShortLong~Treatment) + my_theme +  geom_boxplot(aes(fill=DayNightTot)) +
  scale_fill_manual(values = c("grey70", "black")) + geom_point(aes(col=GrassRat_ID), size=2) +
  theme(axis.text.x = element_text(angle=90), legend.position = "none") + ggtitle("All week") + ylim(0,1550) + 
  ylab("Mean sleep bout length (seconds)") + xlab("Day/Night")

sleep_bout_expt_combined

grid.arrange(sleep_bout_expt_2wk, sleep_bout_expt_4wk, ncol=2)

## Bout
sleep_bout_expt_4wk <- ggplot(m.piezobout_short[m.piezobout_short$DayNightTot=="Total",], aes(ShortLong, value)) + 
  my_theme +  geom_boxplot(aes(fill=DayNightTot)) +
  scale_fill_manual(values = c("grey70", "black")) + geom_point(aes(col=GrassRat_ID), size=2) +
  theme(axis.text.x = element_text(angle=90), legend.position = "none")  + ggtitle("4 week") + ylim(0,1100) +
  ylab("Mean sleep bout length (seconds)") + xlab("Day/Night")

sleep_bout_expt_4wk

sleep_bout_expt_2wk <- ggplot(m.piezobout_long[m.piezobout_long$DayNightTot=="Total",], aes(ShortLong, value)) + 
  my_theme +  geom_boxplot(aes(fill=DayNightTot)) +
  scale_fill_manual(values = c("grey70", "black")) + geom_point(aes(col=GrassRat_ID), size=2) +
  theme(axis.text.x = element_text(angle=90), legend.position = "none") + ggtitle("2 week") + ylim(0,1100) + 
  ylab("Mean sleep bout length (seconds)") + xlab("Day/Night")

sleep_bout_expt_2wk

grid.arrange(sleep_bout_expt_2wk, sleep_bout_expt_4wk, ncol=2)

## Now same graphs for 2-week acclimation period
## Make some test sleep-wake histograms per animal
sleep_wake_2wk <- ggplot(m.piezoday_long[m.piezoday_long$DayNightTot!="Total",], aes(DayNightTot, value)) + 
  facet_grid(.~GrassRat_ID) + my_theme +  geom_boxplot(aes(fill=DayNightTot)) +
  scale_fill_manual(values = c("grey70", "black")) +
  theme(axis.text.x = element_text(angle=90)) +
  ylab("Percent Sleep")

sleep_wake_2wk

## Sleep-wake boxplots overall
sleep_wake_2wk_all <- ggplot(m.piezoday_long[m.piezoday_long$DayNightTot!="Total",], aes(DayNightTot, value)) + 
  facet_grid(.~ShortLong) + my_theme +  geom_boxplot(aes(fill=DayNightTot)) +
  scale_fill_manual(values = c("grey70", "black")) + 
  geom_point(aes(col=GrassRat_ID)) +
  theme(axis.text.x = element_text(angle=90), legend.position = "none") + ylim(0,100) +
  ylab("Percent Sleep") + ggtitle("2 Week Acclimation")

sleep_wake_2wk_all

sleep_wake_2wk_tot <- ggplot(m.piezoday_long[m.piezoday_long$DayNightTot=="Total",], aes(ShortLong, value)) + 
  my_theme +  geom_boxplot(aes(fill=ShortLong)) +
  #scale_fill_manual(values = c("grey70", "black")) + 
  geom_point(aes(col=GrassRat_ID)) +
  theme(axis.text.x = element_text(angle=90), legend.position = "none") + ylim(0,100) +
  ylab("Percent Sleep") + ggtitle("2 Week Acclimation")

sleep_wake_2wk_tot

sleep_wake_4wk_tot <- ggplot(m.piezoday_short[m.piezoday_short$DayNightTot=="Total",], aes(ShortLong, value)) + 
  my_theme +  geom_boxplot(aes(fill=ShortLong)) +
  #scale_fill_manual(values = c("grey70", "black")) + 
  geom_point(aes(col=GrassRat_ID)) +
  theme(axis.text.x = element_text(angle=90), legend.position = "none") + ylim(0,100) +
  ylab("Percent Sleep") + ggtitle("4 Week")

sleep_wake_4wk_tot

sleep_wake_4wk_all <- ggplot(m.piezoday_short[m.piezoday_short$DayNightTot!="Total",], aes(DayNightTot, value)) + 
  facet_grid(.~ShortLong) + my_theme +  geom_boxplot(aes(fill=DayNightTot)) +
  scale_fill_manual(values = c("grey70", "black")) + 
  geom_point(aes(col=GrassRat_ID)) +
  theme(axis.text.x = element_text(angle=90), legend.position = "none") + ylim(0,100) +
  ylab("Percent Sleep") + ggtitle("4 Week")

sleep_wake_4wk_all
  
grid.arrange(sleep_wake_2wk_all, sleep_wake_4wk_all, ncol=2)
