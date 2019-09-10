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
anim_ID <- read.csv("Chamber_Animal_IDs.csv") ## ADD A phase column here for phase 1 and phase 2, and add all animals into same file
anim_sex <- read.csv("Animal_sex.csv") ## ADD animals for phase 2 in here
piezoday_4wk <- read.csv("4WeekPhotoperiod_15Apr2019_dailySS_combinedTime.csv")
piezoday_4wk_2 <- read.csv("4WeekPhotoperiod_24Apr2019_dailySS_combinedTime.csv")
piezoday_4wk_3 <- read.csv("4WeekPhotoperiod_08Apr2019_dailySS_combinedTime.csv")
piezoday_2wk <- read.csv("2WeekAcclimation_26Mar2019_longTime.csv")
piezoday_LowSugar <- read.csv("LowSucrose_07May2019_dailySS_2PercentSucrose_combinedTime.csv")
piezoday_HighSugar <- read.csv("HighSucrose_29May2019_dailySS_combinedTime.csv")
piezoday_presugar_bouts <- read.csv("4WeekPhotoperiod_15Apr2019SleepBout_MeanSB.csv")
piezoday_postsugar_bouts <- read.csv("HighSucrose_29May2019SleepBout_MeanSB.csv")

## ADD in sugar concs for Phase 2 animals (use "Animal_data\\SugarConcTest_weights.xlsx")
sugar <- read.csv("Sugar_conc.csv")


## General functions
my_theme <- theme_classic(base_size = 25) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))

## Melt to form long dataframes
## "1" in an object name meansit's a phase 1 data frame. When processing pase 2, add "2". 
## For combined files including phase 1 and phase 2, there should generally be no ending number
m.piezoday_short1 <- melt(piezoday_4wk, id.vars= c("MOUSE_ID", "TESTDATE"), measure.vars=c("PERCENTSLEEPTOT1", "PERCENTSLEEPNIGHT1",
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

m.piezobout_short1 <- melt(piezoday_4wk, id.vars= c("MOUSE_ID", "TESTDATE"), measure.vars=c("BOUTLENGTHTOT1", "BOUTLENGTHNITE1",
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

m.piezoday_long1 <- melt(piezoday_2wk, id.vars= c("MOUSE_ID", "TESTDATE"), measure.vars=c("PERCENTSLEEPTOT1", "PERCENTSLEEPNIGHT1",
                                                                                            "PERCENTSLEEPDAY1", "PERCENTSLEEPTOT2", 
                                                                                            "PERCENTSLEEPNIGHT2", "PERCENTSLEEPDAY2",
                                                                                            "PERCENTSLEEPTOT3", "PERCENTSLEEPNIGHT3",
                                                                                            "PERCENTSLEEPDAY3", "PERCENTSLEEPTOT4", 
                                                                                            "PERCENTSLEEPNIGHT4", "PERCENTSLEEPDAY4",
                                                                                            "PERCENTSLEEPTOT5", "PERCENTSLEEPNIGHT5",
                                                                                            "PERCENTSLEEPDAY5"))

m.piezobout_long1 <- melt(piezoday_2wk, id.vars= c("MOUSE_ID", "TESTDATE"), measure.vars=c("BOUTLENGTHTOT1", "BOUTLENGTHNITE1",
                                                                                           "BOUTLENGTHDAY1", "BOUTLENGTHTOT2", 
                                                                                           "BOUTLENGTHNITE2", "BOUTLENGTHDAY2",
                                                                                           "BOUTLENGTHTOT3", "BOUTLENGTHNITE3",
                                                                                           "BOUTLENGTHDAY3", "BOUTLENGTHTOT4", 
                                                                                           "BOUTLENGTHNITE4", "BOUTLENGTHDAY4",
                                                                                           "BOUTLENGTHTOT5", "BOUTLENGTHNITE5",
                                                                                           "BOUTLENGTHDAY5"))

m.piezoday_LowSugar1 <- melt(piezoday_LowSugar, id.vars= c("MOUSE_ID", "TESTDATE"), measure.vars=c("PERCENTSLEEPTOT1", "PERCENTSLEEPNIGHT1",
                                                                                         "PERCENTSLEEPDAY1", "PERCENTSLEEPTOT2", 
                                                                                         "PERCENTSLEEPNIGHT2", "PERCENTSLEEPDAY2"))

m.piezobout_LowSugar1 <- melt(piezoday_LowSugar, id.vars= c("MOUSE_ID", "TESTDATE"), measure.vars=c("BOUTLENGTHTOT1", "BOUTLENGTHNITE1",
                                                                                          "BOUTLENGTHDAY1", "BOUTLENGTHTOT2", 
                                                                                          "BOUTLENGTHNITE2", "BOUTLENGTHDAY2"))

m.piezoday_HighSugar1 <- melt(piezoday_HighSugar, id.vars= c("MOUSE_ID", "TESTDATE"), measure.vars=c("PERCENTSLEEPTOT1", "PERCENTSLEEPNIGHT1",
                                                                                         "PERCENTSLEEPDAY1", "PERCENTSLEEPTOT2", 
                                                                                         "PERCENTSLEEPNIGHT2", "PERCENTSLEEPDAY2",
                                                                                         "PERCENTSLEEPTOT3", "PERCENTSLEEPNIGHT3",
                                                                                         "PERCENTSLEEPDAY3", "PERCENTSLEEPTOT4", 
                                                                                         "PERCENTSLEEPNIGHT4", "PERCENTSLEEPDAY4",
                                                                                         "PERCENTSLEEPTOT5", "PERCENTSLEEPNIGHT5",
                                                                                         "PERCENTSLEEPDAY5"))

m.piezobout_HighSugar1 <- melt(piezoday_HighSugar, id.vars= c("MOUSE_ID", "TESTDATE"), measure.vars=c("BOUTLENGTHTOT1", "BOUTLENGTHNITE1",
                                                                                          "BOUTLENGTHDAY1", "BOUTLENGTHTOT2", 
                                                                                          "BOUTLENGTHNITE2", "BOUTLENGTHDAY2",
                                                                                          "BOUTLENGTHTOT3", "BOUTLENGTHNITE3",
                                                                                          "BOUTLENGTHDAY3", "BOUTLENGTHTOT4", 
                                                                                          "BOUTLENGTHNITE4", "BOUTLENGTHDAY4",
                                                                                          "BOUTLENGTHTOT5", "BOUTLENGTHNITE5",
                                                                                          "BOUTLENGTHDAY5"))


m.piezoday_long1$Treatment <- "2wk"
m.piezoday_short1$Treatment <- "4wk"
m.piezoday_LowSugar1$Treatment <- "LowSugar"
m.piezoday_HighSugar1$Treatment <- "HighSugar"

m.piezobout_long1$Treatment <- "2wk"
m.piezobout_short1$Treatment <- "4wk"
m.piezobout_LowSugar1$Treatment <- "LowSugar"
m.piezobout_HighSugar1$Treatment <- "HighSugar"

m.piezoday_short1 <- rename(m.piezoday_short1, replace = c("MOUSE_ID"="GrassRat_ID"))
m.piezoday_long1 <- rename(m.piezoday_long1, replace = c("MOUSE_ID"="GrassRat_ID"))
m.piezoday_LowSugar1 <- rename(m.piezoday_LowSugar1, replace = c("MOUSE_ID"="GrassRat_ID"))
m.piezoday_HighSugar1 <- rename(m.piezoday_HighSugar1, replace = c("MOUSE_ID"="GrassRat_ID"))

m.piezobout_short1 <- rename(m.piezobout_short1, replace = c("MOUSE_ID"="GrassRat_ID"))
m.piezobout_long1 <- rename(m.piezobout_long1, replace = c("MOUSE_ID"="GrassRat_ID"))
m.piezobout_LowSugar1 <- rename(m.piezobout_LowSugar1, replace = c("MOUSE_ID"="GrassRat_ID"))
m.piezobout_HighSugar1 <- rename(m.piezobout_HighSugar1, replace = c("MOUSE_ID"="GrassRat_ID"))

m.piezobout_4wksugars1 <- rbind(m.piezobout_short1, m.piezobout_LowSugar1, m.piezobout_HighSugar1)
m.piezoday_4wksugars1 <- rbind(m.piezoday_short1, m.piezoday_LowSugar1, m.piezoday_HighSugar1)


## After this rbinding, you'll have  combined files for all Phase 1 and phase 2 animals
# rbind() each pair of phase 1 and Phase 2 files
## e.g. m.piezobout_short1 <- rbind(m.piezobout_short1,m.piezobout_short2)
## After binding phase files, you should have one _short, one _long, one _4wksugars

## Add chamber numbers to data frame to add treatment levels
m.piezoday_4wksugars <- merge(m.piezoday_4wksugars1, anim_ID, by=c("GrassRat_ID"))
m.piezoday_4wksugars <- merge(m.piezoday_4wksugars, anim_sex, by=c("GrassRat_ID"))

m.piezobout_4wksugars <- merge(m.piezobout_4wksugars, anim_ID, by=c("GrassRat_ID"))
m.piezobout_4wksugars <- merge(m.piezobout_4wksugars, anim_sex, by=c("GrassRat_ID"))

m.piezoday_long <- merge(m.piezoday_long, anim_ID, by=c("GrassRat_ID"))
m.piezoday_long <- merge(m.piezoday_long, anim_sex, by=c("GrassRat_ID"))

m.piezobout_long <- merge(m.piezobout_long, anim_ID, by=c("GrassRat_ID"))
m.piezobout_long <- merge(m.piezobout_long, anim_sex, by=c("GrassRat_ID"))

### CHANGE CHAMBER NUMBER CUT-OFFS FOR PHASE 2 animals (<12 = Short; >11 = long), 
##  e.g. first 5 lines, use this example to change all the other data frames below that as well
## Add column for Room/Treatment
#m.piezoday_4wksugars$Phase <- 1
m.piezoday_4wksugars$ShortLong <- 0
m.piezoday_4wksugars$ShortLong[m.piezoday_4wksugars$Chamber<13 & m.piezoday_4wksugars$Phase==1] <- "Short"
m.piezoday_4wksugars$ShortLong[m.piezoday_4wksugars$Chamber>12  & m.piezoday_4wksugars$Phase==1] <- "Neutral"
m.piezoday_4wksugars$ShortLong[m.piezoday_4wksugars$Chamber<12 & m.piezoday_4wksugars$Phase==2] <- "Short"
m.piezoday_4wksugars$ShortLong[m.piezoday_4wksugars$Chamber>11  & m.piezoday_4wksugars$Phase==2] <- "Neutral"

m.piezobout_4wksugars$ShortLong <- 0
m.piezobout_4wksugars$ShortLong[m.piezobout_4wksugars$Chamber<13] <- "Short"
m.piezobout_4wksugars$ShortLong[m.piezobout_4wksugars$Chamber>12] <- "Neutral"

m.piezoday_long$ShortLong <- 0
m.piezoday_long$ShortLong[m.piezoday_long$Chamber<13] <- "Short"
m.piezoday_long$ShortLong[m.piezoday_long$Chamber>12] <- "Neutral"

m.piezobout_long$ShortLong <- 0
m.piezobout_long$ShortLong[m.piezobout_long$Chamber<13] <- "Short"
m.piezobout_long$ShortLong[m.piezobout_long$Chamber>12] <- "Neutral"

m.piezoday_short$ShortLong <- 0
m.piezoday_short$ShortLong[m.piezoday_short$Chamber<13] <- "Short"
m.piezoday_short$ShortLong[m.piezoday_short$Chamber>12] <- "Neutral"

m.piezobout_short$ShortLong <- 0
m.piezobout_short$ShortLong[m.piezobout_short$Chamber<13] <- "Short"
m.piezobout_short$ShortLong[m.piezobout_short$Chamber>12] <- "Neutral"


#### Sugar files ####

### Processing sleep bout length changes before and after sugar
m.presugar1 <- melt(piezoday_presugar_bouts, id.vars="Mouse.IDs.", 
                   measure.vars=c("G9", "G5", "G14", "G16", "G18", "G10", "G8", "G2", "T10", "T5", "T17", "T12", "G4", "G1",
                                  "G13", "G17", "G19", "G11", "G12",  "G3", "T9", "T11", "T19", "T13"))
m.postsugar1 <- melt(piezoday_postsugar_bouts, id.vars="Mouse.IDs.", 
                    measure.vars=c("G9", "G5", "G14", "G16", "G18", "G10", "G8", "G2", "T10", "T5", "T17", "T12", "G4", "G1",
                                   "G13", "G17", "G19", "G11", "G12",  "G3", "T9", "T11", "T19", "T13"))


names(m.presugar1) <- c("DateTime", "ID", "Sleep_bout")
names(m.postsugar1) <- c("DateTime", "ID", "Sleep_bout")
#m.presugar$datetime.posix <- as.POSIXct(m.presugar$DateTime, format="%m/%d/%Y %H:%M", tz="GMT")
#m.presugar$date <- as.POSIXct(strptime(m.presugar$DateTime, format="%m/%d/%Y", tz="GMT"))

#m.presugar$time <- strftime(m.presugar$datetime.posix, format="%H:%M", tz = "GMT")
#m.presugar$time <- as.POSIXct(m.presugar$time, format="%H:%M", tz = "GMT")

m.presugar1$Treatment <- 0
m.presugar1$Treatment[m.presugar1$ID %in% c("G9", "G5", "G14", "G16", "G18", "G10", 
                                        "G8", "G2", "T10", "T5", "T17", "T12")] <- "Short"
m.presugar1$Treatment[m.presugar1$ID %in% c("G4", "G1", "G13", "G17", "G19", "G11", "G12", "G3", "T9", 
                                        "T11", "T19", "T13")] <- "Neutral"

## ADD A PHASE COLUMN for m.presugar1 and m.presugar2
m.presugar1$Phase <- 1

m.postsugar1$Treatment <- 0
m.postsugar1$Treatment[m.postsugar1$ID %in% c("G9", "G5", "G14", "G16", "G18", "G10", 
                                          "G8", "G2", "T10", "T5", "T17", "T12")] <- "Short"
m.postsugar1$Treatment[m.postsugar1$ID %in% c("G4", "G1", "G13", "G17", "G19", "G11", "G12", "G3", "T9", 
                                          "T11", "T19", "T13")] <- "Neutral"
## ADD A PHASE COLUMN for m.presugar1 and m.presugar2
m.postsugar1$Phase <- 1

m.presugar1$Sugar <- "Pre"
m.postsugar1$Sugar <- "Post"

## BIND ALL Phase 1 and phase 2, for a total of 4 data frames (one pre- and one post-sugar for each phase)
bound_prepost_sugar <- rbind(m.presugar1, m.postsugar1)

## Sleep bout plot
ggplot(m.presugar[!is.na(m.presugar$Sleep_bout),], aes(ID, Sleep_bout)) + geom_boxplot() + facet_grid(.~Treatment, scales="free_x") + 
  my_theme + ylab("Sleep bout (s)") + ggtitle("Presugar")


ggplot(m.postsugar, aes(ID, Sleep_bout)) + geom_boxplot() + facet_grid(.~Treatment, scales="free_x") + my_theme +
  ylab("Sleep bout (s)")

ggplot(bound_prepost_sugar, aes(ID, Sleep_bout)) + geom_boxplot() + 
  facet_grid(Sugar~Treatment, scales="free_x") + 
  my_theme + ylab("Sleep bout (s)")

#mod_sleepbout_SugarTreatmentIndiv <- lmer(Sleep_bout~Treatment+Sugar+(1|ID), data=bound_prepost_sugar)
#summary(mod_sleepbout_SugarTreatmentIndiv)
#plot(mod_sleepbout_SugarTreatmentIndiv)

#mod_sleepbout_SugarIndiv <- lmer(Sleep_bout~Sugar+(1|ID), data=bound_prepost_sugar)
#summary(mod_sleepbout_SugarIndiv)
#plot(mod_sleepbout_SugarIndiv)

#mod_sleepbout_Sugar <- lm(Sleep_bout~Sugar, data=bound_prepost_sugar)
#summary(mod_sleepbout_Sugar)

#anova(mod_sleepbout_SugarIndiv, mod_sleepbout_SugarTreatmentIndiv)
#anova(mod_sleepbout_Sugar)

#mod_daily_bout_sugar <- lmer(value~Treatment+Sex+ShortLong+Sugar+(1|Chamber),data=m.piezobout_combined[m.piezobout_combined$DayNightTot=="Total",])
#anova(mod_daily_bout_sugar)

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
m.piezoday_short$ExptDay <- substrRight(as.character(m.piezoday_short$variable), 1)
m.piezobout_short$ExptDay <- substrRight(as.character(m.piezoday_short$variable), 1)

## To extract what the measure is
substrLeft <- function(x, n){
  substr(x,1,nchar(x)-1)
}
m.piezoday_4wksugars$Measure <- substrLeft(as.character(m.piezoday_4wksugars$variable), 1)
m.piezobout_4wksugars$Measure <- substrLeft(as.character(m.piezobout_4wksugars$variable), 1)
m.piezoday_long$Measure <- substrLeft(as.character(m.piezoday_long$variable), 1)
m.piezobout_long$Measure <- substrLeft(as.character(m.piezobout_long$variable), 1)
m.piezoday_short$Measure <- substrLeft(as.character(m.piezoday_short$variable), 1)
m.piezobout_short$Measure <- substrLeft(as.character(m.piezobout_short$variable), 1)


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

m.piezoday_short$DayNightTot <- substrMid(as.character(m.piezoday_short$Measure))
m.piezoday_short$DayNightTot<- revalue(as.factor(m.piezoday_short$DayNightTot),c("GHT"="Night", "TOT" = "Total", "DAY" = "Day"))

m.piezobout_short$DayNightTot <- substrMid(as.character(m.piezobout_short$Measure))
m.piezobout_short$DayNightTot<- revalue(as.factor(m.piezobout_short$DayNightTot),c("ITE"="Night", "TOT" = "Total", "DAY" = "Day"))




## Combining the 2wk, 4wk, low sugar and high sugar data frames
m.piezobout_combined <- rbind(m.piezobout_4wksugars, m.piezobout_long) ## Also add names of phase 2 objects
m.piezobout_combined$Treatment <- as.factor(as.character(m.piezobout_combined$Treatment))
m.piezobout_combined$Treatment <- factor(m.piezobout_combined$Treatment, levels = c("2wk", "4wk", "LowSugar", "HighSugar"))


m.piezobout_combined$Sugar <- 0
m.piezobout_combined$Sugar[m.piezobout_combined$Treatment %in% c("2wk", "4wk")] <- "None"
m.piezobout_combined$Sugar[m.piezobout_combined$Treatment %in% c("LowSugar")] <- "2%"
m.piezobout_combined_conc<- merge(m.piezobout_combined, sugar, by = "GrassRat_ID")
m.piezobout_combined_conc$Sugar[m.piezobout_combined_conc$Treatment=="HighSugar" & 
                                  m.piezobout_combined_conc$Sugar==0] <- m.piezobout_combined_conc$Sugar_conc[m.piezobout_combined_conc$Treatment=="HighSugar" & 
                                                                                                                m.piezobout_combined_conc$Sugar==0]
m.piezobout_combined_conc$Sugar <- as.factor(m.piezobout_combined_conc$Sugar)

levels(m.piezobout_combined_conc$Sugar)[match("2%",levels(m.piezobout_combined_conc$Sugar))] <- "0.02"
m.piezobout_combined_conc$ShortLong <- as.factor(as.character(m.piezobout_combined_conc$ShortLong))

mod_sleepbout_full <- lmer(value~Sex+ShortLong+Sugar+DayNightTot+(1|GrassRat_ID),
                          data=m.piezobout_combined_conc[m.piezobout_combined_conc$DayNightTot!="Total",])
mod_sleepbout_noSex <- lmer(value~ShortLong+Sugar+DayNightTot+(1|GrassRat_ID),
                               data=m.piezobout_combined_conc[m.piezobout_combined_conc$DayNightTot!="Total",])
mod_sleepbout_noSex_noPhotoperiod <- lmer(value~Sugar+DayNightTot+(1|GrassRat_ID),
                               data=m.piezobout_combined_conc[m.piezobout_combined_conc$DayNightTot!="Total",])
mod_sleepbout_noSex_noDaynight_noPhotoperiod <- lmer(value~Sugar+(1|GrassRat_ID),
                                          data=m.piezobout_combined_conc[m.piezobout_combined_conc$DayNightTot!="Total",])
mod_sleepbout_onlySugar <- lm(value~Sugar, data=m.piezobout_combined_conc[m.piezobout_combined_conc$DayNightTot!="Total",])

## BEST MODEL
mod_sleepbout_SugarDayNight_interac <- lmer(value~Sugar*DayNightTot+(1|GrassRat_ID), 
                                          data=m.piezobout_combined_conc[m.piezobout_combined_conc$DayNightTot!="Total",])

mod_sleepbout_SugarTreatment_DayNight_interac <- lmer(value~Sugar*ShortLong + DayNightTot +(1|GrassRat_ID), ## BEST MODEL
                                             data=m.piezobout_combined_conc[m.piezobout_combined_conc$DayNightTot!="Total",])


anova(mod_sleepbout_full, mod_sleepbout_noSex, mod_sleepbout_noSex_noPhotoperiod, 
      mod_sleepbout_noSex_noDaynight_noPhotoperiod, mod_sleepbout_onlySugar, mod_sleepbout_SugarDayNight_interac,
      mod_sleepbout_SugarTreatment_DayNight_interac)

summary(mod_sleepbout_SugarDayNight_interac)
anova(mod_sleepbout_SugarDayNight_interac)
qqnorm(resid(mod_sleepbout_SugarDayNight_interac)) ## Not a very skewed line
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
                                                     m.piezobout_4wksugars$Treatment %in% c("4wk"),])

mod_Photoperiod_perc <- lmer(value~ShortLong+DayNightTot + (1|GrassRat_ID),
                        data=m.piezobout_4wksugars[m.piezobout_4wksugars$DayNightTot!="Total" & 
                                                     m.piezobout_4wksugars$Treatment %in% c("4wk"),])


## Make some test sleep percentages per animal
sleep_wake_4wk <- ggplot(m.piezoday_short[m.piezoday_short$DayNightTot!="Total",], aes(DayNightTot, value)) + 
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

## Sleep-wake boxplots for all individuals
sleep_wake_4wk_all <- ggplot(m.piezoday_short[m.piezoday_short$DayNightTot!="Total",], aes(DayNightTot, value)) + 
  facet_grid(.~ShortLong) + my_theme +  geom_boxplot(aes(fill=DayNightTot)) +
  scale_fill_manual(values = c("grey70", "black")) + geom_point(aes(col=GrassRat_ID)) +
  theme(axis.text.x = element_text(angle=90)) + ylim(0,100) +
  ylab("Percent Sleep") + ggtitle("4 Week Test")

sleep_wake_4wk_all

m.piezoday_4wksugars$ShortLong <- as.factor(as.character(m.piezoday_4wksugars$ShortLong))
m.piezoday_4wksugars$Treatment <- as.factor(as.character(m.piezoday_4wksugars$Treatment))
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
