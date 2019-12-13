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
library(effects) ## For plotting model effects
#library(dplyr) ## Just to rename a column in bound_prepost_sugar

## Set wd
#setwd("E:/Piezo_data/SleepBout_histogram_/") ## For Shelby
setwd("E:\\Ex_Google_Drive\\Piezo_data\\SleepBout_histogram_files_SubjDay/") ## For Anusha


## Read in files
#anim_ID_sex <- read.csv("Chamber_Animal_IDs_1_2.csv")
#anim_sex <- read.csv("Animal_sex_1_2.csv")
anim_ID_sex <- read.csv("Chamber_Animal_IDs_Sex_bothPhases.csv")
piezoday_4wk1 <- read.csv("4WeekPhotoperiod_24Apr2019_combined_with_sub_day.csv")
piezoday_2wk1 <- read.csv("2WeekAcclimation_26Mar2019_longTime.csv")
piezoday_LowSugar1 <- read.csv("LowSucrose_andTest_07May2019_combined_with_sub_day.csv")
piezoday_HighSugar1 <- read.csv("HighSucrose_29May2019_combined_with_sub_day.csv")
#piezoday_presugar_bouts1 <- read.csv("..\\SleepBout_histogram_files\\4WeekPhotoperiod_24Apr2019SleepBout_MeanSB.csv")
#piezoday_postsugar_bouts1 <- read.csv("..\\SleepBout_histogram_files\\HighSucrose_29May2019SleepBout_MeanSB_processed.csv") ## For mean hourly sleep bouts
postsugar_percbouts1 <- read.csv("..\\SleepBout_histogram_files\\HighSucrose_29May2019_PercentHourlySleep_processed.csv") ## For % hourly sleep
postsugar_percbouts2 <- read.csv("..\\SleepBout_histogram_files\\Phase2_8%sucrose_21Aug2019_PercentHourlySleep_processed.csv") ## For % hourly sleep

bound_prepost_sugar <- read.csv("HourlySleepBouts_PrePostSugar.csv")

piezoday_4wk2 <- read.csv("Phase2_4WeekPhotoperiod_22Jul2019_combined_with_sub_day.csv")
piezoday_2wk2 <- read.csv("Phase2_2WeekAcclimation_19Jun2019_long.csv")
piezoday_LowSugar2 <- read.csv("Phase2_LowSucroseTest_03Aug2019_combined_with_sub_day.csv")
piezoday_HighSugar2 <- read.csv("Phase2_8%sucrose_21Aug2019_combined_with_sub_day.csv") ## Removed G28
#piezoday_presugar_bouts2 <- read.csv("..\\Phase2_SleepBout_histogram_files\\Phase2_4WeekPhotoperiod_22Jul2019SleepBout_MeanSB.csv")
#piezoday_postsugar_bouts2 <- read.csv("..\\Phase2_SleepBout_histogram_files\\Phase2_8%sucrose_21Aug2019SleepBout_MeanSB.csv")

#sugar <- read.csv("Sugar_conc_1_2.csv") #Don't need this, it's in the anim_ID_sex dataframe now


## General functions
my_theme <- theme_classic(base_size = 25) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))

#### Function for stat_pop_etho from rethomics program ####

## Trying out stat_pop_etho style plot
stat_pop_sleep_trial <- function(mapping = NULL, data = NULL,
                                 geom = "smooth", position = "identity",
                                 ...,
                                 method = mean_se,
                                 method.args = list(),
                                 show.legend = NA,
                                 inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatPopEtho_trial,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      method = method,
      method.args = method.args,
      se=TRUE,
      ...
    )
  )
}


StatPopEtho_trial <- ggplot2::ggproto("StatPopEtho", ggplot2::Stat,
                                      compute_group = function(data, scales, method, method.args = list()) {
                                        data <- data.table::as.data.table(data)
                                        foo <- function(y){
                                          all_args <- append(list(y), method.args)
                                          do.call(method, all_args)
                                        }
                                        out <- data[,
                                                    foo(y),
                                                    keyby="x"]
                                        out
                                      },
                                      required_aes = c("x", "y")
)


#### Melt to form long dataframes ####
## "1" in an object name meansit's a phase 1 data frame. When processing pase 2, add "2". 
## For combined files including phase 1 and phase 2, there should generally be no ending number

#phase 1
m.piezoday_short1 <- melt(piezoday_4wk1, id.vars= c("MOUSE_ID", "TESTDATE"), measure.vars=c("PERCENTSLEEPTOT1", "PERCENTSLEEPNIGHT1",
                                                                             "PERCENTSLEEPDAY1", "PERCENTSLEEPTOT2", 
                                                                             "PERCENTSLEEPNIGHT2", "PERCENTSLEEPDAY2",
                                                                             "PERCENTSLEEPTOT3", "PERCENTSLEEPNIGHT3",
                                                                             "PERCENTSLEEPDAY3", "PERCENTSLEEPTOT4", 
                                                                             "PERCENTSLEEPNIGHT4", "PERCENTSLEEPDAY4",
                                                                             "PERCENTSLEEPTOT5", "PERCENTSLEEPNIGHT5",
                                                                             "PERCENTSLEEPDAY5", "PERCENTSLEEPTOT6", 
                                                                             "PERCENTSLEEPNIGHT6", "PERCENTSLEEPDAY6"))

m.piezobout_short1 <- melt(piezoday_4wk1, id.vars= c("MOUSE_ID", "TESTDATE"), measure.vars=c("BOUTLENGTHTOT1", "BOUTLENGTHNITE1",
                                                                                            "BOUTLENGTHDAY1", "BOUTLENGTHTOT2", 
                                                                                            "BOUTLENGTHNITE2", "BOUTLENGTHDAY2",
                                                                                            "BOUTLENGTHTOT3", "BOUTLENGTHNITE3",
                                                                                            "BOUTLENGTHDAY3", "BOUTLENGTHTOT4", 
                                                                                            "BOUTLENGTHNITE4", "BOUTLENGTHDAY4",
                                                                                            "BOUTLENGTHTOT5", "BOUTLENGTHNITE5",
                                                                                            "BOUTLENGTHDAY5", "BOUTLENGTHTOT6", 
                                                                                            "BOUTLENGTHNITE6", "BOUTLENGTHDAY6"))

m.piezoday_long1 <- melt(piezoday_2wk1, id.vars= c("MOUSE_ID", "TESTDATE"), measure.vars=c("PERCENTSLEEPTOT1", "PERCENTSLEEPNIGHT1",
                                                                                            "PERCENTSLEEPDAY1", "PERCENTSLEEPTOT2", 
                                                                                            "PERCENTSLEEPNIGHT2", "PERCENTSLEEPDAY2",
                                                                                            "PERCENTSLEEPTOT3", "PERCENTSLEEPNIGHT3",
                                                                                            "PERCENTSLEEPDAY3", "PERCENTSLEEPTOT4", 
                                                                                            "PERCENTSLEEPNIGHT4", "PERCENTSLEEPDAY4",
                                                                                            "PERCENTSLEEPTOT5", "PERCENTSLEEPNIGHT5",
                                                                                            "PERCENTSLEEPDAY5"))

m.piezobout_long1 <- melt(piezoday_2wk1, id.vars= c("MOUSE_ID", "TESTDATE"), measure.vars=c("BOUTLENGTHTOT1", "BOUTLENGTHNITE1",
                                                                                           "BOUTLENGTHDAY1", "BOUTLENGTHTOT2", 
                                                                                           "BOUTLENGTHNITE2", "BOUTLENGTHDAY2",
                                                                                           "BOUTLENGTHTOT3", "BOUTLENGTHNITE3",
                                                                                           "BOUTLENGTHDAY3", "BOUTLENGTHTOT4", 
                                                                                           "BOUTLENGTHNITE4", "BOUTLENGTHDAY4",
                                                                                           "BOUTLENGTHTOT5", "BOUTLENGTHNITE5",
                                                                                           "BOUTLENGTHDAY5"))

m.piezoday_LowSugar1 <- melt(piezoday_LowSugar1, id.vars= c("MOUSE_ID", "TESTDATE"), measure.vars=c("PERCENTSLEEPTOT1", "PERCENTSLEEPNIGHT1",
                                                                                         "PERCENTSLEEPDAY1", "PERCENTSLEEPTOT2", 
                                                                                         "PERCENTSLEEPNIGHT2", "PERCENTSLEEPDAY2",
                                                                                         "PERCENTSLEEPTOT3", "PERCENTSLEEPNIGHT3",
                                                                                         "PERCENTSLEEPDAY3", "PERCENTSLEEPTOT4", 
                                                                                         "PERCENTSLEEPNIGHT4", "PERCENTSLEEPDAY4",
                                                                                         "PERCENTSLEEPTOT5", "PERCENTSLEEPNIGHT5",
                                                                                         "PERCENTSLEEPDAY5"))

m.piezobout_LowSugar1 <- melt(piezoday_LowSugar1, id.vars= c("MOUSE_ID", "TESTDATE"), measure.vars=c("BOUTLENGTHTOT1", "BOUTLENGTHNITE1",
                                                                                          "BOUTLENGTHDAY1", "BOUTLENGTHTOT2", 
                                                                                          "BOUTLENGTHNITE2", "BOUTLENGTHDAY2",
                                                                                          "BOUTLENGTHTOT3", "BOUTLENGTHNITE3",
                                                                                          "BOUTLENGTHDAY3", "BOUTLENGTHTOT4", 
                                                                                          "BOUTLENGTHNITE4", "BOUTLENGTHDAY4",
                                                                                          "BOUTLENGTHTOT5", "BOUTLENGTHNITE5",
                                                                                          "BOUTLENGTHDAY5"))

m.piezoday_HighSugar1 <- melt(piezoday_HighSugar1, id.vars= c("MOUSE_ID", "TESTDATE"), measure.vars=c("PERCENTSLEEPTOT1", "PERCENTSLEEPNIGHT1",
                                                                                         "PERCENTSLEEPDAY1", "PERCENTSLEEPTOT2", 
                                                                                         "PERCENTSLEEPNIGHT2", "PERCENTSLEEPDAY2",
                                                                                         "PERCENTSLEEPTOT3", "PERCENTSLEEPNIGHT3",
                                                                                         "PERCENTSLEEPDAY3", "PERCENTSLEEPTOT4", 
                                                                                         "PERCENTSLEEPNIGHT4", "PERCENTSLEEPDAY4",
                                                                                         "PERCENTSLEEPTOT5", "PERCENTSLEEPNIGHT5",
                                                                                         "PERCENTSLEEPDAY5"))

m.piezobout_HighSugar1 <- melt(piezoday_HighSugar1, id.vars= c("MOUSE_ID", "TESTDATE"), measure.vars=c("BOUTLENGTHTOT1", "BOUTLENGTHNITE1",
                                                                                          "BOUTLENGTHDAY1", "BOUTLENGTHTOT2", 
                                                                                          "BOUTLENGTHNITE2", "BOUTLENGTHDAY2",
                                                                                          "BOUTLENGTHTOT3", "BOUTLENGTHNITE3",
                                                                                          "BOUTLENGTHDAY3", "BOUTLENGTHTOT4", 
                                                                                          "BOUTLENGTHNITE4", "BOUTLENGTHDAY4",
                                                                                          "BOUTLENGTHTOT5", "BOUTLENGTHNITE5",
                                                                                          "BOUTLENGTHDAY5"))

#phase 2

m.piezoday_short2 <- melt(piezoday_4wk2, id.vars= c("MOUSE_ID", "TESTDATE"), measure.vars=c("PERCENTSLEEPTOT1", "PERCENTSLEEPNIGHT1",
                                                                                            "PERCENTSLEEPDAY1", "PERCENTSLEEPTOT2", 
                                                                                            "PERCENTSLEEPNIGHT2", "PERCENTSLEEPDAY2",
                                                                                            "PERCENTSLEEPTOT3", "PERCENTSLEEPNIGHT3",
                                                                                            "PERCENTSLEEPDAY3", "PERCENTSLEEPTOT4", 
                                                                                            "PERCENTSLEEPNIGHT4", "PERCENTSLEEPDAY4",
                                                                                            "PERCENTSLEEPTOT5", "PERCENTSLEEPNIGHT5",
                                                                                            "PERCENTSLEEPDAY5", "PERCENTSLEEPTOT6", 
                                                                                            "PERCENTSLEEPNIGHT6", "PERCENTSLEEPDAY6"))

m.piezobout_short2 <- melt(piezoday_4wk2, id.vars= c("MOUSE_ID", "TESTDATE"), measure.vars=c("BOUTLENGTHTOT1", "BOUTLENGTHNITE1",
                                                                                             "BOUTLENGTHDAY1", "BOUTLENGTHTOT2", 
                                                                                             "BOUTLENGTHNITE2", "BOUTLENGTHDAY2",
                                                                                             "BOUTLENGTHTOT3", "BOUTLENGTHNITE3",
                                                                                             "BOUTLENGTHDAY3", "BOUTLENGTHTOT4", 
                                                                                             "BOUTLENGTHNITE4", "BOUTLENGTHDAY4",
                                                                                             "BOUTLENGTHTOT5", "BOUTLENGTHNITE5",
                                                                                             "BOUTLENGTHDAY5", "BOUTLENGTHTOT6", 
                                                                                             "BOUTLENGTHNITE6", "BOUTLENGTHDAY6"))

m.piezoday_long2 <- melt(piezoday_2wk2, id.vars= c("MOUSE_ID", "TESTDATE"), measure.vars=c("PERCENTSLEEPTOT1", "PERCENTSLEEPNIGHT1",
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
                                                                                           "PERCENTSLEEPNIGHT8", "PERCENTSLEEPDAY8", 
                                                                                           "PERCENTSLEEPTOT9", "PERCENTSLEEPNIGHT9",
                                                                                           "PERCENTSLEEPDAY9", "PERCENTSLEEPTOT10",
                                                                                           "PERCENTSLEEPNIGHT10", "PERCENTSLEEPDAY10",
                                                                                           "PERCENTSLEEPTOT11", "PERCENTSLEEPNIGHT11", 
                                                                                           "PERCENTSLEEPDAY11"))
m.piezoday_long2$variable <- revalue(m.piezoday_long2$variable, 
                                      c("PERCENTSLEEPTOT10"="PERCENTSLEEPTOTX","PERCENTSLEEPNIGHT10"="PERCENTSLEEPNIGHTX",
                                        "PERCENTSLEEPDAY10"="PERCENTSLEEPDAYX", "PERCENTSLEEPTOT11"="PERCENTSLEEPTOTY",
                                        "PERCENTSLEEPNIGHT11"="PERCENTSLEEPNIGHTY","PERCENTSLEEPDAY11"="PERCENTSLEEPDAYY"))



m.piezobout_long2 <- melt(piezoday_2wk2, id.vars= c("MOUSE_ID", "TESTDATE"), measure.vars=c("BOUTLENGTHTOT1", "BOUTLENGTHNITE1",
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
                                                                                            "BOUTLENGTHNITE8", "BOUTLENGTHDAY8",
                                                                                            "BOUTLENGTHTOT9", "BOUTLENGTHNITE9",
                                                                                            "BOUTLENGTHDAY9", "BOUTLENGTHTOT10",
                                                                                            "BOUTLENGTHNITE10", "BOUTLENGTHDAY10",
                                                                                            "BOUTLENGTHTOT11", "BOUTLENGTHNITE11",
                                                                                            "BOUTLENGTHDAY11"))

m.piezobout_long2$variable <- revalue(m.piezobout_long2$variable, 
                                      c("BOUTLENGTHTOT10"="BOUTLENGTHTOTX","BOUTLENGTHNITE10"="BOUTLENGTHNITEX",
                                        "BOUTLENGTHDAY10"="BOUTLENGTHDAYX", "BOUTLENGTHTOT11"="BOUTLENGTHTOTY",
                                        "BOUTLENGTHNITE11"="BOUTLENGTHNITEY","BOUTLENGTHDAY11"="BOUTLENGTHDAYY"))

m.piezoday_LowSugar2 <- melt(piezoday_LowSugar2, id.vars= c("MOUSE_ID", "TESTDATE"), measure.vars=c("PERCENTSLEEPTOT1", "PERCENTSLEEPNIGHT1",
                                                                                                    "PERCENTSLEEPDAY1"))

m.piezobout_LowSugar2 <- melt(piezoday_LowSugar2, id.vars= c("MOUSE_ID", "TESTDATE"), measure.vars=c("BOUTLENGTHTOT1", "BOUTLENGTHNITE1",
                                                                                                     "BOUTLENGTHDAY1"))

m.piezoday_HighSugar2 <- melt(piezoday_HighSugar2, id.vars= c("MOUSE_ID", "TESTDATE"), measure.vars=c("PERCENTSLEEPTOT1", "PERCENTSLEEPNIGHT1",
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

m.piezobout_HighSugar2 <- melt(piezoday_HighSugar2, id.vars= c("MOUSE_ID", "TESTDATE"), measure.vars=c("BOUTLENGTHTOT1", "BOUTLENGTHNITE1",
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

#### Mean hourly sleep bout length ####
## CAUTION DO NOT RUN IF TRYING OUT HOURLY SLEEP MODELS OR PLOTS
m.postsugar_bouts1 <- melt(piezoday_postsugar_bouts1, 
                           id.vars=c("Date","Day", "Month", "Year", "Hour", "Minute", "am_pm"), 
                           measure.vars=c("G9", "G5","G14","G16", "G18", "G10",
                                                                   "G8", "G2", "T10", "T5", "T17", "T12",
                                                                   "G4", "G1", "G13", "G17", "G19", "G11",
                                                                   "G12", "G3", "T9", "T11", "T19", "T13"))                                                                               

m.postsugar_bouts1$Hour2[m.postsugar_bouts1$am_pm=="AM"] <- 
  m.postsugar_bouts1$Hour[m.postsugar_bouts1$am_pm=="AM"]

m.postsugar_bouts1$Hour2[m.postsugar_bouts1$am_pm=="PM"  & m.postsugar_bouts1$Hour!=12] <- 
  12+m.postsugar_bouts1$Hour[m.postsugar_bouts1$am_pm=="PM" & m.postsugar_bouts1$Hour!=12]

m.postsugar_bouts1$Hour2[m.postsugar_bouts1$am_pm=="PM"  & m.postsugar_bouts1$Hour==12] <- 12
m.postsugar_bouts1$Hour2[m.postsugar_bouts1$am_pm=="AM"  & m.postsugar_bouts1$Hour==12] <- 00



m.postsugar_bouts1$Date <- as.POSIXct(paste(paste(m.postsugar_bouts1$Day, m.postsugar_bouts1$Month, 
                                                  m.postsugar_bouts1$Year, sep="/"),
                               paste(m.postsugar_bouts1$Hour2, m.postsugar_bouts1$Minute, sep=":")), 
                         format = "%d/%m/%Y %H:%M", tz="America/Anchorage")

m.postsugar_bouts1$Photoperiod <- 0
m.postsugar_bouts1$Photoperiod[m.postsugar_bouts1$variable 
                               %in% c("G9", "G5","G14","G16", "G18", "G10",
                                      "G8", "G2", "T10", "T5", "T17", "T12")] <- "Short"
m.postsugar_bouts1$Photoperiod[m.postsugar_bouts1$variable 
                               %in% c("G4", "G1", "G13", "G17", "G19", "G11",
                                      "G12", "G3", "T9", "T11", "T19", "T13")] <- "Neutral"

## Shifting short photoperiod individuals 3.5 hours earlier
m.postsugar_bouts1$Hour_shifted <- m.postsugar_bouts1$Hour2
m.postsugar_bouts1$Hour_shifted[m.postsugar_bouts1$Photoperiod=="Short"] <- 
  m.postsugar_bouts1$Hour2[m.postsugar_bouts1$Photoperiod=="Short"]+(3)
m.postsugar_bouts1$Hour_shifted[m.postsugar_bouts1$Hour_shifted==24] <- 0
m.postsugar_bouts1$Hour_shifted[m.postsugar_bouts1$Hour_shifted==25] <- 1
m.postsugar_bouts1$Hour_shifted[m.postsugar_bouts1$Hour_shifted==26] <- 2


## Procesing perentage bouts the same way as the mean sleep bouts one
m.postsugar_percbouts1 <- melt(postsugar_percbouts1, 
                           id.vars=c("Date","Day", "Month", "Year", "Hour", "Minute", "am_pm"), 
                           measure.vars=c("G9", "G5","G14","G16", "G18", "G10",
                                          "G8", "G2", "T10", "T5", "T17", "T12",
                                          "G4", "G1", "G13", "G17", "G19", "G11",
                                          "G12", "G3", "T9", "T11", "T19", "T13")) 

m.postsugar_percbouts2 <- melt(postsugar_percbouts2, 
                               id.vars=c("Date","Day", "Month", "Year", "Hour", "Minute", "am_pm"), 
                               measure.vars=c("G35", "G23", "G24", "G28", "G29", "G22", 
                                              "T1", "G36", "G27", "G40", "G33",
                                              "G20", "G21", "G26", "T32", "G31", "G39", "G34", "G37", "G25", 
                                              "G38", "G30")) 

m.postsugar_percbouts1$Hour2[m.postsugar_percbouts1$am_pm=="AM"] <- 
  m.postsugar_percbouts1$Hour[m.postsugar_percbouts1$am_pm=="AM"]

m.postsugar_percbouts1$Hour2[m.postsugar_percbouts1$am_pm=="PM"  & m.postsugar_percbouts1$Hour!=12] <- 
  12+m.postsugar_percbouts1$Hour[m.postsugar_percbouts1$am_pm=="PM" & m.postsugar_percbouts1$Hour!=12]

m.postsugar_percbouts1$Hour2[m.postsugar_percbouts1$am_pm=="PM"  & m.postsugar_percbouts1$Hour==12] <- 12
m.postsugar_percbouts1$Hour2[m.postsugar_percbouts1$am_pm=="AM"  & m.postsugar_percbouts1$Hour==12] <- 00

m.postsugar_percbouts1$Date <- as.POSIXct(paste(paste(m.postsugar_percbouts1$Day, m.postsugar_percbouts1$Month, 
                                                  m.postsugar_percbouts1$Year, sep="/"),
                                            paste(m.postsugar_percbouts1$Hour2, m.postsugar_percbouts1$Minute, sep=":")), 
                                      format = "%d/%m/%Y %H:%M", tz="America/Anchorage")

m.postsugar_percbouts1$Photoperiod <- 0
m.postsugar_percbouts1$Photoperiod[m.postsugar_percbouts1$variable 
                               %in% c("G9", "G5","G14","G16", "G18", "G10",
                                      "G8", "G2", "T10", "T5", "T17", "T12")] <- "Short"
m.postsugar_percbouts1$Photoperiod[m.postsugar_percbouts1$variable 
                               %in% c("G4", "G1", "G13", "G17", "G19", "G11",
                                      "G12", "G3", "T9", "T11", "T19", "T13")] <- "Neutral"

## Shifting short photoperiod individuals 3.5 hours earlier
m.postsugar_percbouts1$Hour_shifted <- m.postsugar_percbouts1$Hour2
m.postsugar_percbouts1$Hour_shifted[m.postsugar_percbouts1$Photoperiod=="Short"] <- 
  m.postsugar_percbouts1$Hour2[m.postsugar_percbouts1$Photoperiod=="Short"]+(3)
m.postsugar_percbouts1$Hour_shifted[m.postsugar_percbouts1$Hour_shifted==24] <- 0
m.postsugar_percbouts1$Hour_shifted[m.postsugar_percbouts1$Hour_shifted==25] <- 1
m.postsugar_percbouts1$Hour_shifted[m.postsugar_percbouts1$Hour_shifted==26] <- 2

## Create a subjDayNight column
#### Add a column for subjective day/night
#### TRIED 0600 - 1800 but now trying 0400 to 1700 from stat_pop_etho graph
m.postsugar_percbouts1$SubjDayNight <- "NA"
m.postsugar_percbouts1$SubjDayNight[m.postsugar_percbouts1$Hour_shifted<17 & m.postsugar_percbouts1$Hour_shifted>=4] <- "Day"
m.postsugar_percbouts1$SubjDayNight[m.postsugar_percbouts1$Hour_shifted>=17|m.postsugar_percbouts1$Hour_shifted<4] <- "Night"

m.postsugar_percbouts2$SubjDayNight <- "NA"
m.postsugar_percbouts2$SubjDayNight[m.postsugar_percbouts2$Hour_shifted<17 & m.postsugar_percbouts2$Hour_shifted>=4] <- "Day"
m.postsugar_percbouts2$SubjDayNight[m.postsugar_percbouts2$Hour_shifted>=17|m.postsugar_percbouts2$Hour_shifted<4] <- "Night"


## For phase 2
m.postsugar_percbouts2$Hour2[m.postsugar_percbouts2$am_pm=="AM"] <- 
  m.postsugar_percbouts2$Hour[m.postsugar_percbouts2$am_pm=="AM"]

m.postsugar_percbouts2$Hour2[m.postsugar_percbouts2$am_pm=="PM"  & m.postsugar_percbouts2$Hour!=12] <- 
  12+m.postsugar_percbouts2$Hour[m.postsugar_percbouts2$am_pm=="PM" & m.postsugar_percbouts2$Hour!=12]

m.postsugar_percbouts2$Hour2[m.postsugar_percbouts2$am_pm=="PM"  & m.postsugar_percbouts2$Hour==12] <- 12
m.postsugar_percbouts2$Hour2[m.postsugar_percbouts2$am_pm=="AM"  & m.postsugar_percbouts2$Hour==12] <- 00

m.postsugar_percbouts2$Date <- as.POSIXct(paste(paste(m.postsugar_percbouts2$Day, m.postsugar_percbouts2$Month, 
                                                      m.postsugar_percbouts2$Year, sep="/"),
                                                paste(m.postsugar_percbouts2$Hour2, m.postsugar_percbouts2$Minute, sep=":")), 
                                          format = "%d/%m/%Y %H:%M", tz="America/Anchorage")

m.postsugar_percbouts2$Photoperiod <- 0
m.postsugar_percbouts2$Photoperiod[m.postsugar_percbouts2$variable 
                                   %in% c("G35", "G23", "G24", "G28", "G29", "G22", 
                                          "T1", "G36", "G27", "G40", "G33")] <- "Short"
m.postsugar_percbouts2$Photoperiod[m.postsugar_percbouts2$variable 
                                   %in% c("G20", "G21", "G26", "T32", "G31", "G39", "G34", "G37", "G25", 
                                          "G38", "G30")] <- "Neutral"

## Shifting short photoperiod individuals 3.5 hours earlier
m.postsugar_percbouts2$Hour_shifted <- m.postsugar_percbouts2$Hour2
m.postsugar_percbouts2$Hour_shifted[m.postsugar_percbouts2$Photoperiod=="Short"] <- 
  m.postsugar_percbouts2$Hour2[m.postsugar_percbouts2$Photoperiod=="Short"]+(3)
m.postsugar_percbouts2$Hour_shifted[m.postsugar_percbouts2$Hour_shifted==24] <- 0
m.postsugar_percbouts2$Hour_shifted[m.postsugar_percbouts2$Hour_shifted==25] <- 1
m.postsugar_percbouts2$Hour_shifted[m.postsugar_percbouts2$Hour_shifted==26] <- 2

##Merge phase 1 and 2
percbouts <- rbind(m.postsugar_percbouts1, m.postsugar_percbouts2)

## Add sugar availability column
mer.percbouts <- merge(percbouts, anim_ID_sex, by.x = "variable", by.y="GrassRat_ID")

head(mer.percbouts)

#### Plotting percent sleep across hour of day, all individuals, for high sugar phase ####
## Percent sleep per hour
ggplot(m.postsugar_percbouts1, aes(Hour2,value)) +
  stat_pop_sleep_trial(aes(col=Photoperiod, fill=Photoperiod)) + my_theme +
  ylab("Percentage hourly sleep") + xlab("Hour of day")+
  scale_x_continuous(breaks =seq(0,24,3)) +
  scale_color_manual(values = c("#F38BA8", "#23988aff")) +
  scale_fill_manual(values = c("#F38BA8", "#23988aff"))
  

## Percent sleep per hour, shifted
ggplot(m.postsugar_percbouts1, aes(Hour_shifted,value)) +  
  stat_pop_sleep_trial(aes(col=Photoperiod, fill=Photoperiod)) + my_theme +
  ylab("Percentage hourly sleep") + xlab("Hour of day")+
  scale_x_continuous(breaks =seq(0,24,1)) +
  scale_color_manual(values = c("#F38BA8", "#23988aff")) +
  scale_fill_manual(values = c("#F38BA8", "#23988aff"))


## Make sugar_conc into a factor column
mer.percbouts$Sugar_conc_factor <- as.factor(as.character(mer.percbouts$Sugar_conc))
mer.percbouts$Sugar_conc_factor<- 
  revalue(as.factor(mer.percbouts$Sugar_conc_factor),c("0"="None", "0.08" = "High"))


## boxplots for percentage sleep, day/night
ggplot(mer.percbouts, aes(Sugar_conc_factor,value)) +  
 #stat_pop_sleep_trial(aes(col=Photoperiod, fill=Photoperiod)) + 
  geom_boxplot(aes(fill=Photoperiod)) + my_theme + facet_grid(.~SubjDayNight) +
  ylab("Percentage hourly sleep") + xlab("Hour of day")+
 #scale_x_continuous(breaks =seq(0,24,3)) +
  scale_color_manual(values = c("#F38BA8", "#23988aff")) +
  scale_fill_manual(values = c("#F38BA8", "#23988aff"))



#phase 1
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

#phase 2
m.piezoday_long2$Treatment <- "2wk"
m.piezoday_short2$Treatment <- "4wk"
m.piezoday_LowSugar2$Treatment <- "LowSugar"
m.piezoday_HighSugar2$Treatment <- "HighSugar"

m.piezobout_long2$Treatment <- "2wk"
m.piezobout_short2$Treatment <- "4wk"
m.piezobout_LowSugar2$Treatment <- "LowSugar"
m.piezobout_HighSugar2$Treatment <- "HighSugar"

m.piezoday_short2 <- rename(m.piezoday_short2, replace = c("MOUSE_ID"="GrassRat_ID"))
m.piezoday_long2 <- rename(m.piezoday_long2, replace = c("MOUSE_ID"="GrassRat_ID"))
m.piezoday_LowSugar2 <- rename(m.piezoday_LowSugar2, replace = c("MOUSE_ID"="GrassRat_ID"))
m.piezoday_HighSugar2 <- rename(m.piezoday_HighSugar2, replace = c("MOUSE_ID"="GrassRat_ID"))

m.piezobout_short2 <- rename(m.piezobout_short2, replace = c("MOUSE_ID"="GrassRat_ID"))
m.piezobout_long2 <- rename(m.piezobout_long2, replace = c("MOUSE_ID"="GrassRat_ID"))
m.piezobout_LowSugar2 <- rename(m.piezobout_LowSugar2, replace = c("MOUSE_ID"="GrassRat_ID"))
m.piezobout_HighSugar2 <- rename(m.piezobout_HighSugar2, replace = c("MOUSE_ID"="GrassRat_ID"))

m.piezobout_4wksugars2 <- rbind(m.piezobout_short2, m.piezobout_LowSugar2, m.piezobout_HighSugar2)
m.piezoday_4wksugars2 <- rbind(m.piezoday_short2, m.piezoday_LowSugar2, m.piezoday_HighSugar2)

## After this rbinding, you'll have  combined files for all Phase 1 and phase 2 animals
# rbind() each pair of phase 1 and Phase 2 files
## e.g. m.piezobout_short1 <- rbind(m.piezobout_short1,m.piezobout_short2)
## After binding phase files, you should have one _short, one _long, one _4wksugars
m.piezobout_short <- rbind(m.piezobout_short1,m.piezobout_short2)
m.piezobout_long <- rbind(m.piezobout_long1,m.piezobout_long2)

m.piezoday_short <- rbind(m.piezoday_short1,m.piezoday_short2)
m.piezoday_long <- rbind(m.piezoday_long1,m.piezoday_long2)

m.piezobout_4wksugars <- rbind(m.piezobout_4wksugars1,m.piezobout_4wksugars2)
m.piezoday_4wksugars <- rbind(m.piezoday_4wksugars1,m.piezoday_4wksugars2)

## Add chamber numbers to data frame
m.piezoday_4wksugars <- merge(m.piezoday_4wksugars, anim_ID_sex, by=c("GrassRat_ID"))
m.piezobout_4wksugars <- merge(m.piezobout_4wksugars, anim_ID_sex, by=c("GrassRat_ID"))

m.piezoday_long <- merge(m.piezoday_long, anim_ID_sex, by=c("GrassRat_ID"))
m.piezobout_long <- merge(m.piezobout_long, anim_ID_sex, by=c("GrassRat_ID"))

m.piezobout_short <- merge(m.piezobout_short, anim_ID_sex, by=c("GrassRat_ID"))
m.piezoday_short <- merge(m.piezoday_short, anim_ID_sex, by=c("GrassRat_ID"))

## Add photoperiod column to each dataframe
add_photoperiod <- function(y) {
  y$Photoperiod <- 0
  for(i in 1:nrow(y)) {
    y$Photoperiod[i][y$Chamber[i]<13 & y$Phase[i]==1] <- "Short"
    y$Photoperiod[i][y$Chamber[i]>12 & y$Phase[i]==1] <- "Neutral"
    y$Photoperiod[i][y$Chamber[i]<12 & y$Phase[i]==2] <- "Short"
    y$Photoperiod[i][y$Chamber[i]>11 & y$Phase[i]==2] <- "Neutral"
  }
  return(y)
}

m.piezobout_4wksugars <- add_photoperiod(m.piezobout_4wksugars)
m.piezoday_4wksugars <- add_photoperiod(m.piezoday_4wksugars)
m.piezobout_long <- add_photoperiod(m.piezobout_long)
m.piezoday_long <- add_photoperiod(m.piezoday_long)
m.piezobout_short <- add_photoperiod(m.piezobout_short)
m.piezoday_short <- add_photoperiod(m.piezoday_short)

head(m.piezobout_4wksugars)
unique(m.piezobout_4wksugars$Photoperiod)
head(m.piezobout_long)
head(m.piezoday_short)
head(m.piezobout_short)

#### Sugar files - DON'T RERUN unless base files changed; wrote to csv ####
### Processing sleep bout length changes before and after sugar
m.presugar1 <- melt(piezoday_presugar_bouts1, id.vars="Mouse.IDs.", 
                   measure.vars=c("G9", "G5", "G14", "G16", "G18", "G10", "G8", "G2", "T10", "T5", "T17", "T12", "G4", "G1",
                                  "G13", "G17", "G19", "G11", "G12",  "G3", "T9", "T11", "T19", "T13"))
m.postsugar1 <- melt(piezoday_postsugar_bouts1, id.vars="Date", 
                    measure.vars=c("G9", "G5", "G14", "G16", "G18", "G10", "G8", "G2", "T10", "T5", "T17", "T12", "G4", "G1",
                                   "G13", "G17", "G19", "G11", "G12",  "G3", "T9", "T11", "T19", "T13"))


names(m.presugar1) <- c("DateTime", "ID", "Sleep_bout")
names(m.postsugar1) <- c("DateTime", "ID", "Sleep_bout")

m.presugar2 <- melt(piezoday_presugar_bouts2, id.vars="Mouse.IDs.", 
                    measure.vars=c("G35", "G23", "G24", "G28", "G29", "G22", "T1", "G36", "G27", "G40", "G33", "G20", "G21", "G26",
                                   "T32", "G31", "G39", "G34", "G37",  "G25", "G38", "G30"))
m.postsugar2 <- melt(piezoday_postsugar_bouts2, id.vars="Mouse.IDs.", 
                     measure.vars=c("G35", "G23", "G24", "G28", "G29", "G22", "T1", "G36", "G27", "G40", "G33", "G20", "G21", "G26",
                                    "T32", "G31", "G39", "G34", "G37",  "G25", "G38", "G30"))


names(m.presugar2) <- c("DateTime", "ID", "Sleep_bout")
names(m.postsugar2) <- c("DateTime", "ID", "Sleep_bout")
#m.presugar$datetime.posix <- as.POSIXct(m.presugar$DateTime, format="%m/%d/%Y %H:%M", tz="GMT")
#m.presugar$date <- as.POSIXct(strptime(m.presugar$DateTime, format="%m/%d/%Y", tz="GMT"))

#m.presugar$time <- strftime(m.presugar$datetime.posix, format="%H:%M", tz = "GMT")
#m.presugar$time <- as.POSIXct(m.presugar$time, format="%H:%M", tz = "GMT")

m.presugar1$Treatment <- 0
m.presugar1$Treatment[m.presugar1$ID %in% c("G9", "G5", "G14", "G16", "G18", "G10", 
                                        "G8", "G2", "T10", "T5", "T17", "T12")] <- "Short"
m.presugar1$Treatment[m.presugar1$ID %in% c("G4", "G1", "G13", "G17", "G19", "G11", "G12", "G3", "T9", 
                                        "T11", "T19", "T13")] <- "Neutral"

m.presugar2$Treatment <- 0
m.presugar2$Treatment[m.presugar2$ID %in% c("G35", "G23", "G24", "G28", "G29", "G22", 
                                            "T1", "G36", "G27", "G40", "G33")] <- "Short"
m.presugar2$Treatment[m.presugar2$ID %in% c("G20", "G21", "G26", "T32", "G31", "G39", "G34", "G37", "G25", 
                                            "G38", "G30")] <- "Neutral"
## ADD A PHASE COLUMN for m.presugar1 and m.presugar2
m.presugar1$Phase <- 1
m.presugar2$Phase <- 2

m.postsugar1$Treatment <- 0
m.postsugar1$Treatment[m.postsugar1$ID %in% c("G9", "G5", "G14", "G16", "G18", "G10", 
                                          "G8", "G2", "T10", "T5", "T17", "T12")] <- "Short"
m.postsugar1$Treatment[m.postsugar1$ID %in% c("G4", "G1", "G13", "G17", "G19", "G11", "G12", "G3", "T9", 
                                          "T11", "T19", "T13")] <- "Neutral"

m.postsugar2$Treatment <- 0
m.postsugar2$Treatment[m.postsugar2$ID %in% c("G35", "G23", "G24", "G28", "G29", "G22", 
                                              "T1", "G36", "G27", "G40", "G33")] <- "Short"
m.postsugar2$Treatment[m.postsugar2$ID %in% c("G20", "G21", "G26", "T32", "G31", "G39", "G34", "G37", "G25", 
                                              "G38", "G30")] <- "Neutral"
## ADD A PHASE COLUMN for m.presugar1 and m.presugar2
m.postsugar1$Phase <- 1
m.postsugar2$Phase <- 2

m.presugar1$Sugar <- "Pre"
m.postsugar1$Sugar <- "Post"

m.presugar2$Sugar <- "Pre"
m.postsugar2$Sugar <- "Post"
## BIND ALL Phase 1 and phase 2, for a total of 4 data frames (one pre- and one post-sugar for each phase)
bound_prepost_sugar <- rbind(m.presugar1, m.presugar2, m.postsugar1, m.postsugar2)

## Added Oct 28, 2019
bound_prepost_sugar <- bound_prepost_sugar[!is.na(bound_prepost_sugar$Sleep_bout),]
bound_prepost_sugar$TreatmentSugar <- paste0(bound_prepost_sugar$Treatment, bound_prepost_sugar$Sugar)

## Adding an 'hour' column
## First make sure all the dates are in the same format
bound_prepost_sugar$datetime.posix <- as.POSIXct(bound_prepost_sugar$DateTime, 
                                                 format = "%Y/%m/%d %H:%M", tz="America/Anchorage")
bound_prepost_sugar$Time <- 
  sapply(strsplit(as.character(bound_prepost_sugar$datetime.posix), "[/ ]+"), "[", 2)

bound_prepost_sugar$datetime.posix[is.na(bound_prepost_sugar$Time)] <- 
  as.POSIXct(bound_prepost_sugar$DateTime[is.na(bound_prepost_sugar$Time)], 
                                                 format = "%Y-%m-%d %H:%M", tz="America/Anchorage")
bound_prepost_sugar$Time[is.na(bound_prepost_sugar$Time)] <- 
  sapply(strsplit(as.character(bound_prepost_sugar$datetime.posix[is.na(bound_prepost_sugar$Time)]), "[/ ]+"), "[", 2)

bound_prepost_sugar$datetime.posix[is.na(bound_prepost_sugar$Time)] <- 
  as.POSIXct(bound_prepost_sugar$DateTime[is.na(bound_prepost_sugar$Time)], 
                                                 format = "%m/%d/%Y %H:%M", tz="America/Anchorage")
bound_prepost_sugar$Time[is.na(bound_prepost_sugar$Time)] <- 
  sapply(strsplit(as.character(bound_prepost_sugar$datetime.posix[is.na(bound_prepost_sugar$Time)]), "[/ ]+"), "[", 2)

bound_prepost_sugar$Hour <- 
  sapply(strsplit(as.character(bound_prepost_sugar$Time), ":"), "[", 1)

bound_prepost_sugar[is.na(bound_prepost_sugar$Time),]

bound_prepost_sugar$Hour <- as.numeric(bound_prepost_sugar$Hour)
bound_prepost_sugar$Hour2 <- bound_prepost_sugar$Hour
bound_prepost_sugar$Hour2[bound_prepost_sugar$Hour2>12] <- bound_prepost_sugar$Hour2[bound_prepost_sugar$Hour2>12]-24

bound_prepost_sugar <- bound_prepost_sugar %>% rename(GrassRat_ID = ID) 
bound_prepost_sugar <- merge(bound_prepost_sugar,
                             y = anim_ID_sex[,c("GrassRat_ID", "Chamber", "Sex", "Sugar_conc")], 
                             by = "GrassRat_ID")

## G28 seeems to have been unplugged Aug 21-Aug 30, need to check records. Removing for now
bound_prepost_sugar$Sleep_bout[bound_prepost_sugar$GrassRat_ID=="G28" & bound_prepost_sugar$Sleep_bout>3000] <- "NA" 
bound_prepost_sugar$Sleep_bout <- as.numeric(as.character(bound_prepost_sugar$Sleep_bout))
bound_prepost_sugar <- bound_prepost_sugar[!is.na(bound_prepost_sugar$Sleep_bout),]

write.csv(bound_prepost_sugar, "HourlySleepBouts_PrePostSugar.csv")

#### Sleep bout plot ####

m.presugar <- rbind(m.presugar1, m.presugar2)

ggplot(m.presugar[!is.na(m.presugar1$Sleep_bout),], aes(ID, Sleep_bout)) + geom_boxplot() + facet_grid(.~Treatment, scales="free_x") + 
  my_theme + ylab("Sleep bout (s)") + ggtitle("Presugar")

ggplot(m.presugar1[!is.na(m.presugar1$Sleep_bout),], aes(ID, Sleep_bout)) + geom_boxplot() + facet_grid(.~Treatment, scales="free_x") + 
  my_theme + ylab("Sleep bout (s)") + ggtitle("Presugar_Phase 1")

ggplot(m.presugar2[!is.na(m.presugar2$Sleep_bout),], aes(ID, Sleep_bout)) + geom_boxplot() + facet_grid(.~Treatment, scales="free_x") + 
  my_theme + ylab("Sleep bout (s)") + ggtitle("Presugar_Phase 2")

ggplot(m.postsugar1, aes(ID, Sleep_bout)) + geom_boxplot() + facet_grid(.~Treatment, scales="free_x") + my_theme +
  ylab("Sleep bout (s)") + ggtitle("Postsugar_Phase 1")

ggplot(m.postsugar2, aes(ID, Sleep_bout)) + geom_boxplot() + facet_grid(.~Treatment, scales="free_x") + my_theme +
  ylab("Sleep bout (s)") + ggtitle("Postsugar_Phase 2")

ggplot(bound_prepost_sugar, aes(GrassRat_ID, Sleep_bout)) + geom_boxplot() + 
  facet_grid(Sugar~Treatment, scales="free_x") + 
  my_theme + ylab("Sleep bout (s)") + ggtitle("Presugar and Postsugar for Both Phases") +
  theme(axis.text.x = element_text(angle=90))

ggplot(bound_prepost_sugar, aes(as.factor(as.character(Hour)), Sleep_bout)) + geom_boxplot() + 
  facet_grid(Sugar~Treatment, scales="free_x") + 
  my_theme + ylab("Sleep bout (s)") + ggtitle("Presugar and Postsugar for Both Phases") +
  theme(axis.text.x = element_text(angle=90))


## Column for whether they got sugar or not.
## Random effect of day, ID
## Subj day/night column to label each row
# Plot of mean hourly sleep bout duration, 
## grouped by subj day/night, boxplots per subj day/night colored by sugar and photoperiod

### DO THIS
## stat_pop_etho plots for neutral/short, and postsugar, 0% vs 8% (these two on the same plots). Facet by photoperiod 
## vs. hour shifted (subjective day/night)

#### DO ########

mod_sleepbout_SugarHourTreatmentIndiv <- lmer(Sleep_bout~-1+(Hour^2)+TreatmentSugar+(1|GrassRat_ID), 
                                          data=bound_prepost_sugar)
summary(mod_sleepbout_SugarHourTreatmentIndiv)
summary(mod_sleepbout_SugarHourTreatmentIndiv)
plot(mod_sleepbout_SugarHourTreatmentIndiv)
coef(mod_sleepbout_SugarHourTreatmentIndiv)
plot(residuals(mod_sleepbout_SugarHourTreatmentIndiv)~
       bound_prepost_sugar$GrassRat_ID)

mod_sleepbout_SugarTreatmentIndiv <- lmer(Sleep_bout~-1+TreatmentSugar+(1|GrassRat_ID), 
                                          data=bound_prepost_sugar)
summary(mod_sleepbout_SugarTreatmentIndiv)

mod_sleepbout_SugarIndiv <- lmer(Sleep_bout~-1+Sugar+(1|GrassRat_ID), data=bound_prepost_sugar)
summary(mod_sleepbout_SugarIndiv)
plot(mod_sleepbout_SugarIndiv)

mod_sleepbout_Sugar <- lm(Sleep_bout~Sugar, data=bound_prepost_sugar)
summary(mod_sleepbout_Sugar)

anova(mod_sleepbout_SugarIndiv, mod_sleepbout_SugarTreatmentIndiv, mod_sleepbout_SugarHourTreatmentIndiv)
anova(mod_sleepbout_Sugar)

plot(allEffects(mod_sleepbout_SugarHourTreatmentIndiv))
plot(allEffects(mod_sleepbout_SugarHourTreatmentIndiv, partial.residuals=T))


mod_daily_bout_sugar <- lmer(value~Treatment+Sex+ShortLong+Sugar+(1|Chamber),data=m.piezobout_combined[m.piezobout_combined$DayNightTot=="Total",])
anova(mod_daily_bout_sugar)

## Histograms of sleep bout length faceted by photoperiod
ggplot(bound_prepost_sugar[!is.na(bound_prepost_sugar$Sleep_bout),], aes(Sleep_bout)) + geom_histogram() + facet_grid(.~Treatment)

## Plotting sleep bouts per individual across treatments and phases
ggplot(bound_prepost_sugar[!is.na(bound_prepost_sugar$Sleep_bout),], 
       aes(GrassRat_ID, Sleep_bout)) + geom_boxplot(aes(fill=TreatmentSugar)) + 
  facet_grid(.~Phase, scales='free_x') + my_theme + theme(axis.text.x = element_text(angle=90))


ggplot(bound_prepost_sugar[!is.na(bound_prepost_sugar$Sleep_bout),], 
       aes(TreatmentSugar, Sleep_bout)) + geom_boxplot(aes(fill=TreatmentSugar)) + 
  facet_grid(.~Phase, scales='free_x') + my_theme + theme(axis.text.x = element_text(angle=90))


ggplot(bound_prepost_sugar, aes(Sleep_bout)) + geom_histogram() + facet_grid(.~Treatment)
ggplot(bound_prepost_sugar, aes(Sugar, Sleep_bout)) + geom_boxplot() + facet_grid(.~Treatment) + my_theme + ggtitle("Sleep_bout Duration vs Sugar")

bound_prepost_sugar$Hour_shifted <- bound_prepost_sugar$Hour
bound_prepost_sugar$Hour_shifted[bound_prepost_sugar$Treatment=="Short" & bound_prepost_sugar$Phase==2] <- 
  bound_prepost_sugar$Hour[bound_prepost_sugar$Treatment=="Short" & bound_prepost_sugar$Phase==2]+3
## If combining pre and post sugar in hourly_shifted plot, this seems to be shifted by 4 instead of 3
bound_prepost_sugar$Hour_shifted[bound_prepost_sugar$Treatment=="Short" & bound_prepost_sugar$Phase==1] <- 
  bound_prepost_sugar$Hour[bound_prepost_sugar$Treatment=="Short" & bound_prepost_sugar$Phase==1]+3
bound_prepost_sugar$Hour_shifted[bound_prepost_sugar$Hour_shifted==24] <- 0
bound_prepost_sugar$Hour_shifted[bound_prepost_sugar$Hour_shifted==25] <- 1
bound_prepost_sugar$Hour_shifted[bound_prepost_sugar$Hour_shifted==26] <- 2
bound_prepost_sugar$Hour_shifted[bound_prepost_sugar$Hour_shifted==27] <- 3

#### Add a column for subjective day/night
bound_prepost_sugar$SubjDayNight <- "NA"
bound_prepost_sugar$SubjDayNight[bound_prepost_sugar$Hour_shifted<18 & bound_prepost_sugar$Hour_shifted>=6] <- "Day"
bound_prepost_sugar$SubjDayNight[bound_prepost_sugar$Hour_shifted>=18|bound_prepost_sugar$Hour_shifted<6] <- "Night"



## Trying out sleep bout plots per individual, might not be very helpful
ggplot(bound_prepost_sugar, aes(Hour,Sleep_bout)) + facet_wrap(.~GrassRat_ID) +
  geom_point(aes(col=Treatment)) + my_theme +
  geom_smooth(method="loess") +
  ylab("Mean hourly sleep duration (sec)") + xlab("Hour of day")+
  scale_x_continuous(breaks =seq(0,24,3)) +
  scale_color_manual(values = c("#F38BA8", "#23988aff")) +
  scale_fill_manual(values = c("#F38BA8", "#23988aff"))

## Trying stat pop graph with sleep bouts instead of percent sleep
ggplot(bound_prepost_sugar, aes(Hour,Sleep_bout)) + #facet_grid(.~Phase) +
  stat_pop_sleep_trial(aes(col=Treatment, fill=Treatment)) + my_theme +
  ylab("Mean hourly sleep duration (sec)") + xlab("Hour of day")+
  scale_x_continuous(breaks =seq(0,24,3)) +
  scale_color_manual(values = c("#F38BA8", "#23988aff")) +
  scale_fill_manual(values = c("#F38BA8", "#23988aff"))

## Mean hourly sleep bout duration, shifted. Can separate by phase or not...
# and by sugar or not by changing facets.
ggplot(bound_prepost_sugar[bound_prepost_sugar$Sugar=="Post",], aes(Hour_shifted,Sleep_bout)) + 
  facet_grid(Sugar_conc_factor~.) +
  stat_pop_sleep_trial(aes(col=Treatment, fill=Treatment)) + my_theme +
  ylab("Mean hourly sleep duration (sec)") + xlab("Hour of day")+
  scale_x_continuous(breaks =seq(0,24,3)) +
  scale_color_manual(values = c("#F38BA8", "#23988aff")) +
  scale_fill_manual(values = c("#F38BA8", "#23988aff"))


## Mean hourly sleep bout duration, shifted. Plotted as boxplots by subj day/night
# Can separate by phase or not and by sugar or not by changing facets.
ggplot(bound_prepost_sugar[bound_prepost_sugar$Sugar=="Post",], aes(Sugar_conc_factor,Sleep_bout)) + 
    facet_grid(.~SubjDayNight) + geom_violin(aes(fill=Treatment))  + my_theme +
    #stat_pop_sleep_trial(aes(col=Treatment, fill=Treatment)) +
    ylab("Mean hourly sleep duration (sec)") + xlab("Sugar concentration")+
    #scale_x_continuous(breaks =seq(0,24,3)) +
    scale_color_manual(values = c("#F38BA8", "#23988aff")) +
    scale_fill_manual(values = c("#F38BA8", "#23988aff"))


bound_prepost_sugar$Sugar_conc_factor<- as.factor(as.character(bound_prepost_sugar$Sugar_conc))
levels(bound_prepost_sugar$Sugar_conc_factor)[levels(bound_prepost_sugar$Sugar_conc_factor)=="0"] <- "None"
levels(bound_prepost_sugar$Sugar_conc_factor)[levels(bound_prepost_sugar$Sugar_conc_factor)=="0.08"] <- "High"

ggplot(bound_prepost_sugar[bound_prepost_sugar$Sugar=="Post",], aes(Sugar_conc_factor,Sleep_bout)) + 
  #facet_grid(Sugar~Phase) +
  #stat_pop_sleep_trial(aes(col=Treatment, fill=Treatment)) + 
  geom_boxplot(aes(fill=Treatment)) +
  my_theme +
  ylab("Mean hourly sleep duration (sec)") + xlab("Hour of day") +
  #scale_x_continuous(breaks =seq(0,24,3)) +
  scale_color_manual(values = c("#F38BA8", "#23988aff")) +
  scale_fill_manual(values = c("#F38BA8", "#23988aff"))


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
m.piezobout_combined <- rbind(m.piezobout_4wksugars, m.piezobout_long)
m.piezobout_combined$Treatment <- as.factor(as.character(m.piezobout_combined$Treatment))
m.piezobout_combined$Treatment <- factor(m.piezobout_combined$Treatment, levels = c("2wk", "4wk", "LowSugar", "HighSugar"))


m.piezobout_combined$Sugar <- 0
m.piezobout_combined$Sugar[m.piezobout_combined$Treatment %in% c("2wk", "4wk")] <- "None"
m.piezobout_combined$Sugar[m.piezobout_combined$Treatment %in% c("LowSugar")] <- "2%"
#sugar <- subset(sugar, select=-Phase)
m.piezobout_combined_conc<- m.piezobout_combined
m.piezobout_combined_conc$Sugar[m.piezobout_combined_conc$Treatment=="HighSugar" & 
                                  m.piezobout_combined_conc$Sugar_conc=="0.08"] <- "8%"
m.piezobout_combined_conc$Sugar <- as.factor(m.piezobout_combined_conc$Sugar)

m.piezobout_combined_conc$Photoperiod <- as.factor(as.character(m.piezobout_combined_conc$Photoperiod))

mod_sleepbout_full <- lmer(value~Sex+Photoperiod+Sugar+DayNightTot+Phase+(1|GrassRat_ID),
                          data=m.piezobout_combined_conc[m.piezobout_combined_conc$DayNightTot!="Total",])
mod_sleepbout_noSex <- lmer(value~Photoperiod+Sugar+DayNightTot+Phase+(1|GrassRat_ID),
                               data=m.piezobout_combined_conc[m.piezobout_combined_conc$DayNightTot!="Total",])
mod_sleepbout_noSex_noPhotoperiod <- lmer(value~Sugar+DayNightTot+Phase+(1|GrassRat_ID),
                               data=m.piezobout_combined_conc[m.piezobout_combined_conc$DayNightTot!="Total",])
mod_sleepbout_noSex_noDaynight_noPhotoperiod <- lmer(value~Sugar+Phase+(1|GrassRat_ID),
                                          data=m.piezobout_combined_conc[m.piezobout_combined_conc$DayNightTot!="Total",])
mod_sleepbout_SugarIndiv <- lmer(value~Sugar+(1|GrassRat_ID),
                                                            data=m.piezobout_combined_conc[m.piezobout_combined_conc$DayNightTot!="Total",])
mod_sleepbout_onlySugar <- lm(value~Sugar, data=m.piezobout_combined_conc[m.piezobout_combined_conc$DayNightTot!="Total",])


mod_sleepbout_SugarDayNight_interac <- lmer(value~Sugar*DayNightTot+(1|GrassRat_ID), 
                                          data=m.piezobout_combined_conc[m.piezobout_combined_conc$DayNightTot!="Total",])

##BEST MODEL--AIC LOWEST
mod_sleepbout_SugarTreatment_DayNight_interac <- lmer(value~Sugar*Photoperiod + DayNightTot +(1|GrassRat_ID), ## Second BEST MODEL
                                             data=m.piezobout_combined_conc[m.piezobout_combined_conc$DayNightTot!="Total",])

mod_sleepbout_SugarTreatment_interac <- lmer(value~Sugar*Photoperiod +(1|GrassRat_ID),
                                                      data=m.piezobout_combined_conc[m.piezobout_combined_conc$DayNightTot!="Total",])

mod_sleepbout_SugarTreatmentDayNight_interac <- lmer(value~Sugar*Photoperiod*DayNightTot +(1|GrassRat_ID), ## BEST MODEL
                                                      data=m.piezobout_combined_conc[m.piezobout_combined_conc$DayNightTot!="Total",])
summary(mod_sleepbout_SugarTreatmentDayNight_interac)


##WITHOUT DAY AND NIGHT INTERACTION
mod_sleepbout_full <- lmer(value~Sex+Photoperiod+Sugar+Phase+(1|GrassRat_ID),
                           data=m.piezobout_combined_conc[m.piezobout_combined_conc$DayNightTot=="Total",])
mod_sleepbout_noSex <- lmer(value~Photoperiod+Sugar+Phase+(1|GrassRat_ID),
                            data=m.piezobout_combined_conc[m.piezobout_combined_conc$DayNightTot=="Total",])
mod_sleepbout_noSex_noPhotoperiod <- lmer(value~Sugar+Phase+(1|GrassRat_ID),
                                          data=m.piezobout_combined_conc[m.piezobout_combined_conc$DayNightTot=="Total",])

mod_sleepbout_noSex_noDaynight_noPhotoperiod_noPhase <- lmer(value~Sugar+(1|GrassRat_ID),
                                                             data=m.piezobout_combined_conc[m.piezobout_combined_conc$DayNightTot=="Total",])
mod_sleepbout_onlySugar <- lm(value~Sugar, data=m.piezobout_combined_conc[m.piezobout_combined_conc$DayNightTot=="Total",])

mod_sleepbout_Photoperiod_ID <- lmer(value~Photoperiod+(1|GrassRat_ID),
                           data=m.piezobout_combined_conc[m.piezobout_combined_conc$DayNightTot=="Total",])

##BEST MODEL--bouts were shorter for Short Animals
mod_sleepbout_SugarTreatment_interac <- lmer(value~Sugar*Photoperiod + (1|GrassRat_ID),
                                                      data=m.piezobout_combined_conc[m.piezobout_combined_conc$DayNightTot=="Total",])
summary(mod_sleepbout_SugarTreatment_interac)
plot(allEffects(mod_sleepbout_SugarTreatment_interac))
plot(allEffects(mod_sleepbout_SugarTreatment_interac, partial.residuals=T))


anova(mod_sleepbout_full, mod_sleepbout_noSex,
      mod_sleepbout_noSex_noPhotoperiod, 
      mod_sleepbout_noSex_noDaynight_noPhotoperiod, 
      mod_sleepbout_SugarIndiv, 
      #mod_sleepbout_onlySugar,
      mod_sleepbout_SugarTreatment_interac, 
      mod_sleepbout_SugarDayNight_interac,
      #mod_sleepbout_Photoperiod_ID,
      mod_sleepbout_SugarTreatment_DayNight_interac,
      mod_sleepbout_SugarTreatmentDayNight_interac)

#summary(mod_sleepbout_SugarDayNight_interac)
anova(mod_sleepbout_SugarTreatment_interac)
qqnorm(resid(mod_sleepbout_SugarTreatment_interac)) ## Not a very skewed line
qqline(resid(mod_sleepbout_SugarTreatment_interac))
coef(mod_sleepbout_SugarTreatment_interac)

tmp <- as.data.frame(confint(glht(mod_sleepbout_SugarTreatmentDayNight_interac))$confint)
tmp$Comparison <- rownames(tmp)
tmp$Comparison <- revalue(as.factor(tmp$Comparison),c("(Intercept)" = "Intercept", "DayNightTotNight" = "Night",
                                                      "Sugar0.08" = "8% sucrose", "Sugar0.08:DayNightTotNight" = "Night 8%",
                                                      "Sugar2%" = "2% sucrose", "Sugar2%:DayNightTotNight" = "Night 2%",
                                                      "SugarNone" = "No sucrose", "SugarNone:DayNightTotNight" = "Night No sucrose"))
##Ansuha fix this por favor
ggplot(tmp, aes(x = Comparison, y = Estimate, ymin = lwr, ymax = upr)) + my_theme +
  theme(axis.text.x = element_text(angle=90, vjust=0.7)) +
  geom_errorbar() + geom_point() ## Shows that animals on 8% sugar, 
#in the short photoperiod had longer nighttime sleep bouts than any other combination

#mod_Photoperiod <- lmer(value~Photoperiod+DayNightTot + (1|GrassRat_ID),
 #                       data=m.piezobout_4wksugars[m.piezobout_4wksugars$DayNightTot!="Total" & 
 #                                                    m.piezobout_4wksugars$Treatment %in% c("4wk"),])

#mod_Photoperiod_perc <- lmer(value~Photoperiod+DayNightTot + (1|GrassRat_ID),
  #                      data=m.piezobout_4wksugars[m.piezobout_4wksugars$DayNightTot!="Total" & 
  #                                                   m.piezobout_4wksugars$Treatment %in% c("4wk"),])


## Histogram of mean bout "total" lengths
ggplot(m.piezobout_4wksugars[m.piezobout_4wksugars$DayNightTot=="Total",], aes(value)) + 
  geom_histogram(bins=50) + my_theme + xlab("Mean bout length (seconds)")

ggplot(m.piezobout_4wksugars[m.piezobout_4wksugars$DayNightTot=="Day",], aes(value)) + 
  geom_histogram(bins=50) + my_theme

ggplot(m.piezobout_4wksugars[m.piezobout_4wksugars$DayNightTot=="Night",], aes(value)) + 
  geom_histogram(bins=50) + my_theme


########## DO IT SHELBY USE THIS - percent sleep in each phase of the expt, day vs night, long vs short
sleep_wake_4wk_all <- ggplot(m.piezoday_4wksugars[m.piezoday_4wksugars$DayNightTot!="Total",], aes(DayNightTot, value)) + 
  facet_grid(Photoperiod~Treatment) + my_theme +  geom_boxplot(aes(fill=DayNightTot)) +
  scale_fill_manual(values = c("grey90", "black")) + geom_point(col="grey70") +
  theme(axis.text.x = element_text(angle=90)) + ylim(0,100) +
  ylab("Percent Sleep") + ggtitle("4 Week Test")

sleep_wake_4wk_all

###### DO IT Bout
sleep_bout_expt_4wk <- ggplot(m.piezobout_4wksugars[m.piezobout_4wksugars$DayNightTot!="Total",], aes(DayNightTot, value)) + 
  facet_grid(Photoperiod~Treatment) + my_theme +  geom_boxplot(aes(fill=DayNightTot)) +
  scale_fill_manual(values = c("grey70", "black")) + geom_point(aes(col=GrassRat_ID), size=2) +
  theme(axis.text.x = element_text(angle=90), legend.position = "none")  + ggtitle("4 week") + ylim(0,1100) +
  ylab("Mean sleep bout length (seconds)") + xlab("Day/Night")

sleep_bout_expt_4wk

sleep_bout_expt_2wk <- ggplot(m.piezobout_long[m.piezobout_long$DayNightTot!="Total" & 
                                                 !is.na(m.piezobout_long$DayNightTot),], aes(DayNightTot, value)) + 
  facet_grid(Photoperiod~.) + my_theme +  geom_boxplot(aes(fill=DayNightTot)) +
  scale_fill_manual(values = c("grey70", "black")) + geom_point(aes(col=GrassRat_ID), size=2) +
  theme(axis.text.x = element_text(angle=90), legend.position = "none") + ggtitle("2 week") + ylim(0,1100) + 
  ylab("Mean sleep bout length (seconds)") + xlab("Day/Night")

sleep_bout_expt_2wk

sleep_bout_expt_combined <- ggplot(m.piezobout_combined[m.piezobout_combined$DayNightTot!="Total",], aes(DayNightTot, value)) + 
  facet_grid(Photoperiod~Treatment) + my_theme +  geom_boxplot(aes(fill=DayNightTot)) +
  scale_fill_manual(values = c("grey70", "black")) + geom_point(aes(col=GrassRat_ID), size=2) +
  theme(axis.text.x = element_text(angle=90), legend.position = "none") + ggtitle("All week") + ylim(0,1550) + 
  ylab("Mean sleep bout length (seconds)") + xlab("Day/Night")

sleep_bout_expt_combined

#grid.arrange(sleep_bout_expt_2wk, sleep_bout_expt_4wk, ncol=2)

## Plotting day, night, and total all on same plot across all expt phases
sleep_bout_expt_combined2 <- ggplot(m.piezobout_combined, aes(DayNightTot, value)) + 
  facet_grid(Photoperiod~Treatment) + my_theme +  geom_boxplot(aes(fill=DayNightTot)) +
  scale_fill_manual(values = c("grey70", "black", "grey30")) + geom_point(aes(col=GrassRat_ID), size=2) +
  theme(axis.text.x = element_text(angle=90), legend.position = "none") + ggtitle("All week") + ylim(0,1550) + 
  ylab("Mean sleep bout length (seconds)") + xlab("Day/Night")

sleep_bout_expt_combined2

## Bout
sleep_bout_expt_4wk <- ggplot(m.piezobout_short[m.piezobout_short$DayNightTot=="Total",], aes(Photoperiod, value)) + 
  my_theme +  geom_boxplot(aes(fill=DayNightTot)) +
  scale_fill_manual(values = c("grey70", "black")) + geom_point(aes(col=GrassRat_ID), size=2) +
  theme(axis.text.x = element_text(angle=90), legend.position = "none")  + ggtitle("4 week") + ylim(0,1100) +
  ylab("Mean sleep bout length (seconds)") + xlab("Day/Night")

sleep_bout_expt_4wk

sleep_bout_expt_2wk <- ggplot(m.piezobout_long[m.piezobout_long$DayNightTot=="Total",], aes(Photoperiod, value)) + 
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
  facet_grid(.~Photoperiod) + my_theme +  geom_boxplot(aes(fill=DayNightTot)) +
  scale_fill_manual(values = c("grey70", "black")) + 
  geom_point(aes(col=GrassRat_ID)) +
  theme(axis.text.x = element_text(angle=90), legend.position = "none") + ylim(0,100) +
  ylab("Percent Sleep") + ggtitle("2 Week Acclimation")

sleep_wake_2wk_all

sleep_wake_2wk_tot <- ggplot(m.piezoday_long[m.piezoday_long$DayNightTot=="Total",], aes(Photoperiod, value)) + 
  my_theme +  geom_boxplot(aes(fill=Photoperiod)) +
  #scale_fill_manual(values = c("grey70", "black")) + 
  geom_point(aes(col=GrassRat_ID)) +
  theme(axis.text.x = element_text(angle=90), legend.position = "none") + ylim(0,100) +
  ylab("Percent Sleep") + ggtitle("2 Week Acclimation")

sleep_wake_2wk_tot

sleep_wake_4wk_tot <- ggplot(m.piezoday_short[m.piezoday_short$DayNightTot=="Total",], aes(Photoperiod, value)) + 
  my_theme +  geom_boxplot(aes(fill=Photoperiod)) +
  #scale_fill_manual(values = c("grey70", "black")) + 
  geom_point(aes(col=GrassRat_ID)) +
  theme(axis.text.x = element_text(angle=90), legend.position = "none") + ylim(0,100) +
  ylab("Percent Sleep") + ggtitle("4 Week")

sleep_wake_4wk_tot

####DO IT
sleep_wake_4wk_all <- ggplot(m.piezoday_short[m.piezoday_short$DayNightTot!="Total",], aes(DayNightTot, value)) + 
  facet_grid(.~Photoperiod) + my_theme +  geom_boxplot(aes(fill=DayNightTot)) +
  scale_fill_manual(values = c("grey70", "black")) + 
  geom_point(aes(col=GrassRat_ID)) +
  theme(axis.text.x = element_text(angle=90), legend.position = "none") + ylim(0,100) +
  ylab("Percent Sleep") + ggtitle("4 Week")

sleep_wake_4wk_all
  
grid.arrange(sleep_wake_2wk_all, sleep_wake_4wk_all, ncol=2)

m.piezoday_combined <- rbind(m.piezoday_4wksugars, m.piezoday_long)
m.piezoday_combined$Treatment <- as.factor(as.character(m.piezoday_combined$Treatment))
m.piezoday_combined$Treatment <- factor(m.piezoday_combined$Treatment, levels = c("2wk", "4wk", "LowSugar", "HighSugar"))
m.piezoday_combined$Sugar <- 0
m.piezoday_combined$Sugar[m.piezoday_combined$Treatment %in% c("2wk", "4wk")] <- "None"
m.piezoday_combined$Sugar[m.piezoday_combined$Treatment %in% c("LowSugar")] <- "2%"
m.piezoday_combined$Sugar[m.piezoday_combined$Treatment=="HighSugar" & 
                                  m.piezoday_combined$Sugar_conc=="0.08"] <- "8%"
m.piezoday_combined$Sugar <- as.factor(m.piezoday_combined$Sugar)

sleep_perc_all <- ggplot(m.piezoday_combined, aes(DayNightTot, value)) + 
  facet_grid(Photoperiod~Treatment) + my_theme +  geom_boxplot(aes(fill=DayNightTot)) +
  scale_fill_manual(values = c("grey30", "black", "grey70")) + 
  geom_point(aes(col=GrassRat_ID)) +
  theme(axis.text.x = element_text(angle=90), legend.position = "none") + ylim(0,100) +
  ylab("Percent Sleep") + ggtitle("All")

sleep_perc_all
