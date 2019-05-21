## Raw voltage analyses
## Started May 20, 2019
## code by: Anusha Shankar

require(reshape2)
require(plyr)
require(ggplot2)

setwd("E:\\Google Drive\\Toshiba_desktop\\Fairbanks\\Research\\GrassRats\\Piezo_data")

rms <- read.csv("HighSucrose_14May2019_RMS_Day1_and2.csv")

rms$Hour_Military <- rms$Hour

for(i in 1:nrow(rms)) {
  if (rms$am_pm[i]=="PM" & rms$Hour[i]<12) {
  rms$Hour_Military[i] <- rms$Hour[i]+12
  }
}

for(i in 1:nrow(rms)) {
  if (rms$am_pm[i]=="AM" & rms$Hour[i]==12) {
    rms$Hour_Military[i] <- 24
  }
}

#### General functions ####
my_theme <- theme_classic(base_size = 30) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))

m.rms <- melt(rms, id.vars=c("Day", "Month", "Year", "Hour_Military", "Minute", "Second"), measure.vars=c("G9", "G5", "G14", "G16", "G18", "G10",
                                                                                        "G8", "G2", "T10", "T5", "T17", "T12",
                                                                                        "G4", "G1", "G13", "G17", "G19", "G3",
                                                                                        "T9", "T11", "T19", "T13"))

m.rms <- rename(m.rms, c("variable"="Individual", "value"="RMS"))

head(m.rms)

agg.rms <- aggregate(m.rms$RMS, by=list(m.rms$Individual, m.rms$Month, m.rms$Day, m.rms$Hour_Military, m.rms$Minute), FUN="mean")
names(agg.rms) <- c("Individual", "Month", "Day", "Hour", "Minute", "RMS_minute_mean")

ggplot(agg.rms, aes(as.factor(Hour), Individual)) + my_theme +
  geom_tile(aes(fill=RMS_minute_mean), stat="identity") +
  scale_fill_gradient(low="white", high="red")
