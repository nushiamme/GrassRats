## Raw voltage analyses
## Started May 20, 2019
## code by: Anusha Shankar

require(reshape2)
require(plyr)
require(ggplot2)

setwd("E:\\Google Drive\\Toshiba_desktop\\Fairbanks\\Research\\GrassRats\\Piezo_data\\RMS")

#rms <- read.csv("HighSucrose_14May2019_RMS_Day1_and2.csv")
rms_post <- read.csv("4WeekPhotoperiod_15Apr2019_RMS_Day1-9.csv")
rms_pre <- read.csv("2WeekAcclimation_26Mar2019_RMS_Day1-7.csv")


#### General functions ####
my_theme <- theme_classic(base_size = 30) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))

process.rms <- function(rms) {
  rms$Hour_Military <- rms$Hour

  for(i in 1:nrow(rms)) {
    if (rms$am_pm[i]=="PM" & rms$Hour[i]<12) {
      rms$Hour_Military[i] <- rms$Hour[i]+12
    }
    if (rms$am_pm[i]=="AM" & rms$Hour[i]==12) {
      rms$Hour_Military[i] <- 24
    }
    }

  m.rms <- melt(rms, id.vars=c("Day", "Month", "Year", "Hour_Military", "Minute", "Second"), 
              measure.vars=c("G9", "G5", "G14", "G16", "G18", "G10", "G8", "G2", "T10", "T5", "T17", "T12", "G4", "G1",
                             "G13", "G17", "G19", "G11", "G12",  "G3", "T9", "T11", "T19", "T13"))

  m.rms <- rename(m.rms, c("variable"="Individual", "value"="RMS"))
  
  m.rms$Treatment <- 0
  m.rms$Treatment[m.rms$Individual %in% c("G9", "G5", "G14", "G16", "G18", "G10", 
                                          "G8", "G2", "T10", "T5", "T17", "T12")] <- "Short"
  m.rms$Treatment[m.rms$Individual %in% c("G4", "G1", "G13", "G17", "G19", "G11", "G12", "G3", "T9", 
                                          "T11", "T19", "T13")] <- "Long"
  
  agg.rms_indiv <- aggregate(m.rms$RMS, by=list(m.rms$Individual, m.rms$Treatment, m.rms$Month, m.rms$Day, 
                                                m.rms$Hour_Military, m.rms$Minute), 
                             FUN="mean")
  names(agg.rms_indiv) <- c("Individual", "Treatment", "Month", "Day", "Hour", "Minute", "RMS_minute_mean")

  ## Summing the RMS values that were the mean of every minute, and then dividing the sum by 60, the no, of min in an hour
  agg.rms_hour <- aggregate(agg.rms_indiv$RMS_minute_mean, by=list(agg.rms_indiv$Individual, agg.rms_indiv$Treatment, 
                                                                   agg.rms_indiv$Month, agg.rms_indiv$Day, agg.rms_indiv$Hour), 
                            FUN = function(x) {S=sum(x)/60})
  names(agg.rms_hour) <- c("Individual", "Treatment", "Month", "Day", "Hour", "RMS_hour")
  
  
  agg.rms_indiv$RMS_capped <- agg.rms_indiv$RMS_minute_mean
  agg.rms_indiv$RMS_capped[agg.rms_indiv$RMS_minute_mean>4] <- 4

  ## Remove individual identity, mean across all individuals per minute of the day
  agg.rms <- aggregate(m.rms$RMS, by=list(m.rms$Treatment, m.rms$Month, m.rms$Day, m.rms$Hour_Military, m.rms$Minute), 
                       FUN= "mean")
  names(agg.rms) <- c("Treatment", "Month", "Day", "Hour", "Minute", "RMS_minute_mean")
 
}

  ## Plotting averages per hour
agg.rms_hour$DayMonth <- as.factor(paste0(agg.rms_hour$Day, "_", agg.rms_hour$Month))
for(i in unique(agg.rms_hour$Individual[agg.rms_hour$Treatment=="Long"])) {
    #windows()
    #png(file=paste0(i, "_4wk.png"),width = 1500, height=954)
    ggplot(agg.rms_hour[agg.rms_hour$Individual==i,], aes(as.factor(Hour), DayMonth)) + my_theme + 
      #facet_grid(Treatment~., scales="free_y") +
      geom_tile(aes(fill=RMS_hour), stat="identity") +
      scale_fill_gradient(name="RMS", low="white", high="red", limits=c(0,3.6)) + xlab("Hour") + ylab("Day") +
      scale_y_discrete(limits=rev(levels(agg.rms_hour$DayMonth))) +
    #scale_y_reverse(breaks=unique(agg.rms_hour$Day)) + 
      ggtitle(paste0(i, "_", "Long on 2 week acclimation")) +
      geom_vline(xintercept=c(5.5,18.5), col="red", size=1) +
      theme(plot.title = element_text(hjust=0.5), axis.text.x = element_text(size=15))
    ggsave(paste0(i, "_2wk.png"),width = 10.5, height=6)
    #dev.off()
}

  
for(i in unique(agg.rms_hour$Individual[agg.rms_hour$Treatment=="Short"])) {
    #windows()
    #png(file=paste0(i, "_4wk.png"),width = 1500, height=954)
    ggplot(agg.rms_hour[agg.rms_hour$Individual==i,], aes(as.factor(Hour), DayMonth)) + my_theme + 
      #facet_grid(Treatment~., scales="free_y") +
      geom_tile(aes(fill=RMS_hour), stat="identity") +
      scale_fill_gradient(name="RMS", low="white", high="red", limits=c(0,3.6)) + xlab("Hour") + ylab("Day") +
    scale_y_discrete(limits=rev(levels(agg.rms_hour$DayMonth))) +
    #scale_y_reverse(breaks=unique(agg.rms_hour$Day)) + 
      ggtitle(paste0(i, "_", "Short on 2 week acclimation")) +
      geom_vline(xintercept=c(9.5,14.5), col="red", size=1) +
      theme(plot.title = element_text(hjust=0.5), axis.text.x = element_text(size=15))
    ggsave(paste0(i, "_2wk.png"),width = 10.5, height=6)
    #dev.off()``
} 
  

process.rms(rms_pre)


ggplot(agg.rms_hour[agg.rms_hour$Individual=="G9",], aes(as.factor(Hour), Day)) + my_theme + 
  #facet_grid(Treatment~., scales="free_y") +
  geom_tile(aes(fill=RMS_hour), stat="identity") +
  scale_fill_gradient(name="RMS", low="white", high="red", limits=c(0,3.6)) + xlab("Hour") + ylab("Day") +
  scale_y_reverse(breaks=unique(agg.rms_hour$Day)) + ggtitle(paste0("G9", "_", agg.rms_hour$Treatment)) +
  geom_vline(xintercept=c(10,14), col="green", size=2) +
  theme(plot.title = element_text(hjust=0.5), axis.text.x = element_text(size=15))

## Mean with individuals accounted more yields a much more stark plot than mean without individuals
ggplot(agg.rms_indiv, aes(as.factor(Hour), Day)) + my_theme + facet_grid(Treatment~.) +
  geom_tile(aes(fill=RMS_capped), stat="identity") +
  geom_vline(xintercept=c(6,18), col="purple", size=1) + geom_vline(xintercept=c(10,14), col="green", size=1) +
  scale_fill_gradient(name="RMS", low="white", high="red") + xlab("Hour") + ylab("Day") +
  scale_y_reverse()

## Plot by individual
ggplot(agg.rms_indiv, aes(as.factor(Hour), Individual)) + my_theme + facet_grid(Treatment~., scales="free_y") +
  geom_tile(aes(fill=RMS_minute_mean), stat="identity") +
  scale_fill_gradient(name="RMS", low="white", high="red") + xlab("Hour") + ylab("Individual") + 
  ggtitle(paste0(agg.rms_indiv$Individual, "_", agg.rms_hour$Treatment))
  
#scale_y_discrete(limits = rev(levels(agg.rms_indiv$Individual)))
