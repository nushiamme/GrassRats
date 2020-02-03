## Analyzing liver images from Janelia-scanned slides, and using ImageJ
## Author: Anusha Shankar, nushiamme@gmail.com

library(ggplot2)
library(gridExtra)

## General functions
my_theme <- theme_classic(base_size = 15) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))

my_theme2 <- theme_classic(base_size = 30) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))

## Set wd and read in files
setwd("E:/Ex_Google_Drive/Toshiba_desktop/Fairbanks/Research/GrassRats/GrassRat_Liver_Analyses")
#setwd("C:/Users/nushi/Desktop/")
liver_summaries <- read.csv("All_Slides_AnalyzeParticles_summary.csv")
liver_key <- read.csv("Liver_Slide_Key.csv")
anim <- read.csv("Chamber_Animal_IDs.csv")

liver_merge <- merge(liver_summaries, liver_key, by = "Slide_Casette_Region")
m.liver <- merge(liver_merge, anim, by = "ID")

m.liver_filter <- m.liver[m.liver$Use=="Y",]

m.liver_filter$Sugar <- as.factor(as.character(m.liver_filter$Sugar))

liv_boxplot <-ggplot(m.liver_filter, aes(PhotoSugar, PercentArea)) + geom_boxplot(aes(fill=PhotoSugar)) + my_theme2 +
  #scale_fill_manual(values=c(100, 200)) +
  theme(legend.key.height = unit(3, 'lines')) + geom_point() +
  ylab("Percent area of liver with Macrosteatosis")

m.liver_filter$Sugar_conc <- "NA"
m.liver_filter$Sugar_conc[m.liver_filter$Sugar=="No sugar"] <- "None"
m.liver_filter$Sugar_conc[m.liver_filter$Sugar=="High sugar"] <- "High"
m.liver_filter$PhotoSugar <- paste0(m.liver_filter$Photoperiod, m.liver_filter$Sugar_conc)
liv_density <- ggplot(m.liver_filter, aes(PercentArea)) + geom_density(aes(col=PhotoSugar)) + my_theme2 +
  scale_fill_manual(values=c(100, 200)) +
  theme(legend.key.height = unit(3, 'lines'), legend.position = "none") +
  ylab("Percent area of liver with Macrosteatosis")


ggplot(m.liver_filter, aes(Photoperiod, Count)) + geom_boxplot(aes(fill=PhotoSugar)) + my_theme2 +
  #scale_fill_manual(values=c(100, 200)) + 
  theme(legend.key.height = unit(3, 'lines')) + geom_point(aes(col=PhotoSugar)) +
  ylab("Number of Macrosteatotic vesicles")

grid.arrange(liv_density, liv_boxplot, nrow=1, ncol=2)

ggplot(m.liver_filter, aes(Photoperiod, AverageSize)) + geom_boxplot(aes(fill=Sugar)) + my_theme2 +
  scale_fill_manual(values = c("#F38BA8", "#23988aff")) +
  theme(legend.key.height = unit(3, 'lines')) +
  ylab("Average size of Macrosteatotic vesicles (in^2)")

lmer()