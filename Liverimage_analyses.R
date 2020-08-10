## Analyzing liver images from Janelia-scanned slides, and using ImageJ
## Slides scanned by: Boaz Mohar
## Author: Anusha Shankar, nushiamme@gmail.com

library(ggplot2)
library(gridExtra)
library(plyr)

## General functions
my_theme <- theme_classic(base_size = 15) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))

my_theme2 <- theme_classic(base_size = 30) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))

my_colors <- c("#23988aff", "#F38BA8", "#440558ff", "#9ed93aff")

## Set wd and read in files
setwd("E:/Google Drive/GrassRats/GrassRat_Liver_Analyses")
#setwd("E:/Ex_Google_Drive/Toshiba_desktop/Fairbanks/Research/GrassRats/GrassRat_Liver_Analyses")
#setwd("C:/Users/nushi/Desktop/")
#liver_summaries <- read.csv("All_Slides_AnalyzeParticles_summary.csv") ## Just phase 1

##Formatting for microbiome metadata for Devin
for_ubiome <- read.csv("Liver_Phase_PercentArea_Chamber.csv")
agg.for_ubiome<- aggregate(for_ubiome[,"PercentArea"], list(for_ubiome$ID), mean)
names(agg.for_ubiome) <- c("ID", "PercentArea")
write.csv(agg.for_ubiome, "Agg_liverfat.csv")



liver_summaries <- read.csv("BothPhases_AnalyzeParticles_summary.csv")
liver_key <- read.csv("Liver_Slide_Key.csv")
anim <- read.csv("Chamber_Animal_IDs.csv")

liver_merge <- merge(liver_summaries, liver_key, by = "Phase_Slide_Casette_Region")
m.liver <- merge(liver_merge, anim, by = "ID")

m.liver_filter <- m.liver[m.liver$Use=="Y",]

m.liver_filter$Sugar <- as.factor(as.character(m.liver_filter$Sugar))

m.liver_filter$Sugar_conc <- "NA"
m.liver_filter$Sugar_conc[m.liver_filter$Sugar=="No sugar"] <- "None"
m.liver_filter$Sugar_conc[m.liver_filter$Sugar=="High sugar"] <- "High"
m.liver_filter$PhotoSugar <- paste0(m.liver_filter$Photoperiod, m.liver_filter$Sugar_conc)

#write.csv(m.liver_filter, file = "Merged_Liver.csv")

### Plots ###
liv_density <- ggplot(m.liver_filter, aes(PercentArea)) + geom_density(aes(col=PhotoSugar)) + my_theme2 +
  scale_fill_manual(values=c(100, 200)) +
  theme(legend.key.height = unit(3, 'lines'), legend.position = "none") +
  ylab("Percent area of liver with Macrosteatosis")

##Both phases separate
liv_boxplot_phase <-ggplot(m.liver_filter, aes(PhotoSugar, PercentArea)) + 
  geom_boxplot(aes(fill=PhotoSugar)) + my_theme2 + facet_grid(.~Phase)  +
  #scale_fill_manual(values=c(100, 200)) +
  theme(legend.key.height = unit(3, 'lines'), 
        axis.text.x = element_text(angle=30, hjust=0.5, vjust=0.8)) + 
  geom_point() +
  ylab("Percent area of liver with Macrosteatosis")
liv_boxplot_phase

##Both phases together
liv_boxplot_overall <-ggplot(m.liver_filter, aes(PhotoSugar, PercentArea)) + 
  geom_boxplot(aes(fill=PhotoSugar)) + my_theme2 + 
  theme(legend.key.height = unit(3, 'lines'), 
        axis.text.x = element_text(angle=30, hjust=0.5, vjust=0.8)) + 
  geom_point() +
  ylab("Percent area of liver with Macrosteatosis")
liv_boxplot_overall

##Both phases together, liver fat vs age at death
liv_loess_phase <-ggplot(m.liver_filter, aes(AgeAtDeath_days, PercentArea)) + 
  geom_smooth(method = "loess", aes(fill=PhotoSugar)) + 
  my_theme2 + facet_grid(.~Phase, scales = "free_x") + 
  theme(legend.key.height = unit(3, 'lines'), 
        axis.text.x = element_text(angle=30, hjust=0.5, vjust=0.8)) + 
  geom_point() +
  ylab("Percent area of liver with Macrosteatosis")
liv_loess_phase

##Microsteatosis
liv_boxplot_micro <-ggplot(m.liver_filter, aes(PhotoSugar, Microsteatosis_0min_4max)) + 
  geom_boxplot(aes(fill=PhotoSugar)) + my_theme2 +  facet_grid(.~Phase)  +
  theme(legend.key.height = unit(3, 'lines'), 
        axis.text.x = element_text(angle=30, hjust=0.5, vjust=0.8)) + 
  geom_point() +
  ylab("Level of Microsteatosis")
liv_boxplot_micro

liv_macro_micro <-ggplot(m.liver_filter, aes(Microsteatosis_0min_4max, PercentArea)) + 
  my_theme2 +  facet_grid(Phase~., scales = "free_y") +
  geom_boxplot(aes(group=Microsteatosis_0min_4max)) +
  theme(legend.key.height = unit(3, 'lines'), 
        axis.text.x = element_text(angle=30, hjust=0.5, vjust=0.8)) + 
  geom_point(aes(col=PhotoSugar), size=3) +
  scale_color_manual(values = my_colors) +
  xlab("Level of Microsteatosis") + ylab("Percent area of liver with Macrosteatosis")
liv_macro_micro



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