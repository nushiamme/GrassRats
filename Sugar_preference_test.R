## Grass rat sucrose concentration preference test

setwd("E:\\Google Drive\\Toshiba_desktop\\Fairbanks\\Research\\GrassRats\\Piezo_data")

conc <- read.csv("SugarConcTest_weights.csv")

#### General functions ####
my_theme <- theme_classic(base_size = 30) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))

## Remove negative values,
conc_sugar <- conc[conc$Fed_sugar_g>-1,]
conc_water <- conc[conc$Fed_water_g>-1,]


## Plot sugar consumption
ggplot(subset(conc_sugar, !is.na(Fed_sugar_g)), aes(IndivSugarDay, Fed_sugar_g)) + my_theme + geom_bar(stat="identity", aes(fill=Sugar_conc)) +
  scale_fill_hue(h = c(100, 270)) +
  theme(axis.text.x = element_text(angle=90, size=15, vjust=0.5)) + guides(fill=guide_legend(title="Sugar \nconcentration")) +
  ylab("Sugar solution consumed (g)") + xlab("Individual_SugarConc_ExptDay")

ggplot(subset(conc_water, !is.na(Fed_water_g)), aes(IndivSugarDay, Fed_water_g)) + my_theme + geom_bar(stat="identity", aes(fill=Sugar_conc)) +
  scale_fill_hue(h = c(100, 270)) +
  theme(axis.text.x = element_text(angle=90, size=15, vjust=0.5)) + guides(fill=guide_legend(title="Sugar \nconcentration")) +
  ylab("Water consumed (g)") + xlab("Individual_SugarConc_ExptDay")
