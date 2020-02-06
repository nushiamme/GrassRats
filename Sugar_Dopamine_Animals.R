## Sugar consumption for dopamine pump animals
## Script started Feb 6, 2020
## Contact: Anusha Shankar, nushiamme<at>gmail<dot>com



## Read in required packages
require(plyr)
require(ggplot2)
require(gridExtra)
require(lme4)
require(lmerTest)
require(factoextra)
require(multcomp)
require(reshape)
require(forcats) ## for fct_rev function

## Sew working directory
setwd("E:\\Ex_Google_Drive\\Toshiba_desktop\\Fairbanks\\Research\\GrassRats\\Animal_data")

## Read in data files
conc_all <- read.csv("SugarConcTest_weights_05Feb2020.csv")

#### General functions ####
my_theme <- theme_classic(base_size = 30) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))

cols_dop <- c("grey60", "grey80", "yellow", "red", "black", "yellow", "red")


## Take out dopamine animals separately
conc <- conc_all[!is.na(conc_all$DopamineStage),]
conc$Fed_sugar_amt_g <- conc$Fed_sugarsoln_g_perday*conc$Sugar_conc 

## Order levels in the Dopamine column
conc$DopamineStage <- factor(conc$DopamineStage, levels=c("Pre", "HCS", "HCS_Surgery", "DopLO1", "DopHI1", "Saline", "DopLO2", "DopHI2"))
conc$IndivSexPhotoperiod <- paste0(conc$Indiv, "_", conc$Sex, "_", conc$Treatment)
conc$IndivSexPhotoperiod <- factor(conc$IndivSexPhotoperiod, levels=c("G6_F_Short", "T35_F_Short", "T37_F_Short",
                                          "T38_F_Long","R31_F_Long", "G7_F_Long",
                                          "R32_M_Short", "GR43_M_Short", "T39_M_Short",
                                          "R33_M_Long", "G32_M_Long", "GR44_M_Long"))

## Subset just values from experimental test
conc_8_all <- conc[conc$Pre_test=="Test" & conc$Sugar_conc==0.08,]
conc_8_all$Prop_sugar <- conc_8_all$Fed_sugarsoln_g_perday/
  (conc_8_all$Fed_sugarsoln_g_perday+conc_8_all$Fed_water_g_perday)
levels(conc_8_all$Treatment)[levels(conc_8_all$Treatment)=="Long"] <- "Neutral"
conc_8 <- conc_8_all[conc_8_all$Sugar_mmt_good=="Y" & conc_8_all$Water_mmt_good=="Y",]


### Histograms of sugar and water consumed to determine "reasonable" cut-offs
ggplot(conc_8_all[!is.na(conc_8_all$Fed_sugarsoln_g_perday),], aes(Fed_sugarsoln_g_perday)) + geom_histogram(aes(fill=Sugar_mmt_good)) + 
 my_theme + ggtitle("Fed Sugar soln")
#ggplot(conc_8[!is.na(conc_8$Fed_water_g),], aes(Fed_water_g)) + geom_histogram(aes(fill=Water_mmt_good)) + 
# my_theme + ggtitle("Fed water")


conc_8_sugar <- subset(conc_8, !is.na(Fed_sugarsoln_g_perday))
conc_8_sugar <- conc_8_sugar[conc_8_sugar$Sugar_mmt_good=="Y",]

conc_8_water <- subset(conc_8, !is.na(Fed_water_g_perday))
conc_8_water <- conc_8_water[conc_8_water$Water_mmt_good=="Y",]


## 8% Sugar solution consumed
sugar_soln_g <- ggplot(conc_8_sugar[!is.na(conc_8_sugar$Treatment),], aes(IndivSugarDay, Fed_sugarsoln_g_perday)) + 
  facet_grid(.~Treatment, scales="free_x") +
  my_theme + geom_boxplot(aes(fill=Treatment)) + #ylim(0,75) +
  scale_fill_hue(h = c(100, 270)) +
  theme(axis.text.x = element_text(angle=90, size=15, vjust=0.5)) + theme(legend.position = "none") +
  #guides(fill=guide_legend(title="Sugar \nconcentration")) +
  ylab("Sugar solution consumed (g)") #+ xlab("Individual_SugarConc_ExptDay")

water_g <- ggplot(conc_8_water[!is.na(conc_8_water$Treatment),], aes(IndivSugarDay, Fed_water_g_perday)) + my_theme + 
  geom_boxplot(aes(fill=Treatment)) + facet_grid(.~Treatment, scales="free_x") +
  scale_fill_hue(h = c(100, 270)) +
  theme(axis.text.x = element_text(angle=90, size=15, vjust=0.5)) + theme(legend.position = "none") +
  #guides(fill=guide_legend(title="Sugar \nconcentration")) +
  ylab("Water consumed (g)") #+ xlab("Individual_SugarConc_ExptDay")

grid.arrange(sugar_soln_g, water_g, nrow=2)


prop_sugar <- ggplot(conc_8[!is.na(conc_8$Prop_sugar),], aes(as.factor(Chamber), Prop_sugar)) + my_theme + 
  geom_boxplot(aes(fill=Treatment)) + #facet_grid(.~sugar_conc_factor, scales="free_x") +
  scale_fill_hue(h = c(100, 270)) +
  theme(axis.text.x = element_text(angle=90, size=20, vjust=0.5), legend.text = element_text(size=20),
        legend.key.height = unit(3,"line")) +
  #guides(fill=guide_legend(title="Sugar \nconcentration")) +
  ylab("Proportion sugar soln consumed") + xlab("Chamber") #+ xlab("Individual_SugarConc_ExptDay")
prop_sugar

## Plot sugar consumption (quantity of sugar in grams) for 8% by treatment
ggplot(conc_8_sugar, aes(Treatment, Fed_sugar_amt_g)) + 
  facet_grid(.~sugar_conc_factor, scales="free_x") +
  my_theme + geom_boxplot(aes(fill=Treatment)) + #ylim(0,75) +
  scale_fill_manual(values = c("#F38BA8", "#23988aff")) +
  theme(legend.key.height = unit(3,"line")) +
  #guides(fill=guide_legend(title="Sugar \nconcentration")) + 
  ylab("Quantity of sugar consumed (g)") + xlab("Photoperiod")


ggplot(conc_8[!is.na(conc_8$Prop_sugar) & conc_8$Pre_test=="Test",], aes(DaySinceStart, Prop_sugar)) + my_theme + 
  geom_line(aes(col=Indiv), size=2) + facet_grid(.~sugar_conc_factor, scales="free_x") +
  scale_fill_hue(h = c(100, 270)) +
  theme(axis.text.x = element_text(angle=90, size=20, vjust=0.5), legend.text = element_text(size=20),
        legend.key.height = unit(3,"line")) +
  #guides(fill=guide_legend(title="Sugar \nconcentration")) +
  ylab("Proportion sugar soln consumed") + xlab("Chamber") #+ xlab("Individual_SugarConc_ExptDay")


prop_sugar_boxplot <- ggplot(conc_8[!is.na(conc_8$Prop_sugar),], aes(Treatment, Prop_sugar)) + my_theme + 
  geom_boxplot(aes(fill=Treatment)) + 
  #facet_grid(.~sugar_conc_factor, scales="free_x") +
  #scale_fill_hue(h = c(100, 270)) +
  scale_fill_manual(values = c("#F38BA8", "#23988aff")) +
  theme(axis.text.x = element_text(angle=90, size=20, vjust=0.5), legend.text = element_text(size=20),
        legend.key.height = unit(3,"line")) +
  #guides(fill=guide_legend(title="Sugar \nconcentration")) +
  ylab("Proportion sugar soln consumed") + xlab("Treatment") #+ xlab("Individual_SugarConc_ExptDay")
prop_sugar_boxplot


prop_sugar_indivs <- ggplot(conc_8[!is.na(conc_8$Prop_sugar),], aes(DopamineStage, Prop_sugar)) + my_theme + 
  geom_boxplot(aes(fill=Indiv), alpha=0.3) + geom_line(aes(group=Indiv, col=Indiv)) +
  #scale_color_manual(values = c("#F38BA8", "#23988aff")) +
  theme(axis.text.x = element_text(angle=90, size=20, vjust=0.5), legend.text = element_text(size=20),
        legend.key.height = unit(3,"line")) +
  #guides(fill=guide_legend(title="Sugar \nconcentration")) +
  ylab("Proportion sugar soln consumed") + xlab("Treatment") #+ xlab("Individual_SugarConc_ExptDay")
prop_sugar_indivs


prop_sugar_boxPhase_all <- ggplot(conc_8_all[!is.na(conc_8_all$Prop_sugar),], aes(DopamineStage, Prop_sugar)) + my_theme + 
  geom_boxplot(aes(fill=DopamineStage)) + geom_point(aes(col=Indiv),show.legend = F, size=2) +
  facet_grid(.~Sex, scales = "free_x") +
  scale_fill_manual(values = cols_dop) +
  theme(axis.text.x = element_text(angle=90, size=20, vjust=0.5), legend.text = element_text(size=20),
        legend.key.height = unit(3,"line")) +
  #guides(fill=guide_legend(title="Sugar \nconcentration")) +
  ylab("Proportion sugar soln consumed") #+ xlab("Treatment") #+ xlab("Individual_SugarConc_ExptDay")
prop_sugar_boxPhase_all

prop_sugar_boxPhase_good <- ggplot(conc_8[!is.na(conc_8$Prop_sugar),], aes(DopamineStage, Prop_sugar)) + my_theme + 
  geom_boxplot(aes(fill=DopamineStage)) + geom_point(aes(col=Indiv),show.legend = F, size=2) +
  facet_grid(.~Sex, scales = "free_x") +
  scale_fill_manual(values = cols_dop) +
  theme(axis.text.x = element_text(angle=90, size=20, vjust=0.5), legend.text = element_text(size=20),
        legend.key.height = unit(3,"line")) +
  #guides(fill=guide_legend(title="Sugar \nconcentration")) +
  ylab("Proportion sugar soln consumed") #+ xlab("Treatment") #+ xlab("Individual_SugarConc_ExptDay")
prop_sugar_boxPhase_good


prop_sugar_indivs_day_all <- ggplot(conc_8_all[!is.na(conc_8_all$Prop_sugar),], aes(DaySinceStart, Prop_sugar)) + my_theme + 
  geom_line(aes(group=IndivSexPhotoperiod), show.legend = F, size=1.5) + 
  geom_point(aes(fill=DopamineStage), col="black", size=3, pch=21) +
  facet_wrap(.~IndivSexPhotoperiod, scales = "free_x") +
  scale_fill_manual(values = cols_dop) +
  theme(axis.text.x = element_text(angle=90, size=20, vjust=0.5), legend.text = element_text(size=20),
        legend.key.height = unit(3,"line")) +
  ylab("Proportion sugar soln consumed") #+ xlab("Treatment")
prop_sugar_indivs_day_all

prop_sugar_indivs_day_good <- ggplot(conc_8[!is.na(conc_8$Prop_sugar),], aes(DaySinceStart, Prop_sugar)) + my_theme + 
  geom_line(aes(group=Indiv), show.legend = F, size=1.5) + 
  geom_point(aes(fill=DopamineStage), col="black", size=3, pch=21) +
  facet_wrap(.~IndivSexPhotoperiod, scales = "free_x") +
  #scale_color_manual(values = cols_dop) +
  scale_fill_manual(values = cols_dop) +
  theme(axis.text.x = element_text(angle=90, size=20, vjust=0.5), legend.text = element_text(size=20),
        legend.key.height = unit(3,"line")) +
  ylab("Proportion sugar soln consumed") #+ xlab("Treatment")
prop_sugar_indivs_day_good


