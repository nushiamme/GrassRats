## Grass rat sucrose consumption, liver fat, fat pad, and mass change analyses
## Anusha Shankar nushiamme<at>gmail<dot>com
## Started Summer 2019
## First sugar plots and models, then liver and fat pad, then animal weights


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


##### TRY tabmodel() to summarize models, library(sjplot)
##### library(eins) and specmeans emeans() calculates least squares marginal means

## Sew working directory
setwd("E:\\Google Drive\\Anusha_personal\\Fairbanks\\Research\\GrassRats\\Animal_data")

## Read in data files
conc <- read.csv("SugarConcTest_weights_05Feb2020.csv")
#liver <- read.csv("Liver_fat.csv") # OLD, delete from rest of script
#liver_rescore <- read.csv("Liver_fat_AS.csv") # OLD, delete from rest of script
fatpad <- read.csv("Fat_pads.csv")
weights <- read.csv("Animal_weights_forMassChange.csv")


#### General functions ####
my_theme <- theme_classic(base_size = 30) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))


## Calculate amount of sugar consumed
#conc$prop_sugar <- as.numeric(conc$Sugar_conc)
## On Sept 15, 2019, changed from Fed_sugarsoln_g to Fed_sugarsoln_g_perday
conc$Fed_sugar_amt_g <- conc$Fed_sugarsoln_g_perday*conc$Sugar_conc 
#conc$Indiv <- factor(conc$Indiv, levels=c("N1", "N2", "BK3", "BK4"))
#conc <- arrange(conc, sugar_conc_factor, Indiv)

## Remove negative values, and make separate data frames for sugar and water
#conc_sugar <- conc[conc$Fed_sugar_amt_g>-1 & conc$Pre_test=="Test",]
#conc_water <- conc[conc$Fed_water_g>-1 & conc$Pre_test=="Test",]

## Subset 2 and 8%
conc_2_8 <- conc[conc$Pre_test=="Test" & conc$Sugar_conc %in% c(0.00, 0.02, 0.08),]

conc_2_8$sugar_conc_factor<- factor(conc_2_8$Sugar_conc, levels=c(0.00, 0.02, 0.08))
conc_2_8 <- conc_2_8[conc_2_8$Sugar_mmt_good=="Y" & conc_2_8$Water_mmt_good=="Y",]
conc_2_8$Prop_sugar <- conc_2_8$Fed_sugarsoln_g_perday/(conc_2_8$Fed_sugarsoln_g_perday+conc_2_8$Fed_water_g_perday)


conc_2_8_sugar <- subset(conc_2_8, !is.na(Fed_sugarsoln_g_perday))
conc_2_8_sugar <- conc_2_8_sugar[conc_2_8_sugar$Sugar_mmt_good=="Y",]

conc_2_8_water <- subset(conc_2_8, !is.na(Fed_water_g_perday))
conc_2_8_water <- conc_2_8_water[conc_2_8_water$Water_mmt_good=="Y",]

## Subset just 2%
conc_2 <- conc[conc$Pre_test=="Test" & conc$Sugar_conc==0.02,]
conc_2_sugar <- subset(conc_2, !is.na(Fed_sugarsoln_g_perday))
conc_2_sugar <- conc_2_sugar[conc_2_sugar$Sugar_mmt_good=="Y",]
conc_2_water <- subset(conc_2, !is.na(Fed_water_g_perday))
conc_2_water <- conc_2_water[conc_2_water$Water_mmt_good=="Y",]

## consumption threshold 
ggplot(conc_2, aes(Fed_sugarsoln_g)) + 
  geom_histogram(aes(fill=Sugar_mmt_good)) + my_theme 

conc_2_good <- conc_2[conc_2$Sugar_mmt_good=="Y" & conc_2$Water_mmt_good=="Y",]
conc_2_good$Prop_sugar <- conc_2_good$Fed_sugarsoln_g/(conc_2_good$Fed_sugarsoln_g+conc_2_good$Fed_water_g)


## Subset just values from experimental test
conc_8 <- conc[conc$Pre_test=="Test" & conc$Sugar_conc %in% c(0.00, 0.08),]
conc_8$sugar_conc_factor<- factor(conc_8$Sugar_conc, levels=c(0, 0.08))
conc_8 <- conc_8[conc_8$Sugar_mmt_good=="Y" & conc_8$Water_mmt_good=="Y",]
conc_8$Prop_sugar <- conc_8$Fed_sugarsoln_g_perday/(conc_8$Fed_sugarsoln_g_perday+conc_8$Fed_water_g_perday)
levels(conc_8$Treatment)[levels(conc_8$Treatment)=="Long"] <- "Neutral"


## Take just phase 1 and 2 animals, leave out dopamine. Both 'reasonable (<30g consumed per day) and 
## unreasonable values. 
conc_8_all_phase1_2 <- conc[conc$Pre_test=="Test" & conc$Sugar_conc %in% c(0.00, 0.08) 
                            & is.na(conc$DopamineStage),]
##Create column for Treatment*Sugar_conc
conc_8_all_phase1_2$PhotoperiodSugarConc <- paste0(conc_8_all_phase1_2$Treatment, "_", 
                                                   conc_8_all_phase1_2$Sugar_conc)
## And column for IndivSexPhotoSugar
conc_8_all_phase1_2$IndivSexPhotoSugar <- paste0(conc_8_all_phase1_2$Indiv, "_", 
                                                   conc_8_all_phase1_2$Sex, "_",
                                                   conc_8_all_phase1_2$PhotoperiodSugarConc)
conc_8_all_phase1_2$Prop_sugar <- conc_8_all_phase1_2$Fed_sugarsoln_g_perday/
  (conc_8_all_phase1_2$Fed_sugarsoln_g_perday+conc_8_all_phase1_2$Fed_water_g_perday)


### Histograms of sugar and water consumed to determine "reasonable" cut-offs
#ggplot(conc_8[!is.na(conc_8$Fed_sugarsoln_g),], aes(Fed_sugarsoln_g)) + geom_histogram(aes(fill=Sugar_mmt_good)) + 
 # my_theme + ggtitle("Fed Sugar soln")
#ggplot(conc_8[!is.na(conc_8$Fed_water_g),], aes(Fed_water_g)) + geom_histogram(aes(fill=Water_mmt_good)) + 
 # my_theme + ggtitle("Fed water")

conc_8_sugar <- subset(conc_8, !is.na(Fed_sugarsoln_g_perday))
conc_8_sugar_good <- conc_8_sugar[conc_8_sugar$Sugar_mmt_good=="Y",]

conc_8_water <- subset(conc_8, !is.na(Fed_water_g_perday))
conc_8_water_good <- conc_8_water[conc_8_water$Water_mmt_good=="Y",]

## Plot sugar consumption (quantity of sugar in grams) for 2%
sugar_g_2 <- ggplot(conc_2, aes(IndivSugarDay, Fed_sugar_amt_g)) + 
  #facet_grid(.~Treatment, scales="free_x") +
  my_theme + geom_bar(stat="identity") + #ylim(0,75) +
  scale_fill_hue(h = c(100, 270)) + 
  theme(axis.text.x = element_text(angle=90, size=15, vjust=0.5), plot.title = element_text(hjust=0.5)) +
  theme(legend.position = "none") +
  #guides(fill=guide_legend(title="Sugar \nconcentration")) + 
  ylab("Quantity of Sugar consumed (g)") + xlab("Individual_SugarConc_ExptDay") +
  ggtitle("2% sucrose")

## Plot sugar consumption (quantity of sugar in grams) for 2% by treatment
sugar_g_2 <- ggplot(conc_2_sugar, aes(IndivSugarDay, Fed_sugar_amt_g)) + 
  facet_grid(.~Treatment, scales="free_x") +
  my_theme + geom_bar(stat="identity", aes(fill=Treatment)) + #ylim(0,75) +
  scale_fill_hue(h = c(100, 270)) + 
  theme(axis.text.x = element_text(angle=90, size=15, vjust=0.5), plot.title = element_text(hjust=0.5)) +
  theme(legend.position = "none") +
  #guides(fill=guide_legend(title="Sugar \nconcentration")) + 
  ylab("Quantity of Sugar consumed (g)") + xlab("Individual_SugarConc_ExptDay") +
  ggtitle("2% sucrose")

## Plot sugar soln consumption for 2% by treatment
sugar_soln_g_2 <- ggplot(conc_2_sugar, aes(IndivSugarDay, Fed_sugarsoln_g)) + 
  facet_grid(.~Treatment, scales="free_x") +
  my_theme + geom_bar(stat="identity", aes(fill=Treatment)) + #ylim(0,75) +
  scale_fill_hue(h = c(100, 270)) + 
  theme(axis.text.x = element_text(angle=90, size=15, vjust=0.5), plot.title = element_text(hjust=0.5)) +
  theme(legend.position = "none") +
  #guides(fill=guide_legend(title="Sugar \nconcentration")) + 
  ylab("Sugar solution consumed (g)") + xlab("Individual_SugarConc_ExptDay") +
  ggtitle("2% sucrose")

## Plot water consumption for 2% by tretament
water_g_2 <- ggplot(conc_2_water, aes(IndivSugarDay, Fed_water_g)) + 
  facet_grid(.~Treatment, scales="free_x") +
  my_theme + geom_bar(stat="identity", aes(fill=Treatment)) + #ylim(0,75) +
  scale_fill_hue(h = c(100, 270)) + 
  theme(axis.text.x = element_text(angle=90, size=15, vjust=0.5), plot.title = element_text(hjust=0.5)) +
  theme(legend.position = "none") +
  #guides(fill=guide_legend(title="Sugar \nconcentration")) + 
  ylab("Water consumed (g)") + xlab("Individual_SugarConc_ExptDay") +
  ggtitle("Water")

grid.arrange(sugar_soln_g_2, water_g_2, nrow=2)

sugar_prop_g_2 <- ggplot(conc_2_good, aes(IndivSugarDay, Prop_sugar)) + 
  facet_grid(.~Treatment, scales="free_x") +
  my_theme + geom_bar(stat="identity", aes(fill=Treatment)) + #ylim(0,75) +
  scale_fill_hue(h = c(100, 270)) + 
  theme(axis.text.x = element_text(angle=90, size=15, vjust=0.5), plot.title = element_text(hjust=0.5)) +
  theme(legend.position = "none") +
  #guides(fill=guide_legend(title="Sugar \nconcentration")) + 
  ylab("Proportion of sugar soln consumed (g)") + xlab("Individual_SugarConc_ExptDay") +
  ggtitle("2% sucrose")

sugar_prop_g_2_boxplot <- ggplot(conc_2_good, aes(Treatment, Prop_sugar)) + 
  #facet_grid(.~Treatment, scales="free_x") +
  my_theme + geom_boxplot(aes(fill=Treatment)) + 
  geom_point(size=3) + #ylim(0,75) +
  scale_fill_hue(h = c(100, 270)) + 
  theme(axis.text.x = element_text(angle=90, size=15, vjust=0.5), plot.title = element_text(hjust=0.5)) +
  theme(legend.position = "none") +
  #guides(fill=guide_legend(title="Sugar \nconcentration")) + 
  ylab("Proportion of sugar soln consumed (g)") + xlab("Individual_SugarConc_ExptDay") +
  ggtitle("2% sucrose")

## Plot sugar consumption (quantity of sugar in grams) for 4 and 6%
sugar_g_4_6 <- ggplot(conc_sugar[conc_sugar$Sugar_conc %in% c(0.04, 0.06),], aes(IndivSugarDay, Fed_sugar_amt_g)) + 
  facet_grid(sugar_conc_factor~Treatment, scales = "free_x") +
  my_theme + geom_bar(stat="identity", aes(fill=Treatment)) + #ylim(0,75) +
  scale_fill_hue(h = c(100, 270)) + 
  theme(axis.text.x = element_text(angle=90, size=15, vjust=0.5), plot.title = element_text(hjust=0.5)) + 
  theme(legend.position = "none") +
  #guides(fill=guide_legend(title="Sugar \nconcentration")) + 
  ylab("Quantity of Sugar consumed (g)") + xlab("Individual_SugarConc_ExptDay") +
  ggtitle("4% and 6% sucrose")

## Plot sugar solution consumption for 4 and 6%
sugar_soln_g_4_6 <- ggplot(conc_sugar[conc_sugar$Sugar_conc %in% c(0.04, 0.06),], aes(IndivSugarDay, Fed_sugarsoln_g)) + 
  facet_grid(sugar_conc_factor~Treatment, scales = "free_x") +
  my_theme + geom_bar(stat="identity", aes(fill=Treatment)) + #ylim(0,75) +
  scale_fill_hue(h = c(100, 270)) + 
  theme(axis.text.x = element_text(angle=90, size=15, vjust=0.5), plot.title = element_text(hjust=0.5)) + 
  theme(legend.position = "none") +
  #guides(fill=guide_legend(title="Sugar \nconcentration")) + 
  ylab("Sugar solution consumed (g)") + xlab("Individual_SugarConc_ExptDay") +
  ggtitle("4% and 6% sucrose")

## Plot water consumption for 4 and 6%
water_g_4_6 <- ggplot(conc_water[conc_water$Sugar_conc %in% c(0.04, 0.06),], aes(IndivSugarDay, Fed_water_g)) + 
  facet_grid(sugar_conc_factor~Treatment, scales = "free_x") +
  my_theme + geom_bar(stat="identity", aes(fill=Treatment)) + #ylim(0,75) +
  scale_fill_hue(h = c(100, 270)) + 
  theme(axis.text.x = element_text(angle=90, size=15, vjust=0.5), plot.title = element_text(hjust=0.5)) + 
  theme(legend.position = "none") +
  #guides(fill=guide_legend(title="Sugar \nconcentration")) + 
  ylab("Water consumed (g)") + xlab("Individual_SugarConc_ExptDay") +
  ggtitle("4% and 6% sucrose")

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
  geom_boxplot(aes(fill=Treatment)) + facet_grid(.~sugar_conc_factor, scales="free_x") +
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
  facet_grid(.~sugar_conc_factor, scales="free_x") +
  #scale_fill_hue(h = c(100, 270)) +
  scale_fill_manual(values = c("#F38BA8", "#23988aff")) +
  theme(axis.text.x = element_text(angle=90, size=20, vjust=0.5), legend.text = element_text(size=20),
        legend.key.height = unit(3,"line")) +
  #guides(fill=guide_legend(title="Sugar \nconcentration")) +
  ylab("Proportion sugar soln consumed") + xlab("Treatment") #+ xlab("Individual_SugarConc_ExptDay")
prop_sugar_boxplot

#conc_2_8$sugar_conc_factor <- factor(conc_2_8$sugar_conc_factor, levels = c(0.02,0,0.08))
#conc_2_8$Treatment <- revalue(conc_2_8$Treatment, c("Long" = "Neutral"))
prop_sugar_2_8 <- ggplot(conc_2_8[!is.na(conc_2_8$Prop_sugar),], aes(Treatment, Prop_sugar)) + my_theme2 + 
  geom_boxplot(aes(fill=Treatment)) + facet_grid(.~sugar_conc_factor, scales="free_x") +
  geom_point() +
  scale_fill_manual(values = c("#F38BA8", "#23988aff")) +
  theme(axis.text.x = element_text(angle=60, size=20, vjust=0.5), legend.text = element_text(size=15),
        legend.key.height = unit(3,"line")) +
  #guides(fill=guide_legend(title="Sugar \nconcentration")) +
  ylab("Proportion sugar soln consumed") #+ xlab("Individual_SugarConc_ExptDay")
prop_sugar_2_8

prop_sugar_2_8_indiv <- ggplot(conc_2_8[!is.na(conc_2_8$Prop_sugar),], aes(Indiv, Prop_sugar)) + my_theme + 
  geom_boxplot(aes(fill=Treatment)) + facet_grid(Treatment~sugar_conc_factor, scales="free_x") +
  geom_point() +
  scale_fill_hue(h = c(100, 270)) +
  theme(axis.text.x = element_text(angle=60, size=20, vjust=0.5), legend.text = element_text(size=15),
        legend.key.height = unit(3,"line")) +
  #guides(fill=guide_legend(title="Sugar \nconcentration")) +
  ylab("Proportion sugar soln consumed") #+ xlab("Individual_SugarConc_ExptDay")
prop_sugar_2_8_indiv

## Boxplots incl unreasonable values
prop_sugar_boxplot <- ggplot(conc_8_all_phase1_2[!is.na(conc_8_all_phase1_2$Prop_sugar),], 
                             aes(Treatment, Prop_sugar)) + my_theme + 
  geom_boxplot(aes(fill=Treatment)) + 
  facet_grid(.~as.factor(Sugar_conc), scales="free_x") +
  #scale_fill_hue(h = c(100, 270)) +
  scale_fill_manual(values = c("#F38BA8", "#23988aff")) +
  theme(axis.text.x = element_text(angle=90, size=20, vjust=0.5), legend.text = element_text(size=20),
        legend.key.height = unit(3,"line")) +
  #guides(fill=guide_legend(title="Sugar \nconcentration")) +
  ylab("Proportion sugar soln consumed") + xlab("Treatment") #+ xlab("Individual_SugarConc_ExptDay")
prop_sugar_boxplot

## Line plots for consumption over time per individual
prop_sugar_indivs_day_all <- ggplot(conc_8_all_phase1_2[!is.na(conc_8_all_phase1_2$Prop_sugar),], 
                                    aes(DaySinceStart, Prop_sugar)) + my_theme + 
  geom_line(aes(group=Indiv), show.legend = F, size=1) + 
  geom_point(aes(fill=PhotoperiodSugarConc), size=3, pch=21) +
  facet_wrap(PhotoperiodSugarConc~., scales = "free_x") +
  scale_fill_viridis_d() + 
  theme(axis.text.x = element_text(angle=90, size=20, vjust=0.5), legend.text = element_text(size=20),
        legend.key.height = unit(3,"line")) +
  ylab("Proportion sugar soln consumed") #+ xlab("Treatment")
prop_sugar_indivs_day_all

prop_sugar_indivs_day_good <- ggplot(conc_8_all_phase1_2[!is.na(conc_8_all_phase1_2$Prop_sugar) &
                                                          conc_8_all_phase1_2$Sugar_mmt_good=="Y" &
                                                          conc_8_all_phase1_2$Water_mmt_good=="Y",], 
                                    aes(DaySinceStart, Prop_sugar)) + my_theme + 
  geom_line(aes(group=Indiv), show.legend = F, size=1) + 
  geom_point(aes(fill=PhotoperiodSugarConc), size=3, pch=21) +
  facet_wrap(PhotoperiodSugarConc~., scales = "free_x") +
  scale_fill_viridis_d() + 
  theme(axis.text.x = element_text(angle=90, size=20, vjust=0.5), legend.text = element_text(size=20),
        legend.key.height = unit(3,"line")) +
  ylab("Proportion sugar soln consumed") #+ xlab("Treatment")
prop_sugar_indivs_day_good

## Fed sugar amount in grams, line per indiv over time
Amt_sugar_indivs_day_all <- ggplot(conc_8_all_phase1_2[!is.na(conc_8_all_phase1_2$Fed_sugar_amt_g),], 
                                   aes(DaySinceStart, Fed_sugar_amt_g)) + my_theme + 
  geom_line(aes(group=Indiv), show.legend = F, size=1) + 
  geom_point(aes(fill=PhotoperiodSugarConc), size=3, pch=21) +
  facet_wrap(PhotoperiodSugarConc~., scales = "free_x") +
  scale_fill_manual(values = cols_dop) +
  theme(axis.text.x = element_text(angle=90, size=20, vjust=0.5), legend.text = element_text(size=20),
        legend.key.height = unit(3,"line")) +
  ylab("Amount of sugar consumed (g)") #+ xlab("Treatment")
Amt_sugar_indivs_day_all



#### Liver and fat
m.liver_rescore <- melt(liver_rescore, id.vars=c("Animal_ID", "Photoperiod", "Sugar_conc"), 
                        measure.vars=c("Steatosis_rating1", "Steatosis_Size1", "Other_white1",
                                       "Steatosis_rating2", "Steatosis_Size2", "Other_white2"))

ggplot(liver_rescore, aes(Photoperiod, Steatosis_rating, fill=Sugar_conc)) + my_theme +
  geom_boxplot(alpha=0.3) + 
  geom_point(aes(size=Steatosis_Size, col=Sugar_conc), position = "jitter") +
  scale_fill_manual(values=c(100, 200)) + scale_color_manual(values=c(100,200))

ggplot(liver_rescore, aes(Animal_ID, Steatosis_rating, col=Photoperiod)) + 
  facet_grid(.~Sugar_conc, scales="free") +
  #geom_boxplot(alpha=0.8) + 
  geom_point(aes(size=Steatosis_Size))+ my_theme +
  scale_fill_hue(h = c(100, 270))

mod_liver <- lm(Liver_fat~Photoperiod*Sugar_conc, liver)
anova(mod_liver)
summary(mod_liver)
plot(mod_liver)
coef(mod_liver)
plot(residuals(mod_liver))

liver_cut <- liver[!is.na(liver$Liver_fat),]
ggplot(liver_cut, aes(Photoperiod, Liver_fat, fill=Sugar_conc)) + geom_boxplot(alpha=0.8) + my_theme +
  scale_fill_hue(h = c(100, 270))

fatpad$HCS <- revalue(fatpad$HCS, c("N"="0%", "Y"="8%"))
ggplot(fatpad, aes(Treatment, Fat_pad_wt_g, fill=HCS)) + geom_boxplot(alpha=0.8) + my_theme +
  scale_fill_hue(h = c(100, 270)) + ylab("Fat pad wt (g)")

mod_fatpad <- lm(Fat_pad_wt_g~Treatment*HCS, fatpad)

#### Sugar models
mod_2_8_Day <- lmer(Prop_sugar~sugar_conc_factor+Treatment+(1|Indiv)+(1|DaySinceStart)+
                      Animal_wt_g, data=conc_2_8)

mod_2_8 <- lmer(Prop_sugar~sugar_conc_factor+Treatment+(1|Indiv), data=conc_2_8)

summary(mod_2_8)
coef(mod_2_8)
anova(mod_2_8)

mod_2_8_noIndiv <- lm(Prop_sugar~sugar_conc_factor+Treatment, data=conc_2_8)

mod_2_8_noIndiv_noTreatment <- lm(Prop_sugar~sugar_conc_factor, data=conc_2_8)
#plot(allEffects(mod_2_8_noIndiv_noTreatment))

## Best model here
mod_2_8_noTreatment <- lmer(Prop_sugar~sugar_conc_factor+(1|Indiv), data=conc_2_8)#[!is.na(conc_2_8$sugar_conc_factor),])
summary(mod_2_8_noTreatment)
anova(mod_2_8_noTreatment)
qqnorm(resid(mod_2_8_noTreatment))
qqline(resid(mod_2_8_noTreatment))
coef(mod_2_8_noTreatment)

tmp_sugar <- as.data.frame(confint(glht(mod_2_8_noIndiv_noTreatment))$confint)
tmp_sugar$Comparison <- rownames(tmp_sugar)
tmp_sugar$Comparison <- revalue(as.factor(tmp_sugar$Comparison),c("(Intercept)" = "Intercept", 
                                                                  "sugar_conc_factor0.02" = "2% sucrose", 
                                                                  "sugar_conc_factor0.08" = "8% sucrose"))


ggplot(tmp_sugar, aes(x = Comparison, y = Estimate, ymin = lwr, ymax = upr)) + my_theme +
  #theme(axis.text.x = element_text(angle=30, vjust=0.7)) +
  geom_errorbar() + geom_point()

mod_2_8_noTreatment_Wt <- lmer(Prop_sugar~sugar_conc_factor+(1|Indiv)+Animal_wt_g, data=conc_2_8)
summary(mod_2_8_noTreatment_Wt)
anova(mod_2_8_noTreatment_Wt)
qqnorm(resid(mod_2_8_noTreatment_Wt))
qqline(resid(mod_2_8_noTreatment_Wt))
coef(mod_2_8_noTreatment_Wt)
library(effects)
plot(allEffects(mod_2_8_noTreatment_Wt)) ## requires 'effects' package

anova(mod_2_8_Day, mod_2_8, mod_2_8_noIndiv, mod_2_8_noIndiv_noTreatment, mod_2_8_noTreatment,
      mod_2_8_noTreatment_Wt)

# Regression of sugar soln vs water
ggplot(conc_8, aes(Fed_sugarsoln_g, Fed_water_g)) + geom_point() + geom_smooth(method = "lm")

ggplot(conc_8, aes(as.factor(Chamber), Prop_sugar)) + geom_boxplot(aes(fill=sugar_conc_factor))

conc_8$Treatment <- droplevels(conc_8$Treatment)

## Lmer model of proportion of liquid consumed that was sugar solution
mod_prop_sugar_full <- lmer(Prop_sugar~-1+Animal_wt_g+sugar_conc_factor+Treatment+Sex + (1|Indiv) + DaySinceStart, 
                            data=conc_8[!is.na(conc_8$Prop_sugar),])
summary(mod_prop_sugar_full)
plot(mod_prop_sugar)

## Cody: Sex is a fixed effect because random effects don't make sense until there's 6-7 factors
mod_prop_sugar_full_noDay <- lmer(Prop_sugar~Animal_wt_g+sugar_conc_factor+Treatment+Sex + (1|Indiv), 
                                  data=conc_8[!is.na(conc_8$Prop_sugar),])
summary(mod_prop_sugar_full_noDay)

mod_prop_sugar_interac <- lmer(Prop_sugar~Animal_wt_g+sugar_conc_factor*Treatment+Sex + (1|Indiv), 
                               data=conc_8[!is.na(conc_8$Prop_sugar),],REML=F)
summary(mod_prop_sugar_interac)

mod_prop_sugar_noMass <- lmer(Prop_sugar~sugar_conc_factor+Treatment+Sex + (1|Indiv), 
                              data=conc_8[!is.na(conc_8$Prop_sugar),])
summary(mod_prop_sugar_noMass)


## Cody: run glm model without individual as a random effect, just to see what the estimates look like
## Create a TreatmentConc column that has 2x2 of values from both factors so that all of them show in the
## model; for some reason one level is getting dropped in lmer and glm otherwise
conc_8$TreatmentConc <- paste0(conc_8$Treatment, conc_8$sugar_conc_factor)
g.propsugar <- glm(Prop_sugar~TreatmentConc, 
    data=conc_8[!is.na(conc_8$Prop_sugar),])


boxplot(g.propsugar$residuals~conc_8$Indiv[!is.na(conc_8$Prop_sugar)])       
      


                        
## Best model
mod_prop_sugar_noMassSex <- lmer(Prop_sugar~TreatmentConc + (1|Indiv)-1, 
                                 data=conc_8[!is.na(conc_8$Prop_sugar),])
summary(mod_prop_sugar_noMassSex)
plot(mod_prop_sugar_noMassSex)
coef(mod_prop_sugar_noMassSex)
plot(ranef(mod_prop_sugar_noMassSex))
residuals(mod_prop_sugar_noMassSex)
boxplot(residuals(mod_prop_sugar_noMassSex)~conc_8$Indiv[!is.na(conc_8$Prop_sugar)])  ## What is index??

boxplot(mod_prop_sugar_noMassSex$Intercept~conc_8$Indiv[!is.na(conc_8$Prop_sugar)])       


### RUN model with 2% consumption vs. 8%
### Consumption across time as a fixed effect - Day of the experiment
### Date as random effect

mod_2_8 <- lmer(Prop_sugar~conc)


mod_prop_sugar_JustSugar <- lmer(Prop_sugar~sugar_conc_factor + (1|Indiv), 
                                 data=conc_8[!is.na(conc_8$Prop_sugar),])
summary(mod_prop_sugar_JustSugar)

mod_prop_sugar_noIndiv <- lm(Prop_sugar~sugar_conc_factor, data=conc_8) 

anova(#mod_prop_sugar_noIndiv,
      mod_prop_sugar_full, mod_prop_sugar_full_noDay, mod_prop_sugar_noMass, mod_prop_sugar_noMassSex, 
      mod_prop_sugar_JustSugar, mod_prop_sugar_interac)



t.test(conc_8_sugar$Prop_sugar[conc_8_sugar$Treatment=="Short" & 
                                 conc_8_sugar$Sugar_mmt_good %in% c("Y", "M") &
                         !is.na(conc_8_sugar$Prop_sugar) &
                           conc_8_sugar$sugar_conc_factor==0.08], 
       conc_8_sugar$Prop_sugar[conc_8_sugar$Treatment=="Long" & 
                                 conc_8_sugar$Sugar_mmt_good %in% c("Y", "M") &
                                 !is.na(conc_8_sugar$Prop_sugar) &
                                 conc_8_sugar$sugar_conc_factor==0.08], paired = F)

mean(conc_sugar$Fed_sugarsoln_g[conc_sugar$Treatment=="Short" & conc_sugar$Sugar_mmt_good %in% c("Y", "M")])
mean(conc_sugar$Fed_sugarsoln_g[conc_sugar$Treatment=="Long" & conc_sugar$Sugar_mmt_good %in% c("Y", "M")])


#### Animal weights ####
#weights$Date <- as.POSIXct(paste(weights$Day, weights$Month, weights$Year, sep="/"), 
#                         format = "%d/%m/%Y", tz="America/Anchorage")

#weights$Photoperiod <- 0
#weights$Photoperiod[weights$Room=="019D"] <- "Neutral"
#weights$Photoperiod[weights$Room=="019F"] <- "Short"
weights$Photoperiod_g <- "NA"
weights$Postsugar_g <- "NA"
weights$wk_euthanasia_g <- "NA"
weights$Photoperiod_g <- weights$Weight_presugar-weights$Weight_2wk
weights$Postsugar_g <- weights$Weight_euthanasia-weights$Weight_presugar
weights$wk_euthanasia_g <- weights$Weight_euthanasia-weights$Weight_2wk

m.weights <- melt(weights, id.vars=c("Individual", "Photoperiod", "Sugar", "Age_death", "Trial"), 
                  measure.vars=c("Photoperiod_g", "Postsugar_g", "wk_euthanasia_g"))

m.weights2 <- melt(weights, id.vars=c("Individual", "Photoperiod", "Sugar", "Age_death", "Trial"), 
                   measure.vars=c("Weight_start", "Weight_2wk", "Weight_presugar",
                                  "Weight_euthanasia"))

ggplot(m.weights[m.weights$variable=="Postsugar_g",], aes(Age_death, value)) + facet_grid(.~Sugar) +
  geom_point(aes(col=Photoperiod), size=3) + my_theme

## Mass change Presugar minus 2 week acclim (mass change over 4 week photoperiod phase)
ggplot(m.weights[m.weights$variable=="Photoperiod_g",], aes(Photoperiod, value)) + 
  geom_boxplot(aes(fill=Photoperiod)) + 
  my_theme + xlab("Photoperiod treatment") + ylab("Mass change (g)") +
  scale_fill_manual(values = c("#F38BA8", "#23988aff")) +
  theme(legend.key.height = unit(3,"line"))

## Switch high and non sugar order on x-axis
## Send all three plots, without geom_point and text
## Mass change Euthanasia minus Pre-sugar (mass change over sugar treatment)

ggplot(m.weights[m.weights$variable=="Postsugar_g",], aes(fct_rev(Sugar), value)) + 
  geom_boxplot(aes(fill=Photoperiod)) + #geom_point(aes(col=Photoperiod)) + geom_text(aes(label=Individual))+
  my_theme + xlab("Sugar treatment") + ylab("Mass change (g)") +
  #facet_grid(.~Trial) +
  scale_fill_manual(values = c("#F38BA8", "#23988aff")) +
  theme(legend.key.height = unit(3,"line"))

ggplot(m.weights[m.weights$variable=="wk_euthanasia_g",], aes(fct_rev(Sugar), value)) + 
  geom_boxplot(aes(fill=Photoperiod)) + #geom_point(aes(col=Photoperiod)) + geom_text(aes(label=Individual))+
  my_theme + xlab("Sugar treatment") + ylab("Mass change (g)") +
  facet_grid(.~Trial) +
  scale_fill_manual(values = c("#F38BA8", "#23988aff")) +
  theme(legend.key.height = unit(3,"line"))

cols_exptphase <- c("grey60", "grey80", "yellow", "red", "red", "yellow", "black")
#Plotting weights over the course of the experiment for all animals individually
ggplot(m.weights2, aes(variable, value)) + my_theme + 
  geom_line(aes(group=Individual), show.legend = F, size=1) + 
  geom_point(aes(fill=variable), col="black", size=3, pch=21) +
  facet_wrap(.~Individual, scales = "free_x") +
  scale_fill_manual(values = cols_exptphase) +
  theme(axis.text.x=element_blank(), legend.text = element_text(size=20),
        legend.key.height = unit(3,"line")) +
    #axis.text.x = element_text(angle=90, size=20, vjust=0.5), legend.text = element_text(size=20),
     #   legend.key.height = unit(3,"line")) +
  ylab("Mass") #+ xlab("Treatment")

m.weights2$variable <- revalue(m.weights2$variable, c("Weight_start"="Start", "Weight_2wk"= "Acclim",
                               "Weight_presugar"= "Photoperiod", "Weight_euthanasia"="Postsugar"))
#Plotting weights over the course of the experiment for all animals by phase as lines
ggplot(m.weights2, aes(variable, value)) + my_theme + 
  geom_line(aes(group=Individual, col=Sugar), show.legend = F, size=1) + 
  geom_point(aes(fill=Sugar), col="black", size=3, pch=21) +
  facet_wrap(Trial~Photoperiod, scales = "free_x") +
  scale_color_manual(values = c("grey60", "blue")) +
  scale_fill_manual(values = c("grey60", "blue"),name = "Sugar") +
  theme(axis.text.x = element_text(size=10), legend.text = element_text(size=20),
     legend.key.height = unit(3,"line")) + 
  #axis.text.x=element_blank(), legend.text = element_text(size=20),legend.key.height = unit(3,"line")) +
  ylab("Mass") #+ xlab("Treatment")


#Plotting weights over the course of the experiment for all animals by phase as boxplots
ggplot(m.weights2, aes(variable, value)) + my_theme +
  geom_boxplot(aes(fill=fct_rev(Sugar))) +
  facet_wrap(Trial~Photoperiod, scales = "free_x") +
  scale_fill_manual(values = c("grey60", "blue"), name = "Sugar") +
  theme(axis.text.x = element_text(size=10), legend.text = element_text(size=20),
        legend.key.height = unit(3,"line")) + 
  ylab("Mass") #+ xlab("Treatment")

m.weights$Photoperiod <- as.factor(as.character(m.weights$Photoperiod))

t.test(m.weights$value[m.weights$variable=="Photoperiod_g" & m.weights$Photoperiod=="Neutral"],
       m.weights$value[m.weights$variable=="Photoperiod_g" & m.weights$Photoperiod=="Short"], paired = F)

m.weights$Sugar <- as.factor(as.character(m.weights$Sugar))
## Using this for Shelby SICB poster
#lm.wt_age <- lm(value~Photoperiod*Sugar+Age_death, 
#                     data=m.weights[m.weights$variable=="Postsugar_g",]) 
## Using this for Shelby SICB poster
lm.wt <- glm(value~-1+Photoperiod*Sugar, 
               data=m.weights[m.weights$variable=="wk_euthanasia_g",], family="gaussian") 

m.weights2$time <- as.numeric(revalue(m.weights2$variable, c("Start"=1, "Acclim"=14, "Photoperiod"=42,"Postsugar"=67)))
## New version of model including trial as random effect, Oct 1, 2020
lm.wt_full <- lmer(value~-1+Photoperiod*Sugar + (1|time) + (1|Trial), 
             data=m.weights2) 

lm.wt_full_timefixed <- lmer(value~-1+Photoperiod*Sugar + time + (1|Trial) + (1|Individual), 
                   data=m.weights2) 


lm.wt$fitted.values ## These are not predictions; these are fitted values and se's or 95% CIs
## To bind back into dataframe
cbind(m.weights[m.weights$variable=="Postsugar_g",],lm.wt$fitted.values) 
summary(lm.wt_full)
summary(lm.wt_full_timefixed)
predict.glm(lm.wt) ## For CIs/se's
lm.wt_pred <- predict.glm(lm.wt, type="response", interval="confidence", se.fit=T) 
lm.wt_pred$LCI <- lm.wt_pred$fit - (1.96*(lm.wt_pred$se.fit))
lm.wt_pred$UCI <- lm.wt_pred$fit + (1.96*(lm.wt_pred$se.fit))

## Can use predict to get a smooth continuous set of predicted values, or to fit predictions
## To another test dataset
## To fit values to other points in the original data's range, but that aren't the original data
## First create a vector from each column in the dataframe
## Run the model using the vectors and no data frame, save that (e.g. vec_mod)
## Now create a new dataframe (Novel) with all the same columns as the vectors, and fill in values you
## want to fit to. Then predict from the vec_mod model, with vec_mod, newdata=Novel


## CI = +/- 1.96*se
names(lm.wt_pred)
anova(lm.wt)
qqnorm(resid(lm.wt))
qqline(resid(lm.wt))
coef(lm.wt)
library(effects)
plot(allEffects(lm.wt), main= "Change in mass by photoperiod and sugar",
     cex.main=1.5, cex.lab=2, cex.axis=1) ## requires 'effects' package

ggplot(m.weights[m.weights$variable=="Postsugar_g",], aes(Sugar, value)) + 
  geom_boxplot(aes(col=Photoperiod)) + my_theme

