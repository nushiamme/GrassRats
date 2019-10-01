## Grass rat sucrose concentration preference test


## Read in required packages
require(plyr)
require(ggplot2)
require(gridExtra)
require(lme4)
require(lmerTest)
require(factoextra)
require(multcomp)

## Sew working directory
setwd("E:\\Ex_Google_Drive\\Toshiba_desktop\\Fairbanks\\Research\\GrassRats\\Animal_data")

## Read in data files
conc <- read.csv("SugarConcTest_weights_Expt_Sept15.csv")
liver <- read.csv("Liver_fat.csv")
liver_rescore <- read.csv("Liver_fat_AS.csv")
fatpad <- read.csv("Fat_pads.csv")
weights <- read.csv("Animal_weights_forMassChange.csv")



#### General functions ####
my_theme <- theme_classic(base_size = 30) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))


#### Animal weights ####
#weights$Date <- as.POSIXct(paste(weights$Day, weights$Month, weights$Year, sep="/"), 
#                         format = "%d/%m/%Y", tz="America/Anchorage")

weights$Photoperiod <- 0
weights$Photoperiod[weights$Room=="019D"] <- "Neutral"
weights$Photoperiod[weights$Room=="019F"] <- "Short"
weights$Photoperiod_g <- "NA"
weights$Postsugar_g <- "NA"
weights$wk_euthanasia_g <- "NA"
weights$Photoperiod_g <- weights$Weight_presugar-weights$Weight_2wk
weights$Postsugar_g <- weights$Weight_euthanasia-weights$Weight_presugar
weights$wk_euthanasia_g <- weights$Weight_euthanasia-weights$Weight_2wk


m.weights <- melt(weights, id.vars=c("Individual", "Photoperiod", "Sugar"), 
                  measure.vars=c("Photoperiod_g", "Postsugar_g", "wk_euthanasia_g"))


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
  scale_fill_manual(values = c("#F38BA8", "#23988aff")) +
  theme(legend.key.height = unit(3,"line"))

ggplot(m.weights[m.weights$variable=="wk_euthanasia_g",], aes(fct_rev(Sugar), value)) + 
  geom_boxplot(aes(fill=Photoperiod)) + #geom_point(aes(col=Photoperiod)) + geom_text(aes(label=Individual))+
  my_theme + xlab("Sugar treatment") + ylab("Mass change (g)") +
  scale_fill_manual(values = c("#F38BA8", "#23988aff")) +
  theme(legend.key.height = unit(3,"line"))

m.weights$Photoperiod <- as.factor(as.character(m.weights$Photoperiod))

t.test(m.weights$value[m.weights$variable=="Photoperiod_g" & m.weights$Photoperiod=="Neutral"],
       m.weights$value[m.weights$variable=="Photoperiod_g" & m.weights$Photoperiod=="Short"], paired = F)

m.weights$Sugar <- as.factor(as.character(m.weights$Sugar))
summary(lm(value~Photoperiod*Sugar, 
                      data=m.weights[m.weights$variable=="Postsugar_g",]))

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
conc_8$Prop_sugar <- conc_8$Fed_sugarsoln_g_perday/(conc_8$Fed_sugarsoln_g_perday+conc_8$Fed_water_g_perday)

### Histograms of sugar and water consumed to determine "reasonable" cut-offs
#ggplot(conc_8[!is.na(conc_8$Fed_sugarsoln_g),], aes(Fed_sugarsoln_g)) + geom_histogram(aes(fill=Sugar_mmt_good)) + 
 # my_theme + ggtitle("Fed Sugar soln")
#ggplot(conc_8[!is.na(conc_8$Fed_water_g),], aes(Fed_water_g)) + geom_histogram(aes(fill=Water_mmt_good)) + 
 # my_theme + ggtitle("Fed water")


conc_8_sugar <- subset(conc_8, !is.na(Fed_sugarsoln_g_perday))
conc_8_sugar <- conc_8_sugar[conc_8_sugar$Sugar_mmt_good=="Y",]

conc_8_water <- subset(conc_8, !is.na(Fed_water_g_perday))
conc_8_water <- conc_8_water[conc_8_water$Water_mmt_good=="Y",]

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

## Plot sugar consumption (quantity of sugar in grams) for 2% by tretament
sugar_g_2 <- ggplot(conc_2_sugar, aes(IndivSugarDay, Fed_sugar_amt_g)) + 
  facet_grid(.~Treatment, scales="free_x") +
  my_theme + geom_bar(stat="identity", aes(fill=Treatment)) + #ylim(0,75) +
  scale_fill_hue(h = c(100, 270)) + 
  theme(axis.text.x = element_text(angle=90, size=15, vjust=0.5), plot.title = element_text(hjust=0.5)) +
  theme(legend.position = "none") +
  #guides(fill=guide_legend(title="Sugar \nconcentration")) + 
  ylab("Quantity of Sugar consumed (g)") + xlab("Individual_SugarConc_ExptDay") +
  ggtitle("2% sucrose")

## Plot sugar soln consumption for 2% by tretament
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
  ylab("Proportion sugar soln consumed") + xlab("Chamber") #+ xlab("Individual_SugarConc_ExptDay")
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
mod_2_8_Day <- lmer(Prop_sugar~sugar_conc_factor+Treatment+(1|Indiv)+(1|DaySinceStart)+Animal_wt_g, data=conc_2_8)

mod_2_8 <- lmer(Prop_sugar~sugar_conc_factor+Treatment+(1|Indiv), data=conc_2_8)

summary(mod_2_8)
coef(mod_2_8)
anova(mod_2_8)

mod_2_8_noIndiv <- lm(Prop_sugar~sugar_conc_factor+Treatment, data=conc_2_8)

mod_2_8_noIndiv_noTreatment <- lm(Prop_sugar~sugar_conc_factor, data=conc_2_8)
#plot(allEffects(mod_2_8_noIndiv_noTreatment))

## Best model here
mod_2_8_noTreatment <- lmer(Prop_sugar~sugar_conc_factor+(1|Indiv), data=conc_2_8)
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

anova(mod_2_8_Day, mod_2_8, mod_2_8_noIndiv, mod_2_8_noIndiv_noTreatment, mod_2_8_noTreatment, mod_2_8_noTreatment_Wt)

# Regression of sugar soln vs water
ggplot(conc_8, aes(Fed_sugarsoln_g, Fed_water_g)) + geom_point() + geom_smooth(method = "lm")

ggplot(conc_8, aes(as.factor(Chamber), Prop_sugar)) + geom_boxplot(aes(fill=sugar_conc_factor))

## Lmer model of proportion of liquid consumed that was sugar solution
mod_prop_sugar_full <- lmer(Prop_sugar~Animal_wt_g+sugar_conc_factor+(1|Treatment)+(1|Sex) + (1|Indiv) + DaySinceStart, data=conc_8)
summary(mod_prop_sugar_full)
plot(mod_prop_sugar)

mod_prop_sugar_full_noDay <- lmer(Prop_sugar~Animal_wt_g+sugar_conc_factor+(1|Treatment)+(1|Sex) + (1|Indiv), data=conc_8)
summary(mod_prop_sugar_full_noDay)

mod_prop_sugar_interac <- lmer(Prop_sugar~Animal_wt_g+sugar_conc_factor*Treatment+(1|Sex) + (1|Indiv), data=conc_8)
summary(mod_prop_sugar_interac)

mod_prop_sugar_noMass <- lmer(Prop_sugar~sugar_conc_factor+Treatment+(1|Sex) + (1|Indiv), data=conc_8)
summary(mod_prop_sugar_noMass)

## Best model
mod_prop_sugar_noMassSex <- lmer(Prop_sugar~sugar_conc_factor+Treatment + (1|Indiv), data=conc_8)
summary(mod_prop_sugar_noMassSex)
plot(mod_prop_sugar_noMassSex)
coef(mod_prop_sugar_noMassSex)
plot(ranef(mod_prop_sugar_noMassSex))
plot(residuals(mod_prop_sugar_noMassSex))  ## What is index??

### RUN model with 2% consumption vs. 8%
### Consumption across time as a fixed effect - Day of the experiment
### Date as random effect

mod_2_8 <- lmer(Prop_sugar~conc)


mod_prop_sugar_JustSugar <- lmer(Prop_sugar~sugar_conc_factor + (1|Indiv), data=conc_8)
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
