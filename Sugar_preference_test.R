## Grass rat sucrose concentration preference test


## Read in required packages
require(plyr)
require(ggplot2)
require(gridExtra)
require(lme4)
require(lmerTest)

## Sew working directory
setwd("E:\\Google Drive\\Toshiba_desktop\\Fairbanks\\Research\\GrassRats\\Animal_data/")

## Read in data files
conc <- read.csv("SugarConcTest_weights_Expt_May27.csv")

#### General functions ####
my_theme <- theme_classic(base_size = 30) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))

## Calculate amount of sugar consumed
#conc$prop_sugar <- as.numeric(conc$Sugar_conc)
conc$Fed_sugar_amt_g <- conc$Fed_sugarsoln_g*conc$Sugar_conc
#conc$Indiv <- factor(conc$Indiv, levels=c("N1", "N2", "BK3", "BK4"))
#conc <- arrange(conc, sugar_conc_factor, Indiv)

## Remove negative values, and make separate data frames for sugar and water
#conc_sugar <- conc[conc$Fed_sugar_amt_g>-1 & conc$Pre_test=="Test",]
#conc_water <- conc[conc$Fed_water_g>-1 & conc$Pre_test=="Test",]



## Subset just values from experimental test
conc_8 <- subset(conc, Pre_test=="Test_8%")
conc_8$sugar_conc_factor<- factor(conc_8$Sugar_conc, levels=c(0, 0.08))
conc_8$Prop_sugar <- conc_8$Fed_sugarsoln_g/(conc_8$Fed_sugarsoln_g+conc_8$Fed_water_g)

### Histograms of sugar and water consumed to determine "reasonable" cut-offs
ggplot(conc_8[!is.na(conc_8$Fed_sugarsoln_g),], aes(Fed_sugarsoln_g)) + geom_histogram(aes(fill=Sugar_mmt_good)) + 
  my_theme + ggtitle("Fed Sugar soln")
ggplot(conc_8[!is.na(conc_8$Fed_water_g),], aes(Fed_water_g)) + geom_histogram(aes(fill=Water_mmt_good)) + 
  my_theme + ggtitle("Fed water")


conc_sugar <- subset(conc_8, !is.na(Fed_sugarsoln_g))
conc_sugar <- conc_sugar[conc_sugar$Sugar_mmt_good=="Y",]

conc_water <- subset(conc_8, !is.na(Fed_water_g))
conc_water <- conc_water[conc_water$Water_mmt_good=="Y",]

#conc_Pre <- subset(conc, Pre_test=="Pre")


## Plot sugar consumption (quantity of sugar in grams) for 2%
sugar_g_2 <- ggplot(conc_Test, aes(IndivSugarDay, Fed_sugar_amt_g)) + 
  #facet_grid(.~Treatment, scales="free_x") +
  my_theme + geom_bar(stat="identity") + #ylim(0,75) +
  scale_fill_hue(h = c(100, 270)) + 
  theme(axis.text.x = element_text(angle=90, size=15, vjust=0.5), plot.title = element_text(hjust=0.5)) +
  theme(legend.position = "none") +
  #guides(fill=guide_legend(title="Sugar \nconcentration")) + 
  ylab("Quantity of Sugar consumed (g)") + xlab("Individual_SugarConc_ExptDay") +
  ggtitle("2% sucrose")

## Plot sugar consumption (quantity of sugar in grams) for 2%
sugar_g_2 <- ggplot(conc_sugar[conc_sugar$Sugar_conc==0.02,], aes(IndivSugarDay, Fed_sugar_amt_g)) + 
  facet_grid(.~Treatment, scales="free_x") +
  my_theme + geom_bar(stat="identity", aes(fill=Treatment)) + #ylim(0,75) +
  scale_fill_hue(h = c(100, 270)) + 
  theme(axis.text.x = element_text(angle=90, size=15, vjust=0.5), plot.title = element_text(hjust=0.5)) +
  theme(legend.position = "none") +
  #guides(fill=guide_legend(title="Sugar \nconcentration")) + 
  ylab("Quantity of Sugar consumed (g)") + xlab("Individual_SugarConc_ExptDay") +
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
sugar_soln_g <- ggplot(conc_sugar, aes(IndivSugarDay, Fed_sugarsoln_g)) + facet_grid(sugar_conc_factor~., scales="free_x") +
  my_theme + geom_boxplot(aes(fill=Treatment)) + #ylim(0,75) +
  scale_fill_hue(h = c(100, 270)) +
  theme(axis.text.x = element_text(angle=90, size=15, vjust=0.5)) + theme(legend.position = "none") +
  #guides(fill=guide_legend(title="Sugar \nconcentration")) +
  ylab("Sugar solution consumed (g)") #+ xlab("Individual_SugarConc_ExptDay")

water_g <- ggplot(conc_water, aes(IndivSugarDay, Fed_water_g)) + my_theme + 
  geom_boxplot(aes(fill=Treatment)) + facet_grid(sugar_conc_factor~., scales="free_x") +
  scale_fill_hue(h = c(100, 270)) +
  theme(axis.text.x = element_text(angle=90, size=15, vjust=0.5)) + theme(legend.position = "none") +
  #guides(fill=guide_legend(title="Sugar \nconcentration")) +
  ylab("Water consumed (g)") #+ xlab("Individual_SugarConc_ExptDay")

grid.arrange(sugar_soln_g, water_g, nrow=2)

prop_sugar <- ggplot(conc_8[!is.na(conc_8$Prop_sugar),], aes(as.factor(Chamber), Prop_sugar)) + my_theme + 
  geom_boxplot(aes(fill=Treatment)) + facet_grid(.~sugar_conc_factor, scales="free_x") +
  scale_fill_hue(h = c(100, 270)) +
  theme(axis.text.x = element_text(angle=90, size=15, vjust=0.5)) +
  #guides(fill=guide_legend(title="Sugar \nconcentration")) +
  ylab("Proportion sugar soln consumed") #+ xlab("Individual_SugarConc_ExptDay")
prop_sugar

# Regression of sugar soln vs water
ggplot(conc_8, aes(Fed_sugarsoln_g, Fed_water_g)) + geom_point() + geom_smooth(method = "lm")

ggplot(conc_8, aes(as.factor(Chamber), Prop_sugar)) + geom_boxplot(aes(fill=sugar_conc_factor))


## Lmer model of proportion of liquid consumed that was sugar solution
mod_prop_sugar_full <- lmer(Prop_sugar~Animal_wt_g+sugar_conc_factor+(1|Treatment)+(1|Sex), data=conc_8)
summary(mod_prop_sugar_full)
plot(mod_prop_sugar)

mod_prop_sugar_noMass <- lmer(Prop_sugar~sugar_conc_factor+Treatment+(1|Sex), data=conc_8)
summary(mod_prop_sugar_noMass)

## Best model
mod_prop_sugar_noMassSex <- lm(Prop_sugar~sugar_conc_factor+Treatment, data=conc_8)
summary(mod_prop_sugar_noMassSex)

mod_prop_sugar_JustSugar <- lm(Prop_sugar~sugar_conc_factor, data=conc_8)
summary(mod_prop_sugar_noMassSex)

anova(mod_prop_sugar_full, mod_prop_sugar_noMass, mod_prop_sugar_noMassSex, mod_prop_sugar_JustSugar)



t.test(conc$Prop_sugar[conc$Treatment=="Short" & conc$Sugar_mmt_good %in% c("Y", "M")], 
       conc$Prop_sugar[conc$Treatment=="Long" & conc$Sugar_mmt_good %in% c("Y", "M")], paired = F)

mean(conc_sugar$Fed_sugarsoln_g[conc_sugar$Treatment=="Short" & conc_sugar$Sugar_mmt_good %in% c("Y", "M")])
mean(conc_sugar$Fed_sugarsoln_g[conc_sugar$Treatment=="Long" & conc_sugar$Sugar_mmt_good %in% c("Y", "M")])
