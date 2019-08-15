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
setwd("E:\\Google Drive\\Toshiba_desktop\\Fairbanks\\Research\\GrassRats\\Animal_data/")

## Read in data files
conc <- read.csv("SugarConcTest_weights_Expt_Jun19.csv")

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

## Subset 2 and 8%
conc_2_8 <- conc[conc$Pre_test %in% c("Test_2%", "Test_8%"),]
conc_2_8$sugar_conc_factor<- factor(conc_2_8$Sugar_conc, levels=c(0, 0.02, 0.08))
conc_2_8 <- conc_2_8[conc_2_8$Sugar_mmt_good=="Y" & conc_2_8$Water_mmt_good=="Y",]
conc_2_8$Prop_sugar <- conc_2_8$Fed_sugarsoln_g/(conc_2_8$Fed_sugarsoln_g+conc_2_8$Fed_water_g)

conc_2_8_sugar <- subset(conc_2_8, !is.na(Fed_sugarsoln_g))
conc_2_8_sugar <- conc_2_8_sugar[conc_2_8_sugar$Sugar_mmt_good=="Y",]

conc_2_8_water <- subset(conc_2_8, !is.na(Fed_water_g))
conc_2_8_water <- conc_2_8_water[conc_2_8_water$Water_mmt_good=="Y",]

## Subset just 2%
conc_2 <- conc[conc$Pre_test %in% c("Test_2%"),]
conc_2_sugar <- subset(conc_2, !is.na(Fed_sugarsoln_g))
conc_2_sugar <- conc_2_sugar[conc_2_sugar$Sugar_mmt_good=="Y",]
conc_2_water <- subset(conc_2, !is.na(Fed_water_g))
conc_2_water <- conc_2_water[conc_2_water$Water_mmt_good=="Y",]

conc_2_good <- conc_2[conc_2$Sugar_mmt_good=="Y" & conc_2$Water_mmt_good=="Y",]
conc_2_good$Prop_sugar <- conc_2_good$Fed_sugarsoln_g/(conc_2_good$Fed_sugarsoln_g+conc_2_good$Fed_water_g)


## Subset just values from experimental test
conc_8 <- subset(conc, Pre_test=="Test_8%")
conc_8$sugar_conc_factor<- factor(conc_8$Sugar_conc, levels=c(0, 0.08))
conc_8$Prop_sugar <- conc_8$Fed_sugarsoln_g/(conc_8$Fed_sugarsoln_g+conc_8$Fed_water_g)

### Histograms of sugar and water consumed to determine "reasonable" cut-offs
#ggplot(conc_8[!is.na(conc_8$Fed_sugarsoln_g),], aes(Fed_sugarsoln_g)) + geom_histogram(aes(fill=Sugar_mmt_good)) + 
 # my_theme + ggtitle("Fed Sugar soln")
#ggplot(conc_8[!is.na(conc_8$Fed_water_g),], aes(Fed_water_g)) + geom_histogram(aes(fill=Water_mmt_good)) + 
 # my_theme + ggtitle("Fed water")


conc_8_sugar <- subset(conc_8, !is.na(Fed_sugarsoln_g))
conc_8_sugar <- conc_8_sugar[conc_8_sugar$Sugar_mmt_good=="Y",]

conc_8_water <- subset(conc_8, !is.na(Fed_water_g))
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
sugar_soln_g <- ggplot(conc_sugar, aes(IndivSugarDay, Fed_sugarsoln_g)) + facet_grid(sugar_conc_factor~Treatment, scales="free_x") +
  my_theme + geom_boxplot(aes(fill=Treatment)) + #ylim(0,75) +
  scale_fill_hue(h = c(100, 270)) +
  theme(axis.text.x = element_text(angle=90, size=15, vjust=0.5)) + theme(legend.position = "none") +
  #guides(fill=guide_legend(title="Sugar \nconcentration")) +
  ylab("Sugar solution consumed (g)") #+ xlab("Individual_SugarConc_ExptDay")

water_g <- ggplot(conc_water, aes(IndivSugarDay, Fed_water_g)) + my_theme + 
  geom_boxplot(aes(fill=Treatment)) + facet_grid(sugar_conc_factor~Treatment, scales="free_x") +
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

prop_sugar_boxplot <- ggplot(conc_8[!is.na(conc_8$Prop_sugar),], aes(Treatment, Prop_sugar)) + my_theme + 
  geom_boxplot(aes(fill=Treatment)) + 
  facet_grid(.~sugar_conc_factor, scales="free_x") +
  scale_fill_hue(h = c(100, 270)) +
  theme(axis.text.x = element_text(angle=90, size=20, vjust=0.5), legend.text = element_text(size=20),
        legend.key.height = unit(3,"line")) +
  #guides(fill=guide_legend(title="Sugar \nconcentration")) +
  ylab("Proportion sugar soln consumed") + xlab("Chamber") #+ xlab("Individual_SugarConc_ExptDay")
prop_sugar_boxplot

conc_2_8$sugar_conc_factor <- factor(conc_2_8$sugar_conc_factor, levels = c(0.02,0,0.08))
conc_2_8$Treatment <- revalue(conc_2_8$Treatment, c("Long" = "Neutral"))
prop_sugar_2_8 <- ggplot(conc_2_8[!is.na(conc_2_8$Prop_sugar),], aes(Treatment, Prop_sugar)) + my_theme + 
  geom_boxplot(aes(fill=Treatment)) + facet_grid(.~sugar_conc_factor, scales="free_x") +
  geom_point() +
  scale_fill_hue(h = c(100, 270)) +
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

## IGNORE
dpr_conc_8 <- conc_8[,c(6,8,11:14,28)]
dpr_conc_8$Treatment <- revalue(dpr_conc_8$Treatment, c("Long"="1", "Short"="0"))
dpr_conc_8$Sex <- revalue(dpr_conc_8$Sex, c("F"="0", "M"="1"))
dpr_conc_8$Sex <- as.numeric(as.factor(dpr_conc_8$Sex))
dpr_conc_8$Treatment <- as.numeric(as.factor(dpr_conc_8$Treatment))
dpr_conc_8$Indiv <- as.numeric(as.factor(dpr_conc_8$Indiv))
dpr_conc_8 <- na.omit(dpr_conc_8)
pr_conc_8 <- prcomp(dpr_conc_8, scale=T)

fviz_eig(pr_conc_8)
fviz_pca_ind(pr_conc_8,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_var(pr_conc_8,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)


mod_2_8_Day <- lmer(Prop_sugar~sugar_conc_factor+Treatment+(1|Indiv)+(1|DaySinceStart)+Animal_wt_g, data=conc_2_8)

mod_2_8 <- lmer(Prop_sugar~sugar_conc_factor+Treatment+(1|Indiv), data=conc_2_8)

summary(mod_2_8)
coef(mod_2_8)
anova(mod_2_8)



mod_2_8_noIndiv <- lm(Prop_sugar~sugar_conc_factor+Treatment, data=conc_2_8)

mod_2_8_noIndiv_noTreatment <- lm(Prop_sugar~sugar_conc_factor, data=conc_2_8)

## Best model here
mod_2_8_noTreatment <- lmer(Prop_sugar~sugar_conc_factor+(1|Indiv), data=conc_2_8)
summary(mod_2_8_noTreatment)
anova(mod_2_8_noTreatment)
qqnorm(resid(mod_2_8_noTreatment))
qqline(resid(mod_2_8_noTreatment))

tmp_sugar <- as.data.frame(confint(glht(mod_2_8_noIndiv_noTreatment))$confint)
tmp_sugar$Comparison <- rownames(tmp_sugar)
tmp_sugar$Comparison <- revalue(as.factor(tmp_sugar$Comparison),c("(Intercept)" = "Intercept", 
                                                                  "sugar_conc_factor0.02" = "2% sucrose", 
                                                                  "sugar_conc_factor0.08" = "8% sucrose"))


ggplot(tmp_sugar, aes(x = Comparison, y = Estimate, ymin = lwr, ymax = upr)) + my_theme +
  #theme(axis.text.x = element_text(angle=30, vjust=0.7)) +
  geom_errorbar() + geom_point()

mod_2_8_noTreatment_Wt <- lmer(Prop_sugar~sugar_conc_factor+(1|Indiv)+Animal_wt_g, data=conc_2_8)


anova(mod_2_8_Day, mod_2_8, mod_2_8_noIndiv, mod_2_8_noIndiv_noTreatment, mod_2_8_noTreatment, mod_2_8_noTreatment_Wt)

# Regression of sugar soln vs water
ggplot(conc_8, aes(Fed_sugarsoln_g, Fed_water_g)) + geom_point() + geom_smooth(method = "lm")

ggplot(conc_8, aes(as.factor(Chamber), Prop_sugar)) + geom_boxplot(aes(fill=sugar_conc_factor))

### INCLUDE RANDOM EFFECT OF INDIVIDUAL ## REPEATED MEASURES OF INDIVIDUALS
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

anova(mod_prop_sugar_full, mod_prop_sugar_full_noDay, mod_prop_sugar_noMass, mod_prop_sugar_noMassSex, 
      mod_prop_sugar_JustSugar, mod_prop_sugar_interac, mod_prop_sugar_noIndiv)



t.test(conc$Prop_sugar[conc$Treatment=="Short" & conc$Sugar_mmt_good %in% c("Y", "M")], 
       conc$Prop_sugar[conc$Treatment=="Long" & conc$Sugar_mmt_good %in% c("Y", "M")], paired = F)

mean(conc_sugar$Fed_sugarsoln_g[conc_sugar$Treatment=="Short" & conc_sugar$Sugar_mmt_good %in% c("Y", "M")])
mean(conc_sugar$Fed_sugarsoln_g[conc_sugar$Treatment=="Long" & conc_sugar$Sugar_mmt_good %in% c("Y", "M")])
