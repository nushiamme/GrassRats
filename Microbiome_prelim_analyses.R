## Started April 15, 2020
## Code Author: Anusha Shankar
## Contact: nushiamme<at>gmail<dot>com; github/nushiamme

## Code to do preliminary analyses of guy microbiome of grass rats from short days/sleep/carbs study
## We had 2 batches of the experiment. Batch 1 had 24 animals; batch 2 had 21. 
## Grass rats were acclimated at 12:12 LD cycles for 2 weeks, then put in either neutral (12:12 LD) or short (4:20 LD) rooms
## for 4 weeks. Then they were given an extra bottle with 2% sugar for about 4 days, and then ramped up from 2% to 8% sucrose
## over the course of a week. Then kept on 8% sucrose for 3 weeks and euthanized. Fecal samples were collected at the end of
## each phase of the experiment and sequenced, and large intestine and caecal samples were collected at the 
## end of the experiment and sequenced.

require(ggplot2)
library(tidyr) ## To reshape dataframe
library(dplyr) ## To reshape dataframe

## Set working directory
setwd("E://Ex_Google_Drive//Toshiba_desktop//Fairbanks//Research//GrassRats//Microbiome_Data//GRP_20191213_16S_qiime")

## read in data
ubiome_rarified <- read.csv("GRP_20191213.rarefied-class-feature-table.csv")

# General functions ####
## Saving standard theme  
my_theme <- theme_classic(base_size = 30) + 
  theme(axis.title.y = element_text(color = "black", vjust = 2),
        panel.border = element_rect(colour = "black", fill=NA))

## Theme with slightly smaller font
my_theme2 <- my_theme + theme_classic(base_size = 15)

ubiome_rarified$D3_order <- as.factor(reorder(ubiome_rarified$D3, ubiome_rarified$GR_Total, median))

## Plots
ggplot(ubiome_rarified, aes(D3_order, log(GR_Total))) + geom_boxplot() + my_theme +
  #scale_x_discrete(limits = rev(levels(ubiome_rarified$D3_order))) +
  theme(axis.text.x = element_text(angle=90,vjust=0.5, size=10))



dw <- read.table(header=T, text='
                 sbj f1.avg f1.sd f2.avg f2.sd  blabla
                 A   10    6     50     10      bA
                 B   12    5     70     11      bB
                 C   20    7     20     8       bC
                 D   22    8     22     9       bD
                 ')

ubiome_rarified %>% 
  gather(v, value, c(GRP009:GRP182,NegPlateA_F02:D3_order)) %>% 
  separate(v, c("var", "col")) %>% 
  arrange(D3_order) %>% 
  spread(col, value)
