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
meta <- read.csv("GRP_20191213.metadata_AS.csv")
meta_litter <- read.csv("Meta_litter_parents.csv")

# General functions ####
## Saving standard theme  
my_theme <- theme_classic(base_size = 30) + 
  theme(axis.title.y = element_text(color = "black", vjust = 2),
        panel.border = element_rect(colour = "black", fill=NA))

## Theme with slightly smaller font
my_theme2 <- my_theme + theme_classic(base_size = 15)

## Reordering D3 by abundance
#ubiome_rarified$D3_order <- as.factor(reorder(ubiome_rarified$D3, ubiome_rarified$GR_Total, median))

## Subsetting metadata to useful columns
litterEndCol <- which(colnames(meta_litter)=="Percent_liver_fat")
meta_litter <- meta_litter[,1:litterEndCol]
metaEndCol <- which(colnames(meta)=="Sugar")
mmeta <- meta[,1:metaEndCol]
mmeta_litter <- merge(mmeta, meta_litter, by="GrassRat_ID")



Startcol <- which(colnames(ubiome_rarified)=="GR_Total")
Endcol <- which(colnames(ubiome_rarified)=="NegPlateB_G07")
m.ubiome <- ubiome_rarified[,-c(Startcol:Endcol)] %>% ## using tidyr for melting
  gather(SampleID, value, GRP009:GRP182)

merge_ubiome<- merge(mmeta_litter, m.ubiome, by = "SampleID")
head(merge_ubiome)


## For learning how to structure 
dw <- read.table(header=T, text='
                 sbj f1.avg f1.sd f2.avg f2.sd  blabla
                 A   10    6     50     10      bA
                 B   12    5     70     11      bB
                 C   20    7     20     8       bC
                 D   22    8     22     9       bD
                 ')

dw %>% 
  gather(v, value, f1.avg:f2.sd) %>% 
  separate(v, c("var", "col")) %>% 
  arrange(sbj) %>% 
  spread(col, value)

  
## Plots

## Old
ggplot(ubiome_rarified, aes(D3_order, log(GR_Total))) + geom_boxplot() + my_theme +
    #scale_x_discrete(limits = rev(levels(ubiome_rarified$D3_order))) +
    theme(axis.text.x = element_text(angle=90,vjust=0.5, size=10))

Fam_by_litter <- ggplot(merge_ubiome[merge_ubiome$ExpTrial=="Int",], aes(fill=D4, y=value, x=GrassRat_ID)) + 
  geom_bar(position="fill", stat="identity") + facet_grid(Daylength+Sugar~Litter, scales = "free")
Fam_by_litter

Fam_by_Parents <- ggplot(merge_ubiome[merge_ubiome$ExpTrial=="Int",], aes(fill=D4, y=value, x=GrassRat_ID)) + 
  geom_bar(position="fill", stat="identity") + facet_grid(.~ParentPair+Daylength+Sugar, scales = "free")
Fam_by_Parents
