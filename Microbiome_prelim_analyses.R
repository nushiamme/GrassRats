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
library(circlize) ## to circularize plots
library(qiime2R) ## See https://forum.qiime2.org/t/tutorial-integrating-qiime2-and-r-for-data-visualization-and-analysis-using-qiime2r/4121
library(tidyverse)
library(gplots)

## Set working directory
setwd("E://Ex_Google_Drive//Toshiba_desktop//Fairbanks//Research//GrassRats//Microbiome_Data//GRP_20191213_16S_qiime")

## read in data
ubiome_rarified <- read.csv("GRP_20191213.rarefied-class-feature-table.csv")
meta <- read.csv("GRP_20191213.metadata_AS.csv")
#meta <- read.csv("GRP_20191213.metadata_LitterFat.csv")
meta_litter <- read.csv("Meta_litter_parents.csv")
metadata <- read.csv("GRP_20191213.metadata.csv")
agg_liverfat <- read.csv("Agg_liverfat.csv")


## producing metadata file with liver fat and litter IDs for Devin
#### Don't run unless needed ####
## Merging litter data including Phase 2, with meta_litter file
meta_litter_bothphases <- merge(meta_litter, agg_liverfat, by.x = "GrassRat_ID", by.y = "ID")


litter_sub <- meta_litter_bothphases[,c(1:6,13)]
mmeta_litter<- merge(meta, litter_sub, by.x= "MouseID", by.y = "GrassRat_ID", all.x = T)
mmeta_litter <- mmeta_litter[,-1]
write.csv(mmeta_litter, "GRP_20191213.metadata_LitterFat.csv")
write_tsv(mmeta_litter, "GRP_20191213.metadata_LitterFat.tsv")
#### Till here ####


## Read in qza file
SVs <- read_qza(".//qiime2_artifacts//table.qza")

## To see the data
SVs$data[1:5,1:5]

## Read in taxonomy
taxonomy<-read_qza(".//qiime2_artifacts//taxonomy.qza")
head(taxonomy$data) ## To see the taxonomic data

## Create phyloseq object
physeq <-qza_to_phyloseq(
  features=".//qiime2_artifacts//table.qza",
  tree=".//qiime2_artifacts//rooted-tree.qza",
  ".//qiime2_artifacts//taxonomy.qza",
  metadata = "GRP_20191213.metadata.tsv"
)

metadata <- read_tsv("GRP_20191213.metadata.tsv")
shannon <- read_qza("core-metrics-results//shannon_vector.qza")
# Move the sample names to a new column, matches the metadata, and allows them to be merged
shannon<-shannon$data %>% rownames_to_column("SampleID") 
gplots::venn(list(metadata=metadata$SampleID, shannon=shannon$SampleID))

## 23 samples in the metadata don't have Shannon diversity values
## To check why these samples have been lost:
metadata_div<-
  +     metadata %>% 
  +     left_join(shannon)


# General functions ####
## Saving standard theme  
my_theme <- theme_classic(base_size = 30) + 
  theme(axis.title.y = element_text(color = "black", vjust = 2),
        panel.border = element_rect(colour = "black", fill=NA))

## Theme with slightly smaller font
my_theme2 <- my_theme + theme_classic(base_size = 15)

#X - Time
#Y - proportion
#Diversity indices - alpha, beta

#families or diversity on the y-axis
#Time on x-axis

#NMDS or PCoA plots - with intestines, effects of 4 groups 
#effects of litters; 2 panel plot
#Circle litter groups if they cluster
#Repeated measures analyses on diversity indices and target groups

#Biplot for specific families or subset groups

#Try out plots at the D1 level and do NMDS

#Heatmap instead of circle?

## Reordering D3 by abundance
#ubiome_rarified$D3_order <- as.factor(reorder(ubiome_rarified$D3, ubiome_rarified$GR_Total, median))

## Subsetting metadata to useful columns
litterEndCol <- which(colnames(meta_litter)=="Percent_liver_fat")
meta_litter <- meta_litter[,1:litterEndCol]
metaEndCol <- which(colnames(meta)=="Sugar")
mmeta <- meta[,1:metaEndCol]
mmeta_litter <- merge(mmeta, meta_litter, by="GrassRat_ID")


#### REPEATED MEASURES for the 22 animals for the Phyla level ####
## Liver adiposity and gut microbiome


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

## DO ##
## Select just groups representing up to 99% of the sample
merge_ubiome_99 <- merge_ubiome


## Plots

## Old
#ggplot(ubiome_rarified, aes(D3_order, log(GR_Total))) + geom_boxplot() + my_theme +
    #scale_x_discrete(limits = rev(levels(ubiome_rarified$D3_order))) +
 #   theme(axis.text.x = element_text(angle=90,vjust=0.5, size=10))

Fam_by_litter <- ggplot(merge_ubiome[merge_ubiome$ExpTrial=="Int",], aes(fill=D4, y=value, x=GrassRat_ID)) + 
  geom_bar(position="fill", stat="identity") + facet_grid(Daylength+Sugar~Litter, scales = "free")
Fam_by_litter

Fam_by_Parents <- ggplot(merge_ubiome[merge_ubiome$ExpTrial=="Int",], aes(fill=D4, y=value, x=GrassRat_ID)) + 
  geom_bar(position="fill", stat="identity") + facet_grid(.~ParentPair+Daylength+Sugar, scales = "free")
Fam_by_Parents

Fam_by_Parents <- ggplot(merge_ubiome[merge_ubiome$ExpTrial=="Int",], aes(fill=D3, y=value, x=GrassRat_ID)) + 
  geom_bar(position="fill", stat="identity") + facet_grid(.~Litter+Daylength+Sugar, scales = "free") +
  theme(legend.position = "none")
Fam_by_Parents


Time_by_Parents <- ggplot(merge_ubiome, aes(fill=D4, y=value, x=ExpTrial)) + 
  geom_bar(position="fill", stat="identity") + facet_grid(.~ParentPair+Daylength+Sugar, scales = "free")
Time_by_Parents

merge_ubiome$D4_order <- as.factor(reorder(merge_ubiome$D4, merge_ubiome$value, sum))

Indivs_lines_time <- ggplot(merge_ubiome, aes(y=D4_order, x=ExpTrial)) + facet_grid(.~Litter, scales = "free") +
  geom_point(aes(col=D4_order, size=value)) + geom_line(aes(group=D4_order)) +
  theme(legend.position = "none", axis.text.x = element_text(angle=90)) +
  scale_size(range = c(0,15))
Indivs_lines_time

## Trying out circlize
merge_ubiome$D4_numeric <- as.numeric(as.factor(merge_ubiome$D4_order))
merge_ubiome$GR_numeric <- as.numeric(as.factor(merge_ubiome$GrassRat_ID))
ubiome_nonzero <- merge_ubiome[merge_ubiome$value>0,]
ubiome_nonzero <- 
ubiome_nonzero$D4_order <- droplevels(ubiome_nonzero$D4_order)
ubiome_nonzero$D4_numeric <- as.numeric(as.factor(ubiome_nonzero$D4_order))
levels(ubiome_nonzero$D4_order)[levels(ubiome_nonzero$D4_order)==""] <- "Unknown"

circos.par("track.height" = 0.3)
circos.initialize(factors = ubiome_nonzero$GrassRat_ID, x=ubiome_nonzero$D4_numeric)
circos.track(factors = ubiome_nonzero$GrassRat_ID, y=ubiome_nonzero$value)
circos.trackPoints(ubiome_nonzero$GrassRat_ID, ubiome_nonzero$D4_numeric, ubiome_nonzero$value, 
                   col=ubiome_nonzero$D4_order, pch = 16, cex = 0.5)




## Microbiome through time
##Trial for G19
indiv <- merge_ubiome[merge_ubiome$GrassRat_ID=="G19",]
indiv$ExpTrial <- ordered(indiv$ExpTrial, levels = c("Acclim", "Photo", "HCS", "Int"))
indiv_time <- ggplot(indiv, aes(fill=D4, y=value, x=ExpTrial)) + 
  geom_bar(position="fill", stat="identity")
indiv_time


## Reordering D4 by abundance
indiv$D4_order <- as.factor(reorder(indiv$D4, indiv$value, sum))
levels(indiv$D4_order)[levels(indiv$D4_order)==""] <- "Unknown"
indiv_lines <- ggplot(indiv, aes(y=D4_order, x=ExpTrial)) + 
  geom_point(aes(col=D4_order, size=log(value))) + geom_line(aes(group=D4_order)) +
  scale_size(range = c(0,15))
indiv_lines

ggplot(indiv_nonzero, aes(y=D4_order, x=ExpTrial)) + 
  geom_point(aes(col=D4_order, size=log(value))) + geom_line(aes(group=D4_order)) +
  scale_size(range = c(0,15))
indiv_lines



## Trying out circular plots
## Replace NA with unknown in D4_ordered
indiv_nonzero <- indiv[indiv$value>0,]
indiv_nonzero <- droplevels(indiv_nonzero)

indiv_nonzero$D4_numeric <- as.numeric(as.factor(indiv_nonzero$D4_order))

circos.par("track.height" = 0.1)
#circos.initialize(factors = indiv_nonzero$D4_order, x = log(indiv_nonzero$value))
circos.initialize(factors = indiv_nonzero$ExpTrial, x=indiv_nonzero$D4_numeric)

## Not working yet
circos.track(factors = indiv_nonzero$ExpTrial, y=indiv_nonzero$value,
             panel.fun = function(x, y) {
               circos.text(CELL_META$xcenter, CELL_META$cell.ylim[2] + uy(5, "mm"), 
                           CELL_META$sector.index)
               circos.axis(labels.cex = 0.6)
             })

circos.track(factors = indiv_nonzero$ExpTrial, y=log(indiv_nonzero$value))

#col = rep(c("#FF0000", "#00FF00"), 4)
circos.trackPoints(indiv_nonzero$ExpTrial, indiv_nonzero$D4_numeric, log(indiv_nonzero$value), 
                   col=indiv_nonzero$D4_order, pch = 16, cex = 0.5)
circos.text(-1, 0.5, "text", sector.index = "a", track.index = 1)
circos.axis()


ggplot(indiv_nonzero, aes(D4_numeric, value)) + geom_point()

circos.initialize(factors = df$factors, x = df$x)
circos.track(factors = df$factors, y = df$y)

circos.track(factors = df$factors, y = df$y,
             panel.fun = function(x, y) {
               circos.text(CELL_META$xcenter, CELL_META$cell.ylim[2] + uy(5, "mm"), 
                           CELL_META$sector.index)
               circos.axis(labels.cex = 0.6)
             })
col = rep(c("#FF0000", "#00FF00"), 4)
circos.trackPoints(df$factors, df$x, df$y, col = col, pch = 16, cex = 0.5)
circos.text(-1, 0.5, "text", sector.index = "a", track.index = 1)



