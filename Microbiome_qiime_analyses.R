## Anusha Shankar
## Grass Rat microbiome analyses

## Processing microbiome files

#https://gist.github.com/erictleung/eda33bceceebb190eb9a44c93a077d32

#Packages
#Part1 
library(here)

#Part 2
library(phyloseq)
library(ggplot2)
library(ape)

#Part 3
library(qiime2R)
library(tidyverse)
library(plyr)

#Part 4
library(tidyverse)
library(qiime2R)
library(ggrepel) # for offset labels
library(ggtree) # for visualizing phylogenetic trees
library(ape) # for manipulating phylogenetic trees

##General functions
my_theme <- theme_classic(base_size = 30) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))

my_theme2 <- theme_classic(base_size = 10) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))

## Read in processed files
merged_file <- read.table(here("Microbiome_Data", "GRP_20191213_16S_qiime", "phyloseq", "combined_otu_tax.tsv"))

#### Part 1 - Don't need to re-run if reading in "combined_otu_tax.tsv") ####
# title: Manipulate QIIME2 output in R
# description: |
#   Take in output from 1_qiime_part.sh and manipulate files in R
# ---
#library(here)
in_path <- here("Microbiome_Data", "GRP_20191213_16S_qiime", "phyloseq")
otu <- read.table(file = here("Microbiome_Data", "GRP_20191213_16S_qiime", "phyloseq", "otu_table.txt"), header = TRUE)
head(otu)

#	Read in taxonomy table
tax	<- read.table(file = here("Microbiome_Data", "GRP_20191213_16S_qiime", "phyloseq", "taxonomy.tsv"), sep = '\t', header = TRUE)
head(tax)

names(otu)[1] <- "OTUID"
names(tax)[1] <- "OTUID"

# Merge files
merged_file <- merge(otu, tax, by.x = c("OTUID"), by.y = c("OTUID"))
head(merged_file)

# Note: the number of rows should equal your shortest file length, drops taxonomy
# for OTUs that don't exist in your OTU table

# # Output merged .txt file
out_path <- here("Microbiome_Data", "GRP_20191213_16S_qiime", "phyloseq", "combined_otu_tax.tsv")
write.table(merged_file, file = out_path, sep = "\t", col.names = TRUE, row.names = FALSE)

#######################
# # It seems tedious but you need to open the merged .txt file in excel and split into 
#two files: one for taxonomy (containing only the columns OTUID and taxonomic info) and the 
#other for the OTU matrix (containing only OTUID and abundances in each sample). 
#Note: for the taxonomy file, you need to use data —> text--‐to--‐columns in Excel and
#separate on semicolon to get columns for kingdom, phylum, class, etc… once you make these 
#two separate files in excel, save each #as a .csv 
#name them otu_matrix.csv and taxonomy.csv

#######################

#### Part 2 ####

# Read in OTU table
otu_table_in <- read.csv(here("Microbiome_Data", "GRP_20191213_16S_qiime", "phyloseq", "otu_matrix.csv"), sep = ",", row.names = 1)
otu_table_in <- as.matrix(otu_table_in)

# Read in taxonomy
# Separated by kingdom, phylum, class, order, family, genus, species
taxonomy <- read.csv(here("Microbiome_Data", "GRP_20191213_16S_qiime", "phyloseq", "taxonomy.csv"), sep = ",", row.names = 1)
taxonomy <- as.matrix(taxonomy)

# Read in metadata
metadata <- read.csv(here("Microbiome_Data", "GRP_20191213_16S_qiime", "phyloseq", "metadata.csv"), row.names = 1, fill=T)
## Found this error in labelling Dec 2020, correcting GrassRat_ID for GRP021 from T5 to T9
## Did this manually for "metadata_cut_for_shannon.csv
metadata$GrassRat_ID[metadata$TubeID=="T9 L.int"] <- "T9"


# Read in tree
phy_tree <- read_tree(here("Microbiome_Data", "GRP_20191213_16S_qiime", "phyloseq", "tree.nwk"))

# Import all as phyloseq objects
OTU <- otu_table(otu_table_in, taxa_are_rows = TRUE)
TAX <- tax_table(taxonomy)
META <- sample_data(metadata, )

# Sanity checks for consistent OTU names
taxa_names(TAX)
taxa_names(OTU)
taxa_names(phy_tree)

# Same sample names
sample_names(OTU)
sample_names(META)

# Finally merge!
ps <- phyloseq(OTU, TAX, META, phy_tree)
ps

#### Part 3: Plots ####
## From QIIME2R tutorial 
#https://forum.qiime2.org/t/tutorial-integrating-qiime2-and-r-for-data-visualization-and-analysis-using-qiime2r/4121

Qmeta <- read.csv(here::here("Microbiome_Data", "GRP_20191213_16S_qiime", "phyloseq", "metadata_cut_for_shannon.csv"), fill=T)
shannon <-read_qza(here::here("Microbiome_Data", "GRP_20191213_16S_qiime", "core-metrics-results", "shannon_vector.qza"))

## Sanity check: Check for full overlap between metadata and shannon file
shannon<-shannon$data %>% rownames_to_column("SampleID") 
gplots::venn(list(metadata=Qmeta$SampleID, shannon=shannon$SampleID))


Qmeta$Sucrose_long <- Qmeta$Sucrose
Qmeta$Sucrose_long <- revalue(Qmeta$Sucrose_long, c("yes"="HighSucrose", "no" = "NoSucrose"))
Qmeta$PhotoSugar <- paste(Qmeta$Photoperiod, Qmeta$Sucrose_long, sep="_")
Qmeta$PhotoSugar[Qmeta$PhotoSugar=="NA_NA"] <- "NA"

# Join metadata and shannon
Qmeta<-  Qmeta %>% 
  left_join(shannon, by = "SampleID")
head(Qmeta)

## Remove NA's
Qmeta <- Qmeta[Qmeta$PhotoSugar != "NA",]

##Plotting alpha diversity across all time points
Qmeta %>%
  #filter(!is.na(shannon)) %>%
  #filter(!is.na(PhotoSugar)) %>%
  ggplot(aes(x=ParentPair, y=shannon_entropy, color=PhotoSugar)) +
  stat_summary(geom="errorbar", fun.data=mean_se, width=0, size=1) +
  stat_summary(geom="line", fun.data=mean_se, size=1.2, alpha=0.8) +
  stat_summary(geom="point", fun.data=mean_se) +
  xlab("Parent") +
  ylab("Shannon Diversity") +
  my_theme + # try other themes like theme_bw() or theme_classic()
  scale_color_viridis_d(name="Photoperiod_Sugar") # use different color scale which is color blind friendly
#ggsave("Shannon_by_time.pdf", height=3, width=4, device="pdf") # save a PDF 3 inches by 4 inches

ggplot(Qmeta, aes(ParentPair, PercentArea_liver_fat)) + geom_point(aes(size=AgeAtDeath_days)) + 
  facet_grid(.~Trial) + my_theme

##Alpha diversity across fecal samples
Qmeta %>%
  filter(Type=="Fecal") %>%
  ggplot(aes(x=Sample, y=shannon_entropy, color=PhotoSugar)) +
  stat_summary(geom="errorbar", fun.data=mean_se, width=0) +
  stat_summary(geom="line", fun.data=mean_se, size=1) +
  stat_summary(geom="point", fun.data=mean_se, size=2) +
  #facet_grid(.~Trial) +
  xlab("Days") +
  ylab("Shannon Diversity") +
  my_theme + theme(legend.key.height = unit(3,"line")) +
  scale_color_viridis_d(name="Photoperiod_Sugar") # use different color scale which is color blind friendly


##PCoA
uwunifrac<-read_qza(here::here("Microbiome_Data", "GRP_20191213_16S_qiime", "core-metrics-results", "unweighted_unifrac_pcoa_results.qza"))

####### TO DO December 3, 2020 ####
# DONE: 1. Devin: We did discover one error with the coding of T5 samples. 
#Sample GRP021 is listed as GrassRat_ID=T5, but the TubeID=T9 L.int. 
#The point does sort out with the other T9 samples in your latest graph, 
#so that's a positive improvement.
## DONE 2. Leaving G16 fecal out from all plots
## 3. Plot beta diversity: average unifrac distance  - OPTIONAL depending on time
#within treatment of Family 1 (or 2-4) at times A, B, C and then also 
#calculate between treatment unifrac distance.
## DONE 4. Change blue/purple to something else
## 5. Can I do the microbial group-specific analyses like here:
# https://jeb.biologists.org/content/223/3/jeb212548.long
##

# my_colors2 <- c("#efdb00", "#3d5887", "#032b43", "#a0b9c6", "#b24c63")
my_colors3 <- c("#ca054d", "#ffd400", "#032b43", "#7b9e89", "#1c448e")
# my_colors4 <- c("#ca054d", "#ffd400", "#032b43", "#f75c03", "#1c448e")
my_colors5 <- c("#004e64", "#ffba08", "#73a580", "#f786aa", "#685369")
# my_colors6 <- c("#004e64", "#ffba08", "#f7b2bd", "#c60f7b", "#bbc7a4")
# my_colors7 <- c("#ffe74c", "#508aa8", "#242f40", "#c60f7b", "#bbc7a4")
# my_colors8 <- c("#7f7caf", "#fcbf49", "#171738", "#f71735", "#c9e4ca")
# my_colors9 <- c("#5bba6f", "#297373", "#171738", "#9e4770", "#f8c630")
# my_colors10 <- c("#264653", "#2a9d8f", "#e9c46a", "#f4a261", "#e76f51")

## Colors are parent pairs, size is alpha diversity
uwunifrac$data$Vectors %>%
  select(SampleID, PC1, PC2) %>%
  left_join(Qmeta, by="SampleID") %>%
  filter(!is.na(PhotoSugar)) %>%
  filter(!is.na(ParentPair)) %>%
  filter(!(GrassRat_ID=="G16" & Sample=="A")) %>%
  #filter(Type=="Fecal") %>%
  ggplot(aes(x=PC1, y=PC2, shape=PhotoSugar, col=as.factor(ParentPair), size=shannon_entropy)) +
  geom_point(alpha=0.6, stroke=2) + #alpha controls transparency and helps when points are overlapping
  my_theme +
  scale_shape_manual(values=c(17,2,19,1,18), name="PhotoSugar") + #see http://www.sthda.com/sthda/RDoc/figure/graphs/r-plot-pch-symbols-points-in-r.png for numeric shape codes
  scale_size_continuous(range = c(1,12), name="Shannon Diversity") +
  #scale_color_viridis_d(name="ParentPair", end = .95) +
  scale_color_manual(name="ParentPair", values=my_colors3) +
  facet_grid(.~Trial) +
  guides(shape = guide_legend(override.aes = list(size=3)), color = guide_legend(override.aes = list(size=3)))
#ggsave("PCoA.pdf", height=4, width=5, device="pdf") # save a PDF 3 inches by 4 inches

## Colors are parent pairs, size is now liver fat
uwunifrac$data$Vectors %>%
  select(SampleID, PC1, PC2) %>%
  left_join(Qmeta, by="SampleID") %>%
  filter(!is.na(PhotoSugar)) %>%
  filter(!is.na(ParentPair)) %>%
  filter(!(GrassRat_ID=="G16" & Sample=="A")) %>%
  ggplot(aes(x=PC1, y=PC2, shape=PhotoSugar, col=as.factor(ParentPair), size=PercentArea_liver_fat)) +
  geom_point(alpha=0.6) + #alpha controls transparency and helps when points are overlapping
  my_theme +
  scale_shape_manual(values=c(17,2,19,1,18), name="PhotoSugar") + #see http://www.sthda.com/sthda/RDoc/figure/graphs/r-plot-pch-symbols-points-in-r.png for numeric shape codes
  scale_size_continuous(range = c(5,12), name="Shannon Diversity") +
  #scale_color_viridis_d(name="ParentPair", end = .9) +
  scale_color_manual(name="ParentPair", values=my_colors5) +
  facet_grid(.~Trial) +
  guides(shape = guide_legend(override.aes = list(size=3)), color = guide_legend(override.aes = list(size=3)))

## All trial one samples
uwunifrac$data$Vectors %>%
  select(SampleID, PC1, PC2) %>%
  left_join(Qmeta, by="SampleID") %>%
  filter(!is.na(PhotoSugar)) %>%
  filter(!is.na(ParentPair)) %>%
  filter(!(GrassRat_ID=="G16" & Sample=="A")) %>%
  #filter(Type=="Fecal") %>%
  filter(Trial==1) %>%
  ggplot(aes(x=PC1, y=PC2, shape=PhotoSugar, col=as.factor(ParentPair))) +
  geom_point(alpha=0.6, size=7) + #alpha controls transparency and helps when points are overlapping
  my_theme +
  scale_shape_manual(values=c(17,2,19,1,18), name="PhotoSugar") + #see http://www.sthda.com/sthda/RDoc/figure/graphs/r-plot-pch-symbols-points-in-r.png for numeric shape codes
  #scale_size_continuous(range = c(5,12), name="Shannon Diversity") +
  #scale_color_viridis_d(name="ParentPair", end = .9) +
  scale_color_manual(name="ParentPair", values=my_colors5) +
  geom_text(aes(label=paste(GrassRat_ID, Sample, sep=",")), alpha=1, col='black') +
  #facet_grid(.~Trial) +
  guides(shape = guide_legend(override.aes = list(size=3)), color = guide_legend(override.aes = list(size=3)))


## Only fecal samples
uwunifrac$data$Vectors %>%
  select(SampleID, PC1, PC2) %>%
  left_join(Qmeta, by="SampleID") %>%
  filter(!is.na(PhotoSugar)) %>%
  filter(!is.na(ParentPair)) %>%
  filter(!(GrassRat_ID=="G16" & Sample=="A")) %>%
  filter(Type=="Fecal") %>%
  #filter(Trial==2) %>%
  ggplot(aes(x=PC1, y=PC2, shape=PhotoSugar, col=as.factor(ParentPair))) +
  geom_point(alpha=0.6, size=7, stroke=2) + #alpha controls transparency and helps when points are overlapping
  my_theme +
  scale_shape_manual(values=c(17,2,19,1,18), name="PhotoSugar") + #see http://www.sthda.com/sthda/RDoc/figure/graphs/r-plot-pch-symbols-points-in-r.png for numeric shape codes
  #scale_size_continuous(range = c(5,12), name="Shannon Diversity") +
  #scale_color_viridis_d(name="ParentPair", end = .9) +
  scale_color_manual(name="ParentPair", values=my_colors5) +
  #geom_text(aes(label=paste0("T", Trial)), alpha=1, col='black') +
  #facet_grid(.~Trial) +
  guides(shape = guide_legend(override.aes = list(size=3)), color = guide_legend(override.aes = list(size=3)))


## Only fecal samples, and only for Trial 2 where we have all fecal sampling points
uwunifrac$data$Vectors %>%
  select(SampleID, PC1, PC2) %>%
  left_join(Qmeta, by="SampleID") %>%
  filter(!is.na(PhotoSugar)) %>%
  filter(!is.na(ParentPair)) %>%
  filter(Type=="Fecal") %>%
  filter(Trial==2) %>%
  ggplot(aes(x=PC1, y=PC2, shape=PhotoSugar, col=as.factor(ParentPair))) +
  geom_point(alpha=0.6, size=7, stroke=2) + #alpha controls transparency and helps when points are overlapping
  my_theme +
  scale_shape_manual(values=c(17,2,19,1,18), name="PhotoSugar") + #see http://www.sthda.com/sthda/RDoc/figure/graphs/r-plot-pch-symbols-points-in-r.png for numeric shape codes
  #scale_size_continuous(range = c(5,12), name="Shannon Diversity") +
  scale_color_manual(name="ParentPair", values=my_colors5) +
  #geom_text(aes(label=paste(GrassRat_ID, Sample, sep=",")), alpha=1, col='black') +
  #facet_grid(.~Trial) +
  guides(shape = guide_legend(override.aes = list(size=3)), color = guide_legend(override.aes = list(size=3)))

## Only intestinal samples
uwunifrac$data$Vectors %>%
  select(SampleID, PC1, PC2) %>%
  left_join(Qmeta, by="SampleID") %>%
  filter(!is.na(PhotoSugar)) %>%
  filter(!is.na(ParentPair)) %>%
  filter(Type=="Int") %>%
  ggplot(aes(x=PC1, y=PC2, shape=PhotoSugar, col=as.factor(ParentPair))) +
  geom_point(alpha=0.6, size=7, stroke=2) + #alpha controls transparency and helps when points are overlapping
  my_theme +
  scale_shape_manual(values=c(17,2,19,1,18), name="PhotoSugar") + #see http://www.sthda.com/sthda/RDoc/figure/graphs/r-plot-pch-symbols-points-in-r.png for numeric shape codes
  #geom_text(aes(label=paste(GrassRat_ID, ",", "T",Trial)), alpha=1, col='black') +
  #scale_size_continuous(range = c(5,12), name="Shannon Diversity") +
  scale_color_manual(values= my_colors5, name="ParentPair") +
  #facet_grid(.~Trial) +
  guides(shape = guide_legend(override.aes = list(size=3)), color = guide_legend(override.aes = list(size=3)))


## No size defining
uwunifrac$data$Vectors %>%
  select(SampleID, PC1, PC2) %>%
  left_join(Qmeta, by="SampleID") %>%
  filter(!is.na(PhotoSugar)) %>%
  filter(!is.na(ParentPair)) %>%
  #filter(Trial==1) %>%
  ggplot(aes(x=PC1, y=PC2, shape=PhotoSugar, col=as.factor(ParentPair))) +
  geom_point(alpha=0.6, size=7) + #alpha controls transparency and helps when points are overlapping
  my_theme +
  scale_shape_manual(values=c(17,2,19,1,18), name="PhotoSugar") + #see http://www.sthda.com/sthda/RDoc/figure/graphs/r-plot-pch-symbols-points-in-r.png for numeric shape codes
  #scale_size_continuous(range = c(5,12), name="Shannon Diversity") +
  scale_color_viridis_d(name="ParentPair", end = .9) +
  #facet_grid(.~Trial) +
  guides(shape = guide_legend(override.aes = list(size=3)), color = guide_legend(override.aes = list(size=3)))


## Label by individual and sampling point, color by sample type (fecal vs. intestinal)
uwunifrac$data$Vectors %>%
  select(SampleID, PC1, PC2) %>%
  left_join(Qmeta, by="SampleID") %>%
  filter(!is.na(PhotoSugar)) %>%
  filter(!is.na(ParentPair)) %>%
  #filter(Trial==1) %>%
  filter(!(GrassRat_ID=="G16" & Sample=="A")) %>%
  ggplot(aes(x=PC1, y=PC2, shape=PhotoSugar, col=as.factor(ParentPair))) +
  geom_point(alpha=0.6, size=7, stroke=2) + #alpha controls transparency and helps when points are overlapping
  my_theme + facet_grid(.~Type) +
  scale_shape_manual(values=c(17,2,19,1,18), name="PhotoSugar") + #see http://www.sthda.com/sthda/RDoc/figure/graphs/r-plot-pch-symbols-points-in-r.png for numeric shape codes
  #scale_size_continuous(range = c(5,12), name="Shannon Diversity") +
  scale_color_manual(name="Parent Pair", values=my_colors5) +
  #facet_grid(.~Trial) +
  #geom_text(aes(label=paste(GrassRat_ID, Sample, sep = ", ")), alpha=1, col='black') +
  guides(shape = guide_legend(override.aes = list(size=3)), color = guide_legend(override.aes = list(size=3)))

## Both trials
uwunifrac$data$Vectors %>%
  select(SampleID, PC1, PC2) %>%
  left_join(Qmeta, by="SampleID") %>%
  filter(!is.na(PhotoSugar)) %>%
  filter(!is.na(ParentPair)) %>%
  filter(!(GrassRat_ID=="G16" & Sample=="A")) %>%
  #filter(Type=="Fecal") %>%
  ggplot(aes(x=PC1, y=PC2, shape=PhotoSugar, col=as.factor(ParentPair), size=shannon_entropy)) +
  geom_point(alpha=0.6) + #alpha controls transparency and helps when points are overlapping
  my_theme +
  scale_shape_manual(values=c(17,2,19,1,18), name="PhotoSugar") + #see http://www.sthda.com/sthda/RDoc/figure/graphs/r-plot-pch-symbols-points-in-r.png for numeric shape codes
  scale_size_continuous(range = c(1,12), name="Shannon Diversity") +
  scale_color_viridis_d(name="ParentPair", end = .95) +
  facet_grid(.~Trial) +
  #geom_text(aes(label=paste(GrassRat_ID, Sample, sep = ", ")), size=2, alpha=1, col='black') +
  guides(shape = guide_legend(override.aes = list(size=3)), color = guide_legend(override.aes = list(size=3)))
#ggsave("PCoA.pdf", height=4, width=5, device="pdf") # save a PDF 3 inches by 4 inches



#### Heatmap ####
SVs<-read_qza(here::here("GRP_20191213_16S_qiime", "table-no-mitochondria-no-chloroplast.no-qc.qza"))$data
taxonomy<-read_qza(here::here("GRP_20191213_16S_qiime", "taxonomy.qza"))$data
Hmeta <- read.csv(here::here("GRP_20191213_16S_qiime", "phyloseq", "metadata_noPhotoperiodNA_noG28.csv"))


Hmeta$Sucrose_long <- Hmeta$Sucrose
Hmeta$Sucrose_long <- revalue(Hmeta$Sucrose_long, c("yes"="HighSucrose", "no" = "NoSucrose"))
Hmeta$PhotoSugar <- paste(Hmeta$Photoperiod, Hmeta$Sucrose_long, sep="_")
Hmeta$PhotoSugar[Hmeta$PhotoSugar=="NA_NA"] <- "NA"

SVs<-apply(SVs, 2, function(x) x/sum(x)*100) #convert to percent

SVsToPlot<-  
  data.frame(MeanAbundance=rowMeans(SVs)) %>% #find the average abundance of a SV
  rownames_to_column("Feature.ID") %>%
  arrange(desc(MeanAbundance)) %>%
  top_n(30, MeanAbundance) %>%
  pull(Feature.ID) #extract only the names from the table

SVs %>%
  as.data.frame() %>%
  rownames_to_column("Feature.ID") %>%
  gather(-Feature.ID, key="SampleID", value="Abundance") %>%
  mutate(Feature.ID=if_else(Feature.ID %in% SVsToPlot,  Feature.ID, "Remainder")) %>% #flag features to be collapsed
  group_by(SampleID, Feature.ID) %>%
  #summarize(Abundance=sum(Abundance)) %>%
  inner_join(Hmeta, by="SampleID") %>% ##Used to be left_join but that kept NAs from SVs in
  mutate(NormAbundance=log10(Abundance+0.01)) %>% # do a log10 transformation after adding a 0.01% pseudocount. Could also add 1 read before transformation to percent
  inner_join(taxonomy, by="Feature.ID") %>%  ##Used to be left_join but that kept NAs from SVs in
  mutate(Feature=paste(Feature.ID, Taxon)) %>%
  mutate(Feature=gsub("[kpcofgs]__", "", Feature)) %>% # trim out leading text from taxonomy string
  ggplot(aes(x=SampleID, y=Feature, fill=NormAbundance)) +
  geom_tile() +
  facet_grid(~PhotoSugar, scales="free_x") +
  my_theme2 +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  scale_fill_viridis_c(name="log10(% Abundance)")
#ggsave("heatmap.pdf", height=4, width=11, device="pdf") # save a PDF 3 inches by 4 inches

##########

## Part 3 ##
#Differential abundances 
results<-read_qza(here::here("GRP_20191213_16S_qiime", "differentials.qza"))$data
taxonomy<-read_qza("taxonomy.qza")$data
tree<-read_qza("rooted-tree.qza")$data
Volcano Plot
results %>%
  left_join(taxonomy) %>%
  mutate(Significant=if_else(we.eBH<0.1,TRUE, FALSE)) %>%
  mutate(Taxon=as.character(Taxon)) %>%
  mutate(TaxonToPrint=if_else(we.eBH<0.1, Taxon, "")) %>% #only provide a label to signifcant results
  ggplot(aes(x=diff.btw, y=-log10(we.ep), color=Significant, label=TaxonToPrint)) +
  geom_text_repel(size=1, nudge_y=0.05) +
  geom_point(alpha=0.6, shape=16) +
  theme_q2r() +
  xlab("log2(fold change)") +
  ylab("-log10(P-value)") +
  theme(legend.position="none") +
  scale_color_manual(values=c("black","red"))
ggsave("volcano.pdf", height=3, width=3, device="pdf")