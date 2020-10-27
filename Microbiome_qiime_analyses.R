## Anusha Shankar
## Grass Rat microbiome analyses

## Processing microbiome files

## Load packages ----
# I think some of these are no longer actually required

library(phyloseq)
setwd("E://Google Drive//Anusha_personal//Fairbanks//Research//GrassRats//Microbiome_Data//GRP_20191213_16S_qiime")

import_biom(BIOMfilename = "rarefied-class-feature-table.biom")