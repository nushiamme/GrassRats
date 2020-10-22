## Processing microbiome files

## Load packages ----
  # I think some of these are no longer actually required
  pacman::p_load(knitr, BiocStyle, ggplot2, gridExtra, dada2, phyloseq, DECIPHER, phangorn, magrittr, 
                 plyr, dplyr, picante, decontam, DESeq2, GUniFrac, vegan, here, reshape2)
  
## Settings to apply throughout ----
  ## Set random seed
    set.seed(297)
  
## Specify where fastq files are stored
  miseq_path <- here("/1_data/16s_sequences/")
  head(list.files(miseq_path))
  
## Begin to process the read files
  # Sort ensures forward/reverse reads are in same order
    fnFs <- sort(list.files(miseq_path, pattern="_R1.fastq"))
    fnRs <- sort(list.files(miseq_path, pattern="_R2.fastq"))
  # Extract sample names, assuming filenames have format: SAMPLENAME_XXX.fastq
    sampleNames <- sapply(strsplit(fnFs, "_"), `[`, 7)
  # Specify the full path to the fnFs and fnRs
    fnFs <- file.path(miseq_path, fnFs)
    fnRs <- file.path(miseq_path, fnRs)
    
## Trim and filter, plot the read qualities
    plotQualityProfile(fnFs[c(1:3)])
    plotQualityProfile(fnRs[c(1:3)])
    
  ## Filter based on plots above to 180 bp long
    ## Define filenames for the new filtered files
      filt_path<-file.path(miseq_path,"filtered")
      if(!file_test("-d", filt_path)) dir.create(filt_path)
      filtFs <- file.path(filt_path, paste0(sampleNames, "_F_filt.fastq.gz"))
      filtRs <- file.path(filt_path, paste0(sampleNames, "_R_filt.fastq.gz"))  
    
    ## Filter out reads based on the numbers that we decided on and some other
    ## expected error rates. Both forward and reverse reads from a pair have
    ## to pass in order to be included.
      out<-filterAndTrim(fnFs,filtFs,fnRs,filtRs,truncLen=c(180, 181),trimLeft=c(19,20),
                  maxN=0,maxEE=c(2,2),truncQ=2,rm.phix=TRUE,compress=TRUE,multithread=TRUE)
      # change trunclen back to 180,181, change trimleft to 19,20, make longer and 20, 20 for coi
      
## Running DADA2
      ## DEREPLICATION (this is the section using dada2 to assign ASVs)
        derepFs<-derepFastq(filtFs,verbose=TRUE)
        derepRs<-derepFastq(filtRs,verbose=TRUE)
      ## Name the derep-class objects by the sample names
        names(derepFs) <- sampleNames
        names(derepRs) <- sampleNames 
      ## Unsupervised learning to build the models that characterize error rates
        errF<-learnErrors(filtFs,multithread=TRUE)
        errR<-learnErrors(filtRs,multithread=TRUE)  
      ## Look at the results of the error learning runs
        plotErrors(errF)
        plotErrors(errR)  
        
## Calling ASVs
        ## Inference of ASVs using the error models built above          
          dadaFs<-dada(derepFs,err=errF,multithread=TRUE)
          dadaRs<-dada(derepRs,err=errR,multithread=TRUE)
                  
        ## Inspect the outcome
          dadaFs[[1]]
          dadaRs[[1]] 
          
## Merge forward and reverse reads
        ## Construct the sequence table with the cleaned dada data
          mergers<-mergePairs(dadaFs,derepFs,dadaRs,derepRs)
          seqtabAll<-makeSequenceTable(mergers)
          seqtab<-seqtabAll[,nchar(colnames(seqtabAll)) %in% seq(247, 270)]  
          table(nchar(getSequences(seqtab)))

## Remove chimeras
          seqtab.nochim<-removeBimeraDenovo(seqtab)
          
## Produce table showing results of all filtering steps so far
        ## Check on table for all the filtering steps taken so far      
          getN<-function(x) sum(getUniques(x))
          track <- cbind(out, sapply(dadaFs, getN), sapply(mergers, getN), rowSums(seqtab), rowSums(seqtab.nochim))
          colnames(track) <- c("input", "filtered", "denoised", "merged", "tabled", "nonchim")
          rownames(track) <- sampleNames
          head(track)  
          
## Assign taxonomy
        ## Assign taxonomy. The silva file is donloaded and placed in the folder ahead of time.
          fastaRef <- here("/1_data/silva_nr_v132_train_set.fa.gz")
          taxTab <- assignTaxonomy(seqtab.nochim, refFasta = fastaRef, multithread = TRUE)          
          unname(head(taxTab))
          
## Build phylogenetic tree based on samples identified
      ## Construct phylogenetic tree
          seq<-getSequences(seqtab.nochim)
          names(seq)<-seq # This propagates to the tip labels of the tree
          alignment <-AlignSeqs(DNAStringSet(seq),anchor=NA,verbose=FALSE)
          
          phangAlign<-phyDat(as(alignment,"matrix"),type="DNA")
          dm<-dist.ml(phangAlign)
          treeNJ<-NJ(dm) #note, tip order != sequence order
          fit=pml(treeNJ,data=phangAlign)
          fitGTR<-update(fit,k=4,inv=0.2)
          fitGTR<-optim.pml(fitGTR,model="GTR",optInv=TRUE,optGamma=TRUE,
                            rearrangement="stochastic",control=pml.control(trace=0))
          
## save the objects
          saveRDS(fitGTR, here("3_output/fit_GTR.rds"))
          saveRDS(taxTab, here("3_output/tax_tab.rds"))
          saveRDS(seqtab.nochim, here("3_output/seqtab.rds"))
          
## Build phyloseq object
      ## Combine otu table, sample data, taxonomy table, and tree into phyloseq object
          samdf <- read.delim(here("1_data/sampdata.txt"))
          samdf$sampleID<-as.character(samdf$sampleID)
          rownames(samdf)<-samdf$sampleID
          ps<-phyloseq(otu_table(seqtab.nochim,taxa_are_rows=FALSE),
                       sample_data(samdf),
                       tax_table(taxTab),
                       phy_tree(fitGTR$tree))
          ps
      ## Save the original unmodified phyloseq objec to disk
          saveRDS(ps, here("3_output/tres18_original_ps.rds"))
          
          
## Remove eukaryotes, archaea, chloroplasts, and mitochondria
          ps2<-subset_taxa(ps,
                           Kingdom == "Bacteria" &
                             Family != "mitochondria" &
                             Class != "Chloroplast"
            )
            ps2
            
## Remove contaminants with decontam
        ## Identify contaminants based on combination of prevalence and frequency then remove them
            sample_data(ps2)$is.neg <- sample_data(ps2)$Sample_or_Control == "Control Sample"
            contamdf.prev <- isContaminant(ps2, method="combined", conc="quant_reading", neg="is.neg")
            table(contamdf.prev$contaminant)
            ps.nc<-prune_taxa(!contamdf.prev$contaminant, ps2)
            
## Plot sample distribution of read depths
         ## Plot distribution of sample read depths
            sample_sum_df <- data.frame(sum = sample_sums(ps.nc))
            
            ggplot(sample_sum_df, aes(x = sum)) +
              geom_histogram(color = "black", fill = "indianred", binwidth = 2500) +
              ggtitle("Distribution of sample sequencing depth") +
              xlab("Read counts") +
              theme(axis.title.y = element_blank())
            
## Remove singletons
        ps.no.sing <- prune_taxa(taxa_sums(ps.nc) > 1,ps.nc)
        
## Save this phyloseq object to disk
        saveRDS(ps.no.sing, here("3_output/tres_ps.rds"))
        tres_ps <- readRDS(here("3_output/tres_ps.rds"))
        
## Rarify and then estimate diversity in phyloseq and picante
        pr <- prune_samples(sample_sums(tres_ps) >= 1000, tres_ps)
        pr1 <- rarefy_even_depth(pr, sample.size = 1000, rngseed = 121, trimOTUs = TRUE)
        pr_rich <- estimate_richness(pr1)
        OTU <- phyloseq::otu_table(pr1)
        if(taxa_are_rows(OTU)){
          OTU<-t(OTU)
        }
        otutable <- as(OTU,"matrix")
        tree <- phyloseq::phy_tree(pr1)
        pdt <- picante::pd(otutable, tree,include.root = FALSE)
        pr_rich$sampleID <- rownames(pr_rich)
        pdt$sampleID <- rownames(pdt)
        rich <- join(pdt, pr_rich, "sampleID")  
        out.rich <- join(samdf, rich, "sampleID")
        write.csv(out.rich, here("3_output/RowPerSample.csv"))
        out.rich$band3 <- paste(out.rich$band, "3", sep="_")
        r3 <- subset(out.rich, out.rich$cap.num == 3)
        r3 <- r3[,c("sampleID", "base", "stress", "dex", "mass", "PD", "Shannon", "Simpson", "InvSimpson", "band3")]
        colnames(r3) <- c("sampleID3", paste(colnames(r3)[2:9], "_3", sep=""), "band3")
        out.rich2 <- subset(out.rich, out.rich$cap.num == 1 | is.na(out.rich$cap.num) == TRUE)
        out.rich2 <- join(out.rich2, r3, "band3")
        write.csv(out.rich2, here("3_output/18_Richness.csv"))
        
## Create relative abundance table at phylum level
      phy_glom <- tres_ps %>%
        tax_glom(taxrank = "Phylum") %>%
        transform_sample_counts(function(x) {x / sum(x)}) %>%
          psmelt() %>%
          filter(Abundance > 0.001) %>%
          arrange(Phylum)
      
      phy_glom$sID_cap <- paste(phy_glom$sampleID, phy_glom$glom$cap.num)
      library(reshape2)
      phy_glom2 <- dcast(phy_glom, sID_cap ~ Phylum, value.var = "Abundance")
      phy_glom2[is.na(phy_glom2)] <- 0
      phy_glom_x <- phy_glom[,c("Sample", "category", "band", "site", "nest", "soc_ub", "nat_ub", "sex", "cap.date", "cap.num", "Age",
                                "treatment", "trt1", "trt2", "cidate", "fledged", "clutch", "maxbrood", "d6num", "d6mass", "d6permass",
                                "numband", "numfled", "fled_age", "base", "stress", "dex", "acth", "glucose.b", "glucose.s", "bbright",
                                "mass", "hbill", "wing", "BUTY", "back_feath", "brst_feath", "crossed", "wp_ep", "d18_bout", "sID_cap")]
      phy_glom3 <- join(phy_glom2, phy_glom_x, "sID_cap", match = "first")
      write.csv(phy_glom3, here::here("3_output/2018_phylum_rel_abund.csv"))
      
## Create relative abundance table at order level
      ord_glom <- tres_ps %>%
        tax_glom(taxrank = "Order") %>%
        transform_sample_counts(function(x) {x / sum(x)}) %>%
        psmelt() %>%
        filter(Abundance > 0.001) %>%
        arrange(Order)
      
      ord_glom$sID_cap <- paste(ord_glom$sampleID, ord_glom$cap.num)
      ord_glom$tax <- paste(ord_glom$Order, ord_glom$Class, ord_glom$Order, sep = "_")
      ord_glom2 <- dcast(ord_glom, sID_cap ~ tax, value.var = "Abundance")
      ord_glom2[is.na(ord_glom2)] <- 0
      ord_glom_x <- ord_glom[,c("Sample", "category", "band", "site", "nest", "soc_ub", "nat_ub", "sex", "cap.date", "cap.num", "Age",
                                "treatment", "trt1", "trt2", "cidate", "fledged", "clutch", "maxbrood", "d6num", "d6mass", "d6permass",
                                "numband", "numfled", "fled_age", "base", "stress", "dex", "acth", "glucose.b", "glucose.s", "bbright",
                                "mass", "hbill", "wing", "BUTY", "back_feath", "brst_feath", "crossed", "wp_ep", "d18_bout", "sID_cap")]
      ord_glom3 <- join(ord_glom2, ord_glom_x, "sID_cap", match = "first")
      write.csv(ord_glom3, here::here("3_output/2018_order_rel_abund.csv"))
      

      ggplot(ord_glom, aes(x = sampleID, y = Abundance, fill = Phylum)) +
        facet_grid(cap.num ~ .) +
        geom_bar(stat = "identity")

      
      