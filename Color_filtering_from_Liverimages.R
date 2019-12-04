# Run this in the shell with Image Magick installed to remove certain colors 
# in this case black and 10% fuzz for similar colors

#convert respirometry_graph.jpg -channel rgba -alpha set -fuzz 10% -fill none -opaque cyan respir_test.png

#convert SaveMyTime_sample_graph.jpeg -channel rgba -alpha set -fuzz 10% -fill none -opaque white SaveMyTime_sample_graph_transp.jpeg

#To transform cyan to red and save into test folder
#mogrify .\Desktop\test -format png -fill red -fuzz 20% -opaque cyan *.png

## Trying this for liver slides, grass rats, Nov 2019
setwd("D:/GR_liver/Export/GF_Liver_job")
liver6.1.001<- read.csv("108Threshold_Slide6Cassette1-Region 001-1.txt")
liver6.1.001<- read.delim("108Threshold_Slide6Cassette1-Region 001-1_crop.txt", sep = "\t")


write.table(liver6.1.001, file = paste0("liver6.1.001", ".csv"), sep = ",", row.names = FALSE, col.names = FALSE, quote = "\"'")

liver6.1.001 <- read.csv("liver6.1.001.csv", sep="\t")

head(liver6.1.001)
