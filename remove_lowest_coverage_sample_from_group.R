working_dir <- "/Users/alanaalexander/Dropbox/lamprey/Allison_18Jan2019_structure/NZ"
structure_file <- "NZ_ONLY_assembly.str"
missing <- "-9"
relatedness_file <- "related_individuals.txt"

remove_lowest_coverage_sample_from_group <- function(working_dir,structure_file,missing,relatedness_file) {
  
library(readr)
library(dplyr)
setwd(working_dir)
  
orig_structure <- readLines(structure_file)

transposed_structure <- matrix(unlist(strsplit(orig_structure[1],"\t")),ncol=1)
transposed_structurename <- gsub(" ","",transposed_structure[1])
transposed_structure <- tibble(transposed_structure[-1])
transposed_structure <- transposed_structure[-(which(transposed_structure=="")),1]
names(transposed_structure) <- transposed_structurename

for (i in 2:length(orig_structure)) {
  temprow <- matrix(unlist(strsplit(orig_structure[i],"\t")),ncol=1)
  temprowname <- gsub(" ","",temprow[1])
  if (temprowname %in% names(transposed_structure)) {
    names(transposed_structure)[which(names(transposed_structure)==temprowname)] <- paste(temprowname,"_A",sep="")
    temprowname <- paste(temprowname,"_B",sep="")
  }
  temprow <- tibble(temprow[-1])
  temprow <- temprow[-(which(temprow=="")),1]
  names(temprow) <- temprowname
  transposed_structure <- bind_cols(transposed_structure,temprow)
  print(paste("Up to ",i," out of ",length(orig_structure),sep=""))
}

### UP TO HERE

sample1cols <- which(structure_names %in% sample1)
sample2cols <- which(structure_names %in% sample2)

sample1NAs <- sum(mod_structure[,sample1cols]==missing)
sample2NAs <- sum(mod_structure[,sample2cols]==missing)

if(sample1NAs >= sample2NAs) {
  newfilename <- paste(sample1,"_removed.stru",sep="")
  print(paste(sample1," has more missing data than ", sample2," so has been removed in ",newfilename,sep=""))
  mod_structure <- orig_structure[,-sample1cols]
  mod_structure <- t(mod_structure)
  write.table(mod_structure,newfilename,quote=FALSE,row.names=FALSE,col.names=FALSE)
} else {
  newfilename <- paste(sample2,"_removed.stru",sep="")
  print(paste(sample2," has more missing data than ", sample1," so has been removed in ",newfilename,sep=""))
  mod_structure <- orig_structure[,-sample2cols]
  mod_structure <- t(mod_structure)
  write.table(mod_structure,newfilename,quote=FALSE,row.names=FALSE,col.names=FALSE)
}
}
