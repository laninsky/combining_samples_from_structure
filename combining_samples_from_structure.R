
working_dir <- "/Users/alanalexander"
file <- "lamprey.ustr"
sample1 <- "3.4"
sample2 <- "3.6"

combining_samples_from_structure <- function(working_dir,structure_file,sample1,sample2) {

library(tidyverse)
setwd(working_dir)
orig_structure <- t(read.table(file,header=FALSE)
structure_names <- orig_structure[1,]
unique_names <- NULL  
for (i in 1:length(structure_names)) {
  if(structure_names[i] %in% unique_names) {
    structure_names[i] <- paste(structure_names[i],"_1",sep="")
  } else {
    unique_names <- c(unique_names,structure_names[i])
  }
}  
                    
mod_structure <- orig_structure[-1,]
                    
subsetcols <- which(structure_names %in% c(sample1,sample2))                    
                    
new_allele <- rep(NA,dim(mod_structure)[1])  
new_allele[(which(mod_structure[,subsetcols[1]]!=mod_structure[,subsetcols[2]] & mod_structure[,subsetcols[1]]!=-9 & mod_structure[,subsetcols[2]]!=-9))] <- "-9"

first_no_data <- which(mod_structure[,subsetcols[1]]==-9 & is.na(new_allele[]))
new_allele[first_no_data] <- as.character(mod_structure[first_no_data,subsetcols[2]])
second_no_data <- which(mod_structure[,subsetcols[2]]==-9 & is.na(new_allele[]))                    
new_allele[second_no_data] <- as.character(mod_structure[second_no_data,subsetcols[1]])
                    
                    
is.na(new_allele[,1])                    
                    
                    
newname <- paste(sample1,sample2,sep="_")
