
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
                    
mod_structure <- as.tibble(orig_structure[-1,])
names(mod_structure) <- structure_names
          
which(names(mod_structure) %in% c(sample1,sample2))                    
                    
                    
                    
                    
new_allele <- matrix(NA,ncol=1,nrow=dim(mod_structure)[2])                    
new_allele                    
                    
                    
                    
newname <- paste(sample1,sample2,sep="_")
