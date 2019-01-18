#working_dir <- "/Users/alanaalexander/Dropbox/lamprey"
#structure_file <- "new_structure.stru"
#sample1 <- "3.Lpry"
#sample2 <- "13.Lpry"
#missing <- "-9"

remove_lowest_coverage_sample_from_group <- function(working_dir,structure_file,missing,sample1,sample2) {

library(tidyverse)
setwd(working_dir)
orig_structure <- t(read.table(structure_file,header=FALSE))
structure_names <- orig_structure[1,]

if (!(sample1 %in% structure_names)) {
  stop(paste(sample1," does not exist in your structure file",sep=""))
}
if (!(sample2 %in% structure_names)) {
  stop(paste(sample2," does not exist in your structure file",sep=""))
}
                    
mod_structure <- orig_structure[-1,]

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
