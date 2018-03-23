
working_dir <- "/Users/alanalexander"
file <- "lamprey.ustr"
firstsample <- "3.4"
secondsample <- "3.6"

combining_samples_from_structure <- function(working_dir,structure_file,firstsample,secondsample) {

library(tidyverse)
setwd(working_dir)
orig_structure <- t(read.table(file,header=FALSE))
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

  
new_col_create <- function(sample1,sample2) {
  subsetcols <- which(structure_names %in% c(sample1,sample2))                    
  new_allele <- rep(NA,dim(mod_structure)[1])  
  new_allele[(which(mod_structure[,subsetcols[1]]!=mod_structure[,subsetcols[2]] & mod_structure[,subsetcols[1]]!=-9 & mod_structure[,subsetcols[2]]!=-9))] <- "-9"
  first_no_data <- which(mod_structure[,subsetcols[1]]==-9 & is.na(new_allele[]))
  new_allele[first_no_data] <- as.character(mod_structure[first_no_data,subsetcols[2]])
  second_no_data <- which(mod_structure[,subsetcols[2]]==-9 & is.na(new_allele[]))                    
  new_allele[second_no_data] <- as.character(mod_structure[second_no_data,subsetcols[1]])
  new_allele[is.na(new_allele[])] <- as.character(mod_structure[is.na(new_allele[]),subsetcols[1]]) 
  mod_structure <- cbind(mod_structure,new_allele)
  return(mod_structure)
}
  
mod_structure <- new_col_create(firstsample,secondsample)  

newname <- paste(firstsample,secondsample,sep="_")
newname_1 <- paste(firstsample,secondsample,"1",sep="_") 
                    
firstsample <- paste(firstsample,"_1",sep="")
secondsample <- paste(secondsample,"_1",sep="")                    
                    
mod_structure <- new_col_create(firstsample,secondsample) 

todelete <- c(grep(firstsample,structure_names),grep(secondsample,structure_names))
todelete <- c(todelete,(todelete-1))
  
mod_structure <- mod_structure[,-todelete]
structure_names <- structure_names[-todelete]
structure_names <- c(structure_names,newname,newname_1)
  
mod_structure <- rbind(structure_names,mod_structure)  
mod_structure <- t(mod_structure)
  
write.table(mod_structure,"new_structure.stru",quote=FALSE,row.names=FALSE,col.names=FALSE)

