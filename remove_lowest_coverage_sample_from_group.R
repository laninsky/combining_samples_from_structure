remove_lowest_coverage_sample_from_group <- function(working_dir,structure_file,missing,relatedness_file) {

# e.g. remove_lowest_coverage_sample_from_group("/Users/alanaalexander/Dropbox/lamprey/Allison_18Jan2019_structure/NZ","NZ_ONLY_assembly.str","-9","related_individuals.txt")

# Loading libraries
library(readr)
library(dplyr)

# Setting working directory  
setwd(working_dir)

# Reading in original structure file  
orig_structure <- readLines(structure_file)

# Transposing first row of data and converting it to tibble  
transposed_structure <- matrix(unlist(strsplit(orig_structure[1],"\\s")),ncol=1)
transposed_structurename <- gsub(" ","",transposed_structure[1])
transposed_structure <- tibble(transposed_structure[-1])
names(transposed_structure) <- transposed_structurename

# Printing out some characteristics of the data so people will know where we are up to  
print(paste(length(orig_structure)/2, " individuals are present in this file (",length(orig_structure)," alleles)",sep=""))
print("Now transposing data")

# Transposing second row and onwards  
for (i in 2:length(orig_structure)) {
  temprow <- matrix(unlist(strsplit(orig_structure[i],"\\s")),ncol=1)
  # Grabbing sample name
  temprowname <- gsub(" ","",temprow[1])
  # If first allele is already present in the dataset, renaming to "_A", and calling this current second allele "_B"
  if (temprowname %in% names(transposed_structure)) {
    names(transposed_structure) <- paste(temprowname,"_A",sep="")
    temprowname <- paste(temprowname,"_B",sep="")
  }
  # Grabbing data (and not name)
  temprow <- tibble(temprow[-1])
  # Grabbing name
  names(temprow) <- temprowname
  # Binding this allele to the rest of the data
  transposed_structure <- bind_cols(transposed_structure,temprow)
  print(paste("Up to ",i," out of ",length(orig_structure)," alleles",sep=""))
}

# Reading in data. These are the pairs you want to just take one of: first sample is in column 1, second sample is column 2.  
relatedness <- read_tsv(relatedness_file)
removed_samples <- "removed_samples"

for (i in 1:dim(relatedness)[1]) {
  # Getting allele names for each sample
  missing1Aname <- paste(relatedness[i,1],"_A",sep="")
  missing1Bname <- paste(relatedness[i,1],"_B",sep="")
  missing2Aname <- paste(relatedness[i,2],"_A",sep="")
  missing2Bname <- paste(relatedness[i,2],"_B",sep="")
  
  if (missing1Aname %in% names(transposed_structure) & missing2Aname %in% names(transposed_structure)) {
    missing1count <- as.numeric(as.matrix(transposed_structure %>% 
      select(missing1Aname,missing1Bname) %>% 
      filter(.[[1]]==missing | .[[2]]==missing) %>% count())[1,1])
    missing2count <- as.numeric(as.matrix(transposed_structure %>% 
      select(missing2Aname,missing2Bname) %>% 
      filter(.[[1]]==missing | .[[2]]==missing) %>% count())[1,1])
    if (missing1count >= missing2count) {
      transposed_structure <- transposed_structure %>% select(-c(missing1Aname,missing1Bname))
      print(paste(relatedness[i,1]," has been removed from dataset. ",relatedness[i,2]," has been retained",sep=""))
      removed_samples <- rbind(removed_samples,as.matrix(relatedness[i,1]))
    } else {
      transposed_structure <- transposed_structure %>% select(-c(missing2Aname,missing2Bname))
      print(paste(relatedness[i,2]," has been removed from dataset. ",relatedness[i,1]," has been retained",sep=""))
      removed_samples <- rbind(removed_samples,as.matrix(relatedness[i,2]))
    }
  } else {
    if (!(missing1Aname %in% names(transposed_structure) & missing2Aname %in% names(transposed_structure))) {
      print(paste("Both ",relatedness[i,1], " and ",relatedness[i,2], " have already been removed from the dataset",sep=""))
    } else {
      if (missing1Aname %in% names(transposed_structure)) {
        print(paste(relatedness[i,2]," has already been removed from dataset, so ",relatedness[i,1]," remains",sep=""))
      } else {
        print(paste(relatedness[i,1]," has already been removed from dataset, so ",relatedness[i,2]," remains",sep=""))
      }
    }
  }
}

print(paste((dim(removed_samples)[1]-1), " samples have been removed and are listed in removed_samples.txt"))
write.table(removed_samples,"removed_samples.txt",col.names=FALSE,row.names=FALSE,quote=FALSE)
print("The modified structure file has been written out to pruned_structure.stru")
transposed_structure <- t(transposed_structure)  
write.table(t(transposed_structure),"pruned_structure.stru",quote=FALSE,row.names=TRUE,col.names=FALSE)

}
