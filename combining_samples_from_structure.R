
working_dir <- "/Users/alanalexander"
file <- "lamprey.ustr"
sample1 <- "3.4"
sample2 <- "3.6"

combining_samples_from_structure <- function(working_dir,structure_file,sample1,sample2) {

library(tidyverse)
setwd(working_dir)
orig_structure <- as.tibble(t(read.table(file,header=FALSE)))
