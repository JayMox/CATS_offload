#script for testing of automation for CATS tag loading. 
#prep workbench
rm(list = ls())
options(digits.secs = 10);
options(stringsAsFactors = FALSE)
library(tidyverse)
library(gRumble)
source('tag_fxns.R')

#set data drive & ingest meta data
dd <- "/Volumes/UNTITLED 1/MBA_Shark_Biologging_test"
tg_mdata <- read_csv(file.path(dd, "metadata_TagGuide_test.csv"))

i = 2
(deployID <- tg_mdata$ID[i])
(projID <- tg_mdata$ProjID[i]) 
rawdir <- file.path(dd, "tag_data_raw", tg_mdata$ProjID[i], tg_mdata$ID[i], "raw")
#check file pathway & GET DATA/METADATA
if(!dir.exists(rawdir)){print(paste("WARNING: directory does not exist; check pathways AND/OR deployment metadata in TagGuide for", deployID))}
if(dir.exists(rawdir)){   
  #scrape metadata
  mdata <- get.metadata(depid = deployID, dir = rawdir)
  sens <- mdata %>% filter(str_detect(field, "[:digit:]{2}\\_(name)") & active == "activated") %>% 
    mutate(name = str_replace(value, "\\s\\([:alnum:]*\\)", ""),
           key = str_sub(name, 1,4)) #remove parentheticals
  
  #stitch .csv files 
  tmp <- list.files(rawdir, pattern = "*.csv|*.CSV", full.names = T)
  print(paste(length(tmp), "CSV files to be stitched for", deployID))
  df <- do.call('rbind', lapply(tmp, read.csv, fileEncoding = "latin1", stringsAsFactors = F, nrows = 100))
}  #load data, mdata, & sens data

######################
##THIS IS WHERE YOU DO DA TIDX & INFER SFREQ
####need some sort of regex definition for keeping UTC and local separate!!!!!
######################

#sketch up code w/ pressure sensor, evaluate possibly looping through sensor list & colnames to determine what conversions are necessary
test <- full_join(data.frame(name = str_split(colnames(df), "\\.{3,}", simplify = T)[,1], 
                             channel = str_split(colnames(df), "\\.{3,}", simplify = T)[,2], 
                             key = str_sub(colnames(df), 1,4),colnames(df)), sens, by = "key")

cols <- bind_rows(cols, test)


