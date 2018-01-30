#script for testing of automation for CATS tag loading. 
#prep workbench
rm(list = ls())
options(digits.secs = 3);
library(tidyverse)
library(gRumble)
source('tag_fxns.R')

#set data drive & ingest meta data
dd <- "/Volumes/UNTITLED 1/MBA_Shark_Biologging_test"
tg_mdata <- read_csv(file.path(dd, "metadata_TagGuide_test.csv"))
ids <- mdata$ID
i = 1
id = ids[i]


#check file pathway & data available
rawdir <- file.path(dd, "tag_data_raw", tg_mdata$ProjID[i], tg_mdata$ID[i], "raw")
deployID <- tg_mdata$ID[i]; projID <- tg_mdata$ProjID[i]; 
if(!dir.exists(rawdir)){print("NO FILES WARNING: check pathways AND/OR deployment metadata in TagGuide")}
if(dir.exists(rawdir)){
  
}
