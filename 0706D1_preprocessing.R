##CATS cam tag pre-processing script for 0706D1 deployment in South Africa, 2017
##drafted by JHMoxley, July 2017

#prep workbench
rm(list = ls())
library(dplyr)
library(ggplot2)

#set working drive to git repo
wd <- "/Users/jmoxley/Documents/MBA_GWS/GitTank/CC_CamTags"

#set up data drive, file structure, & empty data frame
dd <- "/Volumes/UNTITLED 1/CamTag/SA2017_raw"; clip = 26
deployID <- "0706D1"
projID <- "SA2017"
sppID <- "CC"
df <- data.frame()

#data load in
setwd(file.path(dd, paste(sppID, projID, deployID, sep="_")))
temp <- list.files(pattern="*.csv")
dat <- lapply(temp, read.csv, fileEncoding = "latin1")
setwd(wd)

df <- do.call('rbind', dat)
datFreq = 100; #100Hz data 



