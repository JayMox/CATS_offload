#script for inputingg 0706_D2 deployment data & evaluating behavioral trigger
#set up data drive & null data frame
#dd = "/Volumes/UNTITLED 1"
library(ggplot2)
library(dplyr)

#arrange data drives
dd <- "/Volumes/UNTITLED 1/CamTag/"
setwd(dd)

#read in indi's data
id <- "CC_SA2017_7_06_D2"
(tmp <- list.files(path = "~/CC_SA2017_7_06_D2/", pattern = "*.csv"))

