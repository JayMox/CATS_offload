#Pool test processing & visualizing
#Oct 20-24 2017, conducted variety of pool tests aimed at understanding what controls
#& affects sensor-triggered camera files
#here the data from a few key trials is processed and visualized

#prep workbench
rm(list = ls())
options(digits.secs = 3);
library(dplyr)
library(lubridate)
library(gRumble)
source('tag_fxns.R')

#set working/data drives
wd <- "/Users/jmoxley/Documents/MBA_GWS/GitTank/CC_CamTags"
dd <- "/Volumes/UNTITLED 1/CamTag/PoolTests_MBARI2017"

###SET DEPLOYMENT & PARAMETER SETTINGS
sppID <- "CC"
projID <- "0705pt"
deployID <- "d26"
locTZ <- "UTC"     #ie. +0 hr offset
datFreq.desired <- 1 #Hz
dc.prog <- c(1,3,8) #hours in local time
trig.thresh <- 3/3 #m/s
###############

#
df <- load.in(dd, sppID, projID, deployID, stringsAsF = F, dsmooth = T)

df <- read.csv(file.path(dd, deployID, "20171023-223236-CC_7_05_D26_pool2017.csv"), 
               fileEncoding = "latin1", stringsAsFactors = F)
df$dts.UTC <- strptime(paste(df$Date..UTC., df$Time..UTC.), format = "%d.%m.%Y %H:%M:%OS", tz = "UTC")
df$dts.local <- strptime(paste(df$Date..local., df$Time..local.), format = "%d.%m.%Y %H:%M:%OS", tz = locTZ)
datFreq <- sFreq(df$dts.UTC, 100)



