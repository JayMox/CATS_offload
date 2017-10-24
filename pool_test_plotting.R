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
wd <- "/Users/jmoxley/Documents/GitTank/CC_CamTags"
dd <- "/Volumes/UNTITLED 1/CamTag/PoolTests_MBARI2017"

###SET DEPLOYMENT & PARAMETER SETTINGS
sppID <- "CC"
projID <- "0707pt"
deployID <- "d25"
locTZ <- "UTC"     #ie. +0 hr offset
datFreq.desired <- 1 #Hz
dc.prog <- c(1,3,8) #hours in local time
trig.thresh <- 3/3 #m/s
###############

#read in data w/ custom fxn
df <- load.in(dd, sppID, projID, deployID, 
              stringsAsF=F, dsmooth=T, camcodify=T, trigify=T)
df$dc_prog <- ifelse(hour(df$dts.UTC) %in% dc.prog, TRUE, FALSE)
#use rawDEPTH since this is what is available to tag
df$trig <- ifelse(slide.window(data=df$rawDEPTH, window = datFreq*3, step = 1) >= trig.thresh,
                  TRUE, FALSE)

#0707pt_d26 records 1/2 minute of data at 10.12.2017??!
if(projID == "0707pt" & deployID == "d25"){
  df <- df[which(df$dts.UTC >= as.POSIXct("2017-10-20", tz = "UTC")),]
}

#plot
pdf(file = paste(dd, paste(sppID, projID, deployID, "TagCodesPlot.pdf", sep = "_"), sep="/"))
eda.plot(df)
dev.off()
