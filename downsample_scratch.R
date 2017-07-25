##CATS tags downsampling
##JHMoxley, July 2017

#prep workbench
rm(list = ls())
library(plyr)
library(dplyr)
#library(lubridate)

######################
###SET THESE
######################
sppID <- "CC"
deployID <- "0706D1"
projID <- "SA2017"
datFreq = 5
#load in

#set working & data drives
wd <- "/Users/jmoxley/Documents/MBA_GWS/GitTank/CC_CamTags"
dd <- "/Volumes/UNTITLED 1/CamTag/SA2017_raw"; clip = 26

#get the data
load(file.path(substr(dd, 0, clip), paste(paste(sppID, projID, deployID, datFreq, "Hz", sep = "_"), "Rdata", sep = ".")))

df2 <- select(df, dts.local, dts.UTC, rawDEPTH, depth, Flags, trig, CC.status, dc_prog, Camera)
#rm(list = "df") #save some space

#############
#downsample testing
############
tmp <- df2; 
tmp$tidx = seq(0, length.out = nrow(df2), by = 1/datFreq); tmp$itidx <- factor(as.integer(tmp$tidx))
tmp$dts.local = as.POSIXct(tmp$dts.local, origin = "1970-01-01 00:00:00", tz = "Africa/Johannesburg")
tmp$dts.UTC = as.POSIXct(tmp$dts.UTC, origin = "1970-01-01 00:00:00", tz = "UTC")

tmp2 <- tmp[100000:500000,]
#tmp2 <- downSample(tmp2, tmp2$itidx, list = TRUE)
sc <- ddply(tmp2, .(itidx), summarize, nCamCode = length(unique(Camera)), 
      nFlag = length(unique(Flags)), nTrig = length(unique(trig)), nCC = length(unique(CC.status)))
#tmp <- aggregate(tmp, by = list(unique(as.integer(tmp$tidx))), FUN = length(unique(df2$Camera)))
                 
                 sc <- ddply(tmp, .(as.integer(tidx)), summarize, f = length(unique(Flags)), t = length(unique(trig)))
                 