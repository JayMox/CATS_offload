##CATS cam tag pre-processing script
##drafted by JHMoxley, July 2017
##BE SURE TO SET DEPLOYMENT/PARAMETER SETTINGS

#prep workbench
rm(list = ls())
options(digits.secs = 3);
library(dplyr)
library(lubridate)
library(gRumble)
source('tag_fxns.R')

#set working drive to git repo
wd <- "/Users/jmoxley/Documents/GitTank/CC_CamTags"
#set up data drive, 
dd <- "/Volumes/UNTITLED 1/CamTag/CA2017_raw"; clip = 26; substr(dd, 0, clip)

###########
###SET DEPLOYMENT & PARAMETER SETTINGS
###########
deployID <- "0704D3"
projID <- "CA2017"
locTZ <- "UTC"     #ie. +0 hr offset
sppID <- "CC"
dts_tagon <- NA
datFreq.desired <- 1 #Hz
dc.prog <- c(14,17,19,22) #hours in local time
trig.thresh <- 3/3 #m/s
###############

#data load in
df <- load.in(dd, sppID, projID, deployID, stringsAsF = F, 
              dsmooth=T, camcodify=T, trigify=T)
#########
##Set Sampling Freq
#########
datFreq <- sFreq(df$dts.UTC, 100);
#########
##Duty Cycling
#########
df$dc_prog <- ifelse(hour(df$dts.UTC) %in% dc.prog, TRUE, FALSE)
#########
##Post-processed depth-differentials
#########
df$trig <- ifelse(slide.window(data=df$rawDEPTH, window = datFreq*3, step = 1) >= trig.thresh,
                  TRUE, FALSE)

##Save full dataset
save(df, file = file.path(substr(dd, 0, clip), 
                          paste(paste(sppID, projID, deployID, datFreq, "Hz", sep = "_"), "Rdata", sep=".")))
print(paste(deployID, "FROM", projID, "IS NOW SAVED AT FULL RESOLUTION (", datFreq, "Hz) IN THE DATA DRIVE", sep = " "))
#write csv for igor
#write.csv(df, file = file.path(substr(dd, 0, clip), 
#                              paste(paste(sppID, projID, deployID, datFreq, "Hz", sep = "_"), "csv")))

eda.plot(df)

#load(file.path(substr(dd, 0, clip), paste(paste(sppID, projID, deployID, datFreq, "Hz", sep = "_"), "Rdata", sep=".")))
########
###DOWNSAMPLING
########
if(datFreq == datFreq.desired){print(paste("DATA IS NOT DOWNSAMPLED SINCE WAS RECORDED AT DESIRED FREQUENCY (i.e.", datFreq.desired, "Hz, specified via datFreq.desired PARAM)", sep = " "))}
if(datFreq != datFreq.desired){
  df <- df[seq(1, nrow(df), by = datFreq/datFreq.desired),]; 
  datFreq = round(1/as.numeric(diff(head(df$dts.UTC, 2))), 2);
  #print(paste("DATA IS DOWNSAMPLED TO", datFreq, "Hz, AS SPECIFIED BY datFreq.desired PARAM", sep = " "))
  
  #save downsampled
  save(df, file = file.path(substr(dd, 0, clip), paste(paste(sppID, projID, deployID, datFreq, "Hz", sep = "_"), "Rdata", sep = ".")))
  print(paste(deployID, "FROM", projID, "IS ALSO SAVED AT", datFreq, "Hz RESOLUTION, AS SPECIFIED BY datFreq.desired (set above as", datFreq.desired, "Hz )", sep = " "))
}





