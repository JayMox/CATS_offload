##CATS cam tag pre-processing script
##drafted by JHMoxley, July 2017
##BE SURE TO SET DEPLOYMENT/PARAMETER SETTINGS

#prep workbench
rm(list = ls())
library(dplyr)
library(lubridate)

#set working drive to git repo
wd <- "/Users/jmoxley/Documents/MBA_GWS/GitTank/CC_CamTags"
#set up data drive, 
dd <- "/Volumes/UNTITLED 1/CamTag/SA2017_raw"; clip = 26

###########
###SET DEPLOYMENT & PARAMETER SETTINGS
###########
deployID <- "0704D1"
projID <- "SA2017"
locTZ <- "Africa/Johannesburg"
sppID <- "CC"
datFreq.desired <- 5 #Hz
dc.prog <- c(10, 12, 15) #hours in local time
trig.thresh <- 0.2
###############

#data load in
setwd(file.path(dd, paste(sppID, projID, deployID, sep="_")))
temp <- list.files(pattern="*.csv")
dat <- lapply(temp, read.csv, fileEncoding = "latin1")
setwd(wd) #remap to git drive
#build df
df <- data.frame()
df <- do.call('rbind', dat)

#build timestamps & sampling frequency
options(digits.secs = 3);
df$dts.UTC <- strptime(paste(df$Date..UTC., df$Time..UTC.), format = "%d.%m.%Y %H:%M:%OS", tz = "UTC")
df$dts.local <- strptime(paste(df$Date..local., df$Time..local.), format = "%d.%m.%Y %H:%M:%OS", tz = locTZ)
datFreq <- round(1/as.numeric(diff(head(df$dts.UTC, 2))), 2);

print(paste("TAG DEPLOYMENT IS FROM", as.Date(min(df$dts.local)), "TO", as.Date(max(df$dts.local)), sep = " "))
print(paste("DATA RECORD DURATION IS", round(difftime(max(df$dts.UTC), min(df$dts.UTC), units = "hours"), 4), "HOURS LONG", sep = " "))
print(paste("SAMPLING FREQUENCY IS", datFreq, "Hz", sep = " "))

###########
##RENAMING
###########
#rename columns
df <- rename(df, rawDEPTH = Depth..100bar..1..m.)

#########
##Duty Cycling
#########
df$dc_prog <- ifelse(hour(df$dts.local %in% dc.prog), TRUE, FALSE)

########
###SMOOTHING
#########
#smooth depth trace over 5 secs w/ a moving avg
df$depth <- stats::filter(df[,15], filter = rep(1,5*datFreq)/(5*datFreq), sides = 2, circular = T)
df$VV <- c(0, diff(df$depth))
df$VV <- stats::filter(df$VV, filter = rep(1,1*datFreq), sides = 2, circular = T) #smooth VV over 1s

########
###CATS Trigger Simulation
########
#find range diffs with a sliding window, a la CATS trigger methods
slideVVtrig <- function(data, window, step){
  total <- length(data)
  spots <- seq(from=1, to=total-window, by=step)
  result <- vector(length = length(spots))
  for(i in 1:(length(spots)-window)){
    result[i] <- diff(range(data[spots[i]:spots[i + window]]))
  }
  return(c(rep(NA, window), result))  #pad front end of result where diff cannot be calc'ed
}
df$trig <- ifelse(slideVVtrig(data = df$rawDEPTH, window = datFreq, step = 1)>=trig.thresh, TRUE, FALSE)

##Save full dataset
save(df, file = file.path(substr(dd, 0, clip), 
                          paste(paste(sppID, projID, deployID, datFreq, "Hz", sep = "_"), "Rdata", sep=".")))
print(paste(deployID, "FROM", projID, "IS NOW SAVED AT FULL RESOLUTION IN THE DATA DRIVE", sep = " "))

load(file.path(substr(dd, 0, clip), paste(paste(sppID, projID, deployID, datFreq, "Hz", sep = "_"), "Rdata", sep=".")))
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





