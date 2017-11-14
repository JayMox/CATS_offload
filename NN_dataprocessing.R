#Tag processing script for Zac's NN modeling manuscript

library(gRumble)
source('tag_fxns.R')

#set data drive & folders w data
d.dir <- "/Users/jhmoxley/Documents/Biologia & Animales/[[SharkTank]]/data_for_Biologgingwork/Neral_network_datasets_Zac"
#metadata for deployments
#deployment <- "CC_2_24_PR151106"
#fn <- "LOG_CC_2_24_D1"
#datFreq.desired <- 1 #Hz
###THIS MAY ALREADY BE THE ORIGINAL DATA CFW SENT TO ZAC

#deployment <- "11_CATS_Diary_20161101"
#fn <- "20161128-035053-BIO7379_Diary3"
#datFreq.desired <- 1 #Hz
###ISSUE WITH TIME FORMATTING HH:MM:SS:MS.. fucking final colon needs to be a period
#Scratchy fix w/ substr()
#N.B.  THIS CATS DATASET HAS AN EXTRA 0 PRECEEDING THE MILLISECONDS AS WELL

#deployment <- "CC_2_24_PR161108"
#fn <- "CC-2-24_PR161108_PR16110703"
datFreq.desired <- 1 #Hz

df <- read.csv(file.path(d.dir, deployment, paste(fn,".csv",sep="")), stringsAsFactors = F)
#substring(df$Time, 9,10) <- "."   #reformating for CATS tags ISSUES
#df$Time <- sub(".0", ".", df$Time, fixed = T)

#converting date/time issues
df$dts <- as.POSIXct(paste(df$Date, df$Time), format = "%d.%m.%Y %H:%M:%OS", tz = "UTC")
#df$Date <- as.Date(df$Date, format = "%d.%m.%Y")
(datFreq <- 1/as.numeric(Mode(round(diff(df$dts),4))))

print(paste("Modal sampling frequency of raw data  estimated to be ", datFreq, " Hz"))
print(paste("dataset ", ifelse(datFreq == datFreq.desired, "WILL NOT ", "WILL"), "be downsampled"))

##CACLULATE ACCELERATION METRICS PRIOR TO DOWNSAMPLING
#convert accels from what is assumed to be millibar
df$accel.x <- df$Acceleration..2....channel..1/1000
df$accel.y <- df$Acceleration..2....channel..2/1000
df$accel.z <- df$Acceleration..2....channel..3/1000

gees <-data.frame(Gsep(cbind(df$accel.x, df$accel.y, df$accel.z), filt=rep(1, 5*datFreq)/(5*datFreq)))

#downsampling (Should we smooth depth before downsampling??)
df <- df[seq(1, nrow(df), by = datFreq/datFreq.desired),]; 
#collapse acceleration data
df$accel.x <- collapse(gees$X_Dynamic, datFreq)
df$accel.y <- collapse(gees$Y_Dynamic, datFreq)
df$accel.z <- collapse(gees$Z_Dynamic, datFreq)
df$odba <- collapse(gees$ODBA, datFreq)
#inset time index
df$tidx <- (as.numeric(df$dts)-as.numeric(min(df$dts)))/3600  
#update datFreq
datFreq <- datFreq.desired

####
#Unit conversions
####
#convert pressure from what is assumed to be bar
df$depth..m <- df$Pressure...channel..1 /10.197
#2 stage smoothing & VV
df$depth.m <- stats::filter(df$depth..m, filter = rep(1,5*datFreq)/(5*datFreq), sides = 2, circular = T)
df$VV <- c(0, diff(df$depth.m))
df$VV <- stats::filter(df$VV, filter = rep(1,1*datFreq), sides = 2, circular = T) #smooth VV over 1s


#subset data of interest; extract 24 hrs following 1st quaritle
library(dplyr)
plot(df$depth..m[13012:117861], type = "l"); locator(1) #check for tagON/tagOFF

quart.idx <- which.min(abs(df$tidx-quantile(df$tidx, 0.25)))
df2 <- df %>% slice(quart.idx:(quart.idx + 24*60*60)) %>%
  select(dts, tidx, depth.m, VV, accel.x, accel.y, accel.z, odba) %>%
  mutate(id = deployment)

#write data out
write.csv(df2, file = file.path(file.path(d.dir, paste(deployment,"_NN.csv",sep=""))))
