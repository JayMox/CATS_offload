##full suite EDA of July 18 2018 deployment CC-0705 on 7ft animal in Aptos
##tag deployed for 7 days, recorded video 11 & 13 locally for 1st 4 days
source('tag_fxns.R')
dd <- "/Volumes/Avalon/OWTS_CC0737_201808Kona"
files <- list.files(dd, pattern=("*.csv"),full.names = T); length(files)
#files <- files[-3] #accel channels kick error in this file? maybe only in raw?
datFreq = 20 #except T sensors, at 20 (imu) and 10 (depth)
library(gRumble)
library(magrittr)
library(ggplot2)
library(gridExtra)

df <- data.table::fread(
  files[1], select = c(
    "Date (UTC)", "Time (UTC)", 
    "Accelerometer X [m/s\xb2]",
    "Accelerometer Y [m/s\xb2]",
    "Accelerometer Z [m/s\xb2]",
    "Depth (100bar) 1 [m]",
    "Depth (100bar) 2 [\xb0C]"
  ))
colnames(df) <- c("date", "time", "ax", "ay", "az",
                  "depth", "temp.dep")
#manip objects
df$dts <- as.POSIXct(paste(df$date, df$time), format = "%d.%m.%Y %H:%M:%OS", tz = "UTC")
df$depth <- stats::filter(df$depth, filter = rep(1,5*datFreq)/(5*datFreq), sides = 2, circular = T)
df$temp.dep <- stats::filter(df$temp.dep, filter = rep(1, 5*datFreq)/(5*datFreq), sides = 2, circular = T)
  
#isolate gravitation
gees <-data.frame(Gsep(
    cbind(df$ax, df$ay, df$az), 
    filt=rep(1, 5*datFreq)/(5*datFreq)))
g2 <- data.frame(apply(gees, 2, collapse, freq = datFreq))
  
  
idx = seq(1, nrow(df), by=datFreq)
df <- data.frame(dts <- df$dts[idx],
                  depth = collapse(df$depth, datFreq),
                  ax = g2$X_Dynamic,
                  ay = g2$Y_Dynamic,
                  az = g2$Z_Dynamic,
                  odba = g2$ODBA,
                  temp.dep = collapse(df$temp.dep, datFreq)
                  )
#downsample to 1Hz
colnames(dat)[1] <- "dts"
#dat$night <- add_night(dat$dts, loc = c(-155.07, 19.72))   #add day/night vars
# save(df, file = file.path(
#   stringr::str_split(dd, "/SI_conversion", simplify=T)[1], 
#   "EDA", "apt_07182018_EDA_temp_odba_df.Rdata"))

p <- df %>% ggplot(aes(x = dts-hour(10), y = 0-depth)) + geom_line()
ggplotly(p)
