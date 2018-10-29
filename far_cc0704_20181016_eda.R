##full suite EDA of Oct 16 2018 deployment CC-0704 on 16.5ft female (no match..20181027) at SEFI
##tag deployed for >7 days, phid FAR18101603
source('tag_fxns.R')
dd <- "/Users/jhmoxley/Downloads/FAR_CC0704_20181016"
files <- list.files(dd, pattern=("*.csv"),full.names = T); length(files)
#files <- files[-3] #accel channels kick error in this file? maybe only in raw?
datFreq = 50 
library(gRumble)
library(magrittr)
library(ggplot2)
library(gridExtra)

###BUTTER APPROACH
#butter approach
d <- NULL
df <- NULL
for(i in 1:length(files)){
  print(i)
  d <- data.table::fread(files[i], select = 
                           c("Depth (100bar) 1 [m]", 
                             "Date (UTC)", "Time (UTC)",
                             "Temperature (imu) [\xb0C]", 
                             "Depth (100bar) 2 [\xb0C]",
                             "Accelerometer X [m/s\xb2]",
                             "Accelerometer Y [m/s\xb2]",
                             "Accelerometer Z [m/s\xb2]"))
  colnames(d) <- c("depth", "date", "time", "temp.imu", "temp.dep", "ax", "ay", "az")
  
  #manip objects
  d$dts <- as.POSIXct(paste(d$date, d$time), format = "%d.%m.%Y %H:%M:%OS", tz = "UTC")
  d$depth <- stats::filter(d$depth, filter = rep(1,5*datFreq)/(5*datFreq), sides = 2, circular = T)
  d$temp.imu <- stats::filter(d$temp.imu, filter = rep(1, 5*datFreq)/(5*datFreq), sides = 2, circular = T)
  d$temp.dep <- stats::filter(d$temp.dep, filter = rep(1, 5*datFreq)/(5*datFreq), sides = 2, circular = T)
  
  #isolate gravitation
  gees <-data.frame(Gsep(
    cbind(d$ax, d$ay, d$az), 
    filt=rep(1, 5*datFreq)/(5*datFreq)))
  g2 <- data.frame(apply(gees, 2, collapse, freq = datFreq*2))
  
  #DOWNSAMPLER HERE
  idx = seq(1, nrow(d), by=datFreq*2)
  dat <- data.frame(dts <- d$dts[idx],
                    depth = collapse(d$depth, datFreq*2),
                    ax = g2$X_Dynamic,
                    ay = g2$Y_Dynamic,
                    az = g2$Z_Dynamic,
                    odba = g2$ODBA,
                    temp = collapse(d$temp.imu, datFreq*2),
                    temp.dep = collapse(d$temp.dep, datFreq*2),
                    f = as.factor(i))
  
  #downsample to .5Hz
  df <- rbind(df, dat)
}  

#plotting
library(gridExtra)
library(lubridate)
#identify time on animal
colnames(df)[1] <- "dts"
on.an <- df %>% ggplot(aes(x = 1:nrow(df), y = 0-depth)) + geom_line() + themeo
ggplotly(on.an)

library(plotly)
#depth, odba, vv races
idx <- seq(1, 1*10^5, by = 2)
idx <- seq(1, nrow(df), 1)
(depth <- df[idx,] %>% ggplot(aes(x = dts-hours(7), y = 0-depth)) + geom_line() + themeo +
    labs(y = "depth"))
(odba <- df[idx, ] %>% ggplot(aes(x = dts-hours(7), y = odba)) + 
    geom_line() + themeo + labs(y = "odba"))
df$vv <- c(rep(0), diff(df$depth))
(vv <- df[idx,] %>% ggplot(aes(x = dts-hours(7), y = -vv)) + geom_line() +
    scale_y_continuous(limits=c(-2,2)) + themeo + labs(y = "vertical velocity"))
(sway <- df[idx,] %>% ggplot(aes(x = dts-hours(7), y = ay)) + geom_line() + themeo)
grid.arrange(depth, odba, sway, ncol = 1)
