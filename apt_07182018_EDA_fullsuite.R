##full suite EDA of July 18 2018 deployment CC-0705 on 7ft animal in Aptos
##tag deployed for 7 days, recorded video 11 & 13 locally for 1st 4 days
source('tag_fxns.R')
dd <- "/Volumes/WHITE SHARK/CA2018/APT_CC0705_07182018/SI_conversion"
files <- list.files(dd, pattern=("*.csv"),full.names = T); length(files)
#files <- files[-3] #accel channels kick error in this file? maybe only in raw?
datFreq = 50 #except T sensors, at 20 (imu) and 10 (depth)
source("tag_fxns.R")
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
  g2 <- data.frame(apply(gees, 2, collapse, freq = datFreq))
  
  
  idx = seq(1, nrow(d), by=datFreq)
  dat <- data.frame(dts <- d$dts[idx],
                    depth = collapse(d$depth, datFreq),
                    ax = g2$X_Dynamic,
                    ay = g2$Y_Dynamic,
                    az = g2$Z_Dynamic,
                    odba = g2$ODBA,
                    temp = collapse(d$temp.imu, datFreq),
                    temp.dep = collapse(d$temp.dep, datFreq),
                    f = as.factor(i))
  
  #downsample to 1Hz
  df <- rbind(df, dat)
}  
colnames(df)[1] <- "dts"
df$night <- add_night(df$dts)   #add day/night vars
# save(df, file = file.path(
#   stringr::str_split(dd, "/SI_conversion", simplify=T)[1], 
#   "EDA", "apt_07182018_EDA_temp_odba_df.Rdata"))

###ROUGH ZERO-OFFSET
#seems 3.15 is the highest depth recordng outside of duty cycling
df$depth2 <- ifelse(df$depth < 3.15, 0, df$depth - 3.15)

#plotting
library(gridExtra)
library(lubridate)
#approx trimming bw 6000:520000 for on animal
idx <- seq(6000, 520000, 1)
(nights <- nighttime(df$dts))
(odba <- df[seq(min(idx), max(idx), by=120),] %>% ggplot(aes(x = dts-hours(7), y = 0-depth)) + 
  geom_line(alpha = 0.2) +
  geom_point(aes(color = odba))+
  #geom_hline(aes(yintercept = 0))+
  scale_colour_gradient2(low = "blue", mid="green", high = "red", midpoint = 1.2) + 
  geom_rect(data = nights[1:6,], inherit.aes=FALSE, aes(xmin = down-hours(7), xmax = up-hours(7), ymin = -Inf, ymax = Inf), alpha =0.3)+
  #geom_bar(data = nights, inherit.aes=FALSE,aes(x = dts, y = Inf)) + 
  labs(title = "depth [100bar] 2 temp ??", y = "depth", x = "local time") + themeo)
(temp <- df[seq(min(idx), max(idx), by=120),] %>% ggplot(aes(x = dts-hours(7), y = 0-depth2)) + 
  geom_line(alpha = 0.2) +
  geom_point(aes(color = temp.dep))+
  #geom_hline(aes(yintercept = 0))+
  geom_hline(aes(yintercept = 0))+
  scale_color_gradient2(low = "blue", mid="green2", high = "red", midpoint = 15, 
                        limits = c(10, 20), breaks = seq(10,20,2.5), 
                        name= "Temp C", labels = c("", "12.5", "15.0", "17.5", "20.0"))+ 
  geom_rect(data = nights[1:6,], inherit.aes=FALSE, aes(xmin = down-hours(7), xmax = up-hours(7), 
                                                        ymin = -Inf, ymax = Inf), alpha =0.3)+
  #geom_bar(data = nights, inherit.aes=FALSE,aes(x = dts, y = Inf)) + 
  #scale_x_datetime(date_breaks = "hours")+
  scale_y_continuous(breaks = seq(0, -30, by = -5), labels = seq(0,30,5))+
  labs(title = "Depth-Temperature record", y = "depth (m)", x = "") + 
  themeo + theme(title = element_text(size = 40),
                 legend.text = element_text(size = 28),
                 axis.text = element_text(angle = 0, hjust = 0.5, size = 36),
                 axis.title = element_text(size=36)))
ggplotly(temp)
grid.arrange(odba, temp, ncol = 1)

library(plotly)


ggplot(df[idx,]) + geom_histogram(aes(x = depth), binwidth = 1)
