##full suite EDA of Oct 21 2018 deployment CC-0737 on 17ft animal (step-notch) at SEFI
##tag deployed for 4 days, released in 2, recorded video 10 minutes of every day light hour 
source('tag_fxns.R')
dd <- "/Volumes/AVALON/FAR_CC0737_20181021/SI"
files <- list.files(dd, pattern=("*.csv"),full.names = T); length(files)
#files <- files[-3] #accel channels kick error in this file? maybe only in raw?
datFreq = 20 #except T sensors, at 20 (imu) and 10 (depth)
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
#df$depth2 <- ifelse(df$depth < 3.15, 0, df$depth - 3.15)

#plotting
library(gridExtra)
library(lubridate)
#identify time on animal
on.an <- df %>% ggplot(aes(x = 1:nrow(df), y = 0-depth)) + geom_point()
ggplotly(on.an)
idx <- seq(6000, 121000, 1)
#idx <- seq(6000, 520000, 1)
df$odba2 <- stats::filter(df$odba, filt = rep(1,2)/2) #smooth odba over 2 min

(nights <- nighttime(df$dts[idx]))
(odba <- df[seq(min(idx), max(idx), by=120),] %>% ggplot(aes(x = dts-hours(7), y = 0-depth)) + 
    geom_line(alpha = 0.2) +
    geom_point(aes(color = odba2))+
    #geom_hline(aes(yintercept = 0))+
    scale_colour_gradient2(low = "blue", mid="green", high = "red", midpoint = 1.3) + 
    #geom_rect(data = nights[1:6,], inherit.aes=FALSE, aes(xmin = down-hours(7), xmax = up-hours(7), ymin = -Inf, ymax = Inf), alpha =0.3)+
    #geom_bar(data = nights, inherit.aes=FALSE,aes(x = dts, y = Inf)) + 
    labs(title = "odba EDA", y = "depth", x = "local time") + themeo + 
    theme(title = element_text(size = 40),
          legend.text = element_text(size = 22),
          axis.text = element_text(angle = 0, hjust = 0.5, size = 28),
          axis.title = element_text(size=28)))
(temp <- df[seq(min(idx), max(idx), by=120),] %>% ggplot(aes(x = dts-hours(7), y = 0-depth)) + 
    geom_line(alpha = 0.2) +
    geom_point(aes(color = temp.dep))+
    #geom_hline(aes(yintercept = 0))+
    geom_hline(aes(yintercept = 0))+
    scale_color_gradient2(low = "blue", mid="green2", high = "red", midpoint = 15, 
                          limits = c(10, 24), breaks = seq(10,24,2.5), 
                          name= "Temp C", labels = seq(10,24,2.5))+ 
    # geom_rect(data = nights[1:6,], inherit.aes=FALSE, aes(xmin = down-hours(7), xmax = up-hours(7), 
    #                                                       ymin = -Inf, ymax = Inf), alpha =0.3)+
    # #geom_bar(data = nights, inherit.aes=FALSE,aes(x = dts, y = Inf)) + 
    #scale_x_datetime(date_breaks = "hours")+
    scale_y_continuous(breaks = seq(0, -max(df$depth), by = -5), labels = seq(0, max(df$depth), 5))+
    labs(title = "Depth-Temperature record", y = "depth (m)", x = "") + 
    themeo + theme(title = element_text(size = 40),
                   legend.text = element_text(size = 22),
                   axis.text = element_text(angle = 0, hjust = 0.5, size = 28),
                   axis.title = element_text(size=28)))
ggplotly(temp)
grid.arrange(odba, temp, ncol = 1)

library(plotly)
#depth, odba, vv races
(depth <- df[idx,] %>% ggplot(aes(x = dts - hours(7), y = 0-depth)) + geom_line() + themeo +
    labs(y = "depth"))
(odba <- df[idx, ] %>% ggplot(aes(x = dts-hours(7), y = stats::filter(odba, rep(1,3)/3))) + 
    geom_line() + themeo + labs(y = "odba"))
df$vv <- c(rep(0), diff(df$depth))
(vv <- df[idx,] %>% ggplot(aes(x = dts-hours(7), y = vv)) + geom_line() +
    scale_y_continuous(limits=c(-2,2)) + themeo + labs(y = "vertical velocity"))
grid.arrange(depth, odba, vv)

