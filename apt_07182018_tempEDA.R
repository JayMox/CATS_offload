dd <- "/Volumes/WHITE SHARK/CA2018/APT_CC0705_07182018/SI_conversion"
files <- list.files(dd, pattern=("*.csv"),full.names = T); length(files)
#files <- files[-3]
source("tag_fxns.R")

###BUTTER APPROACH
library(gRumble)
datFreq = 50
#butter approach
d <- NULL
df <- data.frame(depth = NULL, f = NULL)
for(i in 1:length(files)){
  print(i)
  d <- data.table::fread(files[i], select = 
                           c("Depth (100bar) 1 [m]", 
                             "Date (UTC)", "Time (UTC)",
                             "Temperature (imu) [\xb0C]", 
                             "Depth (100bar) 2 [\xb0C]"))
  colnames(d) <- c("depth", "date", "time", "temp.imu", "temp.dep")
  
  #manip objects
  d$dts <- as.POSIXct(paste(d$date, d$time), format = "%d.%m.%Y %H:%M:%OS", tz = "UTC")
  d$depth <- stats::filter(d$depth, filter = rep(1,5*datFreq)/(5*datFreq), sides = 2, circular = T)
  
  idx = seq(1, nrow(d), by=datFreq)
  dat <- data.frame(dts <- d$dts[idx],
                    depth = collapse(d$depth, datFreq), 
                    temp = collapse(d$temp.imu, datFreq),
                    temp.dep = collapse(d$temp.dep, datFreq),
                    f = as.factor(i))
  
  #downsample to 1Hz
  df <- rbind(df, dat)
}  
colnames(df)[1] <- "dts"
df$night <- add_night(df$dts)   #add day/night vars

#eda plots
library(gridExtra)
plot(df$depth[seq(1, nrow(df), by = 60)], type = "l")
dep <- plot(df$depth[6000:520000], type = "l")
#approx trimming bw 6000:520000 for on animal
idx <- seq(6000, 520000, 1)

par(mfrow = c(3,1))
plot(df$dts[idx], df$depth[idx], type = "l")
plot(df$temp[idx], type = "l")
hist(df$temp[idx])
#some really abnormally high temps, ~30deg C.. seeming when depth is near 0

#sal's plot w/ geom_point
library(magrittr)
library(ggplot2)
library(gridExtra)
(nights <- nighttime(df$dts))
p <- df[seq(min(idx), max(idx), by=120),] %>% ggplot(aes(x = dts, y = 0-depth)) + 
  geom_line(alpha = 0.2) +
  geom_point(aes(color = temp))+
  #geom_hline( aes(yintercept = 0))+
  scale_colour_gradient2(low = "blue", mid="green", high = "red", midpoint = 22) + 
  geom_rect(data = nights[1:6,], inherit.aes=FALSE, aes(xmin = down, xmax = up, ymin = -Inf, ymax = Inf), alpha =0.3)+
  #geom_bar(data = nights, inherit.aes=FALSE,aes(x = dts, y = Inf)) + 
  labs(title = "temperature imu", y = "depth", x = "local time") + themeo
q <- df[seq(min(idx), max(idx), by=120),] %>% ggplot(aes(x = dts-hours(7), y = 0-depth)) + 
  geom_line(alpha = 0.2) +
  geom_point(aes(color = temp.dep))+
  #geom_hline(aes(yintercept = 0))+
  scale_colour_gradient2(low = "blue", mid="green", high = "red", midpoint = 15) + 
  geom_rect(data = nights[1:6,], inherit.aes=FALSE, aes(xmin = down-hours(7), xmax = up-hours(7), ymin = -Inf, ymax = Inf), alpha =0.3)+
  #geom_bar(data = nights, inherit.aes=FALSE,aes(x = dts, y = Inf)) + 
  labs(title = "depth [100bar] 2 temp ??", y = "depth", x = "local time") + themeo
grid.arrange(p, q, ncol = 1)
library(plotly)
ggplotly(p)

#plot for niko
sc <- df %>% filter(as.numeric(dts) >= 1532190106 &
                as.numeric(dts) <= 1532395421)
sc[seq(1, nrow(sc), by = 120),] %>% ggplot(aes(x = dts, y = 0-depth)) + 
  geom_line(alpha = 0.2) +
  geom_point(aes(color = temp))+
  #geom_hline( aes(yintercept = 0))+
  scale_colour_gradient2(low = "blue", mid="green", high = "red", midpoint = 20) + 
  geom_rect(data = nights[3:5,], inherit.aes=FALSE, aes(xmin = down, xmax = up, ymin = -Inf, ymax = Inf), alpha =0.3)+
  #geom_bar(data = nights, inherit.aes=FALSE,aes(x = dts, y = Inf)) + 
  labs(title = "temperature imu", y = "depth", x = "local time") + themeo
                