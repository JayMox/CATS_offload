dd <- "/Volumes/WHITE SHARK/CA2018/APT_CC0705_07182018/SI_conversion"
files <- list.files(dd, pattern=("*.csv"),full.names = T); length(files)
#files <- files[-3]

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
                             "Temperature (imu) [\xb0C]"))
  colnames(d) <- c("depth", "date", "time", "temp")
  
  #manip objects
  d$dts <- as.POSIXct(paste(d$date, d$time), format = "%d.%m.%Y %H:%M:%OS", tz = "UTC")
  d$depth <- stats::filter(d$depth, filter = rep(1,5*datFreq)/(5*datFreq), sides = 2, circular = T)
  
  idx = seq(1, nrow(d), by=datFreq)
  dat <- data.frame(dts <- d$dts[idx],
                    depth = collapse(d$depth, datFreq), 
                    temp = collapse(d$temp, datFreq),
                    f = as.factor(i))
  
  #downsample to 1Hz
  df <- rbind(df, dat)
}  
colnames(df)[1] <- "dts"

#eda plots
library(gridExtra)
plot(df$depth[seq(1, nrow(df), by = 60)], type = "l")
dep <- plot(df$depth[6000:520000], type = "l")
#approx trimming bw 6000:520000 for on animal
idx <- seq(6000, 520000, 1)

par(mfrow = c(3,1))
d <- plot(df$depth[idx], type = "l")
t <- plot(df$temp[idx], type = "l")
hist(df$temp[idx])
#some really abnormally high temps, ~30deg C.. seeming when depth is near 0


