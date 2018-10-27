#dd <- "/Volumes/WHITE SHARK/CA2018/APT_CC0705_07182018/SI_conversion"
dd <- 
files <- list.files(dd, pattern=("*.csv"),full.names = T); length(files)
#files <- files[-3]
#issues with file 3?

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
                             "Accelerometer X [m/s\xb2]",
                             "Accelerometer Y [m/s\xb2]",
                             "Accelerometer Z [m/s\xb2]",
                             "Magnetometer X [\xb5T]",
                             "Magnetometer Y [\xb5T]",
                             "Magnetometer Z [\xb5T]",
                             "Depth (100bar) 2 [\xb0C]"))
  colnames(d) <- c("depth", "date", "time", "ax", "ay", "az",
                   "mx", "my", "mz", "temp.dep")
  
  d$dts <- as.POSIXct(paste(d$date, d$time), format = "%d.%m.%Y %H:%M:%OS", tz = "UTC")
  
  df <- rbind(df, d)
}

#manip objects
d$depth <- stats::filter(d$depth, filter = rep(1,5*datFreq)/(5*datFreq), sides = 2, circular = T)
d$temp.dep <- stats::filter(d$temp.dep, filter = rep(1, 5*datFreq)/(5*datFreq), sides = 2, circular = T)

#isolate gravitation
gees <-data.frame(Gsep(
  cbind(d$ax, d$ay, d$az), 
  filt=rep(1, 5*datFreq)/(5*datFreq)))
pr <- pitchRoll(gees, degrees = F)
g2 <- data.frame(apply(gees, 2, collapse, freq = datFreq))

#proccessing a la cfw email
err<-function(vars){
  sd(((dat1$mx - vars[1])^2) + ((dat1$my - vars[2])^2) + (( dat1$mz - vars[3])^2))
}
dat1 <- df[seq(1, nrow(df), 120),]    #use downsample to expedite optimization  
out <- optim(c(1,1,1), fn = err) 
#out$Par is the center of the magnetometer data