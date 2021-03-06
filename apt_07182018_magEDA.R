#dd <- "/Volumes/WHITE SHARK/CA2018/APT_CC0705_07182018/SI_conversion"
dd <- 
files <- list.files(dd, pattern=("*.csv"),full.names = T); length(files)
#maghead not in gRumble?? 
magHead<-function(PR,magxyz,magOff){
  magxyz[,1]<- magxyz[,1]-magOff[1]
  magxyz[,2]<- magxyz[,2]-magOff[2]
  magxyz[,3]<- magxyz[,3]-magOff[3]
  yH<-magxyz[,3]*sin(PR[,2]) - magxyz[,2]*cos(PR[,2])
  xH<-magxyz[,1]*cos(PR[,1]) + magxyz[,2]*sin(PR[,1])*sin(PR[,2])+ magxyz[,3]*sin(PR[,1])*cos(PR[,2])
  Heading<-atan2(-yH,xH)
  return(Heading)
}

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
df$depth <- stats::filter(df$depth, filter = rep(1,5*datFreq)/(5*datFreq), sides = 2, circular = T)
df$temp.dep <- stats::filter(df$temp.dep, filter = rep(1, 5*datFreq)/(5*datFreq), sides = 2, circular = T)

#find static & dynamic acceleration
gees <-data.frame(Gsep(
  cbind(df$ax, df$ay, df$az), 
  filt=rep(1, 5*datFreq)/(5*datFreq)))
pr <- pitchRoll(gees, degrees = F)
g2 <- data.frame(apply(gees, 2, collapse, freq = datFreq))

#proccessing mag, a la cfw email
err<-function(vars){
  #seq call is a rough/tumble downsample to speed things up
  sd(((df$mx[seq(1, nrow(df), datFreq)] - vars[1])^2) + 
       ((df$my[seq(1, nrow(df), datFreq)] - vars[2])^2) + 
       (( df$mz[seq(1, nrow(df), datFreq)] - vars[3])^2))
}
out <- optim(c(1,1,1), fn = err) 
#out$par is the center of the magnetometer data

mmms <- data.frame(Gsep(
  cbind(df$mx, df$my, df$mz),
  filt = rep(1,5*datFreq)/(5*datFreq)))

#downsample to 1hz
pr1hz <- apply(pr, MARGIN = 2, FUN = function(x){collapse(x, freq = datFreq)})
odba1hz<-collapse(gees[,7],freq=datFreq)
mxyz1hz <- apply(mmms, MARGIN = 2,  FUN = function(x){collapse(x,freq=datFreq)})

#calc heading
heading <- magHead(PR = pr1hz, magxyz = mxyz1hz, magOff = out$par)
#declination at santa cruz ~13.2deg
declination <- 0.2303834
heading = heading + pi + declination

#combine & plot
library(lubridate)
prhod <- data.frame(pr1hz, heading, odba1hz, 
                    collapse(df$depth, datFreq),
                    df$dts[seq(1,nrow(df), datFreq)]-hours(7))
colnames(prhod) = c("pitch", "roll", "heading", "odba", "depth", "dts.loc")
idx <- seq(6000, 520000, 1) #tagon clip
prhod <- prhod[idx,]
library(magrittr)
library(ggplot2)
library(zoo)
library(circular)
library(lubridate)
prhod$night <- add_night(prhod$dts)
(nights <- nighttime(prhod$dts))

(p <- prhod[seq(1,nrow(prhod),by=120),] %>% ggplot(aes(x = dts.loc, y = 0-depth)) + 
  geom_line() +
  geom_rect(data = nights[1:6,], inherit.aes=FALSE, aes(xmin = down-hours(7), xmax = up-hours(7), ymin = -Inf, ymax = Inf), alpha =0.3)+
    themeo)
varh = rollapply(prhod$heading,
                 width = 75, 
                 FUN = angular.variance)

p + geom_point(data = 
                 data.frame(prhod[seq(1,nrow(prhod),by=120),5:6],
                            varh = varh[seq(1,nrow(prhod),by=120)]),
               aes(color = varh))+
  scale_color_gradient2(low = "blue", mid="green2", high = "red", midpoint = 1.5, 
                        limits = c(0, 3.5), name = "var(heading)")+
  labs(title = "mag headings, APT-cc0705-20180718")

#select different headings during nighttime behavior
prhod %>% ggplot(aes(x = dts.loc, y = heading))+geom_line() + 
  geom_rect(data = nights[1:6,], inherit.aes=FALSE, 
            aes(xmin = down-hours(7), xmax = up-hours(7), ymin = -Inf, ymax = Inf), 
            alpha =0.3) + 
  geom_hline(yintercept = c(0.34, 3.5), color = "red", size = 2)+
  annotate(geom="text", x = min(prhod$dts.loc), y=c(0.34, 3.5), 
           size = 3,label=c("NNE", "SSW"))

#distributions by 1/4day 
#cuts are 03,09,15,21 (straddles midday)
#add 3 to make cleaner breaks, so that 1 is 3hrs on either side of midnight
prhod$qday <- as.numeric(cut(hour(prhod$dts.loc),
  breaks=c(00,03,09,15,21,24), include.lowest = T, left = T))
prhod$qday <- ifelse(prhod$qday == 5, 1, prhod$qday)  #wrap around midnight
prhod$sday <- as.numeric(cut(hour(prhod$dts.loc),
                             breaks=c(00,02,06,10,14,18,22,24),
                             include.lowest=T, right = T))
prhod$sday <- ifelse(prhod$sday == 7, 1, prhod$sday)
#double check
prhod %>% dplyr::sample_frac(0.50) %>% ggplot() + 
  geom_histogram(aes(x = hour(dts.loc), fill = factor(sday))) + 
  scale_x_continuous(breaks = 0:24, labels = 0:24)

mdf.prh <- tidyr::gather(prhod, 
                         var, val, -dts.loc, -qday, -sday, -night)
mdf.prh %>% ggplot() + 
  geom_histogram(aes(x = val, fill = qday), alpha = 0.6)+
  facet_wrap(hour(dts)~var)+
  coord_polar()

#plot prh across qtrdays
vars <- c('pitch', 'roll', 'heading')
p <- list(NULL)
for(i in 1:length(vars)){
  dat <- mdf.prh %>% dplyr::filter(var == vars[i]) %>% 
    dplyr::mutate(hr = factor(hour(dts.loc)))
  (p[[i]] <- dat %>% ggplot() + 
    geom_histogram(aes(x = val, fill = hr), alpha = 0.8)+
    facet_wrap(~hr) + 
    coord_polar() +
    labs(title = "headings of cc0705-20180718")
    #+ theme(panel.background=element_blank())
  
  )
}
library(gridExtra)
grid.arrange(p[[1]], p[[2]], p[[3]], nrow = 3)
p[[3]]
ggplot() + 
  geom_histogram(aes(x = val, fill = qday), alpha = 0.6)+
  facet_wrap(var~qday, scales = "free")+
  coord_polar()
#as circular data
prhod$head.c <- circular(prhod$heading, type = "directions", 
                         units = "radians")
rose.diag(prhod$head.c[seq(1, nrow(prhod), by = 60)])
