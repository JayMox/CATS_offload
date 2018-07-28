#aptos depth trajectory work up
dd <- "/Volumes/WHITE SHARK/CA2018/test"
files <- list.files(dd, pattern=("*.csv"),full.names = T)
files <- files[-3]
##issues with files 3 & 4? 
dep <- NULL
for(i in 1:length(files)){
  print(i)
  dep <- rbind(dep, try(data.table::fread(files[i], select = "Depth (100bar) 1 [m]", colClasses = "numeric")))
  
}
colnames(dep) <- "depth"

summary(dep)
plot(dep$depth, type = "l")


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
                             "Accelerometer Z [m/s\xb2]"))
  colnames(d) <- c("depth", "date", "time", "ax", "ay", "az")
  
  #manip objects
  d$dts <- as.POSIXct(paste(d$date, d$time), format = "%d.%m.%Y %H:%M:%OS", tz = "UTC")
  d$depth <- stats::filter(d$depth, filter = rep(1,5*datFreq)/(5*datFreq), sides = 2, circular = T)
  
  #isolate gravitation
  gees <-data.frame(Gsep(
    cbind(d$ax, d$ay, d$az), 
    filt=rep(1, 5*datFreq)/(5*datFreq)))
  g2 <- data.frame(apply(gees, 2, collapse, freq = datFreq))
  
  #downsample
  idx = seq(1, nrow(d), by=datFreq)
  dat <- data.frame(dts <- d$dts[idx],
                    ax = g2$X_Dynamic,
                    ay = g2$Y_Dynamic,
                    az = g2$Z_Dynamic,
                    odba = g2$ODBA,
                    depth = collapse(d$depth, datFreq), 
                    f = as.factor(i))
  
  #downsample to 1Hz
  df <- rbind(df, dat)
}
str(df)
colnames(df)[1] <- "dts"

q <- ggplot() + 
  geom_line(data = df, aes(x = dts, y = depth))
ggplotly(q)


library(magrittr)
library(ggplot2)
library(plotly)
library(lubridate)
library(gridExtra)
devtools::install_github("IRkernel/repr") #improve plotly w/ big data?
#https://github.com/ropensci/plotly/issues/1104

df$local <- df$dts - hours(7)
df$idx <- seq_len(nrow(df))
df$cam <- ifelse(hour(df$local) %in% c(11, 13), TRUE, FALSE)
df$VV <- c(0, diff(df$depth))
(p <- df[6000:520000,] %>% ggplot(aes(x = local, y = depth, 
                                      color = cam)) + 
  geom_line()) 
q <- df[6000:520000,] %>% ggplot(aes(x = local, y = ))
ggplotly(p)

#zoom in on the deep dive sections
p <- df[50000:100000,] %>% ggplot(aes(x = local, y = depth, 
                                colour = ifelse(odba > 6, 6, odba))) + 
  geom_line() + scale_color_gradientn(colours = rainbow(4))
ggplotly(p)
