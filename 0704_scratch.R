#stitching together data offload files 
#load library
library(tidyr)
library(zoo)

#set up data drive & null data frame
#dd = "/Volumes/HELLBENDY/MBA_GWS/SOAF17_data"
#dd = "/Volumes/UNTITLED 1/CamTag/SA2017/"
dd = "/Users/jmoxley/Desktop/"
df <- data.frame()

#open up first deployment & read in files
setwd(paste(dd, "CC_7_04_SA2017", sep = "/"))
temp <- list.files(pattern="*.csv")
dat <- lapply(temp, read.csv, fileEncoding = "latin1")

df <- do.call('rbind', dat)
datFreq = 20; #20Hz data
#smooth depth trace over 5 secs
df$depth <- stats::filter(df[,15], filter = rep(1,5*datFreq)/(5*datFreq), sides = 2, circular = T)
df$VV <- c(0, diff(df$depth))
#smooth vv over 1s
df$VVsm <- stats::filter(df$VV, filter = rep(1,1*datFreq)/(1*datFreq), sides = 2, circular = T)

#subset salient data
df2 <- dplyr::select(df, depth, VV, VVsm, Camera, Camera.time, CC.status, Flags)
df2$DEPTH <- df$Depth..100bar..1..m.
df2$VV <- as.numeric(df2$VV)

####CATS trigger emulation & eval
####################
slideVVtrig <- function(data, window, step){
  total <- length(data)
  spots <- seq(from=1, to=total-window, by=step)
  result <- vector(length = length(spots))
  for(i in 1:(length(spots)-window)){
    result[i] <- diff(range(data[spots[i]:spots[i + window]]))
  }
  return(c(rep(NA, window), result))  #pad front end of result where diff cannot be calc'ed
}

#emulate CATS trigger method
df2$VVtrig <- slideVVtrig(data = df2$DEPTH, window = datFreq, step = 1)
df2$trig <- ifelse(df2$VVtrig > 0.2, TRUE, FALSE)

#plot
dev.new()
p <- ggplot(df2, aes(y = DEPTH, x=1:nrow(df2))) +
  geom_vline(xintercept = which(df2$Camera %in% c(10, 11,12, 13, 14)), colour = "gray10") + 
  geom_line(aes(col = abs(VVsm))) +
  scale_color_gradient(low = "blue",high = "red") +
  geom_vline(xintercept = which(df2$trig == TRUE), linetype = "dotted") 
p
