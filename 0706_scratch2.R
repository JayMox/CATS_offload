#stitching together data from other tags
library(dplyr)
library(ggplot2)

rm(list = ls())

#set up data drive & null data frame
#dd = "/Volumes/HELLBENDY/MBA_GWS/SOAF17_data"
#dd = "/Volumes/HELLBENDY/MBA_GWS/SOAF17_data"
#dd = "/Volumes/UNTITLED 1"
dd = "/Users/jmoxley/Desktop/"
df <- data.frame()

#open deployments & read in
setwd(paste(dd, "CC_7_06_SA2017", sep = "/"))
temp <- list.files(pattern="*.csv")
dat <- lapply(temp, read.csv, fileEncoding = "latin1")

df <- do.call('rbind', dat)
#datFreq = 100; #100Hz data 

#downsample to 20Hz
df <- df[seq(1, nrow(df), by = 5),]; datFreq = 20;

#smooth depth data w/ moving avg
df$depth <- stats::filter(df[,15], filter = rep(1,5*datFreq)/(5*datFreq), sides = 2, circular = T)
df$VV <- c(0, diff(df$depth)) #calc VV
#smooth VV w/ moving sum
df$VVsm <- stats::filter(df$VV, filter = rep(1,datFreq), sides = 2, circular = T)

#write out data for easy load in
save(df, file = "0706_SA2017_20hz.RData")

#subset salient data for trigger eval
df2 <- dplyr::select(df, depth, VV, VVsm, Camera, Camera.time, CC.status, Flags)
df2$rawDEPTH <- df$Depth..100bar..1..m.
df2$VV <- as.numeric(df2$VV)
 
#custom fxn for CATs trigger evaluation
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
df2$VVtrig <- slideVVtrig(data = df2$rawDEPTH, window = datFreq, step = 1)
df2$trig <- ifelse(df2$VVtrig > 0.2, TRUE, FALSE)
  

dev.new()
p <- ggplot(df2, aes(y = rawDEPTH, x=1:nrow(df2))) +
  geom_vline(xintercept = which(df2$Camera %in% c(10, 11,12, 13, 14)), color = "pink", alpha = 0.1) + 
  geom_line(aes(col = abs(VVsm))) +
  scale_color_gradient(low = "blue",high = "red") +
  geom_vline(xintercept = which(df2$trig == TRUE), linetype = "dotted") 
p
ggsave("0706_trigeval_20Hz.png", p)
#p + geom_vline(xintercept = which(df2$Camera %in% c(10, 11,12, 13, 14)), colour = "red", alpha = 0.2)
#p + geom_vline(xintercept = 1:nrow(df2), colour = "red", alpha = 0.2)

