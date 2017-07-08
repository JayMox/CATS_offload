#stitching together data offload files 
#load library
library(dplyr)
library(ggplot2)
rm(list = ls())

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
##NO DOWNSAMPLE SINCE ALREADY AT 20HZ

#smooth depth trace over 5 secs w/ a moving avg
df$depth <- stats::filter(df[,15], filter = rep(1,5*datFreq)/(5*datFreq), sides = 2, circular = T)
df$VV <- c(0, diff(df$depth))
#smooth vv over 1s w/ a moving sum
df$VVsm <- stats::filter(df$VV, filter = rep(1,1*datFreq), sides = 2, circular = T)

#trim ends based on depth trace
plot(df$depth, type = "l")
df <- df[1:1300000,]

#write out data
save(df, file = "0704_SA2017_20hz.Rdata")

#subset salient data
df2 <- dplyr::select(df, depth, VV, VVsm, Camera, Camera.time, CC.status, Flags)
df2$rawDEPTH <- df$Depth..100bar..1..m.
df2$VV <- as.numeric(df2$VV)

####CATS trigger emulation & eval
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

#plot
dev.new()
p <- ggplot(df2, aes(y = rawDEPTH, x=1:nrow(df2))) +
  geom_vline(xintercept = which(df2$Camera %in% c(10, 11,12, 13, 14)), color = "pink", alpha = 0.1) + 
  geom_line(aes(col = abs(VVsm)), size = 2) +
  scale_color_gradient(low = "blue",high = "red") +
  geom_vline(xintercept = which(df2$trig == TRUE), linetype = "dotted") 
p
ggsave("0704_trigeval.png", p)

dev.new()
tmp <- mutate(df2, CamOn = Camera %in% c(10,11,12,13,14))
t <- ggplot(tmp, aes(y = Camera, x = 1:nrow(tmp))) + geom_line()
t
