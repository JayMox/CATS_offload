#script for inputingg 0707 deployment data & evaluating behavioral trigger
#set up data drive & null data frame
#dd = "/Volumes/UNTITLED 1"
library(ggplot2)
library(dplyr)

dd = "/Users/jmoxley/Desktop/"
df <- data.frame()

setwd(paste(dd, "CC_7_07_SA2017", sep = "/"))
temp <- list.files(pattern="*.csv")
dat <- lapply(temp, read.csv, fileEncoding = "latin1")

df <- do.call('rbind', dat)
datFreq = 100; #20Hz data

#remove what looks like extraneous values
filter(df, abs(Depth..100bar..1..m.) > 100)
df <- filter(df, abs(Depth..100bar..1..m.) < 100)

#smooth depth trace over 5 sec???
df$depth <- stats::filter(df[,15], filter = rep(1,5*datFreq)/(5*datFreq), sides = 2, circular = T)
df$VV <- c(0, diff(df$depth))
df$VVsm <- stats::filter(df$VV, filter = rep(1,datFreq), sides = 2, circular = T) #smooth a la CFW filter rep(1,5) in 5hz data??

#pull data needed for triggerQA
df2 <- select(df, depth, VV, VVsm, Camera, Camera.time, CC.status, Flags)
df2$DEPTH <- df$Depth..100bar..1..m.
df2$VV <- as.numeric(df2$VV)

#emulate CATS trigger method
df2$VVtrig <- slideVVtrig(data = df2$DEPTH, window = datFreq, step = 1)
df2$trig <- ifelse(df2$VVtrig > 0.2, TRUE, FALSE)


###issues with ggplot cmd & aesthetics
dev.new()
p <- ggplot(df2, aes(y = DEPTH, x=1:nrow(df2))) +
  geom_vline(xintercept = which(df2$Camera %in% c(10, 11,12, 13, 14)), colour = "gray10") + 
  geom_line(aes(col = abs(VVsm))) +
  scale_color_gradient(low = "blue",high = "red") +
  geom_vline(xintercept = which(df2$trig == TRUE), linetype = "dotted") 
p
ggsave("0707_trigeval.png", p)

#troubleshoot gg
tmp <- df2[10000:50000,]
dev.new()
p <- ggplot(tmp, aes(y = DEPTH, x=1:nrow(tmp))) +
  geom_vline(xintercept = which(tmp$Camera %in% c(10, 11,12, 13, 14)), colour = "gray10") + 
  geom_line(aes(colour = abs(VVsm))) +
  scale_color_gradient(low = "blue",high = "red") +
  geom_vline(xintercept = which(tmp$trig == TRUE), linetype = "dotted") 
p
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

