#script for inputingg 0707 deployment data & evaluating behavioral trigger
#set up data drive & null data frame
dd = "/Volumes/UNTITLED 1"
df <- data.frame()

setwd(paste(dd, "CC_7_07_SA2017", sep = "/"))
temp <- list.files(pattern="*.csv")
dat <- lapply(temp, read.csv, fileEncoding = "latin1")

df <- do.call('rbind', dat)
datFreq = 100; #20Hz data

#smooth depth trace over 5 sec???
df$depth <- filter(df[,15], filter = rep(1,5*datFreq)/(5*datFreq), sides = 2, circular = T)

