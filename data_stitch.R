#stitching together data offload files 
#load library
library(tidyr)
library(zoo)

#set up data drive & null data frame
dd = "/Volumes/HELLBENDY/MBA_GWS/SOAF17_data"
df <- data.frame()

#open up first deployment & read in files
setwd(paste(dd, "0704_SOAF01", sep = "/"))
temp <- list.files(pattern="*.csv")
dat <- lapply(temp, read.csv, fileEncoding = "latin1")

df <- do.call('rbind', dat)
datFreq = 20; #20Hz data

#plot depth
#plot(df[seq(1, nrow(df), 10), 15])
#looks like tag off just before 150000, subsetting for computation

df2 <- df[1:1250000,];
#plot(df2[seq(1, nrow(df2), 10), 15], type = "l")

#plot(df2[, 15], type = "p")
#lines(df2[,15], col = ifelse(df2$Camera != 0, "red","black"))

#smooth depth trace over 1 sec???
df2$depth <- filter(df2[,15], filter = rep(1,1*datFreq)/(1*datFreq), sides = 2, circular = T)
df2$VV <- c(rep(0,1),diff(df2$depth, 1))
#smooth vv over 1s
df2$VV <- filter(df2$VV, filter = rep(1,1*datFreq)/(1*datFreq), sides = 2, circular = T)
df2$VV_1Hz <- c(rep(0,20), diff(df2$depth, 20))

#save data
save(df2, file = "cat_scratch.RData")

colfunc<-colorRampPalette(brewer.pal(9, "Blues"))(10)
plot(df2$depth, type = "p", col = colfunc[abs(df2$VV_1Hz)])
lines(df2$depth, type = "p", col = ifelse(abs(df2$VV_1Hz) > 1, "red", 
                                          ifelse(abs(df2$VV_1Hz) > 0.5, "orange", "black")))

