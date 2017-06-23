#stitching together data offload files 
#load library
library(tidyr)

#set up data drive & null data frame
dd = "/Volumes/HELLBENDY/MBA_GWS/SOAF17_data"
df <- data.frame()

#open up first deployment & read in files
setwd(paste(dd, "0704_SOAF01", sep = "/"))
temp <- list.files(pattern="*.csv")
dat <- lapply(temp, read.csv, fileEncoding = "latin1")

df <- do.call('rbind', dat)

#plot depth
plot(df[seq(1, nrow(df), 10), 15])
