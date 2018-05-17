#script to evaluate depth changes in Cafe Cam Deployments
#depth data from Pool Test at MBARI April 2018
rm(list = ls())
library(tidyverse)

dd <- "/Volumes/UNTITLED/CamTag/PoolTest_CCdepthsensor_Apr2018"
setwd(dd)
cams <- c("Luke32", "Chewy36", "Han37", "Leia38")
test.start <- as.POSIXct("2018-04-13 22:00:00", tz ="UTC")
test.end <- as.POSIXct("2018-04-13 22:10:00", tz ="UTC")

df1 <- read.csv(file = paste(cams[1], "depthtestSI.csv", sep = "_"), fileEncoding = "latin1") %>% 
  mutate(tag = cams[1], dts.UTC = as.POSIXct(strptime(paste(Date..UTC., Time..UTC.), format = "%d.%m.%Y %H:%M:%OS", tz = "UTC"))) %>% 
  filter(dts.UTC > test.start & dts.UTC < test.end)
df2 <- read.csv(file = paste(cams[2], "depthtestSI.csv", sep = "_"), fileEncoding = "latin1") %>% 
  mutate(tag = cams[2], dts.UTC = as.POSIXct(strptime(paste(Date..UTC., Time..UTC.), format = "%d.%m.%Y %H:%M:%OS", tz = "UTC"))) %>% 
  filter(dts.UTC > test.start & dts.UTC < test.end)
df3 <- read.csv(file = paste(cams[3], "depthtestSI.csv", sep = "_"), fileEncoding = "latin1") %>% 
  mutate(tag = cams[3], dts.UTC = as.POSIXct(strptime(paste(Date..UTC., Time..UTC.), format = "%d.%m.%Y %H:%M:%OS", tz = "UTC"))) %>% 
  filter(dts.UTC > test.start & dts.UTC < test.end)
df4 <- read.csv(file = paste(cams[4], "depthtestSI.csv", sep = "_"), fileEncoding = "latin1") %>% 
  mutate(tag = cams[4], dts.UTC = as.POSIXct(strptime(paste(Date..UTC., Time..UTC.), format = "%d.%m.%Y %H:%M:%OS", tz = "UTC"))) %>% 
  filter(dts.UTC > test.start & dts.UTC < test.end)

par(mfrow = c(4,1))
plot(df1$dts.UTC, df1$Depth..100bar..1..m., type = 'l')
plot(df2$dts.UTC, df2$Depth..100bar..1..m., type = 'l')
plot(df3$dts.UTC, df3$Depth..100bar..1..m., type = 'l')
plot(df4$dts.UTC, df4$Depth..100bar..1..m., type = 'l')
dev.off()

plot(df1$dts.UTC, df1$Depth..100bar..1..m., type = 'l')
lines(df2$dts.UTC, df2$Depth..100bar..1..m., type = 'l', col = 2)

lines(df3$dts.UTC, df3$Depth..100bar..1..m., type = 'l', col = 3)
lines(df4$dts.UTC, df4$Depth..100bar..1..m., type = 'l', col = 4)
title("depth tests of CafeCams at MBARI, April 2018")
legend("topright", legend = cams, fill = 1:4)
