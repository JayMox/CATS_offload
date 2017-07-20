##0704 trig/cam code plotting
##subsampling by the hour to create animations of time series of various codes
##JHMoxley

#prep workbench
#prep workbench
rm(list = ls())
library(dplyr)
library(lubridate)
library(RColorBrewer)

#set working drive to git repo
wd <- "/Users/jmoxley/Documents/MBA_GWS/GitTank/CC_CamTags"
setwd(wd)
#set up data drive, 
dd <- "/Volumes/UNTITLED 1/CamTag/SA2017_raw"; clip = 26

#get data
sppID <- "CC"
deployID <- "0704D1"
projID <- "SA2017"
datFreq = 5
#load in
load(file.path(substr(dd, 0, clip), paste(paste(sppID, projID, deployID, datFreq, "Hz", sep = "_"), "Rdata", sep = ".")))

#add in duty cycling
dc.prog <- c(7:8, 10:12, 14:16)
df$dc_prog <- ifelse(hour(df$dts.local) %in% dc.prog, TRUE, FALSE)

#setup key for plotting codes on correct intercept
unique(df$CC.status); unique(df$Flags); unique(df$Camera);
flags_key <- data.frame(Flags = c("-T-", "-TS", "PTS", "--S", "P--", "---"), key = c(4, 5, 6, 7, 8, NA))
cam_key <- data.frame(Camera = c(0, 10, 11, 12, 13, 14, 20:26, 90:91), 
                      key = c(NA, -2, -3, -4, -5, -6, rep(-7, 7), rep(-8, 2)), 
                      fxn = c("off", rep("BU", 4), "rec", rep("BD", 7), rep("err", 2))) 

#coerce Camera & Flags into factors for plotting
tmp <- df
tmp$CamCode <- factor(tmp$Camera)
levels(tmp$CamCode) = list("BU10" = "10", "BU11" = "11", "BU12" = "12", "BU13" ="13", "rec" = "14", 
                  "BD20" = "20", "BD21" ="21", "BD22" = "22", "BD23" = "23", "BD24" = "24", "BD25" ="25", 
                  "BD26" ="26", "err90" = "90", "err91" = "91")
#levels(tmp$CamCode) = list("BU" = c("10", "11", "12", "13"), "rec" = "14", 
                           "BD" = c("20", "21", "22", "23", "24", "25", "26"), 
                           "err" = c("90", "91"))
tmp$Flags <- factor(tmp$Flags, levels = c("-T-", "-TS", "PTS", "--S", "P--", "---"))


#plot
#tmp <- df;
quartz()
plot(tmp$dts.local, tmp$dc_prog, type = "n", axes = F, ylim=c((length(levels(tmp$CamCode))+1)),(length(levels(tmp$Flags))+1), xlab = "", ylab = "", main = paste(deployID, "cam/trig codes", sep = " "))
points(tmp$dts.local, ifelse(tmp$CC.status == "R--", 0, NA), col = 'red') #what recorded
points(tmp$dts.local, ifelse(tmp$dc_prog == TRUE, -1, NA), col = 'pink') #what is programmed to record
points(tmp$dts.local, ifelse(tmp$trig == TRUE, 1, NA), col = 'green') #observed triggers
#add codes
#CamCols <- brewer.pal(nlevels(tmp$CamCode), "Dark2")
points(tmp$dts.local, (0-(as.numeric(tmp$CamCode) + 1)))
points(tmp$dts.local, (as.numeric(tmp$Flags)+1))
abline(h = 1.5, lty = 2); abline(h = -1.5, lty = 2)
#y axis labels
axis(side=2, at = seq((0-(length(levels(tmp$CamCode))+1)),(length(levels(tmp$Flags))+1),1), 
     labels = c(rev(levels(tmp$CamCode)), "DC_prog", "CC.status", "trig", levels(tmp$FlagCode)), 
     las = 2, cex = 0.5)
axis.POSIXct(side = 1, at = seq(as.POSIXct(min(tmp$dts.local)), as.POSIXct(max(tmp$dts.local)), length.out = 12), format = "%b%d %H:%M", las = 2)

#track/catalogue events in time series
df3 <- df
df4 <- data.frame(NA)
df4 <- select(df3, dts.local)
df4$dc_prog <- ifelse(hour(df4$dts.local) %in% c(10, 12, 15), 1, NA)
#df4$VVtrig_expc <- ifelse(df3$VVtrig >= trig.thresh, 3, NA)
df4$VVtrig_expc <- ifelse(df3$trig == TRUE, 3, NA)
df4$CC.status <- ifelse(df3$CC.status == "R--", 2, NA)
#NEEDS SOME METRIC HERE OF WHEN THE CAMERA WAS ACTUALLY On

#Cam fields
#unique(df$Camera)
df4$cam10 <- ifelse(df3$Camera == 10, -1, NA)
df4$cam11 <- ifelse(df3$Camera == 11, -2, NA)
df4$cam12 <- ifelse(df3$Camera == 12, -3, NA)
df4$cam13 <- ifelse(df3$Camera == 13, -4, NA)
df4$cam14 <- ifelse(df3$Camera == 14, -5, NA)
df4$cam20s <- ifelse(df3$Camera >= 20 & df3$Camera <= 30, -6, NA)
df4$cam90s <- ifelse(df3$Camera >= 90, -7, NA)

#Trigger fields
#unique(df$Flags)
df4$.T. <- ifelse(df3$Flags == "-T-", 4, NA)
df4$.TS <- ifelse(df3$Flags == "-TS", 5, NA)
df4$PTS <- ifelse(df3$Flags == "PTS", 6, NA)
df4$..S <- ifelse(df3$Flags == "--S", 7, NA)
df4$P.. <- ifelse(df3$Flags == "P--", 8, NA)


#plot

quartz()
plot(df4$dts.local, df4$dc_prog, col = "white", axes = FALSE, ylim = c(-7,8), xlab = "", ylab = "", main = "0706D1 camera/trigger codes")
points(df4$dts.local, as.numeric(df4$VVtrig_expc), col = "green")
points(df4$dts.local, as.numeric(df4$dc_prog), col = "pink")
points(df4$dts.local, as.numeric(df4$CC.status), col = "red")
#add camera codes
points(df4$dts.local, df4$cam10, col = "slateblue"); points(df4$dts.local, df4$cam11, col = "skyblue"); points(df4$dts.local, df4$cam12, col = "royalblue"); points(df4$dts.local, df4$cam13, col = "steelblue"); points(df4$dts.local, df4$cam14, col = "turquoise"); points(df4$dts.local, df4$cam20s, col = "tan"); points(df4$dts.local, df4$cam90s, col = "tomato")
#add triger codes
points(df4$dts.local, df4$.T., col = "seagreen"); points(df4$dts.local, df4$.TS, col = "palegreen"); points(df4$dts.local, df4$PTS, col = "olivedrab2"); points(df4$dts.local, df4$..S, col = "khaki"); points(df4$dts.local, df4$P.., col = "lavender")
abline(h = 3.5, lty = 2); abline(h = -0.5, lty = 2)
#y axis labels
axis(side=2, at = seq(-7,8,1), labels =  c("CC Err", "CC BD", "CC Rec", 
                                           "CC BU13", "CC BU12", "CC BU11", "CC BU10",  "", "DC programmed", "CC.Status", "VV > trig", 
                                           "-T-", "-TS", "PTS", "--S", "P--"), las = 2, cex = 0.5)
axis.POSIXct(side = 1, at = seq(as.POSIXct(min(df4$dts.local)), as.POSIXct(max(df4$dts.local)), length.out = 12), format = "%b%d %H:%M", las = 2)