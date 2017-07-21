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
#dc.prog <- c(7:8, 10:12, 14:16)
#df$dc_prog <- ifelse(hour(df$dts.local) %in% dc.prog, TRUE, FALSE)

#subset to fields of interest
df2 <- select(df, dts.local, Flags, trig, CC.status, dc_prog, Camera)
df2$CamCode <- factor(df2$Camera)
levels(df2$CamCode) = list("BU10" = "10", "BU11" = "11", "BU12" = "12", "BU13" ="13", "rec14" = "14", 
                  "BD20" = "20", "BD21" ="21", "BD22" = "22", "BD23" = "23", "BD24" = "24", "BD25" ="25", 
                  "BD26" ="26", "err90" = "90", "err91" = "91")
#aggregated factor levels
#levels(tmp$CamCode) = list("BU" = c("10", "11", "12", "13"), "rec" = "14", 
#                           "BD" = c("20", "21", "22", "23", "24", "25", "26"), 
#                           "err" = c("90", "91"))
df2$Flags <- factor(ifelse(as.character(df2$Flags) == "---", NA, as.character(df2$Flags)), 
                    levels(df2$Flags) <- c("-T-", "-TS", "PTS", "--S", "P--"))
            #remove blank reading, "---", for plotting

###########
#plot
##########
#tmp <- df;
#loop to plot by 3 hour segments
deploy_idx <- seq(min(df2$dts.local), max(df2$dts.local), by = 3*60*60)
#open pdf
pdf(paste(dd, "figs", paste(sppID, projID, deployID, "TagCodesPlot.pdf", sep = "_"), sep="/"))
for(i in 1:length(deploy_idx)){
  if(i < length(deploy_idx)) {start <- deploy_idx[i]; stop <- deploy_idx[i+1]; }
  print(i)
  print(paste("start set as", start, sep = " "))
  print(paste("stop set as", stop, sep = " "))
  if(i == length(deploy_idx)){ start = deploy_idx[i]; stop <- max(df2$dts.local);}
  print(i)
  print(paste("start set as", start, sep = " "))
  print(paste("stop set as", stop, sep = " "))
  
  
  #subset data
  tmp <- df2[df2$dts.local >= start & df2$dts.local <= stop,]
  
  #plot
  plot(tmp$dts.local, tmp$dc_prog, type = "n", axes = F, ylim=c(-(length(levels(tmp$CamCode))+1),(length(levels(tmp$Flags))+1)), 
       xlab = "", ylab = "", main = paste(deployID, "cam/trig codes;", i, "of", length(deploy_idx), "trihour segments", sep = " "))
  
  #y axis labels
  axis(side=2, at = seq((0-(length(levels(tmp$CamCode))+1)),(length(levels(tmp$Flags))+1),1), 
       labels = c(rev(levels(tmp$CamCode)), "DC_prog", "CC.status", "trig_obs", levels(tmp$Flags)), 
       las = 2, cex = 0.5)
  #x axis labels
  axis.POSIXct(side = 1, at = seq(start, stop, length.out = 6), format = "%b%d %H:%M", las = 2)
  
  #add data
  points(tmp$dts.local, ifelse(tmp$CC.status == "R--", 0, NA), col = '#fc8d59', pch = "|") #what recorded
  points(tmp$dts.local, ifelse(tmp$dc_prog == TRUE, -1, NA), col = '#ef6548', pch = "|") #what is programmed to record
  points(tmp$dts.local, ifelse(tmp$trig == TRUE, 1, NA), col = '#41ae76', pch = "|") #observed triggers
  #prep color palettes
  camcolfunc <- colorRampPalette(c("#ffffbf", "#f46d43"))
  flagcolfunc <- colorRampPalette(c("#e7d4e8", "#762a83"))
  #add cam/trig codes
  points(tmp$dts.local, (0-(as.numeric(tmp$CamCode) + 1)), col = camcolfunc(nlevels(tmp$CamCode))[as.numeric(tmp$CamCode)], pch = "|")
  points(tmp$dts.local, (as.numeric(tmp$Flags)+1), col = flagcolfunc(nlevels(tmp$Flags))[as.numeric(tmp$Flags)], pch = "|")
  abline(h = 1.5, lty = 2); abline(h = -1.5, lty = 2)
  
  
  print(paste("plot", i, "of", length(deploy_idx), "has been appended to the file", sep = " "))
  print(paste("start was", start, sep = " "))
  print(paste("stop was", stop, sep = " "))
}
#close file
dev.off()


########################################
####SCRATCH CODE
#setup key for plotting codes on correct intercept
#unique(df$CC.status); unique(df$Flags); unique(df$Camera);
#flags_key <- data.frame(Flags = c("-T-", "-TS", "PTS", "--S", "P--", "---"), key = c(4, 5, 6, 7, 8, NA))
#cam_key <- data.frame(Camera = c(0, 10, 11, 12, 13, 14, 20:26, 90:91), 
#                      key = c(NA, -2, -3, -4, -5, -6, rep(-7, 7), rep(-8, 2)), 
#                      fxn = c("off", rep("BU", 4), "rec", rep("BD", 7), rep("err", 2))) 

#coerce Camera & Flags into factors for plotting

##ggplot
#ggplotter <- select(tmp, dts.local, Flags, trig, CC.status, dc_prog, Camera, CamCode)
#str(ggplotter)
#ggplot(ggplotter, aes(x= dts.local))+
#  geom_point(aes(y=Flags,color = Flags))+
# geom_point(aes(y=CamCode, color = CamCode))+
#  scale_y_discrete(drop=F)










#