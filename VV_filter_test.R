##getting VVs of different scales when I use the filter fxn as shown in CFW code
##see here for example: https://github.com/MBayOtolith/StomachTagCorrection_MS/blob/master/Scripts/FigureScripts/Depth_WindowsizeFigure.R
##this is a scratch sheet to explore the data

setwd("/Volumes/HELLBENDY/MBA_GWS/stomachT")
dat_Stomach<-read.table("6_Pepe_20101105_InternalTemp.tab")

dat_Stomach$Depth<-filter(dat_Stomach$Depth,rep(1,5*5)/(5*5),sides=2,circular = TRUE)
dat_Stomach$VV<-0
dat_Stomach$VV1[2:nrow(dat_Stomach)]<-dat_Stomach$Depth[2:nrow(dat_Stomach)] - dat_Stomach$Depth[1:(nrow(dat_Stomach)-1)]
dat_Stomach$VV2 <- c(0, diff(dat_Stomach$Depth))

dat_Stomach$VV1_sm<-filter(dat_Stomach$VV1,filter=rep(1,5),sides=2,circular = TRUE)
dat_Stomach$VV2_sm<-filter(dat_Stomach$VV2,filter=rep(1,5),sides=2,circular = TRUE)


###result of filtering VV is to provide a moving sum of VV over 1s
###VV is kinda uninterpretible at subsecond resolution; filter=rep(1,5) means
###to take 5 adjacent values and sum them and at current time pt