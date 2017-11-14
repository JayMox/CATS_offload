load(file.path(substr(dd, 0, clip), paste(paste(sppID, projID, deployID, datFreq, "Hz", sep = "_"), "Rdata", sep = ".")))

df3 <- df

#track/catalogue events in time series
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