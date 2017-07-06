#stitching together data from other tags
library(dplyr)

#set up data drive & null data frame
#dd = "/Volumes/HELLBENDY/MBA_GWS/SOAF17_data"
#dd = "/Volumes/HELLBENDY/MBA_GWS/SOAF17_data"
#dd = "/Volumes/UNTITLED 1"
dd = "/Users/jmoxley/Desktop/"
df <- data.frame()

#open deployments & read in
setwd(paste(dd, "CC_7_06_SA2017", sep = "/"))
temp <- list.files(pattern="*.csv")
#temp <- temp[37:38]
dat <- lapply(temp, read.csv, fileEncoding = "latin1")

df <- do.call('rbind', dat)
datFreq = 20; #20Hz data 


#non zero camera entries
sum(df$Camera != 0)
sum(df$Camera != 0)/nrow(df)

#smooth depth data
df$depth <- stats::filter(df[,15], filter = rep(1,5*datFreq)/(5*datFreq), sides = 2, circular = T)
#calc instaneous VV & VV over 1s
df$VV <- c(0, diff(df$depth))
#df$VV_1Hz <- c(rep(0, datFreq), diff(df$depth, datFreq))

#smooth VV (CFW uses this filter rep(1,5) in depth_window_fig in gRumble ms; not sure why)
df$VVsm <- stats::filter(df$VV, filter = rep(1,datFreq), sides = 2, circular = T)
#df$VV_1Hz <- filter(df$VV_1Hz, filter = rep(1,datFreq), sides = 2, circular = T)

#I BELIEVE the instaneous VV calc w/ a filter at datFreq produces VV rates over 1 sec; negating need for VV_1Hz

#write out data for easy load in
#save(df, file = (paste(dd, "CC_7_06_SA2017/CC0706_SA2017_all.RData", sep = "/")))

#subset salient data for trigger eval
df2 <- select(df, depth, VV, VVsm, Camera, Camera.time, CC.status, Flags)
df2$DEPTH <- df$Depth..100bar..1..m.
df2$VV <- as.numeric(df2$VV)
 
#emulate CATS trigger method
df2$VVtrig <- slideVVtrig(data = df2$DEPTH, window = datFreq, step = 1)
df2$trig <- ifelse(df2$VVtrig > 0.2, TRUE, FALSE)
  

quartz()
p <- ggplot(df2, aes(y = DEPTH, x=1:nrow(df2))) +
  geom_vline(xintercept = which(df2$Camera %in% c(10, 11,12, 13, 14)), colour = "gray10") + 
  geom_line(aes(col = abs(VVsm))) +
  scale_color_gradient2(low = "gray50",mid = "orange", high = "red",  midpoint = 0.15) +
  geom_vline(xintercept = which(df2$trig == TRUE), linetype = "dotted") 
p
#p + geom_vline(xintercept = which(df2$Camera %in% c(10, 11,12, 13, 14)), colour = "red", alpha = 0.2)
#p + geom_vline(xintercept = 1:nrow(df2), colour = "red", alpha = 0.2)


###############
#plotting
quartz()
plot(df2$DEPTH, type = "n")
abline(v = which(df2$Camera %in% c(11, 12)), col = "gray")
lines(df2$DEPTH, type = "l", col = "lightblue")
abline(v = which(df2$trig == TRUE), col = "red", lwd = 0.5)
segments()

quartz()
ggplot(df2, aes(y = DEPTH, x = seq(1:nrow(df2)))) + geom_line(aes(colour = abs(smVV)))

#attempting to recreate CATS trigger, see email from Nikolai.. essentially holds min/max depths over the prior 1s in buffer & uses diff to trigger camera
#library(rollply)
#sc <- rollapply(df2$DEPTH, width = datFreq, range);
#sc2 <- rbind(matrix(c(rep(0,99), rep(0,99)), ncol = 2), sc)
#colnames(sc2) <- c("minD_prior100", "maxD_prior100")
#df <- cbind(df, sc2); df2 <- cbind(df2, sc2);

#mark instances of 0.2m/s VV w/in buffer 
#df2$trig_diff <- df2$maxD_prior100 - df2$minD_prior100
#df2$trig <- ifelse(df2$trig_diff >= 0.2, TRUE, FALSE)

#plot
quartz()
plot(df$depth, type = "n")
abline(v = which(df$Camera != 0),  col = "lightgray")
abline(v = which(df2$trig == TRUE), col = "red")
abline(v = which(df2$flags == "-T-"))
lines(df$depth, col = "royalblue", lwd = 2)

#segments(y = df$depth, y0 = df$depth[1:nrow(df) - 1], y1 = df$depth[2:nrow(df)], x0= 1:(nrow(df)-1),
#         x1 = 2:(nrow(df)), col=color.scale(df$VV, color.spec = "rgb"))
#segments(y = df$depth, y0 = df$depth[1:nrow(df) - 1], y1 = df$depth[2:nrow(df)], x0= 1:(nrow(df)-1),
#         x1 = 2:(nrow(df)), col=rainbow(100))

#doing it in ggplot
#cam.on <- which(df2$Camera %in% c(10, 11,12, 13, 14))



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
