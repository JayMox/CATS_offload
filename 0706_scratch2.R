#stitching together data from other tags


#set up data drive & null data frame
#dd = "/Volumes/HELLBENDY/MBA_GWS/SOAF17_data"
#dd = "/Volumes/HELLBENDY/MBA_GWS/SOAF17_data"
dd = "/Volumes/UNTITLED 1"
df <- data.frame()

#open deployments & read in
setwd(paste(dd, "CC_7_06_SA2017", sep = "/"))
temp <- list.files(pattern="*.csv")
dat <- lapply(temp, read.csv, fileEncoding = "latin1")

df <- do.call('rbind', dat)
datFreq = 100; #20Hz data 

#non zero camera entries
sum(df$Camera != 0)

#smooth depth data
df$depth <- filter(df[,15], filter = rep(1,5*datFreq)/(5*datFreq), sides = 2, circular = T)
#calc instaneous VV & VV over 1s
df$VV <- c(0, diff(df$depth))
#df$VV_1Hz <- c(rep(0, datFreq), diff(df$depth, datFreq))

#smooth VV (CFW uses this filter rep(1,5) in depth_window_fig in gRumble ms; not sure why)
df$VV <- filter(df$VV, filter = rep(1,datFreq), sides = 2, circular = T)
#df$VV_1Hz <- filter(df$VV_1Hz, filter = rep(1,datFreq), sides = 2, circular = T)

#I BELIEVE the instaneous VV calc w/ a filter at datFreq produces VV rates over 1 sec; negating need for VV_1Hz

#plot
quartz()
plot(df$depth, type = "n")
abline(v = which(df$Camera != 0), alpha = 0.25, col = "lightgray")
lines(df$depth, col = "royalblue", lwd = 2)
segments(y = df$depth, y0 = df$depth[1:nrow(df) - 1], y1 = df$depth[2:nrow(df)], x0= 1:(nrow(df)-1), x1 = 2:(nrow(df)), col=color.scale(df$VV, color.spec = "rgb"))
