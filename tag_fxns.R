#Fxns for tag processing, primarily CATS Camera & Diary tags

#fxn to calculate the mean of a set of data
Mode <- function(x){
  ux <- unique(x)
  ux[which.max(tabulate(match(x,ux)))]
}

#fxn to do things temporarily inside a directory
#see here: https://stackoverflow.com/questions/26825000/in-r-do-an-operation-temporarily-using-a-setting-such-as-working-directory
with_dir <- function(dir, expr) {
  old_wd <- getwd()
  on.exit(setwd(old_wd))
  setwd(dir)
  evalq(expr)
}

#fxn to use deployment parameters set in processing script to load in raw csv 
#from correct deployment folder
#sAsF is a logical to specify strings as Factors
#dsmooth is logical specifying whether to smooth depth data
#camcodify is logical specifying whether to factorize camera codes
load.in <- function(dd, sppID, projID, deployID, stringsAsF, dsmooth, camcodify, trigify){
  require(dplyr)
  #get files
  temp <- list.files(file.path(dd, paste(sppID, projID, deployID, sep="_")), pattern="*.csv",
                     full.names = T)
  dat <- lapply(temp, read.csv, fileEncoding = "latin1", stringsAsFactors = stringsAsF)
  
  #build df
  df <- data.frame()
  df <- do.call('rbind', dat)
  
  #build date time stamps
  df$dts.UTC <- strptime(paste(df$Date..UTC., df$Time..UTC.), format = "%d.%m.%Y %H:%M:%OS", tz = "UTC")
  #df$dts.local <- strptime(paste(df$Date..local., df$Time..local.), format = "%d.%m.%Y %H:%M:%OS", tz = locTZ)
  
  #estimate sampling freq
  datFreq <- sFreq(df$dts.UTC, 100)
  
  #massage depth if dsmooth = T
  df <- rename(df, rawDEPTH = Depth..100bar..1..m.)
  if(dsmooth == T){
    #smooth depth trace over 5s w/ moving avg
    df$depth <- stats::filter(df$rawDEPTH, filter = rep(1,5*datFreq)/(5*datFreq), sides = 2, circular = T)
    df$VV <- c(0, diff(df$depth))
    #smooth VV over 1s w/ moving avg
    df$VV <- stats::filter(df$VV, filter = rep(1,1*datFreq), sides = 2, circular = T) 
  }
  
  #add in camcodes
  #df <- rename(df, CamCode = Camera)
  if(camcodify == T){
    df$CamCode <- factor(df$Camera, levels = list("BU10" = "10", "BU11" = "11", "BU12" = "12", "BU13" ="13", "rec14" = "14", 
                                     "BD20" = "20", "BD21" ="21", "BD22" = "22", "BD23" = "23", "BD24" = "24", "BD25" ="25", 
                                     "BD26" ="26", "err90" = "90", "err91" = "91"))
  }
  
  #add in trigcodes
  #df <- rename(df, TrigCode = Flags)
  if(trigify == T){
    df$Flags <- factor(ifelse(as.character(df$Flags) == "---", NA, as.character(df$Flags)), 
                                  levels <- c("-T-", "-TS", "PTS", "--S", "P--"))
    #remove blank reading, "---", for plotting
  }
  
  #summarize msgs
  print(paste("TEMPORAL EXTENT OF DATA IS FROM", as.Date(min(df$dts.UTC)), "TO", as.Date(max(df$dts.UTC)), sep = " "))
  print(paste("DATA'S DURATION COVERS", round(difftime(max(df$dts.UTC), min(df$dts.UTC), units = "hours"), 4), "HOURS LONG", sep = " "))
  print(paste("SAMPLING FREQUENCY IS", datFreq, "Hz", sep = " "))
  print(paste("DEPTH & VV DATA WAS", ifelse(dsmooth==T, "", "NOT"), "SMOOTHED OR CALC'ED"))
  
  #return data
  return(df)
}

#function to calculate the modal sampling frequency of a tag dataset
#uses the date time stamp column and calculates stepwise differences at N-specified spots
sFreq <- function(dts, n){
  ridx <- as.integer(runif(n, 1, length(dts))) #calc sampling frequency at 100 instances
  dFreq <- Mode(round((1/as.numeric(dts[ridx] - dts[ridx-1])), 3))  #calc iterated diffs
  print(paste("MODAL SAMPLING FREQUENCY IS ESTIMATED TO BE ", dFreq, "Hz"))
  return(dFreq)
}

#function to implement sliding window for use in replicating CATS tigger methods
#CAREFUL if trigger specified larger than 1s (ie., datFreq)
slide.window <- function(data, window, step){
  total <- length(data)
  spots <- seq(from=1, to=total-window, by=step)
  result <- vector(length = length(spots))
  for(i in 1:(length(spots)-window)){
    result[i] <- diff(range(data[spots[i]:spots[i + window]]))
  }
  return(c(rep(NA, window), result))  #pad front end of result where diff cannot be calc'ed
}

#function to produce EDA plots for CC tags
#ingests entire data frame
#fine.scale is a logical specifying whether to produce tri-hrly plots
eda.plot <- function(df2){
  layout(matrix(c(1,1,2,2,2,2,2,2), ncol = 1, nrow = 8))
  #depth trace plot
  par(mar=c(1, 5.1, 4.1, 2.1))  #widen y axis margin
  plot(df2$dts.UTC, df2$depth, type = "n", axes = F, xlab = "", ylab = "depth (m)", main = paste(deployID, "FULL DATA RECORD", sep = " "))
  lines(df2$dts.UTC, df2$depth, col = "#0570b0", type = "l", xlab = "", ylab = "depth")
  axis(side = 2, cex = 0.5, las = 2)
  #choose one of the other x-axis labelers
  axis(side = 1, at = seq(min(df2$dts.UTC), max(df2$dts.UTC), by = 3*60*60), labels = FALSE)
  #axis(side = 1, at = seq(min(df2$dts.UTC), max(df2$dts.UTC), by = 60), labels = FALSE)
  #axis.POSIXct(side = 1, at = seq(min(df2$dts.local), max(df2$dts.local), by = 3*60*60), format = "%b%d %H:%M", las = 2)
  #plot
  par(mar=c(6.1, 5.1, 1, 2.1))
  plot(df2$dts.UTC, df2$dc_prog, type = "n", axes = F, ylim=c(-(length(levels(df2$CamCode))+1),(length(levels(df2$Flags))+1)), 
       xlab = "", ylab = "", main = "CAM/TRIG CODES")
  #y axis labels
  axis(side=2, at = seq((0-(length(levels(df2$CamCode))+1)),(length(levels(df2$Flags))+1),1), 
       labels = c(rev(levels(df2$CamCode)), "DC_prog", "CC.status", "trig_obs", levels(df2$Flags)), 
       las = 2, cex = 0.5)
  #x axis labels ***choose one or the other labeler
  #axis.POSIXct(side = 1, at = seq(min(df2$dts.UTC), max(df2$dts.UTC), by = 3*60*60), format = "%b%d %H:%M", las = 2)
  axis.POSIXct(side = 1, at = seq(min(df2$dts.UTC), max(df2$dts.UTC), by = 60), format = "%b%d %H:%M", las = 2)
  #add data
  points(df2$dts.UTC, ifelse(df2$CC.status == "R--", 0, NA), col = '#fc8d59', pch = 3) #what recorded
  points(df2$dts.UTC, ifelse(df2$dc_prog == TRUE, -1, NA), col = '#ef6548', pch = 3) #what is programmed to record
  points(df2$dts.UTC, ifelse(df2$trig == TRUE, 1, NA), col = '#41ae76', pch = 3) #observed triggers
  #prep color palettes
  camcolfunc <- colorRampPalette(c("#ffffbf", "#f46d43"))
  flagcolfunc <- colorRampPalette(c("#e7d4e8", "#762a83"))
  #add cam/trig codes
  points(df2$dts.UTC, (0-(as.numeric(df2$CamCode) + 1)), col = camcolfunc(nlevels(df2$CamCode))[as.numeric(df2$CamCode)], pch = 3)
  points(df2$dts.UTC, (as.numeric(df2$Flags)+1), col = flagcolfunc(nlevels(df2$Flags))[as.numeric(df2$Flags)], pch = 3)
  abline(h = 1.5, lty = 2); abline(h = -1.5, lty = 2)
}

