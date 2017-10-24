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
load.in <- function(dd, sppID, projID, deployID, stringsAsF, dsmooth){
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
  df$dts.local <- strptime(paste(df$Date..local., df$Time..local.), format = "%d.%m.%Y %H:%M:%OS", tz = locTZ)
  
  #estimate sampling freq
  datFreq <- sFreq(df$dts.UTC, 100)
  print(paste("MODAL SAMPLING FREQUENCY IS ASSUMPED TO BE ", datFreq, "Hz"))
  
  #massage depth if dsmooth = T
  df <- rename(df, rawDEPTH = Depth..100bar..1..m.)
  if(dsmooth == T){
    #smooth depth trace over 5s w/ moving avg
    df$depth <- stats::filter(df$rawDEPTH, filter = rep(1,5*datFreq)/(5*datFreq), sides = 2, circular = T)
    df$VV <- c(0, diff(df$depth))
    #smooth VV over 1s w/ moving avg
    df$VV <- stats::filter(df$VV, filter = rep(1,1*datFreq), sides = 2, circular = T) 
  }
  
  #summarize msgs
  print(paste("TEMPORAL EXTENT OF DATA IS FROM", as.Date(min(df$dts.local)), "TO", as.Date(max(df$dts.local)), sep = " "))
  print(paste("DATA'S DURATION COVERS", round(difftime(max(df$dts.UTC), min(df$dts.UTC), units = "hours"), 4), "HOURS LONG", sep = " "))
  print(paste("SAMPLING FREQUENCY IS", datFreq, "Hz", sep = " "))
  print(paste("DEPTH & VV DATA WAS", ifelse(dsmooth==T, "", "NOT"), "SMOOTHED OR CALC'ED"))
  
  #return data
  return(df)
}

#function to calculate the modal sampling frequency of a tag dataset
#uses the date time stamp column and calculates stepwise differences at N-specified spots
sFreq <- function(dts, n){
  ridx <- as.integer(runif(n, 1, nrow(df))) #calc sampling frequency at 100 instances
  dFreq <- Mode(round((1/as.numeric(dts[ridx] - dts[ridx-1])), 3))  #calc iterated diffs
  print(paste("MODAL SAMPLING FREQUENCY IS ESTIMATED TO BE ", datFreq, "Hz"))
  return(dFreq)
}


