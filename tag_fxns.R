#Fxns for tag processing, primarily CATS Camera & Diary tags

#fxn to calculate the mean of a set of data
Mode <- function(x){
  ux <- unique(x)
  ux[which.max(tabulate(match(x,ux)))]
}

#fxn to use deployment parameters set in processing script to load in raw csv 
#from correct deployment folder
#sAsF is a logical to specify strings as Factors
load.in <- function(dd, sppID, projID, deployID, stringsAsF){
  #get files
  temp <- list.files(file.path(dd, paste(sppID, projID, deployID, sep="_")), pattern="*.csv")
  dat <- lapply(temp, read.csv, fileEncoding = "latin1", stringsAsFactors = stringsAsF)
  
  #build df
  df <- data.frame()
  df <- do.call('rbind', dat)
  
  #build date time stamps
  df$dts.UTC <- strptime(paste(df$Date..UTC., df$Time..UTC.), format = "%d.%m.%Y %H:%M:%OS", tz = "UTC")
  df$dts.local <- strptime(paste(df$Date..local., df$Time..local.), format = "%d.%m.%Y %H:%M:%OS", tz = locTZ)
  
  #return data
  datFreq <- round(1/as.numeric(diff(head(df$dts.UTC, 2))), 2);
  print("MODAL SAMPLING FREQUENCY IS ASSUMPED TO BE ", )
  return(df)
}
  
  #data load in
  setwd(file.path(dd, paste(sppID, projID, deployID, sep="_")))
temp <- list.files(pattern="*.csv")
dat <- lapply(temp, read.csv, fileEncoding = "latin1")
setwd(wd) #remap to git drive
#build df
df <- data.frame()
df <- do.call('rbind', dat)