#Fxns for tag processing, primarily CATS Camera & Diary tags
require(ggplot2)
require(plotly)
require(data.table)

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

#fxn for scraping metadata from CATS
#add in functionality for creating a directory if not provided
get.metadata <- function(depid, dir = NA){
  #check params
  if(is.na(depid)){print("please provide deployment id");
    return()}
  if(is.na(dir)){
    print("no dir provided by user")
    return()
  }
  
  require(dplyr); require(stringr);
  #find info file; formats capable .txt or .cfg
  (files <- list.files(dir, pattern=("*.txt|CFG"),full.names = T))
  ifelse(length(files) > 1, print("more than 1 txt file in raw data drive"), print("scraping 1 txt file in dir provided"))
  #metadata scraping, read in as character strings for regex
  txt <- rbind(paste("sourcedir=", str_split(rawdir, "tag_data_raw", simplify = T)[1], sep=""), 
               paste("sourcepath=", paste("/tag_data_raw",str_split(rawdir, "tag_data_raw", simplify = T)[2],sep=""),sep=""),
               paste("deployID=", deployID, sep = ""), 
               sapply(read.delim(files), "as.character"))
  
  #get field categories & delimit field names from field values
  labels <- c("[global]", txt[which(str_detect(txt, "\\[[:alpha:]*[:space:]?[:alpha:]*?\\]"))])
  fields <- data.frame(str_split(txt, "\\=", n = 2, simplify = T)) %>% #split names & fields
    rename(field = X1, value = X2) %>% 
    mutate(class = labels[findInterval(seq(1:nrow(txt)), which(str_detect(txt, "\\[[:alpha:]*[:space:]?[:alpha:]*?\\]")))+1]) %>% 
    filter(!(str_detect(txt, "\\[[:alpha:]*[:space:]?[:alpha:]*?\\]")))
  
  #parse video trigger field in old gen tags that doesn't mine well
  idx <- str_detect(fields$field, "rawon")
  fields <- fields %>% mutate(field = ifelse(idx, str_replace(field, " rawon", ""), as.character(field)),
                              value = ifelse(idx, str_c("rawon=", value), as.character(value)))

  #index sensors
  fields <- fields %>% mutate(sensor = ifelse(str_detect(class, "sensor"), str_extract(field, "[:digit:]{2}"), NA),
                              active = ifelse(str_detect(class, "sensor"), str_trim(substr(class, 2, str_locate(class, "\\s")[,1]), "right"), NA))
  
  return(fields)
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
  plot(df$dts.UTC, df$rawDEPTH, type = "l")
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
  axis(side = 1, at = seq(min(df2$dts.UTC), max(df2$dts.UTC), by = 6*60*60), labels = FALSE)
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
  axis.POSIXct(side = 1, at = seq(min(df2$dts.UTC), max(df2$dts.UTC), by = 3*60*60), format = "%b%d %H:%M", las = 2)
  #axis.POSIXct(side = 1, at = seq(min(df2$dts.UTC), max(df2$dts.UTC), by = 60), format = "%b%d %H:%M", las = 2)
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

#function to append a vector of day/night times
##https://www.rdocumentation.org/packages/maptools/versions/0.6-5/topics/sunriset-methods
#based on sunriset(), defaults coordinates of MBay Inner Buoy
add_night <- function(dts, loc = NULL){
  if(as.numeric(max(dts)-min(dts)) > 30){print("long deployment; method not quite adequate")}
  #NB: not adequate for multi-month deployments if 
  #sunrise/sunset will change dramatically over time
  require(maptools)
  require(sp)
  
  #default to middle of monty bay
  if(is.null(loc)){
    print("no loc provided")
    print("DEFAULTING TO MONTY BAY BUOY")
    loc = matrix(c(-122.029, 36.751))
    coord = matrix(loc, nrow=1)
    #https://www.ndbc.noaa.gov/station_page.php?station=46092
    loc = SpatialPoints(coord, proj4string=CRS("+proj=longlat +datum=WGS84"))
  }else{
    print(paste("lat coord used is ", loc[2]))
    print(paste("lon coord used is ", loc[1]))
    coord = matrix(loc, nrow = 1)
    loc = SpatialPoints(coord, proj4string=CRS("+proj=longlat +datum=WGS84"))
  }
  
  #use mode as a dummy day
  dummy.dt <- as.POSIXct(Mode(format(dts, format = '%Y-%m-%d')), 
                          tz = attr(dts, "tz"))
  print(paste("dummy date is", dummy.dt))
  #get vector of sunrise/set
  up <- sunriset(loc, dummy.dt, 
                    direction="sunrise", POSIXct.out = TRUE)
  print(paste("sunrise times will be", up$time))
  down <- sunriset(loc, dummy.dt, 
                   direction="sunset", POSIXct.out = TRUE)
  if(down$day_frac > 1){down$day_frac <- down$day_frac - 1} #don't let creep into next day
  print(paste("sunset times will be", down$time))
  
  #test if a reading is at night
  night <- NA
  # dummy.dts <- as.POSIXct(paste(dummy.dt, 
  #                               strftime(dts, "%H:%M:%S", tz = attr(dts, "tz"))), 
  #                         tz = attr(dts, "tz"))
  d <- secs_midnight(dts)/(3600*24)  #proportion of day
  night <- factor(ifelse(d > down$day_frac & d < up$day_frac,
                  "night", "day"))
  
  return(night)
}

nighttime <- function(dts, loc = matrix(c(-122.029, 36.751), nrow=1)){
  #function to return a data frame of sunsets/sunrises for plotting tag data
  require(maptools)
  #default to middle of monty bay
  if(class(loc) != "SpatialPoints"){
    coord = matrix(c(-122.029, 36.751), nrow=1)
    #https://www.ndbc.noaa.gov/station_page.php?station=46092
    loc = SpatialPoints(coord, proj4string=CRS("+proj=longlat +datum=WGS84"))
    print("loc provided was not SpatialPoints class")
    print("DEFAULTING TO MONTY BAY BUOY")
  }
  
  dates <- as.POSIXct(unique(strftime(dts, format = "%Y:%m:%d")), 
                   format = "%Y:%m:%d", tz = attr(dts, "tz"))
  
  df <- data.frame(
    down = sunriset(loc, dates, 
                    direction="sunset", POSIXct.out = TRUE)$time,
    up = sunriset(loc, dates + (60*60*24), 
                  direction="sunrise", POSIXct.out = TRUE)$time
  )
  return(df)
}

#function to tally secs since midnight
secs_midnight <- function(dts){
  return(as.numeric(dts) - 
           as.numeric(as.POSIXct(
             paste(as.Date(dts), "00:00:00"), tz = attr(dts, "tz"))))
}

#simple ggplot theme
themeo <-theme_classic()+
  theme(strip.background = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(margin = margin( 0.2, unit = "cm")),
        axis.text.y = element_text(margin = margin(c(1, 0.2), unit = "cm")),
        axis.ticks.length=unit(-0.1, "cm"),
        panel.border = element_rect(colour = "black", fill=NA, size=.5),
        legend.title=element_blank(),
        strip.text=element_text(hjust=0) )
