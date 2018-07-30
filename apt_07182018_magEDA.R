dd <- "/Volumes/WHITE SHARK/CA2018/APT_CC0705_07182018"
files <- list.files(dd, pattern=("*.csv"),full.names = T); length(files)
#files <- files[-3]
#issues with file 3?

###BUTTER APPROACH
library(gRumble)
datFreq = 50
#butter approach
d <- NULL
df <- data.frame(depth = NULL, f = NULL)
for(i in 1:length(files)){
  print(i)
  d <- data.table::fread(files[i], select = 
                           c("Depth (100bar) 1 [m]", 
                             "Date (UTC)", "Time (UTC)",
                             "Accelerometer X [m/s\xb2]",
                             "Accelerometer Y [m/s\xb2]",
                             "Accelerometer Z [m/s\xb2]"))
  colnames(d) <- c("depth", "date", "time", "ax", "ay", "az")
  