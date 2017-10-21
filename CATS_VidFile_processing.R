##Script to rename video files from CATS tags
##JHMoxley, July 2017

#set datadrive to video file location
dd <- "/Volumes/UNTITLED 1/CamTag/CA2017_raw/CC_CA2017_0705D1/Vid"
setwd(dd)

sppID <- "CC"
deployID <- "0705D1"
projID <- "CA2017"

#get filenames
tmp <- list.files(pattern = "*.mp4")
tmp2 <- unlist(strsplit(tmp, split = ".mp4"))
paste(deployID, "_", substr(tmp2, nchar(tmp2)-5, nchar(tmp2)), ".mp4", sep = "")

#rename files with deployID & final digits in file index
file.rename(from = tmp, to = paste(paste(sppID, projID, deployID, sep="_"), "_", 
             substr(tmp2, nchar(tmp2)-4, nchar(tmp2)), ".mp4", sep = ""))


