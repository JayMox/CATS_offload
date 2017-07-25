##Script to rename video files from CATS tags
##JHMoxley, July 2017

#set datadrive to video file location
dd <- "/Volumes/UNTITLED/CamTag/CC_SA2017_0706D1/Vid"
setwd(dd)

deployID <- "0706D1"

#get filenames
tmp <- list.files(pattern = "*.mp4")
tmp2 <- unlist(strsplit(tmp, split = ".mp4"))
paste(deployID, "_", substr(tmp2, nchar(tmp2)-5, nchar(tmp2)), ".mp4", sep = "")

#rename files with deployID & final digits in file index
file.rename(from = tmp, to = paste(deployID, "_", 
             substr(tmp2, nchar(tmp2)-4, nchar(tmp2)), ".mp4", sep = ""))


