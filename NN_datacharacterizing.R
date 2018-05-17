dd <- "/Volumes/UNTITLED/grumble_iMacDataFiles_pre_github/FinMountedOriginal"
df1 <- read_csv(file.path(dd, "dataout.csv"))
df <- read_csv(file.path(dd, "LOG_CC_2_24_D1.CSV"))
par(mfrow = c(2,1))
plot(df$`Pressure - channel: 1`, type = "l")
plot(df1$depth, type = "l")
dev.off()


dd <- "/Volumes/WHITE SHARK/Neral_network_datasets_Zac/CC_2_24_PR161108"
df <- read_csv(file.path(dd, "CC-2-24_PR161108_PR16110703.csv"))
plot(df$`Pressure - channel: 1`, type = "l")
locator(1)

end <- 4657246


dd <- "/Users/jmoxley/Dropbox (MBA)/NN_processing/data/input"
df <- read_csv(file.path(dd, "PR161108_tagonextent.csv"))

### FMO
dd <- "/Volumes/WHITE SHARK/Neral_network_datasets_Zac/CC_2_24_PR151106"
df <- read_csv(file.path(dd, "LOG_CC_2_24_D1.CSV"))
