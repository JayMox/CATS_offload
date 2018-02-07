#Saturation curves for NN manuscript
#building a figure for goodness of fits using increasing amounts of training data.  

rm(list = ls())
library(ggplot2)
library(plotly)
library(DescTools)
library(dplyr)
library(tidyverse)
library(reshape)
library(gridExtra)
library(grid)

#set drive
dd <- "/Users/jmoxley/Documents/GitTank/CC_CamTags/data/"
df <- read_csv(file.path(dd, "PR161108_ODBA_SaturationTest_obs_pred_1_18_hours.csv")) %>% 
  filter(!is.na(`time-sec`))  #clip NAs to create a 6 hour test set using increasing amts of trianing data

#Goodness of Fit simulations
window_max <- 10000 # in seconds
low_win <- seq(from = 1, to = window_max,length.out = 50 )

sims <- 100
saturation <- data.frame(interval = rep(low_win, times = sims))

for(j in 4:ncol(df)){
  #subset data of interet
  modeled <- df[,c(1,3,j)]  #time-sec, obsODBA, & predOBA w/ relevant training data
  colnames(modeled) <- c("time.sec", "observed", "predicted")

  #clear storage space
  metric_df_sim <- NULL

  #jackknife
  for(x in 1:sims){
    metric_df <- NULL
    
    for(i in 1:length(low_win)){
      
      # random number selection from which to start window grab
      rand_num <- sample(1:15000,1)
      print(rand_num)
      
      # acquire vectors describing that window based on indexed window size
      # and starting point random-uniform drawing
      x <- modeled$time.sec[rand_num:(rand_num+low_win[i])]
      y_obs  <- modeled$observed[rand_num:(rand_num+low_win[i])]
      y_pred <- modeled$predicted[rand_num:(rand_num+low_win[i])]
      
      # calculate area under the curve for actual and observed of the selected window
      AUCobs  <- DescTools::AUC(x,y_obs)
      AUCpred <- DescTools::AUC(x,y_pred)
      
      # two evaluation metrics, metric 1 appears sensitive to the magnitude of the AUC
      #metric1 <- 1- ( abs( AUCobs - AUCpred )/ low_win[i] )
      #metric2 <- ifelse(AUCobs > AUCpred, AUCpred/AUCobs, AUCobs/AUCpred)
      metric3 <- 1 - abs( AUCobs - AUCpred )/ AUCobs  #error percentage
      
      # Build dataframe for one simulation
      # DF: 1 - window size, 2 - metric1 , 3 - metric2
      metric_df$window[i]  <- low_win[i]
      metric_df$metric3[i] <- metric3
    }  
    
    # Repeat X times and row bind the simulations
    metric_df <- as.data.frame(metric_df)
    metric_df_sim <- rbind(metric_df,metric_df_sim)
    print(x) 
  }
  #store data
  saturation[,j-2] <- metric_df_sim$metric3
  colnames(saturation)[j-2] <- paste(colnames(df[,j]), "-sim", sep="");
  print(paste(j, "IS DONE"))
}
save(saturation, file = file.path(dd, "saturaton_sims.RData"))