#Figure to assess ANN swapping bw datasets
#May 2018
rm(list = ls())
library(ggplot2)
library(plotly)
library(DescTools)
library(dplyr)
library(tidyverse)
library(reshape2)
library(gridExtra)
library(grid)

#set drive & get data
dd <- "/Users/jmoxley/Dropbox (MBA)/NN_processing/data/ANN_swap"
themeo <-theme_classic()+
  theme(strip.background = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(margin = margin( 0.2, unit = "cm")),
        axis.text.y = element_text(margin = margin(c(1, 0.2), unit = "cm")),
        axis.ticks.length=unit(-0.1, "cm"),
        panel.border = element_rect(colour = "black", fill=NA, size=.5),
        legend.title=element_blank(),
        strip.text=element_text(hjust=0) )

#AUC procedure
#get data
fmo <- read.csv(file.path(dd, "FinMountOrigChks_ODBA_test_obs_pred_1_17_hours_SWAP.csv"), header = T) %>% 
  select(time.sec, depth, observed = ODBA.obs, ODBA.pred.3hr) %>% 
  filter(!is.na(time.sec)) %>% 
  mutate(id = "FMO")
pr <- read.csv(file.path(dd, "PR161108_ODBA_test_obs_pred_1_18_hours_SWAP.csv"), header = T) %>% 
  select(time.sec, depth, observed = ODBA.obs, ODBA.pred.3hr) %>% 
  filter(!is.na(time.sec)) %>% 
  mutate(id = "PR")
# tst1 <- read.csv(file.path(dd, "FinMountOrigChks_ODBA_test_obs_pred_1_17_hours.csv"), header = T) %>%
#   select(time.sec, depth, observed = ODBA.obs, ODBA.pred.3hr) %>%
#   filter(!is.na(time.sec)) %>%
#   mutate(id = "FMO")
# tst2 <- read.csv(file.path(dd, "PR161108_ODBA_test_obs_pred_1_18_hours.csv"), header = T) %>% 
#   select(time.sec, depth, observed = ODBA.obs, ODBA.pred.3hr) %>% 
#   filter(!is.na(time.sec)) %>% 
#   mutate(id = "PR")

#calc time-integrated accuracy
modeled <- fmo
saturation = NULL
window_max <- 5400 #just over 1.5hr
sims <- 500   # Number of sims
low_win <- seq(from = 2, to = window_max,length.out = 60 )

#quick look
basic_plot <- ggplot(modeled, aes(x = time.sec))+
  geom_line(aes(y=observed), color = "red", size = 0.2)+
  geom_line(aes(y=ODBA.pred.3hr), color = "blue", size = 0.2)+themeo
chk_plot <- ggplot() +
  geom_line(data = fmo, aes(x = time.sec, y=ODBA.pred.3hr), color = "red", size = 0.2)+
  geom_line(data = tst1, aes(x = time.sec, y=ODBA.pred.3hr), color = "blue", size = 0.2)+
  geom_line(data = fmo, aes(x = time.sec, y=observed), color = "dark green", size = 0.2)+
  labs(title = "green = observed, red = native, blue = generalized") + themeo
ggplotly(chk_plot)

for(j in 4:ncol(modeled)-1){
  metric_df_sim <- NULL
  for(x in 1:sims){
    metric_df <- NULL
    x <- NULL
    y_obs <- NULL
    y_pred <- NULL
    for(i in 1:length(low_win)){
      
      # random number selection from which to start window grab
      #rand_num <- sample(1:15000,1)
      #more uniform sampler of entire dataset
      success <- FALSE
      while(!success){    #sample until idx + window is within data bounds
        m <- sample(1:nrow(modeled), 1)
        success <- (m + low_win[i]) <= nrow(modeled)
      }
      rand_num <- m
      print(rand_num)
      
      # acquire vectors describing that window based on indexed window size
      # and starting point random-uniform drawing
      x <- modeled$time.sec[rand_num:(rand_num+low_win[i])]
      y_obs  <- modeled$observed[rand_num:(rand_num+low_win[i])]
      y_pred <- modeled[rand_num:(rand_num+low_win[i]), j]
      
      # calculate area under the curve for actual and observed of the selected window
      AUCobs  <- AUC(x,y_obs)
      AUCpred <- AUC(x,y_pred)
      
      # two evaluation metrics, metric 1 appears sensitive to the magnitude of the AUC
      metric1 <- 1- ( abs( AUCobs - AUCpred )/ low_win[i] )
      metric2 <- ifelse(AUCobs > AUCpred, AUCpred/AUCobs, AUCobs/AUCpred)
      metric3 <- 1 - abs( AUCobs - AUCpred )/ AUCobs
      
      # Build dataframe for one simulation
      # DF: 1 - window size, 2 - metric1 , 3 - metric2
      metric_df$window[i]  <- low_win[i]
      metric_df$metric1[i] <- metric1
      metric_df$metric2[i] <- metric2
      metric_df$metric3[i] <- metric3
    }  
    
    # Repeat X times and row bind the simulations
    metric_df <- as.data.frame(metric_df)
    metric_df_sim <- rbind(metric_df,metric_df_sim)
    print(x) 
  }
  #store results
  if(j==4)  {saturation <- data.frame(interval = metric_df_sim[,1])}   #add windows
  saturation <- bind_cols(saturation, metric_df_sim %>% select(metric3))
  colnames(saturation)[j-2] = colnames(modeled)[j]    
}
#save dataaaa
#fmo.saturation <- mutate(saturation, shark = "Shark 2")
#save(fmo.saturation, file = file.path(dd, "fmo.saturate.3hr.swap.RData"))
# pr.saturation <- mutate(saturation, shark = "Shark 1")
# save(pr.saturation, file = file.path(dd, "pr.saturation.3hr.swap.RData"))



#get data
load(file.path(dd, "fmo_saturate3hr.swap.RData"))
load(file.path(dd, "pr.saturation.3hr.swap.RData"))
df <- bind_rows(fmo.saturation %>% select(-id), pr.saturation %>% select(-id))

#melt
mdf <- melt(df, id.vars = c("interval", "shark"))
#plot (based on SO sol'n here: https://stackoverflow.com/questions/48835600/building-a-ggproto-geom-extension)

#FIGURE CODE
mdf <- mdf %>% mutate(label = factor(paste(str_extract(variable, "[:digit:]{1,2}"), "hr"), 
                                     levels = paste(seq(1:17), "hr")))
#full scale
ggplot(mdf, aes(x = interval, y = value, group=label, color=label)) + 
  geom_point(color = "light gray", alpha = 0.2) +
  lapply(1:40, # NUMBER OF LOESS
         function(i) {
           geom_smooth(data=mdf[sample(1:nrow(mdf), 
                                       2000),  #NUMBER OF POINTS TO SAMPLE
                                ], aes(color = factor(shark)), se=FALSE, span = .95, size = 0.2, method = "loess")
         }) +
  facet_grid(label~shark) + themeo +   guides(color = F) +
  labs(x = "AUC Interval (in mins)", y = "Accuracy (inferred from error %age") + 
  scale_x_continuous(expand=c(0,0), breaks = seq(600, 5400, by = 1200), 
                     labels = seq(10, 90, by = 20)) + 
  scale_y_continuous(limits = c(0.5, 1.0), #breaks=c(0.5, 0.75, 1.0)
                     breaks = seq(0.5, 1.0, length.out = 5))

