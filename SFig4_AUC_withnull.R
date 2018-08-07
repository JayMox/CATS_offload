#New & final figures of time-integrated ODBA predictions
#AUC figure w/ null median only model
#using ANNs w/ consistent hyper parameter architecture (30s window, 35 nodes, 3 layers)
#August 2018, JHMoxley
rm(list = ls())
library(ggplot2)
library(plotly)
library(DescTools)
library(dplyr)
library(reshape)
library(gridExtra)
library(grid)
library(forcats)
dd <- "/Users/jmoxley/Dropbox (MBA)/NN_processing/data/1hr_Prediction_testset_June18"
themeo <-theme_classic()+
  theme(strip.background = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(margin = margin( 0.2, unit = "cm")),
        axis.text.y = element_text(margin = margin(c(1, 0.2), unit = "cm")),
        axis.ticks.length=unit(-0.1, "cm"),
        panel.border = element_rect(colour = "black", fill=NA, size=.5),
        legend.title=element_blank(),
        strip.text=element_text(hjust=0) )
#get data
fmo <- read.csv(file.path(dd, "Shark2_ODBA_test_6_hours_pred_1_18_hours.csv"), header = T) %>% 
  select(time.sec, depth, obs = ODBA.obs, 
         pred.1hr = ODBA.pred.1hr, 
         pred.3hr = ODBA.pred.3hr, 
         pred.12hr = ODBA.pred.12hr) %>% 
  filter(!is.na(time.sec)) %>%
  mutate(id = "fmo", shark = "Shark 2", tuning = "native", null.median =) #add fields

pr <- read.csv(file.path(dd, "Shark1_ODBA_test_6_hours_pred_1_18_hours.csv"), header = T) %>% 
  select(time.sec, depth, obs = ODBA.obs, 
         pred.1hr = ODBA.pred.1hr, 
         pred.3hr = ODBA.pred.3hr, 
         pred.12hr = ODBA.pred.12hr) %>% 
  filter(!is.na(time.sec)) %>%
  mutate(id = "pr", shark = "Shark 1", tuning = "native", null.median = median(obs)) #add fields

#calc time-integrated accuracy
modeled <- fmo
saturation = NULL
#window_max <- 10000 # in seconds
window_max <- 5400 #just over 1.5hr
sims <- 500   # Number of sims
low_win <- seq(from = 2, to = window_max,length.out = 60 )

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
    y_obs  <- modeled$obs[rand_num:(rand_num+low_win[i])]
    y_pred <- modeled$null.median[rand_num:(rand_num+low_win[i])]
    
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

saturation <- metric_df_sim %>% select(interval = window, metric3)

#save dataaaa
# fmo.saturation.null <- mutate(saturation, shark = "Shark 2")
# save(fmo.saturation.null, file = file.path(dd, "fmo_saturate.null.RData"))
# pr.saturation.null <- mutate(saturation, shark = "Shark 1")
# save(pr.saturation.null, file = file.path(dd, "pr.saturation.null.RData"))


# load(file = file.path(dd, "fmo_saturate.1.3.12.RData"))
# load(file = file.path(dd, "pr.saturation.1.3.12.RData"))


#PLOTTING
library(tidyverse)
sc <- bind_rows(fmo.saturation %>% select(interval, shark, ))
mdf <- bind_rows(fmo.saturation.null, pr.saturation.null) %>% 
  gather(training, accuracy, -interval, -shark) %>% 
  mutate(shark = factor(shark), training = "null")
#combine w/ base predictions
sc <- bind_rows(fmo.saturation %>% mutate(training = "ANNpred") %>% 
                  select(interval, shark, training, accuracy = pred.1hr),
                pr.saturation %>% mutate(training = "ANNpred") %>% 
                  select(interval, shark, training, accuracy = pred.1hr))
mdf <- bind_rows(mdf, sc)

#Panel by panel plot         
set.seed(70-1)
ggplot(data = mdf %>% group_by(shark, training, interval), 
       aes(x = interval, y = accuracy, color=shark)) + 
  geom_point(data = mdf %>% group_by(shark, training, interval) %>% sample_n(100),
             color = "light gray", alpha = 0.2) +
  lapply(1:50, # NUMBER OF LOESS
         function(i) {
           # geom_smooth(data=mdf[sample(1:nrow(mdf), 
           #                             2000),  #NUMBER OF POINTS TO SAMPLE
           #                      ], se=FALSE, span = .95, size = 0.2, method = "loess") 
           geom_smooth(data=(mdf %>% group_by(shark, training, interval) %>% sample_n(4)),  #NUMBER OF POINTS TO SAMPLE
                       aes(x = interval, y = accuracy), se=FALSE, span = .90, size = 0.2, method = "loess")
           # geom_smooth(data=(mdf %>% group_by(variable) %>% sample_n(250)),  #NUMBER OF POINTS TO SAMPLE
           #             se=FALSE, span = .95, size = 0.2, method = "loess")
         }) +
  # geom_line(data = mdf %>% group_by(variable, interval) %>% 
  #             summarize(sd = sd(value)), aes(x = interval, y = 1-sd), color = 'black')+
  facet_grid(training~shark) +
  themeo + guides(color = F) +
  scale_x_continuous(expand=c(0,0), limits = c(0, window_max), breaks = seq(600, window_max, length.out = 5), labels = c("10","30","50","70","90")) +
  scale_y_continuous(limits = c(0.5, 1.0), breaks=seq(0.5,1.0, length.out=5)) + 
  labs(x = "Sampling interval (in mins)", y = "Predictive accuracy (as inferred from AUC)")

#null as single line
set.seed(70-1)
ggplot(data = mdf %>% filter(training == "ANNpred") %>% 
         group_by(shark, training, interval), 
       aes(x = interval, y = accuracy, color=shark)) + 
  geom_point(data = mdf %>% filter(training == "ANNpred") %>% 
               group_by(shark, training, interval) %>% sample_n(100),
             color = "light gray", alpha = 0.2) +
  lapply(1:50, # NUMBER OF LOESS
         function(i) {
           # geom_smooth(data=mdf[sample(1:nrow(mdf), 
           #                             2000),  #NUMBER OF POINTS TO SAMPLE
           #                      ], se=FALSE, span = .95, size = 0.2, method = "loess") 
           geom_smooth(data=(mdf %>% filter(training == "ANNpred") %>% 
                               group_by(shark, training, interval) %>% sample_n(4)),  #NUMBER OF POINTS TO SAMPLE
                       aes(x = interval, y = accuracy), se=FALSE, span = .90, size = 0.2, method = "loess")
           # geom_smooth(data=(mdf %>% group_by(variable) %>% sample_n(250)),  #NUMBER OF POINTS TO SAMPLE
           #             se=FALSE, span = .95, size = 0.2, method = "loess")
         }) +
  # geom_line(data = mdf %>% group_by(variable, interval) %>% 
  #             summarize(sd = sd(value)), aes(x = interval, y = 1-sd), color = 'black')+
  geom_smooth(data = mdf %>% filter(training == "null") %>% 
              group_by(shark, training, interval), aes(x = interval, y = accuracy, color = 'grey'), 
              se = FALSE, span = 0.90, size = 0.2, method = "loess") + 
  facet_wrap(~shark) +
  themeo + guides(color = F) +
  scale_x_continuous(expand=c(0,0), limits = c(0, window_max), breaks = seq(600, window_max, length.out = 5), labels = c("10","30","50","70","90")) +
  scale_y_continuous(limits = c(0.5, 1.0), breaks=seq(0.5,1.0, length.out=5)) + 
  labs(x = "Sampling interval (in mins)", y = "Predictive accuracy (as inferred from AUC)")
