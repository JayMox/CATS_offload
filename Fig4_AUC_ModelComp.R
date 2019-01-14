#Fig 4 w/ model comparison for reviewer response
#AUC figure w/ null median only model, as well as RF, XGB, LR, SVM
#using ANNs w/ consistent hyper parameter architecture (30s window, 35 nodes, 3 layers)
#December 2018, JHMoxley

rm(list = ls())
library(ggplot2)
library(plotly)
library(DescTools)
library(dplyr)
library(reshape)
library(gridExtra)
library(grid)
library(forcats)

dd <- "/Users/jmoxley/Dropbox (MBA)/NN_processing/data/1hrTr_ModelComp_ReviewerResponse"
themeo <-theme_classic()+
  theme(strip.background = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(margin = margin( 0.2, unit = "cm")),
        axis.text.y = element_text(margin = margin(c(1, 0.2), unit = "cm")),
        axis.ticks.length=unit(-0.1, "cm"),
        panel.border = element_rect(colour = "black", fill=NA, size=.5),
        legend.title=element_blank(),
        strip.text=element_text(hjust=0) )

train <- read.csv(file.path(dd, "PR161108_ODBA_1hrTrain_obs_pred_multiple_models.csv"))
test <- read.csv(file.path(dd, "PR161108_ODBA_6hrTest_obs_pred_1hrTrain_multiple_models.csv")) %>% 
    select(time.sec, depth, obs = ODBA.obs, 
         pred.ann = ODBA.ANN.1hr, 
         pred.rf = ODBA.RF.1hr, 
         pred.xgb = ODBA.XGB.1hr,
         pred.lr = ODBA.LR.1hr) %>% 
  filter(!is.na(time.sec)) #%>%
  #mutate(id = "pr", shark = "Shark 1", tuning = "native")
gen <- read.csv(file.path(dd, "Shark2_ODBA_6hrTest_pred_with_Shark1_multiple_models.csv")) %>% 
  select(time.sec, depth, obs = ODBA.obs,
         pred.ann.gen = ODBA.ANN.1hr, 
         pred.rf.gen = ODBA.RF.1hr,
         pred.svm.gen = ODBA.SVM.1hr,
         pred.xgb.gen = ODBA.XGB.1hr,
         pred.lr.gen = ODBA.LR.1hr,
         pred.ann = ODBA.ANN_native.1hr
         ) %>% 
  filter(!is.na(time.sec))

modeled <- gen
saturation = NULL
#window_max <- 10000 # in seconds
window_max <- 5400 #just over 1.5hr
sims <- 500   # Number of sims
low_win <- seq(from = 2, to = window_max,length.out = 60 )

for(j in 4:ncol(modeled)){
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
# pr.saturation <- mutate(saturation, shark = "Shark 1")
# save(pr.saturation, file = file.path(dd, "pr.saturation.1.ModelComp.RData"))
##GENERAL CASE
#pr.saturation.gen <- mutate(saturation, shark = "Shark 1")
#save(pr.saturation.gen, file =file.path(dd, "pr.saturation.1.genModelComp.RData"))
#plotting
pr.saturation %>% 
  group_by(interval) %>% 
  summarize(ann = mean(pred.ann),
            rf = mean(pred.rf), 
            xgb = mean(pred.xgb),
            lr = mean(pred.lr)) %>% 
  gather(method, accuracy, -interval) %>% 
  ggplot() + 
  geom_point(aes(x = interval, y = accuracy, color = method))+ 
  geom_smooth(aes(x = interval, y = accuracy, color = method, group = method), se = F) + 
  themeo

mdf <- pr.saturation %>% 
  gather(method, accuracy, -interval, -shark)
set.seed(70-1)
ggplot(data = mdf %>% group_by(interval),
       aes(x = interval, y = accuracy, group=method, color=method)) + 
  geom_point(data = mdf %>% group_by(interval, method) %>% sample_n(100),
             color = "light gray", alpha = 0.2) +
  lapply(1:50, # NUMBER OF LOESS
         function(i) {
           # geom_smooth(data=mdf[sample(1:nrow(mdf), 
           #                             2000),  #NUMBER OF POINTS TO SAMPLE
           #                      ], se=FALSE, span = .95, size = 0.2, method = "loess") 
           geom_smooth(data=(mdf %>% group_by(interval) %>% sample_n(4)),  #NUMBER OF POINTS TO SAMPLE
                       aes(x = interval, y = accuracy, color = method), se=FALSE, span = .90, size = 0.2, method = "loess")
           # geom_smooth(data=(mdf %>% group_by(variable) %>% sample_n(250)),  #NUMBER OF POINTS TO SAMPLE
           #             se=FALSE, span = .95, size = 0.2, method = "loess")
         }) +
  facet_wrap(~method) +
  themeo + guides(color = F) +
  scale_x_continuous(expand=c(0,0), limits = c(0, window_max), breaks = seq(600, window_max, length.out = 5), labels = c("10","30","50","70","90")) +
  scale_y_continuous(limits = c(0.5, 1.0), breaks=seq(0.5,1.0, length.out=5)) + 
  labs(x = "Sampling interval (in mins)", y = "Predictive accuracy (as inferred from AUC)")



################
##GENERALIZATION TEST
################
#load(file.path(dd, "pr.saturation.1.genModelComp.RData"))
pr.saturation.gen %>% 
  group_by(interval) %>% 
  summarize(ann.native = mean(pred.ann),
            ann = mean(pred.ann.gen),
            rf = mean(pred.rf.gen), 
            svm = mean(pred.svm.gen),
            xgb = mean(pred.xgb.gen),
            lr = mean(pred.lr.gen)
            ) %>% 
  gather(method, accuracy, -interval) %>% 
  ggplot() + 
  geom_point(aes(x = interval, y = accuracy, color = method))+ 
  geom_smooth(aes(x = interval, y = accuracy, color = method, group = method), se = F) + 
  themeo

mdf <- pr.saturation.gen %>% 
  gather(method, accuracy, -interval, -shark)
set.seed(70-1)
ggplot(data = mdf %>% group_by(interval),
       aes(x = interval, y = accuracy, group=method, color=method)) + 
  geom_point(data = mdf %>% group_by(interval, method) %>% sample_n(100),
             color = "light gray", alpha = 0.2) +
  lapply(1:50, # NUMBER OF LOESS
         function(i) {
           # geom_smooth(data=mdf[sample(1:nrow(mdf), 
           #                             2000),  #NUMBER OF POINTS TO SAMPLE
           #                      ], se=FALSE, span = .95, size = 0.2, method = "loess") 
           geom_smooth(data=(mdf %>% group_by(interval) %>% sample_n(6)),  #NUMBER OF POINTS TO SAMPLE
                       aes(x = interval, y = accuracy, color = method), se=FALSE, span = .90, size = 0.2, method = "loess")
           # geom_smooth(data=(mdf %>% group_by(variable) %>% sample_n(250)),  #NUMBER OF POINTS TO SAMPLE
           #             se=FALSE, span = .95, size = 0.2, method = "loess")
         }) +
  facet_wrap(~fct_relevel(as.factor(method), "pred.ann", "pred.ann.gen", "pred.rf.gen", "pred.svm.gen", "pred.xgb.gen", "pred.lr.gen")) +
  themeo + guides(color = F) +
  scale_x_continuous(expand=c(0,0), limits = c(0, window_max), breaks = seq(600, window_max, length.out = 5), labels = c("10","30","50","70","90")) +
  scale_y_continuous(limits = c(0.5, 1.0), breaks=seq(0.5,1.0, length.out=5)) + 
  labs(x = "Sampling interval (in mins)", y = "Predictive accuracy (as inferred from AUC)")
