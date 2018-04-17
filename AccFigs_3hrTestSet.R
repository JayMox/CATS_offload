##Fig4 Mar2018 updates
##uses 3 hr training set, on 6 hr test set, extracted from the sensitivity analysis
rm(list = ls())
library(ggplot2)
library(plotly)
library(DescTools)
library(dplyr)
library(reshape)
library(gridExtra)
library(grid)
library(forcats)

dd <- "/Users/jmoxley/Documents/GitTank/CC_CamTags/data/"

##############################
###  Popular ggPlot theme  ###
##############################
themeo <-theme_classic()+
  theme(strip.background = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(margin = margin( 0.2, unit = "cm")),
        axis.text.y = element_text(margin = margin(c(1, 0.2), unit = "cm")),
        axis.ticks.length=unit(-0.1, "cm"),
        panel.border = element_rect(colour = "black", fill=NA, size=.5),
        legend.title=element_blank(),
        strip.text=element_text(hjust=0) )
#TG Plots:
MANY_LOESS <-          
  function(data_l,SPAN,N_size,response,predictor,pt_alpha,N_LOESS, FRAC,ymax,ymin, color, pt_color){
    LAND_LOESS_DF <- data.frame(mean = seq(min(data_l[,predictor]), max(data_l[,predictor]), length.out = 500))
    
    for(i in 1:N_LOESS){
      # sample 1000 points
      LAND_sample <- data_l %>% sample_n(N_size)
      # fit a loess
      xx <- LAND_sample[,predictor]
      yy <- LAND_sample[,response]
      tp_est <- loess(yy ~ xx , span = SPAN) 
      # predict accross range of x using loess model
      loess_vec <- data.frame(predict(tp_est, newdata = data.frame(xx = seq(min(data_l[,predictor]), max(data_l[,predictor]), length.out = 500)) ))
      colnames(loess_vec) <- as.character(i)
      # repeat x times
      LAND_LOESS_DF <- cbind(LAND_LOESS_DF,loess_vec)
    }
    
    LAND_long <- reshape2::melt(LAND_LOESS_DF, id = "mean")
    point_sample <- sample_frac(data_l,FRAC)
    xp <- point_sample[,predictor]
    yp <- point_sample[,response]
    LOW_PLOT <<- ggplot()+
      geom_point(aes(x=xp,y=yp), alpha = pt_alpha ,size = 1, color = pt_color) +
      geom_line(data = LAND_long,aes(x=mean,y=value,group=variable),size = 0.1, color = color)+ 
      #annotation_custom( grobTree( textGrob(predictor, x = 0.1, y = 0.9, hjust = 0, gp = gpar(col = "black", fontsize = 6 ) ) ) )+
      scale_y_continuous(limits = c(ymin,ymax))+
      scale_x_continuous() +
      xlab(predictor)+
      ylab(response)+
      themeo #+ 
    #theme(axis.text.x = element_blank(),
    #      axis.text.y=element_blank(),
    #      plot.margin=unit(c(0,0,0,0), "null"))
    print(LOW_PLOT)
  }
MANY_LOESS_CV_MAR <- 
  function(data_l,SPAN,N_size,response,predictor,pt_alpha,N_LOESS, FRAC,ymax, color, pt_color){
    LAND_LOESS_DF <- data.frame(mean = seq(min(data_l[,predictor]), max(data_l[,predictor]), length.out = 500))
    
    for(i in 1:N_LOESS){
      # sample 1000 points
      LAND_sample <- data_l %>% sample_n(N_size)
      # fit a loess
      xx <- LAND_sample[,predictor]
      yy <- LAND_sample[,response]
      tp_est <- loess(yy ~ xx , span = SPAN) 
      
      # predict accross range of x using loess model
      loess_vec <- data.frame(predict(tp_est, newdata = data.frame(xx = seq(min(data_l[,predictor]), max(data_l[,predictor]), length.out = 500)) ))
      colnames(loess_vec) <- as.character(i)
      # repeat x times
      LAND_LOESS_DF <- cbind(LAND_LOESS_DF,loess_vec)
    }
    
    LAND_long <- melt(LAND_LOESS_DF, id = "mean")
    
    # gather quantiles from data
    sum_data <- do.call(data.frame, aggregate(value ~ mean, data = LAND_long, 
                                              function(x) c(quantile(x, 0.975), quantile(x, 0.025), median(x))))
    # build plots
    
    point_sample <- sample_n(data_l,FRAC)
    xp <- point_sample[,predictor]
    yp <- point_sample[,response]
    
    LOW_PLOT <<-ggplot()+
      geom_point(aes(x=xp,y=yp), alpha = pt_alpha ,size = 0.02, color = pt_color) +
      geom_ribbon(data = sum_data, aes(x=mean,ymax=value.97.5.,ymin = value.2.5.),  
                  size = 0, 
                  # color = color, 
                  fill = color, 
                  alpha = .5) +
      geom_line(data = sum_data, aes(x=mean,y=value.V3),       size = 1, color = color)+ # Median
      #geom_line(data = sum_data, aes(x=mean,y=value.97.5.), size = 0.3, color = color)+ # Upper
      #geom_line(data = sum_data, aes(x=mean,y=value.2.5.),  size = 0.3, color = color)+ # Lower
      annotation_custom( grobTree( textGrob(predictor, x = 0.1, y = 0.9, hjust = 0, gp = gpar(col = "black", fontsize = 6 ) ) ) )+
      scale_x_continuous(expand = c(0.000, 0.000), breaks = c(.25,.5,.75))+
      scale_y_continuous(expand = c(0.000, 0.000), breaks = c(0.2,0.4)) +
      coord_cartesian(ylim = c(c(0,0.75)))+
      xlab(NULL)+
      ylab(NULL)+
      themeo + 
      theme(axis.text.x = element_blank(),
            axis.text.y=element_blank(),
            plot.margin=unit(c(4,4,4,4), "pt"))
    print(LOW_PLOT)
  }

##PRETTY GROSS BUT WORKS
#############
#build for PR
#############
pr <- read.csv(file.path(dd, "PR161108_ODBA_test_obs_pred_3_hours.csv"), header = T, stringsAsFactors = F) %>%
   dplyr::rename(observed = ODBA.obs) %>% 
   filter(!is.na(observed)) %>% 
   select(time.sec, depth, observed, predicted = ODBA.train.3hr.NEW)
fmo <- read.csv(file.path(dd, "FinMountOrigChks_ODBA_test_obs_pred_3_hours.csv"), header = T, stringsAsFactors = F) %>%
  dplyr::rename(observed = ODBA.obs) %>% 
  filter(!is.na(observed)) %>% 
  select(time.sec, depth, observed, predicted = ODBA.train.3hr.NEW)

#modeled <- modeled[9201:nrow(modeled),]
#params
#data storage
modeled <- pr
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
#qplot
# saturation <- saturation %>% dplyr::rename("Old" = "ODBA.train.3hr.OLD",
#                                            "New" = "ODBA.train.3hr.NEW",
#                                            "New2" = "ODBA.train.3hr.NEW2",
#                                            "New3" = "ODBA.train.3hr.NEW3")
#pr.saturation <- saturation
#save(pr.saturation, file = file.path(dd, "pr_saturation_20180403.RData"))
#fmo.saturation <- saturation
#save(fmo.saturation, file = file.path(dd, "fmo_saturation_20180403.RData"))



#########################
#####Plot up side by side
#########################
load(file.path(dd, "pr_saturation_20180403.RData")); 
pr.saturation <- pr.saturation %>% mutate(id = "pr116")
load(file.path(dd, "fmo_saturation_20180403.RData"))
fmo.saturation <- fmo.saturation %>% mutate(id = "fmo")
saturation <- merge(bind_rows(pr.saturation, fmo.saturation), 
                    data.frame(id = c('pr116', 'fmo'), labels = factor(c('Shark 1', 'Shark 2'))), 
                    by = 'id'); summary(saturation)
mdf <- melt(saturation, id.vars = c("id", "interval", "labels")) %>% group_by(id, variable, interval)
ggplot(mdf %>% group_by(id), aes(x = interval, y = value, group=id, color=id)) + 
  geom_point(color = "light gray", alpha = 0.2) +
  lapply(1:50, # NUMBER OF LOESS
         function(i) {
           # geom_smooth(data=mdf[sample(1:nrow(mdf), 
           #                             2000),  #NUMBER OF POINTS TO SAMPLE
           #                      ], se=FALSE, span = .95, size = 0.2, method = "loess")
            geom_smooth(data=(mdf %>% group_by(id, variable, interval) %>% sample_n(5)),  #NUMBER OF POINTS TO SAMPLE
                        se=FALSE, span = .95, size = 0.2, method = "loess")
           # geom_smooth(data=(mdf %>% group_by(variable) %>% sample_n(250)),  #NUMBER OF POINTS TO SAMPLE
           #             se=FALSE, span = .95, size = 0.2, method = "loess")
         }) +
  # geom_line(data = mdf %>% group_by(variable, interval) %>% 
  #             summarize(sd = sd(value)), aes(x = interval, y = 1-sd), color = 'black')+
  facet_wrap(~labels) +
  themeo + guides(color = F) +
  scale_x_continuous(expand=c(0,0), limits = c(0, window_max), breaks = seq(600, window_max, length.out = 5), labels = c("10","30","50","70","90")) +
  scale_y_continuous(limits = c(0.5, 1.0), breaks=seq(0.5,1.0, length.out=5)) + 
  labs(x = "Sampling interval (in mins)", y = "Predictive accuracy (as inferred from AUC)")
  

#plot just the bounds
(bounds <- saturation %>% dplyr::group_by(id, interval) %>% 
  dplyr::summarize(mean = mean(predicted), median = median(predicted), low = quantile(predicted, probs = 0.025),
            up = quantile(predicted, probs = 0.975), n = n(), std.err = mean/sqrt(n))) 
mbounds <- gather(bounds %>% select(-median), moment, value,-id, -interval, -n, - std.err, 3:5)
ggplot(data =  gather(bounds %>% select(-median, -n, -std.err), moment, value,-id, -interval, 3:5)) + 
  geom_line(aes(x = interval, y = value, group = moment, color = moment)) + facet_wrap(~id)
  #geom_smooth(aes(x = interval, y = value, group = moment, color = moment), se = F, span = 0.9) + 
  #geom_smooth(data = mbounds %>% filter(moment == "mean"), aes(x = interval, y = mean), se = F)+
  geom_ribbon(data = mbounds %>% filter(moment == "mean"), aes(x=interval, ymin = value - std.err, ymax = value + std.err), stat = 'smooth')+
  facet_wrap(~id) + themeo



geom_ribbon(data = sum_dataPR, aes(x=mean,ymax=value.97.5.,ymin = value.2.5.),  
            size = 0, 
            # color = color, 
            fill = "orange", 
            alpha = .6) +b
  geom_line(data = sum_dataPR, aes(x=mean,y=value.V3), size = 1, color = "black") +
  #add bounds
  stat_smooth(data = boundsPR, aes(x = interval, y = upper), color = 'orange', 
              alpha = .5, method = "loess", se = F) + 
  stat_smooth(data = boundsPR, aes(x = interval, y = lower), color = 'orange', 
              alpha = .5, method = "loess", se = F) +
%>% melt(id.==s=c("id", "interval"))

#think about it more as accuracy (y intercepts) vs. precision (variability in y-intercepts
#across sampling intervals); mean accuracy is relatively invariant in the model, but the
#spread in that dataset shrinks as you integrate over longer scales







