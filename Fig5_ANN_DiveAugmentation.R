#script for plotting simulations of deep learning shark dive behavior paper
#
rm(list = ls())
library(ggplot2)
library(plotly)
library(DescTools)
library(dplyr)
library(reshape)
library(gridExtra)
library(grid)

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
load(file.path(dd, "Pr116_3hrTestSet_AUCaccuracy.RData")) 
colnames(pr.acc)[1] <- "interval"
data_l = pr.acc; SPAN = .9; N_size = 500; response = "metric3"; predictor = "interval"; pt_alpha = .1;
N_LOESS = 500; FRAC = 1; ymax = 1; ymin = .80;color= "dark green"; pt_color = "black";

LAND_LOESS_PR <- data.frame(mean = seq(min(data_l[,predictor]), max(data_l[,predictor]), length.out = 500))
#Simulation
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
      LAND_LOESS_PR <- cbind(LAND_LOESS_PR,loess_vec)
}
    
    
LAND_longPR <- reshape2::melt(LAND_LOESS_PR, id = "mean")
# gather quantiles from data
sum_dataPR <- do.call(data.frame, aggregate(value ~ mean, data = LAND_longPR, 
                                              function(x) c(quantile(x, 0.975), quantile(x, 0.025), median(x))))
#build df 
point_samplePR <- sample_frac(data_l,FRAC)
xpPR <- point_samplePR[,predictor]
ypPR <- point_samplePR[,response]
#get quantiles to create data boundaries for plot
boundsPR <- point_samplePR %>% group_by(interval) %>% 
  summarize(upper = quantile(metric3, probs = .975, na.rm=T), 
            lower = quantile(metric3, probs = 0.025, na.rm=T))

##########
##With FINMOUNTORIG
###########
load(file.path(dd, "FMO_3hrTestSet_AUCaccuracy.Rdata"))
colnames(fmo.acc)[1] <- "interval"
data_l = fmo.acc; SPAN = .9; N_size = 500; response = "metric3"; predictor = "interval"; pt_alpha = .1;
N_LOESS = 500; FRAC = 1; ymax = 1; ymin = .80;color= "red"; pt_color = "black";

LAND_LOESS_FMO <- data.frame(mean = seq(min(data_l[,predictor]), max(data_l[,predictor]), length.out = 500))
#Simulation
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
  LAND_LOESS_FMO <- cbind(LAND_LOESS_FMO,loess_vec)
}
LAND_longFMO <- reshape2::melt(LAND_LOESS_FMO, id = "mean")
# gather quantiles from data
sum_dataFMO <- do.call(data.frame, aggregate(value ~ mean, data = LAND_longFMO, 
                                            function(x) c(quantile(x, 0.975), quantile(x, 0.025), median(x))))

#build df
point_sampleFMO <- sample_frac(data_l,FRAC)
xpFMO <- point_sampleFMO[,predictor]
ypFMO <- point_sampleFMO[,response]

#get quantiles to create data boundaries for plot
boundsFMO <- point_sampleFMO %>% group_by(interval) %>% 
  summarize(upper = quantile(metric3, probs = .975, na.rm=T), 
            lower = quantile(metric3, probs = 0.025, na.rm=T))


#PLOT IN COMBINATION
ymin = .7
ggplot()+
  #data set 1
  geom_point(aes(x=xpPR,y=ypPR), alpha = .2 ,size = 0.02, color = pt_color)+
  geom_smooth(aes(x=xpPR,y=ypPR), method="loess")+themeo

ggplot()+
  #data set 1
  geom_point(aes(x=xpPR,y=ypPR), alpha = .2 ,size = 0.02, color = pt_color) +
  geom_ribbon(data = sum_dataPR, aes(x=mean,ymax=value.97.5.,ymin = value.2.5.),  
              size = 0, 
              # color = color, 
              fill = "orange", 
              alpha = .6) +
  geom_line(data = sum_dataPR, aes(x=mean,y=value.V3), size = 1, color = "black") +
  #add bounds
  stat_smooth(data = boundsPR, aes(x = interval, y = upper), color = 'orange', 
              alpha = .5, method = "loess", se = F) + 
  stat_smooth(data = boundsPR, aes(x = interval, y = lower), color = 'orange', 
              alpha = .5, method = "loess", se = F) +
  #data set 2
  #geom_point(aes(x=xpFMO,y=ypFMO), alpha = .2 ,size = 0.02, color = pt_color) +
  #geom_line(aes(x = , y = ), alpha = .2, linteype = "solid", color = "gray") +
  geom_ribbon(data = sum_dataFMO, aes(x=mean,ymax=value.97.5.,ymin = value.2.5.),
              size = 0,
              # color = color,
              fill = ("sky blue"),
              alpha = .5) +
  geom_line(data = sum_dataFMO, aes(x=mean,y=value.V3), size = .8, color = "black") +
  #add bounds
  #geom_line(data = boundsFMO, aes(x = interval, y = upper), alpha = 0.5) + 
  stat_smooth(data = boundsFMO, aes(x = interval, y = upper), color = 'sky blue', 
              alpha = .1, method = "loess", se = F) + 
  stat_smooth(data = boundsFMO, aes(x = interval, y = lower), color = 'sky blue', 
              alpha = .5, method = "loess", se = F) +
  
  scale_y_continuous(limits=c(0.5, 1), expand = c(0.01,0.01)) +
  scale_x_continuous(expand = c(0.01,0.01), breaks = c(0, 2500, 5000, 7500, 10000), labels = c(0, 25, 50, 75, 100)) +
  xlab("AUC Interval (sec x 10^2)") +
  ylab("Accuracy (inferred via error %age)") +
  
  themeo
ggsave(file = "goodness_of_fit_fig.png", path = dd)
