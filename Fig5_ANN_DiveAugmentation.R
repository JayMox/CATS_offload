#script for plotting simulations of deep learning shark dive behavior paper
#

library(ggplot2)
library(plotly)
library(DescTools)
library(dplyr)
library(reshape)
library(gridExtra)
library(grid)

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
load("/Users/jmoxley/Documents/GitTank/CC_CamTags/data/PR116_TestSet_sims.RData") 
colnames(pr.sim)[1] <- "interval"
data_l = pr.sim; SPAN = .9; N_size = 500; response = "metric3"; predictor = "interval"; pt_alpha = .1;
N_LOESS = 100; FRAC = 1; ymax = 1; ymin = .80;color= "dark green"; pt_color = "black"?

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
      LAND_LOESS_PR <- cbind(LAND_LOESS_DF,loess_vec)
}
    
    
LAND_longPR <- reshape2::melt(LAND_LOESS_DF, id = "mean")
# gather quantiles from data
sum_dataPR <- do.call(data.frame, aggregate(value ~ mean, data = LAND_longPR, 
                                              function(x) c(quantile(x, 0.975), quantile(x, 0.025), median(x))))
    
    
point_samplePR <- sample_frac(data_l,FRAC)
xpPR <- point_sample[,predictor]
ypPR <- point_sample[,response]

LOW_PLOT_PR <<-ggplot()+
      geom_point(aes(x=xpPR,y=ypPR), alpha = pt_alpha ,size = 0.02, color = pt_color) +
      geom_ribbon(data = sum_dataPR, aes(x=mean,ymax=value.97.5.,ymin = value.2.5.),  
                  size = 0, 
                  # color = color, 
                  fill = color, 
                  alpha = .5) +
      geom_line(data = sum_dataPR, aes(x=mean,y=value.V3),       size = 1, color = "black")
+ # Median
      #geom_line(data = sum_data, aes(x=mean,y=value.97.5.), size = 0.3, color = color)+ # Upper
      #geom_line(data = sum_data, aes(x=mean,y=value.2.5.),  size = 0.3, color = color)+ # Lower
      #annotation_custom( grobTree( textGrob(predictor, x = 0.1, y = 0.9, hjust = 0, gp = gpar(col = "black", fontsize = 6 ) ) ) )+
      #scale_x_continuous(expand = c(0.000, 0.000), breaks = c(.25,.5,.75))+
      #scale_y_continuous(expand = c(0.000, 0.000), breaks = c(0.2,0.4)) +
      #coord_cartesian(ylim = c(c(0,0.75)))+
      #xlab(NULL)+
      #ylab(NULL)+
      #themeo + 
      #theme(axis.text.x = element_blank(),
      #      axis.text.y=element_blank(),
      #     plot.margin=unit(c(4,4,4,4), "pt"))
print(LOW_PLOT_PR)

##########
##With FINMOUNTORIG
###########
load("/Users/jmoxley/Documents/GitTank/CC_CamTags/data/FinMountOrig_TestSet_sims.RData")
colnames(fmo.sim)[1] <- "interval"
data_l = fmo.sim; SPAN = .9; N_size = 500; response = "metric3"; predictor = "interval"; pt_alpha = .1;
N_LOESS = 100; FRAC = 1; ymax = 1; ymin = .80;color= "red"; pt_color = "black"?


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


point_sampleFMO <- sample_frac(data_l,FRAC)
xpFMO <- point_sample[,predictor]
ypFMO <- point_sample[,response]

LOW_PLOT_FMO <<-ggplot()+
  geom_point(aes(x=xpFMO,y=ypFMO), alpha = pt_alpha ,size = 0.02, color = pt_color) +
  geom_ribbon(data = sum_dataFMO, aes(x=mean,ymax=value.97.5.,ymin = value.2.5.),  
              size = 0, 
              # color = color, 
              fill = color, 
              alpha = .5) +
  geom_line(data = sum_dataFMO, aes(x=mean,y=value.V3),       size = 1, color = "black")
print(c(LOW_PLOT_FMO, LOW_PLOT_PR))
+ # Median
  #geom_line(data = sum_data, aes(x=mean,y=value.97.5.), size = 0.3, color = color)+ # Upper
  #geom_line(data = sum_data, aes(x=mean,y=value.2.5.),  size = 0.3, color = color)+ # Lower
  #annotation_custom( grobTree( textGrob(predictor, x = 0.1, y = 0.9, hjust = 0, gp = gpar(col = "black", fontsize = 6 ) ) ) )+
  #scale_x_continuous(expand = c(0.000, 0.000), breaks = c(.25,.5,.75))+
  #scale_y_continuous(expand = c(0.000, 0.000), breaks = c(0.2,0.4)) +
  #coord_cartesian(ylim = c(c(0,0.75)))+
  #xlab(NULL)+
  #ylab(NULL)+
  #themeo + 
  #theme(axis.text.x = element_blank(),
  #      axis.text.y=element_blank(),
#     plot.margin=unit(c(4,4,4,4), "pt"))
print(LOW_PLOT)
