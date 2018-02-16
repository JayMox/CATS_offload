#development of code to handle multiple curves of saturation
rm(list = ls())
library(ggplot2)
library(plotly)
library(DescTools)
library(dplyr)
library(tidyverse)
library(reshape)
library(gridExtra)
library(grid)

#set drive & get data
dd <- "/Users/jmoxley/Documents/GitTank/CC_CamTags/data/"
load(file.path(dd, "saturation_mini.RData"))
satu <- saturation


load(file.path(dd, "FinMountOrig_TestSet_sims.RData"))
colnames(fmo.sim)[1] <- "interval"
data_l = fmo.sim; SPAN = .9; N_size = 500; response = "metric3"; predictor = "interval"; pt_alpha = .1;
N_LOESS = 500; FRAC = 1; ymax = 1; ymin = .80;color= "red"; pt_color = "black";


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