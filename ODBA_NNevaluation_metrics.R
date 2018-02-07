# TG wrote initial scripts/plots
library(ggplot2)
library(plotly)
library(DescTools)
library(dplyr)
library(reshape)
library(gridExtra)
library(grid)
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

##### Functions #####
quantile_trim <-  # quantile truncation function
  function(df, varname){                                               # trim quantiles function 
    
    high_tail <- 0.975
    low_tail  <- 0.0275
    
    upper <- quantile(df[,varname],  c(high_tail)) %>% as.numeric()
    lower <- quantile(df[,varname],  c(low_tail))  %>% as.numeric()
    
    #df <- transmute(df[,varname] = replace(df[,varname], df[,varname] < lower, lower))
    #df <- transmute(df[,varname] = replace(df[,varname], df[,varname] > upper, upper))
    
    df[,varname] <- ifelse(df[,varname] > lower, df[,varname], lower)
    df[,varname] <- ifelse(df[,varname] < upper, df[,varname], upper)
    #df <-  df[ (lower < df[varname] & df[varname] < upper), ]
    return(df)
    #str(df)
  }
lseq <-           # logarithmic sequence function
  function(from=1, to=100000, length.out=6) {
  # logarithmic spaced sequence
  # blatantly stolen from library("emdbook"), because need only this
  exp(seq(log(from), log(to), length.out = length.out))}
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


### Read in pred/observed dataset on test set
modeled <- read.csv('data/PR116_pred_results_18hrtrain.csv', header = T, stringsAsFactors = F)
#modeled <- read.csv('data/Original_pred_results_18hrtrain.csv', header = T, stringsAsFactors = F)

# quick interactive time series plot
basic_plot <- ggplot(modeled, aes(x = time.sec))+
  geom_line(aes(y=observed), color = "red", size = 0.2)+
  geom_line(aes(y=predicted), color = "blue", size = 0.2)+themeo
ggplotly(basic_plot)



# logarithmic window sequence
#low_win <- lseq(from = 1, to = 10000, length = 50)
# conventional linear window sequence

window_max <- 10000 # in seconds

low_win <- seq(from = 1, to = window_max,length.out = 50 )

# blank dataframe to stack simulations in
metric_df_sim <- NULL

# Number of sims
sims <- 100

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
  
# trim off 95% tails
# metric_df_sim <- quantile_trim(metric_df_sim, "metric2")  
  
str(metric_df_sim)
#fmo_eval <- metric_df_sim


# plot on reg scale - metric 2
  ggplot(metric_df_sim,aes(x=window))+
    geom_point(aes(y=metric2), size = .2) #+
    #coord_cartesian(ylim = c(.85,1)) + themeo

# plot on log scale - metric 2
  ggplot(metric_df_sim,aes(x=window))+
    scale_x_log10()+
    geom_point(aes(y=metric2), size = .2)+
    coord_cartesian(ylim = c(.65,1))+
    annotation_logticks()+
    geom_smooth(aes(y=metric2)) + themeo

 
  MANY_LOESS(data_l = metric_df_sim
             , SPAN = .9
             , N_size = 500
             , response = "metric3"
             , predictor = "window"
             , pt_alpha = .1
             , N_LOESS = 100
             , FRAC = 1
             , ymax = 1
             , ymin = .80
             , color= "red"
             , pt_color = "black") 
  

  
   
# metric 1 
  
  
  ggplot(metric_df_sim,aes(x=window))+
    geom_point(aes(y=metric1)) 
  ggplot(metric_df_sim,aes(x=window))+
    geom_point(aes(y=metric1)) 
  
  MANY_LOESS(data_l = metric_df_sim
             , SPAN = .95
             , N_size = 500
             , response = "metric1"
             , predictor = "window"
             , pt_alpha = .1
             , N_LOESS = 100
             , FRAC = 0.8
             , ymax = 1
             , ymin = .9
             , color= "red"
             , pt_color = "black") 
  
  
  # metric 3
  
  
  ggplot(metric_df_sim,aes(x=window))+
    geom_point(aes(y=metric3)) 
  ggplot(metric_df_sim,aes(x=window))+
    geom_point(aes(y=metric3)) 
  
  MANY_LOESS(data_l = metric_df_sim
             , SPAN = .95
             , N_size = 500
             , response = "metric3"
             , predictor = "window"
             , pt_alpha = .1
             , N_LOESS = 100
             , FRAC = 0.8
             , ymax = 1
             , ymin = .5
             , color= "red"
             , pt_color = "black") 
  
  MANY_LOESS(data_l = metric_df_sim
             , SPAN = .95
             , N_size = 500
             , response = "metric1"
             , predictor = "window"
             , pt_alpha = .1
             , N_LOESS = 100
             , FRAC = 0.8
             , ymax = 1
             , ymin = .5
             , color= "red"
             , pt_color = "black") 
  
  MANY_LOESS(data_l = metric_df_sim
             , SPAN = .95
             , N_size = 500
             , response = "metric2"
             , predictor = "window"
             , pt_alpha = .1
             , N_LOESS = 100
             , FRAC = 0.8
             , ymax = 1
             , ymin = .5
             , color= "red"
             , pt_color = "black") 

  

