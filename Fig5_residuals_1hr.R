#Update to generalization plot swapping ANNs to be tested on other dataset
#June2018, JHMoxley
rm(list = ls())
library(ggplot2)
library(plotly)
library(DescTools)
library(dplyr)
library(tidyverse)
library(reshape2)
library(gridExtra)
library(grid)
themeo <-theme_classic()+
  theme(strip.background = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(margin = margin( 0.2, unit = "cm")),
        axis.text.y = element_text(margin = margin(c(1, 0.2), unit = "cm")),
        axis.ticks.length=unit(-0.1, "cm"),
        panel.border = element_rect(colour = "black", fill=NA, size=.5),
        legend.title=element_blank(),
        strip.text=element_text(hjust=0) )

#set drive & get data
dd <- "/Users/jmoxley/Dropbox (MBA)/NN_processing/data/1hr_Prediction_testset_June18"
#get data
fmo.swap <- read.csv(file.path(dd, "Shark2_ODBA_6hrTest_pred_with_Shark1ANN.csv"), header = T) %>% 
  filter(!is.na(time.sec)) %>% 
  mutate(id = "fmo", shark = "Shark 2", tuning = "swapped", train = "1hr") %>% #add fields
  dplyr::rename(obs = ODBA.obs, pred = ODBA.train.1hr) %>% 
  #mutate(obs.sc = obs/median(obs), pred.sc = pred/median(pred), obs.sc2 = obs/10, pred.sc2 = pred/10) %>% #normalize
  mutate(error = obs - pred, obs.sc = obs/median(obs), pred.sc = pred/median(pred), error.sc = obs.sc - pred.sc) #, error.sc = obs.sc - pred.sc, error.sc2 = obs.sc2 - pred.sc2)
pr.swap <- read.csv(file.path(dd, "Shark1_ODBA_6hrTest_pred_with_Shark2ANN.csv"), header = T) %>% 
  filter(!is.na(time.sec)) %>% 
  mutate(id = "pr", shark = "Shark 1", tuning = "swapped", train = "1hr") %>% #add fields
  dplyr::rename(obs = ODBA.obs, pred = ODBA.train.1h) %>% 
  #mutate(obs.sc = obs/median(obs), pred.sc = pred/median(pred), obs.sc2 = obs, pred.sc2 = pred) %>% #normalize
  mutate(error = obs - pred, obs.sc = obs/median(obs), pred.sc = pred/median(pred), error.sc = obs.sc - pred.sc) #, error.sc = obs.sc - pred.sc, error.sc2 = obs.sc2 - pred.sc2)
fmo.native <- read.csv(file.path(dd, "Shark2_ODBA_test_6_hours_pred_1_18_hours.csv"), header = T) %>% 
  select(time.sec, depth, obs = ODBA.obs, pred = ODBA.pred.1hr) %>% 
  filter(!is.na(time.sec)) %>%
  mutate(id = "fmo", shark = "Shark 2", tuning = "native", train = "1hr") %>% #add fields
  #mutate(obs.sc = obs/median(obs), pred.sc = pred/median(pred), obs.sc2 = obs/10, pred.sc2 = pred/10) %>% #normalize
  mutate(error = obs - pred, obs.sc = obs/median(obs), pred.sc = pred/median(pred), error.sc = obs.sc - pred.sc)  #, error.sc = obs.sc - pred.sc, error.sc2 = obs.sc2 - pred.sc2)
pr.native <- read.csv(file.path(dd, "Shark1_ODBA_test_6_hours_pred_1_18_hours.csv"), header = T) %>% 
  select(time.sec, depth, obs = ODBA.obs, pred = ODBA.pred.1hr) %>% 
  filter(!is.na(time.sec)) %>%
  mutate(id = "pr", shark = "Shark 1", tuning = "native") %>% #add fields
  #mutate(obs.sc = obs/median(obs), pred.sc = pred/median(pred), obs.sc2 = obs, pred.sc2 = pred) %>% #normalize
  mutate(error = obs - pred, obs.sc = obs/median(obs), pred.sc = pred/median(pred), error.sc = obs.sc - pred.sc) #, error.sc = obs.sc - pred.sc, error.sc2 = obs.sc2 - pred.sc2)
df <- bind_rows(fmo.swap, pr.swap, fmo.native, pr.native)


##r2 for native vs. generalized prediction
pr.test <- data.frame(depth = pr.native$depth, 
                            native = pr.native$pred, 
                            swap = pr.swap$pred)
(r2 <- 1 - (sum((pr.test$native-pr.test$swap )^2)/
             sum((pr.test$native-mean(pr.test$native))^2)))

fmo.test <- data.frame(depth = fmo.native$depth, 
                             native = fmo.native$pred, 
                             swap = fmo.swap$pred)
(r2 <- 1 - (sum((fmo.test$native-fmo.test$swap)^2)/
              sum((fmo.test$native-mean(fmo.test$native))^2)))

#correlation test
cor.test(pr.test$native, pr.test$swap, method = "pearson")
cor.test(fmo.test$native, fmo.test$swap, method = "pearson")

###
#plot
###
#plotting
(p <- ggplot(data = df, aes(x = error.sc)) + #geom_histogram(aes(y = ..density.., fill = fct_rev(tuning), alpha = 0.3)) + 
    geom_density(alpha = 0.5, aes(fill = fct_rev(tuning)))+
    geom_vline(xintercept = 0, linetype = "dashed")+
    scale_fill_manual(values = c("#e11f28", "#397fba")) + 
    scale_x_continuous(limits = c(-2,2))+
    #scale_x_continuous(limits = c(-2,2))+
    facet_wrap(~shark) + guides(fill = F) + labs(x = "residuals", y = "") + 
    themeo)
#trace plots
chk_plot <- ggplot(data = df %>% select(time.sec, depth, obs = obs.sc, shark, id, tuning, pred = pred.sc) %>% 
                     spread(tuning, pred) %>% 
                     filter(time.sec > 79000 & time.sec < 82500), aes(x=time.sec)) +
  geom_line(aes(y=obs), color = "black", size = 0.3) + 
  geom_line(aes(y=native), color = "#397fba", size = 0.3)+    #blue
  geom_line(aes(y=swapped), color = "#e11f28", size = 0.35)+  #red
  scale_x_continuous(breaks = seq(79000,82500,length.out=5), labels = c(0,15,30,45,60))+
  labs(x = "time (m)", y = "ODBA", title = "black = observed, blue = native, red = generalized") + 
  themeo + facet_wrap(~shark)
ggplotly(chk_plot)

grid.arrange(chk_plot, p, ncol = 1)

#check ts traces for 1 & 3 hr training in PR data set
pr <- read.csv(file.path(dd, "Shark1_ODBA_test_6_hours_pred_1_18_hours.csv"), header = T) %>% 
  select(time.sec, depth, obs = ODBA.obs, 
         pred.1 = ODBA.pred.1hr, 
         pred.2 = ODBA.pred.2hr,
         pred.3 = ODBA.pred.3hr) %>% 
  filter(!is.na(time.sec)) %>%
  mutate(id = "pr", shark = "Shark 1", tuning = "native") %>% 
  mutate(error.1 = obs - pred.1, error.3 = obs - pred.3) %>% 
  #filter(time.sec > 79000 & time.sec < 82500) %>% 
  ggplot(aes(x = time.sec)) + 
  geom_line(aes(y=obs), color = "black", size = 0.3) + 
  geom_line(aes(y=pred.1), color = "#397fba", size = 0.3)+    #blue
  #geom_line(aes(y=pred.2), color = "purple", size = 0.35)+  #green
  geom_line(aes(y=pred.3), color = "#e11f28", size = 0.35)+  #red
  scale_x_continuous(labels = NULL)+
  labs(x = "time", y = "ODBA", 
       title = "black = observed, pred.1hr = blue, pred.whr = green, pred.3hr = red") + 
  themeo
ggplotly(pr)
