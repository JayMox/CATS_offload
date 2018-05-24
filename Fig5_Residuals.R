#fig of dist of residuals in native/generalized models
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
dd <- "/Users/jmoxley/Dropbox (MBA)/NN_processing/data/ANN_swap"
#get data
fmo.swap <- read.csv(file.path(dd, "FinMountOrigChks_ODBA_test_obs_pred_1_17_hours_SWAP.csv"), header = T) %>% 
  select(time.sec, depth, observed = ODBA.obs, ODBA.pred.3hr) %>% 
  filter(!is.na(time.sec)) %>% 
  mutate(id = "FMO", tuning = "swapped", error = observed - ODBA.pred.3hr)
pr.swap <- read.csv(file.path(dd, "PR161108_ODBA_test_obs_pred_1_18_hours_SWAP.csv"), header = T) %>% 
  select(time.sec, depth, observed = ODBA.obs, ODBA.pred.3hr) %>% 
  filter(!is.na(time.sec)) %>% 
  mutate(id = "PR", tuning = "swapped", error = observed - ODBA.pred.3hr)
fmo.native <- read.csv(file.path(substr(dd, 0, 47), "FinMountOrigChks_ODBA_test_obs_pred_1_17_hours.csv"), header = T) %>% 
  select(time.sec, depth, observed = ODBA.obs, ODBA.pred.3hr) %>% 
  filter(!is.na(time.sec)) %>% 
  mutate(id = "FMO", tuning = "native", error = observed - ODBA.pred.3hr) 
pr.native <- read.csv(file.path(substr(dd, 0, 48), "PR161108_ODBA_test_obs_pred_1_18_hours.csv"), header = T) %>% 
  select(time.sec, depth, observed = ODBA.obs, ODBA.pred.3hr) %>% 
  filter(!is.na(time.sec)) %>% 
  mutate(id = "PR", tuning = "native", error = observed - ODBA.pred.3hr)
df <- bind_rows(fmo.swap, pr.swap, fmo.native, pr.native)

#plotting
p <- ggplot(data = df, aes(x = error)) + #geom_histogram(aes(y = ..density.., fill = fct_rev(tuning), alpha = 0.3)) + 
  geom_density(alpha = 0.5, aes(fill = fct_rev(tuning)))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  facet_wrap(~id, scales = "free") + 
  themeo

#ts
#quick look
chk_plot <- ggplot(data = df %>% select(-error) %>% spread(tuning, ODBA.pred.3hr), aes(x=time.sec)) +
    geom_line(aes(y=observed), color = "black", size = 0.2) + 
    geom_line(aes(y=native), color = "#397fba", size = 0.2)+
    geom_line(aes(y=swapped), color = "#e11f28", size = 0.2)+
    labs(title = "green = observed, red = native, blue = generalized") + themeo + facet_wrap(~id, scales = "free")
ggplotly(chk_plot)
