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
  select(time.sec, depth, obs = ODBA.obs, pred = ODBA.pred.3hr) %>% 
  filter(!is.na(time.sec)) %>% 
  mutate(id = "fmo", shark = "Shark 2", tuning = "swapped") %>% #add fields
  mutate(obs.sc = obs/median(obs), pred.sc = pred/median(pred), obs.sc2 = obs/10, pred.sc2 = pred/10) %>% #normalize
  mutate(error = obs - pred, error.sc = obs.sc - pred.sc, error.sc2 = obs.sc2 - pred.sc2)
pr.swap <- read.csv(file.path(dd, "PR161108_ODBA_test_obs_pred_1_18_hours_SWAP.csv"), header = T) %>% 
  select(time.sec, depth, obs = ODBA.obs, pred = ODBA.pred.3hr) %>% 
  filter(!is.na(time.sec)) %>% 
  mutate(id = "pr", shark = "Shark 1", tuning = "swapped") %>% #add fields
  mutate(obs.sc = obs/median(obs), pred.sc = pred/median(pred), obs.sc2 = obs, pred.sc2 = pred) %>% #normalize
  mutate(error = obs - pred, error.sc = obs.sc - pred.sc, error.sc2 = obs.sc2 - pred.sc2)
fmo.native <- read.csv(file.path(substr(dd, 0, 47), "FinMountOrigChks_ODBA_test_obs_pred_1_17_hours.csv"), header = T) %>% 
  select(time.sec, depth, obs = ODBA.obs, pred = ODBA.pred.3hr) %>% 
  filter(!is.na(time.sec)) %>%
  mutate(id = "fmo", shark = "Shark 2", tuning = "native") %>% #add fields
  mutate(obs.sc = obs/median(obs), pred.sc = pred/median(pred), obs.sc2 = obs/10, pred.sc2 = pred/10) %>% #normalize
  mutate(error = obs - pred, error.sc = obs.sc - pred.sc, error.sc2 = obs.sc2 - pred.sc2)
pr.native <- read.csv(file.path(substr(dd, 0, 48), "PR161108_ODBA_test_obs_pred_1_18_hours.csv"), header = T) %>% 
  select(time.sec, depth, obs = ODBA.obs, pred = ODBA.pred.3hr) %>% 
  filter(!is.na(time.sec)) %>%
  mutate(id = "pr", shark = "Shark 1", tuning = "native") %>% #add fields
  mutate(obs.sc = obs/median(obs), pred.sc = pred/median(pred), obs.sc2 = obs, pred.sc2 = pred) %>% #normalize
  mutate(error = obs - pred, error.sc = obs.sc - pred.sc, error.sc2 = obs.sc2 - pred.sc2)
df <- bind_rows(fmo.swap, pr.swap, fmo.native, pr.native)

#plotting
p <- ggplot(data = df %>% select(-error, -error.sc, -obs.sc, -pred.sc, -obs.sc2, -pred.sc2) %>% 
       gather(source, odba, -time.sec, -depth, -id, -tuning)) + geom_line(aes(x = time.sec, y = odba, color = source, linetype = tuning)) + 
  facet_wrap(~id) + themeo+ geom_hline(yintercept = 0) + labs(title = "non-normalized")
q <- ggplot(data = df %>% select(-error, -error.sc, -obs, -pred, -obs.sc2, -pred.sc2) %>% 
              gather(source, odba, -time.sec, -depth, -id, -tuning)) + geom_line(aes(x = time.sec, y = odba, color = source, linetype = tuning)) + 
  facet_wrap(~id) + themeo + geom_hline(yintercept = 0) + labs(title = "normalized from median")
s <- ggplot(data = df %>% select(-error, -error.sc, -obs, -pred, -obs.sc, -pred.sc) %>% 
              gather(source, odba, -time.sec, -depth, -id, -tuning)) + geom_line(aes(x = time.sec, y = odba, color = source, linetype = tuning)) + 
  facet_wrap(~id) + themeo + geom_hline(yintercept = 0) + labs(title = "fmo/10")
grid.arrange(p, q, s)

#plotting
(p <- ggplot(data = df, aes(x = error.sc)) + #geom_histogram(aes(y = ..density.., fill = fct_rev(tuning), alpha = 0.3)) + 
  geom_density(alpha = 0.5, aes(fill = fct_rev(tuning)))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  scale_fill_manual(values = c("#e11f28", "#397fba")) + 
  scale_x_continuous(limits = c(-2,2))+
  facet_wrap(~shark) + guides(fill = F) + labs(x = "residuals", y = "") + 
  themeo)

#ts
#quick look
chk_plot <- ggplot(data = df %>% select(time.sec, depth, obs.sc, shark, id, tuning, pred.sc) %>% spread(tuning, pred.sc) %>% 
                     filter(time.sec > 79000 & time.sec < 82500), aes(x=time.sec)) +
    geom_line(aes(y=obs.sc), color = "black", size = 0.3) + 
    geom_line(aes(y=native), color = "#397fba", size = 0.3)+    #blue
    geom_line(aes(y=swapped), color = "#e11f28", size = 0.35)+  #red
    scale_y_continuous(limits = c(0, 5)) + 
    scale_x_continuous(labels = NULL)+
    labs(x = "time", y = "ODBA (standardized)", title = "black = observed, blue = native, red = generalized") + 
  themeo + facet_wrap(~shark)
#ggplotly(chk_plot)
chk_plot




#find a more dynamic section for shrk2
(chk_plot <- ggplot(data = bind_rows(df %>% #filter(shark == "Shark 1" & time.sec > 79000 & time.sec < 82500),
                                     #df %>% 
                                       filter(shark == "Shark 2" 
                                              #& time.sec > 63500 & time.sec < 63500+3500
                                              )) %>% 
                      select(time.sec, depth, obs.sc, shark, id, tuning, pred.sc) %>% 
                      spread(tuning, pred.sc),
                     aes(x=time.sec)) +
  geom_line(aes(y=obs.sc), color = "black", size = 0.3) + 
  geom_line(aes(y=native), color = "#397fba", size = 0.3)+    #blue
  geom_line(aes(y=swapped), color = "#e11f28", size = 0.35)+  #red
  scale_y_continuous(limits = c(0, 5)) + 
  scale_x_continuous(labels = NULL)+
  labs(x = "time", y = "ODBA (standardized)", title = "black = observed, blue = native, red = generalized") + 
  themeo + facet_wrap(~shark, scales = "free_x"))
ggplotly(chk_plot)

grid.arrange(chk_plot, p, ncol = 1)
