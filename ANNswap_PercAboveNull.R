#script to calc the number of estimates above the null model for generalization
rm(list = ls())
library(dplyr)

dd <- "/Users/jmoxley/Dropbox (MBA)/NN_processing/data/1hr_Prediction_testset_June18"
 load(file = file.path(dd, "fmo_saturate.1.3.12.RData")) 
 load(file = file.path(dd, "pr.saturation.1.3.12.RData"))
 load(file.path(dd, "pr.saturation.null.RData"))
 load(file.path(dd, "fmo_saturate.null.RData"))
fmo.native <- fmo.saturation %>% 
  mutate(model = "nativeANN") %>% 
  select(interval, pred = pred.1hr, shark, model);
fmo.null <- fmo.saturation.null %>% 
  mutate(model = "null") %>% 
  select(everything(), pred = metric3)
pr.native <- pr.saturation %>% 
  mutate(model = "nativeANN") %>% 
  select(interval, pred.1hr = pred.1hr, shark, model)
pr.null <- pr.saturation.null %>% 
  mutate(model = "nativeANN") %>% 
  select(everything(), pred = metric3)
#get & set up swap data
dd <- "/Users/jmoxley/Dropbox (MBA)/NN_processing/data/ANN_swap" 
 load(file.path(dd, "fmo.saturate.1hr.3hr.swap.RData"))
 load(file.path(dd, "pr.saturation.1hr.3hr.swap.RData"))
fmo.swap = fmo.saturation %>% 
  mutate(model = "swapANN") %>% 
  select(interval, pred = ODBA.pred.1hr, shark, model)
pr.swap = pr.saturation %>% 
  mutate(model = "swapANN") %>% 
  select(interval, pred = ODBA.pred.1hr, shark, model)
  
#get loess curve of null prediction
#see here: https://stackoverflow.com/questions/9789871/method-to-extract-stat-smooth-line-fit
ggplot() +
  geom_point(data = fmo.null %>% group_by(interval),
             aes(x = interval, y = pred), alpha = 0.6)+
  geom_smooth(data = fmo.null %>% group_by(interval), 
              aes(x = interval, y = pred, color = 'grey'), 
              se = FALSE, span = 0.90, size = 2, method = "loess") +
  scale_y_continuous(breaks = seq(0.5,1.0,.05))
##DOES NOT MATCH PREVIOUS FIGURE. 