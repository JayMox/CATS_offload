#script to calc the number of estimates above the null model for generalization
rm(list = ls())
library(dplyr)
library(ggplot2)

dd <- "/Users/jmoxley/Dropbox (MBA)/NN_processing/data/1hr_Prediction_testset_June18"
 load(file = file.path(dd, "fmo_saturate.1.3.12.RData")) 
 load(file = file.path(dd, "pr.saturation.1.3.12.RData"))
 load(file.path(dd, "pr.saturation.null.RData"))
 load(file.path(dd, "fmo_saturate.null.RData"))
native <- bind_rows(fmo.saturation %>% 
  mutate(model = "nativeANN") %>% 
  select(interval, pred = pred.1hr, shark, model),
  pr.saturation %>% 
    mutate(model = "nativeANN") %>% 
    select(interval, pred= pred.1hr, shark, model))
null <- bind_rows(fmo.saturation.null %>% 
  mutate(model = "null") %>% 
  select(everything(), pred = metric3),
 pr.saturation.null %>% 
   mutate(model = "null") %>% 
   select(everything(), pred = metric3))
#get & set up swap data
dd <- "/Users/jmoxley/Dropbox (MBA)/NN_processing/data/ANN_swap" 
 load(file.path(dd, "fmo.saturate.1hr.3hr.swap.RData"))
 load(file.path(dd, "pr.saturation.1hr.3hr.swap.RData"))
swap = bind_rows(fmo.saturation %>% 
  mutate(model = "swapANN") %>% 
  select(interval, pred = ODBA.pred.1hr, shark, model), 
  pr.saturation %>% 
    mutate(model = "swapANN") %>% 
    select(interval, pred = ODBA.pred.1hr, shark, model))
rm(fmo.saturation, fmo.saturation.null, pr.saturation, pr.saturation.null)  
#get loess curve of median null prediction
#see here: https://stackoverflow.com/questions/9789871/method-to-extract-stat-smooth-line-fit
p <- null %>% filter(shark == "Shark 2") %>% group_by(interval) %>% summarize(med = median(pred)) %>% 
  ggplot(aes(x = interval, y = med)) + 
  stat_smooth(aes(shark2.null<<-..y..),method = "loess", se = F)
null.pred <- 
  
null1 <- loess(med ~ interval, data = null %>% 
                 filter(shark == "Shark 1") %>% 
                 group_by(interval) %>% 
                 summarize(med = median(pred)), 
               span = 0.9)
null2 <- loess(med ~ interval, data = null %>% 
                 filter(shark == "Shark 2") %>% 
                 group_by(interval) %>% 
                 summarize(med = median(pred)), 
               span = 0.9)
#make df
nullfit <- data.frame(interval = unique(null$interval), 
                      nullpred = c(null1$fitted, null2$fitted),
                      shark = c(rep("Shark 1", length(null1$fitted)),
                                rep("Shark 2", length(null2$fitted)))) %>% 
  group_by(shark, interval)
#calc percents
native %>% group_by(shark, interval) %>% 
  summarize(over = sum(pred > nullfit$nullpred)) %>% 
  ggplot() + geom_point(aes(x = interval, y = over, color = shark))
