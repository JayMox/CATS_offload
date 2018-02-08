#data to train on
load(file.path(dd, "FinMountOrig_TestSet_sims.RData"))
#data_l = fmo.sim; SPAN = .9; N_size = 500; response = "metric3"; predictor = "interval"; 
#pt_alpha = .1; N_LOESS = 500; FRAC = 1; ymax = 1; ymin = .80;color= "red"; pt_color = "black";

geom_mloess <- function(mapping = NULL, data = NULL, stat = "xspline",
                         position = "identity", show.legend = NA,
                         inherit.aes = TRUE, na.rm = TRUE,
                         #these go into the actual fxn call
                         SPAN = .9, N_size = 500, N_LOESS = 500, ...) {
  layer(
    geom = GeomMLoess,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(SPAN = .9, 
                  N_size = 500, 
                  N_LOESS = 500,
                  ...)
  )
}

GeomMLoess <- ggproto("GeomMLoess", GeomLine,
                       required_aes = c("x", "y"),
                       default_aes = aes(colour = "red", size = 0.5, linetype = 1, alpha = NA)
)

stat_mloess <- function(mapping = NULL, data = NULL, geom = "line",
                         position = "identity", show.legend = NA, inherit.aes = TRUE,
                         SPAN = .9, N_size = 500, N_LOESS = 500, ...) {
  layer(
    stat = StatMLoess,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(SPAN = .9, 
                  N_size = 500, 
                  N_LOESS = 500,
                  ...
    )
  )
}

StatMLoess <- ggproto("StatMLoess", Stat,
                       
                      required_aes = c("x", "y"),
                       
                      compute_group = function(self, data, scales, params,
                                               SPAN = .9, N_size = 500, N_LOESS = 500) {
                        #right now works with 2-col matrix (col1: predictor, col2: response)
                        #could consider 
                        colnames(data) <- c("predictor", "response")
                        
                        LOESS_DF <- data.frame(interval = seq(min(data$predictor), 
                                                          max(data$predictor), 
                                                          length.out = 500))
                        
                        for(i in 1:N_LOESS){
                          # sample 1000 points
                          df_sample <- dplyr::sample_n(data, N_size)
                          # fit a loess
                          xx <- df_sample$predictor
                          yy <- df_sample$response
                          tp_est <- loess(yy ~ xx , span = SPAN) 
                          # predict accross range of x using loess model
                          loess_vec <- data.frame(
                            predict(tp_est, newdata = 
                                      data.frame(xx = seq(min(data$predictor), max(data$predictor), length.out = 500))))
                          colnames(loess_vec) <- as.character(i)
                          # repeat x times
                          LOESS_DF <- cbind(LOESS_DF,loess_vec)
                        }
                        
                        DF_long <- reshape2::melt(LOESS_DF, id = "interval")
                        point_sample <- sample_frac(data,1)
                        xp <- point_sample$predictor
                        yp <- point_sample$response
                        
                        data.frame(x = xp, y = yp)
                      }
)


ggplot(df, aes(interval, metric3)) + geom_mloess(SPAN = .9, N_size = 500, N_LOESS = 500)
