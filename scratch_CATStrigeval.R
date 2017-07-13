data <- c(runif(100000, min=0, max=5),runif(100000, min=5, max=25),runif(10000, min=0, max=10), runif(100000, min=0, max=15))


#generalized function for sliding function applys https://stats.stackexchange.com/questions/3051/mean-of-a-sliding-window-in-r
slideFunct <- function(data, window, step){
  total <- length(data)
  spots <- seq(from=1, to=(total-window), by=step)
  result <- vector(length = length(spots))
  for(i in 1:length(spots)){
    result[i] <- mean(data[spots[i]:(spots[i]+window)])
  }
  return(result)
}

#CATS trigger emulation
#currently only rear looking, ie. first apply is 1 window away from 1st
slideVVtrig <- function(data, window, step){
  total <- length(data)
  spots <- seq(from=1, to=total-window, by=step)
  result <- vector(length = length(spots))
  for(i in 1:(length(spots)-window)){
    result[i] <- diff(range(data[spots[i]:spots[i + window]]))
  }
  return(c(rep(NA, window), result))  #pad front end of result where diff cannot be calc'ed
}

