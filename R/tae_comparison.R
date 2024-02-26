get_tae <- function(data){
  get_positions <- function(x){
    if(x %% 1 == 0){
      return(c(x))
    }else{
      return(c(as.integer(x), as.integer(x)+1))
    }
  }
  
  min_quantile <- .5 + 20*.025
  max_quantile <- .5 + 20*.975
  
  bias <- sort(data$Hilab - data$Conventional)
  
  bias_perc <- sort((data$Hilab - data$Conventional) / data$Hilab*100)
  
  data.frame(
    `_` = c('Low', 'High'),
    `TAE` = c(mean(bias[get_positions(min_quantile)]), mean(bias[get_positions(max_quantile)])),
    `TAE Percentage`= c(mean(bias_perc[get_positions(min_quantile)]), mean(bias_perc[get_positions(max_quantile)]))
  )  
}
