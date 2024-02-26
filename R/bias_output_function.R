bias_outputs <- function(dat){
  if(dat$signif){
    subtitle <- "Difference is significant the test point is outside the true value CI"
  }else{
    subtitle <- "Difference is not significant the test point is inside the true value CI"
  }
  
  df <-  
    data.frame(dat) |> 
    select(Mean = mean, `True Value (TV)` = TV, Bias = bias, `TV Lower Limit` = interval.lower_limit,
           `TV Higher Limit` = interval.higher_limit) |> 
    mutate(across(everything(), \(x)round(x, 3)))
  
  p <-
    data.frame(dat) |> 
    ggplot(aes())+
    geom_point(aes(mean, 0, color = 'Test'), size = 3)+
    geom_errorbar(aes(y=0, xmin = interval.lower_limit, xmax = interval.higher_limit, color = 'True Value'), linetype = 2)+
    geom_point(aes(TV, 0, color = 'True Value'), size = 3)+
    scale_color_manual(values = c('True Value' = 'blue', 'Test' = ifelse(dat$signif, 'red', 'green')))+
    labs(title = paste0('Bias = ', round(dat$bias, 2)), subtitle = subtitle, color = '')+
    theme_bw()+
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          legend.position = 'top',
          plot.title = element_text(hjust = .5),
          plot.subtitle = element_text(hjust = .5)
    )
  
  return_l <- list(df = df, plot = p)  
  
  return(return_l)
}
