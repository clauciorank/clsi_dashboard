plot_ci <- function(model, value1, value2, value3){
  info <- c(value1, value2, value3)[!is.na(c(value1, value2, value3))]
  
  response <- data.frame(mcr::calcResponse(model, info))
  
  ggplot(response, aes(0, X))+
    geom_point(size = 3)+
    geom_errorbar(aes(ymin = Y.LCI, ymax=Y.UCI), linetype=2)+
    geom_text(aes(label = round(Y.LCI, 2), y = Y.LCI), vjust = 1.3, size=8)+
    geom_text(aes(label = round(Y.UCI, 2), y = Y.UCI), vjust = -.5, size=8)+
    labs(y = 'Value')+
    facet_wrap(~X)+
    theme_bw()+
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank(),
          text = element_text(size = 22))  
}
