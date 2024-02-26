difference_plots <- function(regression_object, acceptable_difference, method = 'percentage', font_size = 22){
  data <- regression_object@data
  
  data$resid <- data$x - data$y
  data$resid_prop <- (data$x - data$y)/data$y
  data$x_plot <- (data$x + data$y)/2
  
  pontos_dentro <- sum(abs(data$resid_prop) <= acceptable_difference)
  total_pontos <- length(data$resid_prop)
  prop_pontos <- round(pontos_dentro/total_pontos*100, 2)
  
  title_plot <- paste(
    'Points inside limits:', pontos_dentro,
    'Total of points:', total_pontos,
    'Proportion inside:', prop_pontos, '%'
  )
  
  
  
  if(method == 'percentage'){
    p1 <-
      ggplot(data, aes(x_plot, resid_prop))+
      geom_point()+
      geom_hline(yintercept = 0, linetype = 2)+
      geom_hline(yintercept = acceptable_difference, linetype = 1)+
      geom_hline(yintercept = -acceptable_difference, linetype = 1)+
      labs(x = 'Concentration', y = 'Bias (%)', title = 'Percent Difference Plot', subtitle = title_plot)+
      theme_bw()+
      theme(text = element_text(size = font_size))
    
    return(ggExtra::ggMarginal(p1, type = 'histogram', margins = 'y'))    
  }else{
    p3 <-
      data |> 
      ggplot(aes(x_plot, resid))+
      geom_point()+
      geom_hline(yintercept = 0, linetype = 2)+
      geom_function(fun = \(x){x*acceptable_difference})+
      geom_function(fun = \(x){x*-acceptable_difference})+
      labs(x = 'Concentration', y = 'Bias', title = 'Absolute Difference Plot')+
      theme_bw()+
      theme(text = element_text(size = font_size))
    
    
    return(ggExtra::ggMarginal(p3, type = 'histogram', margins = 'y'))
  }
}
