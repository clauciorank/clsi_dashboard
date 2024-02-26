library(mcr)
library(dplyr)
library(ggplot2)

plot_regression <- function(regression_fit, cor.method = 'pearson', clinical_decision1, clinical_decision2, clinical_decision3){
  points <- regression_fit@data
  
  ## line regression and ribbon
  x_data <- seq(min(points$x)*.7, max(points$x)*1.3, length.out=100)
  bias <- calcResponse(regression_fit, x_data)
  ribbon <- data.frame(bias)
  
  ## Clinical decision segments
  clinical_segments <- data.frame(
    x = c(clinical_decision1, clinical_decision2, clinical_decision3),
    y = c(clinical_decision1, clinical_decision2, clinical_decision3)
  ) |> 
    mutate(across(everything(), \(x){ifelse(is.na(x), 0, x)}))
  
  correlation <- round(cor(points$x, points$y, method = cor.method), 3)
  
  int <- round(regression_fit@para[1,1], 2)
  slope <- round(regression_fit@para[2,1], 2)
  
  reg_formula <- paste0(int, '+', slope, '*', 'x')
  
  
  subtitle <- paste0(cor.method, "'s r = ", correlation, '; Regression = ', reg_formula)
  
  caption <- paste0((1 - regression_fit@alpha) *100, '% CI calculated by ', regression_fit@cimeth, ' method')
  
  
  points |> 
    ggplot(aes())+
    geom_point(aes(x, y))+
    geom_line(data = ribbon, aes(X, Y, color = 'Regression'))+
    geom_ribbon(data = ribbon, aes(x = X, ymin = Y.LCI, ymax = Y.UCI), alpha = .3, fill = 'blue')+
    geom_abline(aes(slope = 1, intercept = 0, color = 'Perfect fit'), linetype = 2)+
    labs(title = paste0(regression_fit@regmeth, '- Regression'),
         subtitle = subtitle,
         caption = caption
         )+
    geom_segment(data = clinical_segments, aes(x = x, y = 0, xend=x, yend = y), linetype = 2)+
    geom_segment(data = clinical_segments, aes(x = 0, y = y, xend=x, yend = y), linetype = 2)+
    # xlim(c(min(test$x)*.95, max(test$x)*1.15))+
    # ylim(c(min(test$y)*.95, max(test$y)*1.15))+
    scale_color_manual(values = c('Regression' = 'blue', 'Perfect fit' = 'red'))+
    theme_bw()+
    theme(text = element_text(size = 22))
}
