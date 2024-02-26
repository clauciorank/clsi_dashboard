calculate_total_bias <- function(x, y, alpha = .05){
  data <- data.frame(x, y)
  
  data$bias <- data$x - data$y
  data$bias_percentage <- data$bias/data$y
  
  
  media <- mean(data$bias)
  media_percentage <- mean(data$bias_percentage)
  
  sd <- sd(data$bias)
  sd_percentage <- sd(data$bias_percentage)
  
  N <- length(data$bias)
  
  se <- sd/sqrt(N)
  se_percentage <- sd_percentage/sqrt(N)
  
  quantile <- 1-(alpha/2)
  
  factor <- qt(quantile, N-1)
  
  ci <- factor*sd
  ci_percentage <- factor*sd_percentage
  
  abs <-
    data.frame(
      list(
        LCI = media-ci,
        mean = media,
        UCI = media+ci,
        type = 'Absoluto'
      )
    )
  
  perc <-
    data.frame(
      list(
        LCI = media_percentage-ci_percentage,
        mean = media_percentage,
        UCI = media_percentage+ci_percentage,
        type = 'Percentual'
      )
    )
  
  
  df <- bind_rows(abs, perc)
  
  return(df)
}


generate_bias_plot <- function(bias_results, absolute_allowed_diff, percentage_allowed_diff){
  bias_results |> 
    mutate(limits = c(absolute_allowed_diff, percentage_allowed_diff)) |> 
    ggplot(aes(mean, 1))+
    geom_point()+
    ylim(c(0,2))+
    geom_text(aes(LCI, 1.2, label = round(LCI, 3)))+
    geom_text(aes(UCI, 1.2, label = round(UCI, 3)))+
    geom_text(aes(mean, 1.2, label = round(mean, 3)))+
    geom_errorbar(aes(xmin = LCI, xmax = UCI), width = .1)+
    geom_vline(aes(xintercept = limits), linetype=2)+
    geom_vline(aes(xintercept = -limits), linetype =2)+
    facet_wrap(~type, ncol = 1, scales = 'free_x')+
    labs(x = 'Bias', y = '')+
    theme_bw()+
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          text = element_text(size = 22))  
}


