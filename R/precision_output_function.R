library(CLSIEP15)

calculate_precision_outputs <- function(data, s_allowed_r, s_allowed_wl, cv_allowed_r, cv_allowed_wl){
  aov_info <- CLSIEP15::calculate_aov_infos(data)
  
  uvl_s <- CLSIEP15::calculate_uvl_info(aov_info, cvr_or_sr = s_allowed_r, cvwl_or_swl = s_allowed_wl)
  uvl_cv <- CLSIEP15::calculate_uvl_info(aov_info, cvr_or_sr = cv_allowed_r, cvwl_or_swl = cv_allowed_wl)
  
  value = c(
    round(aov_info$N, 2),
    round(aov_info$aov_table$ms[1], 2),
    round(aov_info$aov_table$ms[2], 2),
    round(aov_info$n0, 2),
    round(aov_info$Vbetween, 2),
    round(aov_info$Vwithin, 2),
    round(aov_info$mean, 2),
    paste0(round(aov_info$SR,2), '(', round(aov_info$CVR,2), '%)'),
    paste0(round(aov_info$SWL,2), '(', round(aov_info$CVWL,2), '%)')
  )
  
  aov_df <-
    data.frame(
      parameter = c('N', 'MS1 (between)', 'MS2 (within)', 'n0', 'Vb (between)', 
                    'Vw (within)', 'Mean', 'SR (%CV)', 'SWL (%CV)'),
      value = value
    )
  
  df <-
    data.frame(
      type = c('Repetability', 'Within-Lab'),
      S = c(aov_info$SR, aov_info$SWL),
      CV = c(aov_info$CVR, aov_info$CVWL),
      limits_s = c(uvl_s$cv_uvl_r, uvl_s$cv_uvl_wl),
      limits_cv = c(uvl_cv$cv_uvl_r, uvl_cv$cv_uvl_wl)
    ) |> 
    tidyr::pivot_longer(c(S, CV)) |> 
    mutate(limit_uvl = case_when(
      name == 'S' ~ limits_s,
      TRUE ~ limits_cv
    )) |> 
    select(-c(limits_s, limits_cv)) |> 
    mutate(limit_informed = c(s_allowed_r, cv_allowed_r, s_allowed_wl, cv_allowed_wl)) |> 
    relocate(limit_informed, .before = limit_uvl)
  
  
  p <- ggplot(df, aes(value, name, fill = name))+
    geom_col()+
    geom_errorbar(aes(xmin = as.numeric(limit_uvl), xmax = as.numeric(limit_uvl)), color = 'red', linetype = 2)+
    geom_errorbar(aes(xmin = as.numeric(limit_informed), xmax = as.numeric(limit_informed)), color = 'orange', linetype = 2)+
    facet_wrap(~type, ncol = 1)+
    theme_bw()
  
  return(list(df = aov_df, plot = p, df_uvl = df, aov_info = aov_info))
}

# s <- calculate_precision_outputs(create_table_ep_15(ferritin_wider), 2.9, 5.1, 1.4, 2.4)

# calculate_precision_outputs(create_table_ep_15(ferritin_wider), NA, NA, NA, NA)
