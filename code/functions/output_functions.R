
retrieve_result = function(reg){
  summary = summary(reg)
  
  # Extract the distance to treatment (numeric part before "::")
  exposure = as.numeric(gsub("^.*distance_treat_1965::(-?[0-9]+):.*$", "\\1", names(summary$coefficients)))
  
  # Build the summary table
  tables = data.frame(
    dist_to_treat = exposure,
    estimate = summary$coefficients,
    se = summary$se
  )
  
  # Add confidence intervals
  tables = tables %>% 
    mutate(ci_low = estimate - 1.96 * se,
           ci_high = estimate + 1.96 * se)
  
  return(tables)
}

event_study_plot = function(data, title = "title", x_label = "distance_to_treat", y_label = "estimate"  ,save = FALSE, output_path = "."){
  plot_et = ggplot(data , aes(x = dist_to_treat, y = estimate)) +
    geom_point(color = "blue") +  # Add points for the estimated coefficients
    # geom_line(color = "blue")+
    geom_hline(yintercept = 0, linetype = "solid", color = "black")+
    geom_vline(xintercept = 0, linetype = "dashed", color = "red") + 
    geom_errorbar(aes(ymin = ci_low , ymax = ci_high), width = 0.2, color = "blue") +  # 95% CI
    labs(title = title,
         x = x_label,
         y = y_label) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  if (save){
    ggsave(output_path, plot = plot_et, width = 10, height = 7)
  }
  return(plot_et)
}
