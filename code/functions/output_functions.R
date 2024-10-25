
retrieve_result = function(reg){
  summary = summary(reg)
  
  # Extract the distance to treatment (numeric part before "::")
  # exposure = as.numeric(gsub("^.*distance_treat_1965::(-?[0-9]+):.*$", "\\1", names(summary$coefficients)))
  exposure = as.numeric(gsub("^.*distance_treat_196[0-9](?:_year|_q)?::?(-?[0-9]+).*", "\\1", names(summary$coefficients)))
  
  # Build the summary table
  tables = data.frame(
    dist_to_treat = exposure,
    estimate = summary$coefficients,
    se = summary$se
  )
  
  # Add confidence intervals with tolerance for edge cases
  tol <- 1e-8  # Small tolerance to avoid edge case issues
  tables = tables %>%
    mutate(ci_low_95 = estimate - 1.96 * se,
           ci_high_95 = estimate + 1.96 * se,
           ci_low_90 = estimate - 1.645 * se,
           ci_high_90 = estimate + 1.645 * se)
  
  # Determine significance at 5% and 10% with NA handling
  tables = tables %>%
    mutate(significance = case_when(
      is.na(estimate) ~ "Not Significant",  # Handle NA estimates
      ci_low_95 > tol | ci_high_95 < -tol ~ "Significant at 5%",
      (ci_low_95 < tol & ci_high_95 > -tol) & (ci_low_90 > tol | ci_high_90 < -tol) ~ "Significant at 10%",
      TRUE ~ "Not Significant"
    ),
    shape = case_when(
      significance == "Significant at 5%" ~ 15, # Star shape for 5% significance
      significance == "Significant at 10%" ~ 17, # Rectangle for 10% significance
      TRUE ~ 21 # Empty circle for non-significant points
    ))
  
  return(tables)
}

event_study_plot = function(data, title = "title", x_label = "distance_to_treat", y_label = "estimate", save = FALSE, output_path = "."){
  
  plot_et = ggplot(data, aes(x = dist_to_treat, y = estimate)) +
    geom_point(aes(shape = as.factor(shape)), color = "blue", size = 3) +  # Map shape aesthetic to 'shape'
    geom_hline(yintercept = 0, linetype = "solid", color = "black") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red") + 
    geom_errorbar(aes(ymin = ci_low_95 , ymax = ci_high_95), width = 0.2, color = "blue") +  # 95% CI
    labs(title = title,
         x = x_label,
         y = y_label) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_shape_manual(values = c(15, 17, 21), 
                       labels = c( "Significant at 5%","Significant at 10%","Not Significant")) # Map shapes
  
  if (save){
    ggsave(output_path, plot = plot_et, width = 10, height = 7)
  }
  
  return(plot_et)
}
