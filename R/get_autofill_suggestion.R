get_autofill_suggestion <- function(client_name, selected_fields, predict_field, client_rules_list) {
  
  rules_for_client <- client_rules_list[[client_name]]
  
  # Build lhs condition vector
  lhs_conditions <- paste(names(selected_fields), selected_fields, sep = "=")
  
  # Filter rules
  filtered_rules <- subset(rules_for_client,
                           lhs %ain% lhs_conditions &
                           rhs %pin% predict_field)
  
  # Sort by confidence
  sorted_rules <- sort(filtered_rules, by = "confidence", decreasing = TRUE)
  
  # Return the top suggestion (or NULL if none)
  if (length(sorted_rules) > 0) {
    return(inspect(sorted_rules[1]))
  } else {
    return(NULL)
  }
}