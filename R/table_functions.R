

make_summary_table <- function(column_id = colnames(df[,column_n]),
                               df,
                               column_n = which(colnames(df) == column_id)) {
  nrow = nrow(df)
  # summarise the results
  df_summary <- df |>
    group_by(across(all_of(column_id))) |>
    summarise(count = n(), .groups = 'drop') |>
    mutate(percentage = round(100*(count/nrow),1)) |>
    knitr::kable(caption = questions[1, column_n])

  return(df_summary)
}



