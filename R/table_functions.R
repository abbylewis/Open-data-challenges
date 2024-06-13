

make_summary_table <- function(column_id = colnames(df[,column_n]),
                               df,
                               column_n = which(colnames(df) == column_id)) {
  nrow = nrow(df)
  # summarise the results
  df_summary <- df |>
    group_by(across(all_of(column_id))) |>
    summarise(Count = n(), .groups = 'drop') |>
    mutate(Percentage = round(100*(Count/nrow),1)) |>
    rename(Response = all_of(column_id)) |>
    knitr::kable(caption = questions[1, column_n])

  return(df_summary)
}

make_summary_table_checkbox <- function(column_id = colnames(df[,column_n]),
                               df,
                               column_n = which(colnames(df) == column_id)) {

  #Re-format multiple selections
  df_long <- survey_results_formatted %>%
    mutate(Response = gsub(",([A-Z])", " ,\\1", get(column_id))) %>%
    mutate(Response = str_split(Response, " ,")) %>%
    unnest(Response) %>%
    mutate(Response = str_split(Response, "\\),")) %>%
    unnest(Response) %>%
    mutate(Response = trimws(Response),
           Response = ifelse(grepl("\\(", Response) &
                               !grepl("\\)", Response),
                             paste0(Response, ")"),
                             Response),
           Response = ifelse(grepl("Other:", Response),
                             "Other",
                             Response))

  nrow = nrow(df)
  # summarise the results
  df_summary <- df_long |>
    group_by(Response) |>
    summarise(Count = n(), .groups = 'drop') |>
    mutate(Percentage = round(100*(Count/nrow),1)) |>
    knitr::kable(caption = questions[1, column_n])

  return(df_summary)
}

