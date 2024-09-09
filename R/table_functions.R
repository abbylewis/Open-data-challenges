

make_summary_table <- function(column_id = colnames(df[,column_n]),
                               df,
                               column_n = which(colnames(df) == column_id),
                               caption = NULL,
                               label = NULL) {
  #Avoid dropping unused levels for Ease
  nrow = nrow(df)
  if(grepl("Ease", column_id)) {
    levels <- df %>%
      arrange(Ease_2) %>%
      pull(Ease_2) %>%
      unique()

    df <- df |>
      mutate(across(all_of(column_id),
                    ~factor(., levels = levels)))
  }

  #Order levels for challenges
  if(grepl("Challenges_[1|2|3|4|5|6]...[2|3]", column_id)) {
    levels <- c("Frequent challenge", "Occasional challenge", "Rare challenge", "Never encountered", NA)
    df <- df |>
      mutate(across(all_of(column_id),
                    ~factor(., levels = levels)))
  }

  if(grepl("Challenges_[1|2|3|4|5|6]...4", column_id)) {
    levels <- c("Frequent challenge", "Occasional challenge", "Never encountered", NA)
    df <- df |>
      mutate(across(all_of(column_id),
                    ~factor(., levels = levels)))
  }

  if(grepl("Software", column_id)) {
    levels <- c("Extremely familiar", "Very familiar", "Moderately familiar",
                "Slightly familiar", "Not familiar at all", NA)
    df <- df |>
      mutate(across(all_of(column_id),
                    ~factor(., levels = levels)))
  }

  #Always put "Other" last
  if(grepl("Other", df[column_id])) {
    df <- df |>
      mutate(across(all_of(column_id),
                    ~forcats::fct_relevel(., "Other", after = Inf)))
  }

  #Always put "Both" last
  if(grepl("Both", df[column_id])) {
    df <- df |>
      mutate(across(all_of(column_id),
                    ~forcats::fct_relevel(., "Both", after = Inf)))
  }

  #Always put "Neither" last
  if(grepl("Neither", df[column_id])) {
    df <- df |>
      mutate(across(all_of(column_id),
                    ~forcats::fct_relevel(., "Neither", after = "Both")))
  }

  # summarise the results
  df_summary <- df |>
    group_by(across(all_of(column_id)), .drop = F) |>
    summarise(Count = n()) |>
    mutate(Percentage = round(100*(Count/nrow),1)) |>
    rename(Response = all_of(column_id)) |>
    #mutate(Response = kableExtra::linebreak(stringr::str_wrap(Response, 50))) |>
    knitr::kable(caption = questions[1, column_n], escape = F)

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

  #Re-arrange levels for requirements
  if(sum(grepl("Not necessary, available data is enough", df_long$Response)) > 0) {
    df_long <- df_long |>
      mutate(across(all_of("Response"),
                    ~forcats::fct_relevel(., "Not necessary, available data is enough", after = Inf)))
  }

  #Always put "Other" last
  if(sum(grepl("Other", df_long$Response)) > 0) {
    df_long <- df_long |>
      mutate(across(all_of("Response"),
                    ~forcats::fct_relevel(., "Other", after = Inf)))
  }

  # summarise the results
  df_summary <- df_long |>
    group_by(Response) |>
    summarise(Count = n(), .groups = 'drop') |>
    mutate(Percentage = round(100*(Count/nrow),1)) |>
    knitr::kable(caption = questions[1, column_n])

  return(df_summary)
}

