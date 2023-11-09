plot_global_result <- function(column_n, df = survey_results) {


  # extract column id
  column_id <- colnames(df[,column_n])
  nrow <- nrow(df)

  # summarise the results
  df_plot <- df |>
    group_by(across(all_of(column_id))) |>
    summarise(count = n(), .groups = 'drop') |>
    mutate(percentage = 100*(count/nrow))

  # plot results
  ggplot(df_plot, aes(x=get(column_id), y=percentage)) +
    geom_bar(stat = 'identity') +
    labs(x=column_id,
         y= 'Percentage of respondants') +
    them_bw()

}


plot_facet_result <- function(column_n, group, df = survey_results) {


  # extract column id
  column_id <- colnames(df[,column_n])
  nrow <- df |>
    group_by(across(all_of(group))) |>
    summarise(total = n(), .groups = 'drop')

  # summarise the results
  df_plot <-
    df |>
    group_by(across(all_of(c(group, column_id)))) |>
    summarise(count = n(), .groups = 'drop') |>
    full_join(nrow, by = group) |>
    mutate(percentage = 100*(count/total))

  # plot results
  ggplot(df_plot, aes(x=get(column_id), y=percentage)) +
    geom_bar(stat = 'identity') +
    labs(x=column_id,
         y= 'Percentage of respondants') +
    theme_bw() +
    facet_wrap(~get(group))

}
