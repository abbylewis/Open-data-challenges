plot_global_result <- function(column_n, df = survey_results, as_percent = T, questions) {


  # extract column id
  column_id <- colnames(df[,column_n])
  nrow <- nrow(df)

  # summarise the results
  df_plot <- df |>
    group_by(across(all_of(column_id))) |>
    summarise(count = n(), .groups = 'drop') |>
    mutate(percentage = 100*(count/nrow))

  # plot results
  if (as_percent) {
    p <- ggplot(df_plot, aes(x=get(column_id), y=percentage)) +
      geom_bar(stat = 'identity') +
      labs(x=column_id,
           y = 'Percentage of respondants')
  } else {
    p <- ggplot(df_plot, aes(x=get(column_id), y=count)) +
      geom_bar(stat = 'identity') +
      labs(x=column_id,
           y= 'Number of respondants')
  }
  p +
    theme_bw() +
    labs(subtitle = questions[1, column_n]) +
    theme(axis.text.x = element_text(angle = 90))

}


plot_facet_result <- function(column_n,
                              group_n,
                              df = survey_results,
                              questions,
                              as_percent = T) {


  # extract column id
  column_id <- colnames(df[,column_n])
  group_id <- colnames(df[,group_n])

  nrow <- df |>
    group_by(across(all_of(group_id))) |>
    summarise(total = n(), .groups = 'drop')

  # summarise the results
  df_plot <-
    df |>
    group_by(across(all_of(c(group_id, column_id)))) |>
    summarise(count = n(), .groups = 'drop') |>
    full_join(nrow, by = group_id) |>
    mutate(percentage = 100*(count/total))

  # plot results
  if (as_percent) {
    p <- ggplot(df_plot, aes(x=get(column_id), y=percentage)) +
      geom_bar(stat = 'identity') +
      labs(x=column_id,
           y= 'Percentage of respondants') +
      facet_wrap(~get(group_id))
  } else {
    p <- ggplot(df_plot, aes(x=get(column_id), y=count)) +
      geom_bar(stat = 'identity') +
      labs(x=column_id,
           y= 'Number of respondants') +
      facet_wrap(~get(group_id))
  }
  p +
    theme_bw() +
    labs(subtitle = questions[1, column_n]) +
    theme(axis.text.x = element_text(angle = 90))
}
