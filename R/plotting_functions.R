plot_global_result <- function(df = survey_results,
                               column_id = colnames(df[,column_n]),
                               column_n = as.numeric(which(colnames(df) == column_id)),
                               as_percent = T,
                               questions) {

  nrow = nrow(df)

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


plot_facet_result <- function(df = survey_results,
                              column_id = colnames(df[,column_n]),
                              column_n = which(colnames(df) == column_id),
                              group_id = colnames(df[,group_n]),
                              group_n = which(colnames(df) == group_id),
                              questions,
                              as_percent = T) {

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
