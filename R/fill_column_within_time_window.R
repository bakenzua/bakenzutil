#' fill_column_within_time_window
#'
#' fill_column_within_time_window uses tidyr::fill to fill a column within a time window.
#'
#' @export
#'
#' @param df
#' @param group_by_col
#' @param time_col
#' @param fill_col
#' @param window_length_hours
#' @param fill_direction
#'
#' @return full_query

fill_column_within_time_window <- function(df, group_by_col, time_col, fill_col, window_length_hours=2, fill_direction = "down") {

  # handle column type detection and unknown type in a bad fragile way
  ################
  fill_col_type <- typeof(pull(df, {{fill_col}}))
  if(!(fill_col_type %in% c('character', 'double', 'integer'))) { stop('fill_col is unknown type!!')}
  if (fill_col_type  == 'character') {
    na_replace_value = NA_character_
  }
  if (fill_col_type  == 'double') {
    na_replace_value = NA_real_
  }
  if (fill_col_type  == 'integer') {
    na_replace_value = NA_real_
  }
  ################

  df %>%
    arrange({{group_by_col}}, {{time_col}}) %>%
    group_by({{group_by_col}}) %>%
    mutate(
      compare_chart_time = if_else(
        is.na({{fill_col}}),
        lubridate::NA_POSIXct_,
        {{time_col}}
      )
    ) %>%
    tidyr::fill(compare_chart_time, {{fill_col}}, .direction = fill_direction) %>%
    mutate(
      dt = as.numeric(difftime({{time_col}}, compare_chart_time, units = 'hours')),
      dt = abs(dt),
      {{fill_col}} := if_else(
        dt <= window_length_hours,
        {{fill_col}},
        na_replace_value
      )
    ) %>%
    ungroup() %>%
    select(-c(compare_chart_time, dt))
}


