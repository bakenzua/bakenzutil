test_that("multiplication works", {

  df <- data.frame(
    id = rep(1,10),
    time_column = seq(as.POSIXct('2021-01-22 12:00:00'), len=10, by="1 hour"),
    variable = c(1,NA,NA,NA,2,3,4,5,6,7)
  )

  #    id         time_column     variable
  # 1   1 2021-01-22 12:00:00        1
  # 2   1 2021-01-22 13:00:00       NA
  # 3   1 2021-01-22 14:00:00       NA
  # 4   1 2021-01-22 15:00:00       NA
  # 5   1 2021-01-22 16:00:00        2
  # 6   1 2021-01-22 17:00:00        3
  # 7   1 2021-01-22 18:00:00        4
  # 8   1 2021-01-22 19:00:00        5
  # 9   1 2021-01-22 20:00:00        6
  # 10  1 2021-01-22 21:00:00        7

  expect_true(
    is.na(
      df[[2, 'variable']]
    )
  )
  expect_equal(
    fill_column_within_time_window(df, group_by_col=id, time_col=time_column, fill_col=variable)[[2, 'variable']],
    1
  )

  expect_true(
    is.na(
      df[[3, 'variable']]
    )
  )
  expect_equal(
    fill_column_within_time_window(df, group_by_col=id, time_col=time_column, fill_col=variable)[[3, 'variable']],
    1
  )


  expect_true(
    is.na(
      df[[4, 'variable']]
    )
  )
  expect_true(
    is.na(
      fill_column_within_time_window(df, group_by_col=id, time_col=time_column, fill_col=variable)[[4, 'variable']]
    )
  )
})

