test_that("age_calc works for birthday already occurred this year", {
  today <- as.Date('2020-06-01')
  birthday <- as.Date('2010-05-01')

  expect_equal(age_calc(birthday, enddate=today, units = 'years'), 10.0846995)
  expect_equal(age_calc(birthday, enddate=today, units = 'years', floor=TRUE), 10)
})

test_that("age_calc works for birthday on  today's date", {
  today <- as.Date('2020-06-01')
  birthday <- as.Date('2010-06-01')

  expect_equal(age_calc(birthday, enddate=today, units = 'years'), 10)
  expect_equal(age_calc(birthday, enddate=today, units = 'years', floor=TRUE), 10)
})

test_that("age_calc works for birthday yet to occurr this year", {
  today <- as.Date('2020-06-01')
  birthday <- as.Date('2010-07-01')

  expect_equal(age_calc(birthday, enddate=today, units = 'years'), 9.916678)
  expect_equal(age_calc(birthday, enddate=today, units = 'years', floor=TRUE), 9)
})
