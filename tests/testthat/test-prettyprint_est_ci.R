test_that("prettyprint_est_ci works", {
  expect_equal(prettyprint_est_ci(1,2,3), "1 (95%CI: 2, 3)")


  expect_equal(prettyprint_est_ci(1.1, 2.2, 3.3, digits=0), "1 (95%CI: 2, 3)")

  expect_equal(prettyprint_est_ci(1.1, 2.2, 3.3, digits=0, ci_pc=""), "1 (2, 3)")



})
