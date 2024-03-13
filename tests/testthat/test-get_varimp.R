test_that("get_varimp works", {
  expect_error(get_varimp(sgb_model = 'sgb'),
               'Model must be of class mboost')
})
