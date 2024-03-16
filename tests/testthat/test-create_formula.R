test_that("create_formula works", {
  library(stats)
  expect_error(create_formula(group_df = 'group'),
               'group_df must be a data.frame')
  expect_error(create_formula(alpha = '0.5'),
               'Mixing parameter alpha must be numeric')
  expect_error(create_formula(alpha = -0.5),
               'Mixing parameter alpha must between zero and one')
  expect_error(create_formula(alpha = 5),
               'Mixing parameter alpha must between zero and one')
  expect_error(
    create_formula(group_df = data.frame(group = 1)),
    'group_name and var_name have to be columns of group_df'
  )
  group_df <-
    data.frame(group_name = c(1, 1, 2),
               var_name = c('x1', 'x2', 'x3'))
  expect_equal(
    as.formula(create_formula(alpha = 0.5, group_df = group_df)),
    y ~ bols(x1, df = 0.5, intercept = F) +
      bols(x2, df = 0.5, intercept = F) +
      bols(x3, df = 0.5, intercept = F) +
      bols(x1, x2, df = 0.5, intercept = F) +
      bols(x3, df = 0.5, intercept = F)
  )
  expect_equal(
    as.formula(create_formula(alpha = 1, group_df = group_df)),
    y ~ bols(x1, df = 1, intercept = F) +
      bols(x2, df = 1, intercept = F) +
      bols(x3, df = 1, intercept = F)
  )
  expect_equal(
    as.formula(create_formula(alpha = 0, group_df = group_df)),
    y ~ bols(x1, x2, df = 1, intercept = F) + bols(x3, df = 1, intercept = F)
  )
})
