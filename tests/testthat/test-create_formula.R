test_that("multiplication works", {
  group_df <- data.frame(group_name = c(1,1,2), var_name = c('x1','x2','x3'))

  expect_equal(create_formula(alpha=0.5, group_df = group_df),
               y ~ bols(x1, df = 0.5, intercept = F) +
                 bols(x2, df = 0.5, intercept = F) +
                 bols(x3, df = 0.5, intercept = F) +
                 bols(x1, x2, df = 0.5, intercept = F) +
                 bols(x3, df = 0.5, intercept = F))
})
