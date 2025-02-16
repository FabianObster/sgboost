test_that("bance works", {
  df <- data.frame(
    x1 = rnorm(100), x2 = rnorm(100), x3 = rnorm(100),
    x4 = rnorm(100), x5 = runif(100)
  )
  df$y <- df$x1 + df$x4 + df$x5
  group_df <- data.frame(
    group_name = c(1, 1, 1, 2, 2),
    var_name = c("x1", "x2", "x3", "x4", "x5")
  )
  set.seed(2)
  balances <- balance(df = df, group_df = group_df, n_reps = 11, iterations = 5, intercept = T)
  expect_error(
    create_formula(group_df = data.frame(group = 1)),
    "group_name and var_name have to be columns of group_df"
  )
  expect_equal(round(balances$opt_weights$error[1], 1), 0)
})
