test_that("create_formula works", {
  # library(stats)
  expect_error(
    create_formula(group_df = "group"),
    "group_df must be a data.frame"
  )
  expect_error(
    create_formula(alpha = "0.5"),
    "Mixing parameter alpha must be numeric"
  )
  expect_error(
    create_formula(alpha = -0.5),
    "Mixing parameter alpha must between zero and one"
  )
  expect_error(
    create_formula(alpha = -0.5),
    "Mixing parameter alpha must between zero and one"
  )
  expect_error(
    create_formula(alpha = 5),
    "Mixing parameter alpha must between zero and one"
  )
  expect_error(
    create_formula(group_df = data.frame(group = 1)),
    "group_name and var_name have to be columns of group_df"
  )
  group_df <-
    data.frame(
      group_name = c(1, 1, 2),
      var_name = c("x1", "x2", "x3")
    )
  expect_warning(create_formula(alpha = 0.5, group_df = group_df, blearner = "bbs"))
  expect_equal(
    create_formula(alpha = 0.5, group_df = group_df),
    y ~ bols(x1, df = 0.5, intercept = FALSE) +
      bols(x2, df = 0.5, intercept = FALSE) +
      bols(x3, df = 0.5, intercept = FALSE) +
      bols(x1, x2, df = 0.5, intercept = FALSE) +
      bols(x3, df = 0.5, intercept = FALSE),
    ignore_formula_env = TRUE
  )
  expect_equal(
    create_formula(alpha = 1, group_df = group_df),
    y ~ bols(x1, df = 1, intercept = FALSE) +
      bols(x2, df = 1, intercept = FALSE) +
      bols(x3, df = 1, intercept = FALSE),
    ignore_formula_env = TRUE
  )
  expect_equal(
    create_formula(alpha = 0, group_df = group_df),
    y ~ bols(x1, x2, df = 1, intercept = FALSE) + bols(x3, df = 1, intercept = FALSE),
    ignore_formula_env = TRUE
  )
  colnames(group_df) <- c("group", "var")
  expect_equal(
    create_formula(alpha = 0, group_df = group_df, group_name = "group", var_name = "var"),
    y ~ bols(x1, x2, df = 1, intercept = FALSE) + bols(x3, df = 1, intercept = FALSE),
    ignore_formula_env = TRUE
  )
  group_df <-
    data.frame(
      group_name = c(1, 1, 2),
      var_name = c("x1", "x2", "x3"),
      group_weights = c(0.3, 0.3, 0.5)
    )
  expect_equal(
    create_formula(alpha = 0, group_df = group_df, group_name = "group_name", var_name = "var_name"),
    y ~ bols(x1, x2, df = 0.3, intercept = FALSE) + bols(x3, df = 0.5, intercept = FALSE),
    ignore_formula_env = TRUE
  )
})
