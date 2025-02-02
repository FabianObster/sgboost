test_that("plot path works", {
  library(dplyr)
  library(mboost)
  set.seed(10)
  beta <- c(
    rep(5, 5), c(5, -5, 2, 0, 0), rep(-5, 5),
    c(2, -3, 8, 0, 0), rep(0, (200 - 20))
  )
  X <- matrix(data = rnorm(20000, mean = 0, sd = 1), 100, 200)
  df <- data.frame(X) %>%
    mutate(y = X %*% beta + rnorm(100, mean = 0, sd = 1)) %>%
    mutate_all(function(x) {
      as.numeric(scale(x))
    })
  group_df <- data.frame(group_name = rep(1:40, each = 5), variable_name = colnames(df)[1:200])

  # Input data.frames
  sgb_formula <- create_formula(
    alpha = 0.4, group_df = group_df, outcome_name = "y", intercept = FALSE,
    group_name = "group_name", var_name = "variable_name",
  )

  sgb_model <- mboost(
    formula = sgb_formula, data = df,
    control = boost_control(nu = 1, mstop = 600)
  )
  path <- sgboost::get_coef_path(sgb_model[150])
  plot_path(sgb_model[150])
  expect_error(plot_path(sgb_model = sgb_model, max_char_length = -1),
               "max_char_length must be a positive number")
})
