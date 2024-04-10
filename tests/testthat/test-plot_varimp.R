test_that("plot_varimp works", {
  expect_error(
    plot_varimp(sgb_model = "sgb"),
    "Model must be of class mboost"
  )
  library(dplyr)
  library(mboost)
  set.seed(1)
  df <- data.frame(
    x1 = rnorm(100), x2 = rnorm(100), x3 = rnorm(100),
    x4 = rnorm(100), x5 = runif(100)
  )
  df <- df %>%
    mutate_all(function(x) {
      as.numeric(scale(x))
    })
  df$y <- df$x1 + df$x4 + df$x5
  group_df <- data.frame(
    group_name = c(1, 1, 1, 2, 2),
    var_name = c("x1", "x2", "x3", "x4", "x5")
  )
  sgb_formula <- as.formula(create_formula(alpha = 0.3, group_df = group_df, intercept = T))
  sgb_model <- mboost(formula = sgb_formula, data = df)
  expect_error(plot_varimp(sgb_model = sgb_model, prop = 1.2), "prop must be between zero and one")
  expect_message(plot_varimp(sgb_model = sgb_model, prop = 0.2))
  expect_message(plot_varimp(sgb_model = sgb_model, n_predictors = 1), "1 predictors were removed. Use prop or n_predictors to change")
})
