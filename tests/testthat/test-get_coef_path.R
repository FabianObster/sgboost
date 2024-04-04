test_that("get_coef_path works", {
  expect_error(
    get_varimp(sgb_model = "sgb"),
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
  get_coef_path <- get_coef_path(sgb_model)
  expect_equal(mstop(sgb_model), 100)
})
