test_that("get_varimp works", {
  expect_error(
    get_varimp(sgb_model = "sgb"),
    "Model must be of class mboost"
  )
  library(mboost)
  library(dplyr)
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

  sgb_formula <- as.formula(create_formula(alpha = 0.3, group_df = group_df))
  sgb_model <- mboost(formula = sgb_formula, data = df)
  sgb_varimp <- sgboost::get_varimp(sgb_model)
  expect_equal(is.data.frame(sgb_varimp$varimp), TRUE)
  expect_equal(is.data.frame(sgb_varimp$group_importance), TRUE)
  expect_equal(dim(sgb_varimp$group_importance)[2], 2)
  expect_equal(
    tibble(
      reduction = c(0.588, 1.62),
      blearner = c(
        "bols(x1, intercept = FALSE, df = 0.3)",
        "bols(x4, x5, intercept = FALSE, df = 0.7)"
      ),
      predictor = c("x1", "x4, x5"),
      selfreq = c(0.41, 0.59),
      type = c("individual", "group"),
      relative_importance = c(0.2668, 0.7332)
    ),
    sgb_varimp$varimp,
    tolerance = 0.011
  )
  expect_equal(
    tibble(
      type = c("group", "individual"),
      importance = c(0.7332, 0.2668)
    ),
    sgb_varimp$group_importance,
    tolerance = 0.011
  )
})
