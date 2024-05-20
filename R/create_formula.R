#' Create a sparse-group boosting formula
#'
#' @description
#' Creates a `mboost` formula that allows to fit a sparse-group boosting model based on
#' boosted Ridge Regression with mixing parameter `alpha`. The formula consists of a
#' group baselearner part with degrees of freedom
#' 1-`alpha` and individual baselearners with degrees of freedom `alpha`.
#' Groups should be defined through `group_df`. The corresponding modeling data
#' should not contain categorical variables with more than two categories,
#' as they are then treated as a group only.
#'
#' @param alpha Numeric mixing parameter. For alpha = 0 only group baselearners and for
#' alpha = 1 only individual baselearners are defined.
#' @param group_df input data.frame containing variable names with group structure.
#' @param var_name Name of column in group_df containing the variable names
#' to be used as predictors. Default is `"var_name"`. should not contain categorical
#' variables with more than two categories, as they are then treated as a group only.
#' @param group_name Name of column in group_df indicating the group structure of the variables.
#' Default is `"group_name`.
#' @param blearner Type of baselearner. Default is `'bols'`.
#' @param outcome_name String indicating the name of dependent variable. Default is `"y"`
#' @param intercept Logical, should intercept be used?
#' @importFrom dplyr select group_by summarize mutate %>%
#' @importFrom rlang .data
#' @return Character containing the formula to be passed to [mboost::mboost()]
#'  yielding the sparse-group boosting for a given value mixing parameter `alpha`.
#' @export
#'
#' @examples
#' library(mboost)
#' library(dplyr)
#' set.seed(1)
#' df <- data.frame(
#'   x1 = rnorm(100), x2 = rnorm(100), x3 = rnorm(100),
#'   x4 = rnorm(100), x5 = runif(100)
#' )
#' df <- df %>%
#'   mutate_all(function(x) {
#'     as.numeric(scale(x))
#'   })
#' df$y <- df$x1 + df$x4 + df$x5
#' group_df <- data.frame(
#'   group_name = c(1, 1, 1, 2, 2),
#'   var_name = c("x1", "x2", "x3", "x4", "x5")
#' )
#'
#' sgb_formula <- create_formula(alpha = 0.3, group_df = group_df)
#' sgb_model <- mboost(formula = sgb_formula, data = df)
#' summary(sgb_model)
create_formula <- function(alpha = 0.3, group_df = NULL, blearner = "bols",
                           outcome_name = "y", group_name = "group_name",
                           var_name = "var_name", intercept = FALSE) {
  stopifnot("Mixing parameter alpha must be numeric" = is.numeric(alpha))
  stopifnot("Mixing parameter alpha must between zero and one" = (alpha >= 0 & alpha <= 1))
  stopifnot("group_df must be a data.frame" = is.data.frame(group_df))
  stopifnot(
    "group_name and var_name have to be columns of group_df" =
      (group_name %in% colnames(group_df) &
        var_name %in% colnames(group_df))
  )
  if (blearner != "bols") {
    warning("passing a baselearner other than bols does not guarantee
            that mboost() returns a sparse-group boosting model")
  }
  if (any(table(group_df$group_name) == 1)) {
    warning("there is a group containing only one variable.
            It will be treated as individual variable and as group")
  }
  var_names <- group_names <- NULL
  formula_df <- group_df
  formula_df$var_names <- group_df[[var_name]]
  formula_df$group_names <- group_df[[group_name]]
  formula_group <- formula_df %>%
    dplyr::select(var_names, group_names) %>%
    dplyr::group_by(.data$group_names) %>%
    dplyr::summarize(var_names = paste0(.data$var_names, collapse = " , ")) %>%
    dplyr::mutate(term = paste0(
      blearner, "(", .data$var_names, ", df = ",
      (1 - alpha), ", intercept=", intercept, ")"
    ))
  formula <- paste0(paste0(
    blearner, "(", formula_df$var_names, ", df = ",
    alpha, ", intercept=", intercept, ")"
  ), collapse = "+")
  formula_group <- paste0(formula_group$term, collapse = "+")
  if (alpha == 0) {
    final_formula <- formula_group
  } else if (alpha == 1) {
    final_formula <- formula
  } else {
    final_formula <- paste0(formula, " + ", formula_group)
  }
  final_formula <- paste0(outcome_name, "~", final_formula)
  return(formula(final_formula))
}
