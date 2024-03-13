#' Create a sparse-group boosting formula
#'
#' @description
#' Creates a mboost formula that allows to fit a sparse group boosting model based on
#' boosted Ridge Regression with mixing parameter alpha. The formula consists of a
#' group baselearner part with degrees of freedom
#' 1-alpha and individual baselearners with degrees of freedom alpha.
#'
#' @param alpha mixing parameter. For alpha = 0 only group baselearners and for
#' alpha = 1 only idividual baselearners are defined.
#' @param group_df data frame containing variable names with group structure
#' @param var_name name of column in group_df containing the variable names
#' to be used as predictors
#' @param group_name name of column in group_df indicating the group structure of the variables
#' @param blearner type of baselearner
#' @param outcome_name name of dependent variable
#' @importFrom dplyr filter select group_by summarize mutate %>%
#' @importFrom stats as.formula
#'
#' @return formula to be passed to mboost() yielding the sparse group boosting for a given value mixing parameter alpha.
#' @export
#'
#' @examples
#' library(mboost)
#' library(tidyverse)
#' set.seed(1)
#' df <- data.frame(
#'  x1 = rnorm(100),x2 = rnorm(100),x3 = rnorm(100),
#'  x4 = rnorm(100), x5 = runif(100)
#'  )
#' df <- df %>%
#' mutate_all(function(x){as.numeric(scale(x))})
#' df$y <- df$x1+df$x4+df$x5
#' group_df <- data.frame(
#'  group_name = c(1,1,1,2,2),
#'  var_name = c('x1','x2','x3','x4','x5')
#' )
#'
#' sgb_formula <- create_formula(alpha = 0.3, group_df = group_df)
#' sgb_model <- mboost(formula = sgb_formula, data = df)
#' summary(sgb_model)
#' plot(sgb_model)
create_formula <- function(alpha = 0.05, group_df = NULL, blearner = "bols",
                           outcome_name = "y", group_name = "group_name",
                           var_name = "var_name") {
  stopifnot('Mixing parameter alpha must be numeric' = is.numeric(alpha))
  stopifnot('Mixing parameter alpha must between zero and one' = (alpha >= 0 & alpha <= 1))
  stopifnot('group_df must be a data.frame' = is.data.frame(group_df))
  stopifnot('group_name and var_name have to be columns of group_df' = (group_name %in% colnames(group_df) &
               var_name %in% colnames(group_df)))
    formula_group <- group_df %>%
      dplyr::select(group_name,var_name) %>%
      dplyr::group_by(group_name) %>%
      dplyr::summarize(var_name = paste0(var_name, collapse = " , ")) %>%
      dplyr::mutate(term = paste0(blearner, "(", var_name, ", df = ",
                                  (1 - alpha), ", intercept=F)"))
  formula <- paste0(paste0(blearner, "(", group_df$var_name, ", df = ",
                           alpha, ", intercept=F)"),collapse = '+')
  formula_group <- paste0(formula_group$term, collapse = " + ")
  if (alpha == 0) {
    final_formula <- formula_group
  } else if (alpha == 1) {
    final_formula <- formula
  } else {
    final_formula <- paste0(formula, " + ", formula_group)
  }
  final_formula <- stats::as.formula(paste0(outcome_name, '~', final_formula))
  return(final_formula)
}
