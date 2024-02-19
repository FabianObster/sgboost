#' Create a sgb formula
#'
#' @param alpha mixing parameter
#' @param group_df data frame containing variable names with group structure
#' @param var_name name of column in group_df containing the variable names
#' @param group_name name of column in group_df indicating the group structure of the variables
#' @param blearner type of baselearner
#' @param outcome_name name of dependent variable
#' @importFrom dplyr filter select group_by summarize mutate %>%
#' @importFrom stats as.formula
#'
#' @return formula to be passed to mboost() yielding the sparse group boosting for a given value mixing parameter alpha.
#' @export
#'
create_formula <- function(alpha = 0.05, group_df = NULL, blearner = "bols",
                           outcome_name = "y", group_name = "group_name", var_name = "var_name") {
    formula_group <- group_df %>%
      dplyr::select(group_name,var_name) %>%
      dplyr::group_by(group_name) %>%
      dplyr::summarize(var_name = paste0(var_name, collapse = " , ")) %>%
      dplyr::mutate(term = paste0(blearner, "(", var_name, ", df = ", (1 - alpha), ", intercept=F)"))
  formula <- paste0(paste0(blearner, "(", group_df$var_name, ", df = ", alpha, ", intercept=F)"),collapse = '+')
  formula_group <- paste0(formula_group$term, collapse = " + ")
  if (alpha == 0) {
    final_formula <- formula_group
  }
  if (alpha == 1) {
    final_formula <- formula
  } else {
    final_formula <- paste0(formula, " + ", formula_group)
  }
  final_formula <- stats::as.formula(paste0(outcome_name, '~', final_formula))
  return(final_formula)
}
