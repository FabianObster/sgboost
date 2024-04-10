#' Variable importance of a sparse-group boosting model
#'
#' @description
#'  Variable importance is computed as relative reduction of loss-function attributed
#'  to each predictor (groups and individual variables).
#'  Returns a list of two data.frames. The first contains the variable importance
#'  of a sparse-group model in a data.frame for each predictor.
#'  The second one contains the aggregated relative importance of all groups vs. individual variables.
#'
#' @param sgb_model Model of type `mboost` to compute the variable importance for.
#' @importFrom dplyr filter mutate %>%
#' @importFrom stringr str_detect
#' @importFrom mboost varimp
#' @importFrom rlang .data
#'
#' @return List of two data.frames. `$raw` contains the name of the variables, group structure and
#' variable importance on both group and individual variable basis.
#' `$group_importance` contains the the aggregated relative importance of all
#' group baselearners and of all individual variables.
#' @export
#' @seealso [mboost::varimp()] which this function uses.
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
#' sgb_formula <- as.formula(create_formula(alpha = 0.3, group_df = group_df))
#' sgb_model <- mboost(formula = sgb_formula, data = df)
#' sgb_varimp <- get_varimp(sgb_model)
get_varimp <- function(sgb_model) {
  stopifnot("Model must be of class mboost" = class(sgb_model) == "mboost")
  sgb_varimp <- mboost::varimp(sgb_model) %>%
    as.data.frame() %>%
    dplyr::rename("predictor" = "variable") %>%
    dplyr::filter(.data$reduction != 0) %>%
    dplyr::mutate(
      type = dplyr::case_when(
        stringr::str_detect(.data$predictor, ",") ~ "group",
        T ~ "individual"
      ),
      predictor = as.character(.data$predictor),
      blearner = as.character(.data$blearner)
    ) %>%
    dplyr::mutate(relative_importance = .data$reduction / sum(.data$reduction)) %>%
    dplyr::group_by(.data$type) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(-.data$relative_importance)
  group_importance <- sgb_varimp %>%
    dplyr::group_by(.data$type) %>%
    dplyr::summarize(importance = sum(.data$relative_importance)) %>%
    dplyr::arrange(-.data$importance)
  return(list(varimp = sgb_varimp, group_importance = group_importance))
}
