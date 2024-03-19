#' Variable importance of a sparse group boosting model
#'
#' @description
#' Returns the variable importance of a sparse-group mboost model in a dataframe.
#'
#' @param sgb_model mboost model to compute the variable importance for.
#' @importFrom dplyr filter mutate %>%
#' @importFrom stringr str_detect
#' @importFrom mboost varimp
#' @importFrom rlang .data
#' @import ggnetwork
#'
#' @return list of two dataframes. varimp contains the variables, group structure and
#' variable importance on both group and individual variable basis.
#' group_importance contains the the aggregated relative importance
#' (relative reduction of loss-function) of all group baselearners and of all
#' individual variables
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(mboost)
#' library(dplyr)
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
#' sgb_formula <- as.formula(create_formula(alpha = 0.3, group_df = group_df))
#' sgb_model <- mboost(formula = sgb_formula, data = df)
#' sgb_varimp <- get_varimp(sgb_model)}

get_varimp <- function(sgb_model) {
  stopifnot('Model must be of class mboost' = class(sgb_model) == 'mboost')
  library(mboost)
  library(dplyr)
  library(ggnetwork)
  set.seed(1)
  df <- data.frame(
   x1 = rnorm(100),x2 = rnorm(100),x3 = rnorm(100),
   x4 = rnorm(100), x5 = runif(100)
   )
  df <- df %>%
  mutate_all(function(x){as.numeric(scale(x))})
  df$y <- df$x1+df$x4+df$x5
  group_df <- data.frame(
   group_name = c(1,1,1,2,2),
   var_name = c('x1','x2','x3','x4','x5')
  )

  sgb_formula <- as.formula(create_formula(alpha = 0.3, group_df = group_df))
  sgb_model <- mboost(formula = sgb_formula, data = df)
  sgb_varimp <- get_varimp(sgb_model)

  return(list(varimp = sgb_varimp, group_importance = group_importance))
}
