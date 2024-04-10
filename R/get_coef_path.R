#' Path of aggregated and raw coefficients in a sparse-group boosting model
#'
#' @description
#' Computes the aggregated coefficients from group and individual baselearners for each boosting iteration.
#' @details in a sparse-group boosting models a variable in a dataset can be selected
#' as an individual variable or as a group. Therefore there can be two associated effect sizes for the
#' same variable. This function aggregates both and returns it in a data.frame for each boosting iteration
#'
#' @param sgb_model Model of type `mboost` to compute the coefficient path for .
#' @importFrom dplyr mutate %>%
#'
#' @return List of data.frames containing the a data.frame `$raw` with the
#' variable and the raw (Regression) coefficients and the data.frame `$aggregated` with the
#' aggregated (Regression) coefficients.
#' @export
#' @seealso [get_coef()]
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
#' sgb_coef_path <- get_coef_path(sgb_model)
get_coef_path <- function(sgb_model) {
  stopifnot("Model must be of class mboost" = class(sgb_model) == "mboost")
  initial_mstop <- mboost::mstop(sgb_model)
  coef_path <- get_coef(sgb_model) %>%
    lapply(function(x){x %>% mutate(iteration = mboost::mstop(sgb_model))})
  for(i in (mboost::mstop(sgb_model)-1):1){
    coef_path$raw <- coef_path$raw %>%
      dplyr::bind_rows(get_coef(sgb_model[i])$raw %>% mutate(iteration = i))
    coef_path$aggregated <- coef_path$aggregated %>%
      dplyr::bind_rows(get_coef(sgb_model[i])$aggregated %>% mutate(iteration = i))
  }
  mboost::mstop(sgb_model) <- initial_mstop
  return(coef_path)
}
