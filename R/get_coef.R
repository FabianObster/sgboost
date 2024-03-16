#' Aggregated coefficients in a sparse group boosting model
#'
#' @description
#' Computes the aggregate coefficients from group and individual baselearners
#'
#' @param sgb_model mboost model to compute the variable importance for.
#' @importFrom dplyr filter mutate %>%
#' @mportFrom tibble rownames_to_column
#'
#' @return dataframe containing the variable and the aggregated (Regression) coefficients
#' @export
#'
#' @examples
#' \dontrun{
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
#' sgb_varimp <- get_varimp(sgb_model)}

get_coef <- function(sgb_model) {
  stopifnot('Model must be of class mboost' = class(sgb_model) == 'mboost')
  sgb_coef <- sgb_model$coef()
  coef_df <- sgb_model$coef() %>%
    seq_along() %>%
    lapply(function(i){
      as.data.frame(sgb_model$coef()[[i]]) %>%
        tibble::rownames_to_column() %>%
      mutate(blearner = names(sgb_model$coef())[i])
      }) %>%
    dplyr::bind_rows()
  colnames(coef_df)[1:2] <- c('variable', 'effect')
  coef_df <- coef_df %>%
    dplyr::group_by(.data$variable) %>%
    dplyr::reframe(effect = sum(.data$effect),
                   blearner = paste0(.data$blearner, collapse = '; ')) %>%
    dplyr::arrange(abs(.data$effect))
  return(coef_df)
}
