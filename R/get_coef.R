#' Aggregated and raw coefficients in a sparse group boosting model
#'
#' @description
#' Computes the aggregated coefficients from group and individual baselearners.
#' Also returns the raw coefficients associated with each baselearner.
#' @details in a sparse group boosting models a variable in a dataset can be selected
#' as an individual variable or as a group. Therefore there can be two associated effect sizes for the
#' same variable. This function aggregates both and returns it in a data.frame.
#'
#' @param sgb_model Model of type `mboost` to compute the coefficients for.
#' @importFrom dplyr filter mutate %>%
#' @importFrom tibble rownames_to_column tibble
#' @importFrom stringr str_replace str_detect
#'
#' @return List of data.frames containing the a data.frame `'$raw'` with the
#' variable and the raw (Regression) coefficients and the data.frame `'$aggregated'` with the
#' aggregated (Regression) coefficients.
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
#' sgb_coef <- get_coef(sgb_model)
get_coef <- function(sgb_model) {
  stopifnot("Model must be of class mboost" = class(sgb_model) == "mboost")
  sgb_coef <- sgb_model$coef()
  coef_df <- sgb_model$coef() %>%
    seq_along() %>%
    lapply(function(i) {
      as.data.frame(sgb_model$coef()[[i]]) %>%
        tibble::rownames_to_column() %>%
        mutate(blearner = names(sgb_model$coef())[i])
    }) %>%
    dplyr::bind_rows() %>%
    tibble()
  colnames(coef_df)[1:2] <- c("variable", "effect")
  coef_df <- coef_df %>%
    mutate(
      predictor = str_replace(.data$blearner, ",[^,]*=.*", ""),
      predictor = str_replace(.data$predictor, "bols\\(", ""),
      predictor = str_replace(.data$predictor, "\\)", ""),
      type = dplyr::case_when(
        stringr::str_detect(predictor, ",") ~ "group",
        T ~ "individual"
      )
    ) %>%
    dplyr::arrange(-abs(.data$effect)) %>%
    dplyr::filter(.data$effect != 0)
  coef_df_aggregate <- coef_df %>%
    dplyr::group_by(.data$variable) %>%
    dplyr::reframe(
      effect = sum(.data$effect),
      blearner = paste0(.data$blearner, collapse = "; "),
      predictor = paste0(.data$predictor, collapse = "; ")
    ) %>%
    dplyr::arrange(-abs(.data$effect)) %>%
    dplyr::filter(.data$effect != 0)
  return(list(raw = coef_df, aggregated = coef_df_aggregate))
}
