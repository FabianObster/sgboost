#' Variable importance bar plot of a sparse group boosting model
#'
#' @description
#' Visualizes the variable importance of a sparse-group boosting model.
#' Color indicates if a predictor is an individual variable or a group.
#' @details
#' Note that aggregated group and individual variable importance printed in the legend is based
#' only on the plotted variables and not on all variables that were selected in the sparse-group
#' boosting model.
#'
#' @param sgb_model Model of type `mboost` to plot the variable importance.
#' @param prop Numeric value indicating the minimal importance a predictor/baselearner has to have.
#' Default value is zero, meaning all predictors are plotted. By increasing prop the number of
#' plotted variables can be reduced. One can also use `'n_predictors'` for limiting
#' the number of variables to be plotted directly.
#' @param n_predictors The maximum number of predictors to be plotted. Default is 30.
#' Alternative to `'prop'`.
#' @param max_char_length The maximum character length of a predictor to be printed.
#' Default is 15. For larger groups or long variable names one may adjust this number to
#' differentiate variables from groups.
#' @param base_size The `base_size` argument to be passed to the `ggplot2` theme
#' [ggplot2::theme_bw] to be used to control the overall size of the figure.
#' Default value is 8.
#' @importFrom dplyr filter  arrange mutate group_by ungroup order_by %>%
#' @importFrom rlang .data
#' @import ggplot2
#'
#' @return object of type `ggplot2`.
#' @seealso [sgboost::get_varimp] which this function uses.
#'
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
#' sgb_formula <- as.formula(create_formula(alpha = 0.3, group_df = group_df))
#' sgb_model <- mboost(formula = sgb_formula, data = df)
#' sgb_varimp <- plot_varimp(sgb_model)
plot_varimp <- function(sgb_model, prop = 0, n_predictors = 30, max_char_length = 15,
                        base_size = 8) {
  stopifnot("Model must be of class mboost" = class(sgb_model) == "mboost")
  stopifnot("prop must be numberic" = is.numeric(prop))
  stopifnot("prop must be between zero and one" = prop <= 1 & prop >= 0)
  stopifnot("n_predictors must be a positive number" =
              is.numeric(n_predictors) & n_predictors > 0)
  stopifnot(
    "max_char_length must be a positive number" =
      is.numeric(max_char_length) & max_char_length > 0
  )
  sgb_varimp <- get_varimp(sgb_model)
  plotdata <- sgb_varimp$varimp %>%
    dplyr::arrange(-.data$relative_importance) %>%
    dplyr::mutate(cum_importance = cumsum(.data$relative_importance)) %>%
    dplyr::filter(.data$relative_importance >= prop) %>%
    dplyr::slice(1:n_predictors) %>%
    dplyr::group_by(.data$type) %>%
    dplyr::mutate(total_importance = sum(.data$relative_importance)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      type_label = paste0(.data$type, " (", round(.data$total_importance, 2), ")"),
      predictor = substr(.data$predictor, 1, max_char_length)
    )
  if (sum(nchar(sgb_varimp$varimp$predictor) > max_char_length)) {
    message("The number characters of some predictors were reduced.
            Adjust with max_char_length")
  }
  if (dim(plotdata)[1] < dim(sgb_varimp$varimp)[1]) {
    message(paste0(
      dim(sgb_varimp$varimp)[1] - dim(plotdata)[1],
      " predictors were removed. Use prop or n_predictors to change"
    ))
  }
  plot_out <- plotdata %>%
    ggplot2::ggplot(aes(
      x = dplyr::order_by(.data$predictor,as.numeric(.data$relative_importance)),
      fill = .data$type_label, y = .data$relative_importance
    )) +
    ggplot2::geom_col() +
    coord_flip() +
    xlab("Predictor") +
    ylab("Relative importance") +
    theme_bw(base_size = base_size) +
    theme(legend.title = element_blank())
  return(plot_out)
}
