#' Coefficient path of a sparse-group boosting model
#'
#' @description
#' Shows how the effect sizes change throughout the boosting iterations
#' in a sparse-group boosting model. Works also for a regular mboost models.
#' Color indicates the selection of group or individual variables within a boosting iteration.
#'
#' @param sgb_model Model of type `mboost` to be used.
#' @param max_char_length The maximum character length of a predictor to be printed.
#' Default is 5. For long variable names one may adjust this number.
#' @param base_size The `base_size` argument to be passed to the `ggplot2` theme
#' [ggplot2::theme_bw] to be used to control the overall size of the figure.
#' Default value is 8.
#' @importFrom dplyr filter mutate case_when %>%
#' @importFrom mboost mstop
#' @importFrom rlang .data
#' @import ggplot2
#'
#' @returns `ggplot2` object mapping the effect sizes and variable importance.
#' @seealso [sgboost::get_coef_path()] which this function uses.
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
#' sgb_formula <- as.formula(create_formula(alpha = 0.4, group_df = group_df))
#' sgb_model <- mboost(formula = sgb_formula, data = df)
#' plot_path(sgb_model)
plot_path <- function(sgb_model, max_char_length = 5, base_size = 8) {
  stopifnot("Model must be of class mboost" = class(sgb_model) == "mboost")
  stopifnot(
    "max_char_length must be a positive number" =
      is.numeric(max_char_length) & max_char_length > 0
  )
 sgb_coef_path <- get_coef_path(sgb_model)
 plotdata <- sgb_coef_path$aggregated %>%
   dplyr::mutate(
   type = dplyr::case_when(
     stringr::str_detect(.data$predictor, ",") ~ "group",
     T ~ "individual"
   ))
 plot_out <- plotdata %>%
   ggplot2::ggplot(aes(x = .data$iteration, y = .data$effect, group = .data$variable, color = .data$type)) +
   ggplot2::geom_point(aes(color = .data$type), size = 0.2) + ggplot2::geom_line() + ggplot2::theme_bw(base_size = 8) +
   ggplot2::geom_label(aes(x = .data$iteration*1.02, y = .data$effect, label = .data$variable),
                       data = plotdata %>% dplyr::filter(.data$iteration == mboost::mstop(sgb_model)),
                       size = base_size/4) +
   ggplot2::theme(legend.title = element_blank()) +
   ggplot2::geom_hline(yintercept = 0, linetype = 'dashed', color = 'grey')
  return(plot_out)
}
