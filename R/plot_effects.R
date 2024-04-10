#' Visualizing a sparse-group boosting model
#'
#' @description
#' Radar or scatter/lineplot visualizing the effects sizes relative to the variable importance
#' in a sparse-group boosting model. Works also for a regular mboost model.
#'
#' @param sgb_model Model of type `mboost` to be used.
#' @param plot_type String indicating the type of visualization to use.
#' `'radar'` refers to a radar plot using polar coordinates.
#' Here the angle is relative to the cumulative relative importance of predictors and
#' the radius is proportional to the effect size. `"clock"` does the same as `"radar"` but uses clock coordinates
#' instead of polar coordinates. `"scatter"` uses the effect size as y-coordinate and the cumulative relative
#' importance as x-axis in a classical Scatter plot.
#' @param prop Numeric value indicating the minimal importance a predictor/baselearner has to have to be plotted.
#' Default value is zero, meaning all predictors are plotted. By increasing prop the number of
#' plotted variables can be reduced. One can also use `n_predictors` for limiting
#' the number of variables to be plotted directly.
#' @param n_predictors The maximum number of predictors to be plotted. Default is 30.
#' Alternative to `prop`.
#' @param max_char_length The maximum character length of a predictor to be printed.
#' Default is 5. For long variable names one may adjust this number.
#' @param base_size The `base_size` argument to be passed to the `ggplot2` theme
#' [ggplot2::theme_classic] to be used to control the overall size of the figure.
#' Default value is 8.
#' @importFrom dplyr filter mutate case_when %>%
#' @importFrom rlang .data
#' @importFrom ggforce geom_circle
#' @import ggplot2
#'
#' @return `ggplot2` object mapping the effect sizes and variable importance.
#' @seealso [sgboost::get_coef()], [sgboost::get_varimp()] which this function uses.
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
#' plot_effects(sgb_model)
plot_effects <- function(sgb_model, plot_type = "radar", prop = 0, n_predictors = 30,
                         max_char_length = 5, base_size = 8) {
  stopifnot("Model must be of class mboost" = class(sgb_model) == "mboost")
  stopifnot("prop must be numberic" = is.numeric(prop))
  stopifnot("prop must be between zero and one" = prop <= 1 & prop >= 0)
  stopifnot(
    "n_predictors must be a positive number" =
      is.numeric(n_predictors) & n_predictors > 0
  )
  stopifnot(
    "max_char_length must be a positive number" =
      is.numeric(max_char_length) & max_char_length > 0
  )
  sgb_varimp <- get_varimp(sgb_model)$varimp %>%
    dplyr::arrange(-.data$relative_importance) %>%
    dplyr::mutate(cum_importance = cumsum(.data$relative_importance)) %>%
    dplyr::filter(.data$relative_importance >= prop) %>%
    dplyr::slice(1:n_predictors) %>%
    dplyr::mutate(predictor = dplyr::case_when(
      .data$predictor == "(Intercept)" ~ "I",
      T ~ .data$predictor
    ))
  sgb_effects <- get_coef(sgb_model)$raw
  plotdata <- dplyr::inner_join(sgb_effects, sgb_varimp,
    by = c("predictor", "blearner", "type")
  )
  if (sum(nchar(plotdata$variable) > max_char_length) >= 1) {
    message("The number characters of some predictors were reduced.
            Adjust with max_char_length")
  }
  if (dim(sgb_varimp)[1] < dim(get_varimp(sgb_model)$varimp)[1]) {
    message(paste0(
      dim(get_varimp(sgb_model)$varimp)[1] - dim(sgb_varimp)[1],
      " predictors were removed. Use prop or n_predictors to change"
    ))
  }
  if (plot_type == "radar") {
    plotdata <- plotdata %>%
      dplyr::mutate(
        x = abs(.data$effect) * cos(.data$cum_importance * 2 * pi),
        y = abs(.data$effect) * sin(.data$cum_importance * 2 * pi),
        xend = 0, yend = 0, variable = substr(.data$variable, 1, max_char_length)
      )
    max_lim <- max(abs(c(plotdata$x, plotdata$y)))
    max_diam <- max(abs(plotdata$effect))
    plot_out <- plotdata %>%
      ggplot2::ggplot(aes(x = .data$x, y = .data$y, xend = .data$x * 0.13, yend = .data$y * 0.13)) +
      ggplot2::geom_segment(arrow = arrow(length = unit(6, "pt")), aes(color = .data$type)) +
      ggplot2::theme_classic(base_size = base_size) +
      ggplot2::theme(legend.title = element_blank()) +
      ggplot2::ylim(c(-max_diam * 1.2, max_diam * 1.2)) +
      xlim(c(-max_diam * 1.2, max_diam * 1.2)) +
      ggplot2::coord_equal() +
      ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = max_lim * 1.15),
        linetype = 2,
        color = "lightgray"
      ) +
      ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = max_lim * 1.15 * 0.5),
        linetype = 3,
        color = "lightgray"
      ) +
      ggplot2::geom_segment(aes(x = 0, y = 0, xend = max_lim * 1.15, yend = 0),
        data = data.frame(),
        linetype = 2, color = "grey"
      ) +
      ggplot2::geom_curve(
        aes(
          x = .data$x, y = .data$y,
          xend = max_lim * 1.15 * cos(pi * 0.05),
          yend = max_lim * 1.15 * sin(pi * 0.05)
        ),
        data = data.frame(x = max_lim * 1.15, y = 0, xend = max_lim * 1.15, yend = 0.25),
        arrow = arrow(length = unit(6, "pt")), angle = 0, color = "grey"
      ) +
      ggplot2::geom_label(aes(color = .data$type, label = .data$variable),
        fontface = "bold", alpha = 0.9, size = base_size / 4
      ) +
      ggplot2::geom_label(aes(
        x = .data$x / 2, y = .data$y / 2,
        label = round(.data$effect, 2)
      ), size = base_size / 6, alpha = 0.9) +
      ggplot2::geom_label(aes(x = 0, y = 0, label = "y"), alpha = 0.5, size = base_size / 2) +
      ggplot2::xlab("") +
      ggplot2::ylab("")
  } else if (plot_type == "clock") {
    plotdata <- plotdata %>%
      dplyr::mutate(
        x = abs(.data$effect) * cos(-.data$cum_importance * 1.5 * pi),
        y = abs(.data$effect) * sin(-.data$cum_importance * 1.5 * pi),
        xend = 0, yend = 0, variable = substr(.data$variable, 1, max_char_length)
      )
    max_lim <- max(abs(c(plotdata$x, plotdata$y)))
    max_diam <- max(abs(plotdata$effect))
    plot_out <- plotdata %>%
      ggplot2::ggplot(aes(
        x = .data$x, y = .data$y,
        xend = .data$x * 0.13, yend = .data$y * 0.13
      )) +
      ggplot2::geom_segment(
        arrow = arrow(length = unit(6, "pt")),
        aes(color = .data$type)
      ) +
      ggplot2::theme_classic(base_size = base_size) +
      ggplot2::theme(legend.title = element_blank()) +
      ggplot2::ylim(c(-max_diam * 1.2, max_diam * 1.2)) +
      xlim(c(-max_diam * 1.2, max_diam * 1.2)) +
      ggplot2::coord_equal() +
      ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = max_lim * 1.15),
        linetype = 2,
        color = "lightgray"
      ) +
      ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = max_lim * 1.15 * 0.5),
        linetype = 3,
        color = "lightgray"
      ) +
      ggplot2::geom_segment(aes(x = 0, y = 0, xend = 0, yend = max_lim * 1.15),
        data = data.frame(),
        linetype = 2, color = "grey"
      ) +
      ggplot2::geom_curve(
        aes(
          x = .data$x, y = .data$y,
          xend = max_lim * 1.15 * cos(pi * 0.45),
          yend = max_lim * 1.15 * sin(pi * 0.45)
        ),
        data = data.frame(
          x = 0, y = max_lim * 1.15,
          xend = max_lim * 1.15, yend = 0.25
        ),
        arrow = arrow(length = unit(6, "pt")), angle = 0, color = "grey"
      ) +
      ggplot2::geom_label(aes(color = .data$type, label = .data$variable),
        fontface = "bold", alpha = 0.9, size = base_size / 4
      ) +
      ggplot2::geom_label(aes(
        x = .data$x / 2, y = .data$y / 2,
        label = round(.data$effect, 2)
      ), size = base_size / 6, alpha = 0.9) +
      ggplot2::geom_label(aes(x = 0, y = 0, label = "y"), alpha = 0.5, size = base_size / 2) +
      ggplot2::xlab("") +
      ggplot2::ylab("")
  } else if (plot_type == "scatter") {
    plotdata <- plotdata %>%
      dplyr::mutate(variable = substr(.data$variable, 1, max_char_length))
    plot_out <- plotdata %>%
      ggplot2::ggplot(aes(x = .data$cum_importance, y = .data$effect)) +
      ggplot2::theme_classic(base_size = base_size) +
      ggplot2::theme(legend.title = element_blank()) +
      ggplot2::geom_curve(aes(
        x = .data$cum_importance, y = 0, xend = .data$cum_importance,
        yend = .data$effect, color = .data$type
      ), angle = 0) +
      ggplot2::geom_label(aes(
        x = .data$cum_importance, y = .data$effect / 2,
        label = round(.data$effect, 2)
      ), size = base_size / 6, alpha = 0.9) +
      ggplot2::geom_label(aes(color = .data$type, label = .data$variable),
        fontface = "bold", alpha = 0.9, size = base_size / 4
      ) +
      ggplot2::xlim(c(0, 1)) +
      ggplot2::xlab("Cumulative relative importance") +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
      ggplot2::ylab("Effect size")
  }
  return(plot_out)
}
