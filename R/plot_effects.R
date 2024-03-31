#' Variable importance of a sparse group boosting model
#'
#' @description
#' Returns the variable importance of a sparse-group mboost model in a dataframe.
#'
#' @param sgb_model mboost model to compute the variable importance for.
#' @param prop the maximum proportion of explained importance. Default value is one,
#' meaning all predictors are plotted. By setting prop smaller than one the number of
#' plotted variables can be reduced. One can also use 'nvars' for limiting
#' the number of variables to be plotted directly.
#' @param n_vars The maximum number of predictors to be plotted. Default is 30
#' @param max_char_length The maximum character length of a predictor to be printed.
#' Default is 5. For long variable names one may adjust this number.
#' @importFrom dplyr filter mutate %>%
#' @importFrom stringr str_detect str_replace
#' @importFrom mboost varimp
#' @importFrom rlang .data
#' @importFrom ggforce geom_circle
#' @import ggplot2
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

plot_effects <- function(sgb_model, prop = 1, n_vars = 30, max_char_length = 5) {
  stopifnot('Model must be of class mboost' = class(sgb_model) == 'mboost')
  # library(mboost)
  # library(dplyr)
  # set.seed(1)
  # df <- data.frame(
  #  x1 = rnorm(100),x2 = rnorm(100),x3 = rnorm(100),
  #  x4 = rnorm(100), x5 = runif(100)
  #  )
  # df <- df %>%
  # mutate_all(function(x){as.numeric(scale(x))})
  # df$y <- -df$x1+df$x4+df$x5
  # group_df <- data.frame(
  #  group_name = c(1,1,1,2,2),
  #  var_name = c('x1','x2','x3','x4','x5')
  # )
  # sgb_formula <- as.formula(create_formula(alpha = 0.3, group_df = group_df))
  # sgb_model <- mboost(formula = sgb_formula, data = df)
  sgb_varimp <- get_varimp(sgb_model)$varimp %>%
    dplyr::arrange(-.data$relative_importance) %>%
    dplyr::mutate(cum_importance = cumsum(.data$relative_importance)) %>%
    dplyr::filter(.data$cum_importance <= prop, .data$cum_importance <= n_vars)
  sgb_effects <- get_coef(sgb_model)$raw
  plotdata <- dplyr::inner_join(sgb_effects, sgb_varimp, by = c('predictor','blearner', 'type'))
  if(sum(nchar(plotdata$variable) > max_char_length) >= 1){
    message('The number characters of some predictors were reduced.
            Adjust with max_char_length')
  }
  if(dim(sgb_varimp)[1] < dim(get_varimp(sgb_model)$varimp)[1]){
    message(paste0(dim(sgb_varimp$varimp)[1]-dim(plotdata)[1],
                   ' predictors were removed. Use prop or n_vars to change'))
  }
  plotdata <- plotdata %>%
    dplyr::mutate(x = abs(.data$effect)*cos(.data$cum_importance*2*pi),
           y = abs(.data$effect)*sin(.data$cum_importance*2*pi),
           xend = 0, yend = 0, variable = substr(.data$variable, 1, max_char_length)
           )
  max_lim <- max(abs(c(plotdata$x,plotdata$y)))
  max_diam <- max(abs(plotdata$effect))
  plot_out <- plotdata %>%
  ggplot2::ggplot(aes(x = .data$x, y = .data$y, xend = .data$x*0.13, yend = .data$y*0.13)) +
    ggplot2::geom_segment(arrow = arrow(length = unit(6,'pt')), aes(color = .data$type)) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.title=element_blank()) +
    ggplot2::ylim(c(-max_diam*1.2,max_diam*1.2)) + xlim(c(-max_diam*1.2,max_diam*1.2)) +
    ggplot2::coord_equal() + ggforce::geom_circle(aes(x0 = 0,y0=0, r = max_lim*1.15), linetype = 2,
                                         color = 'lightgray') +
    ggforce::geom_circle(aes(x0 = 0,y0=0, r = max_lim*1.15*0.5), linetype = 3,
                         color = 'lightgray') +
    ggplot2::geom_segment(aes(x = 0, y = 0, xend = max_lim*1.15,yend = 0), data = data.frame(),
                 linetype = 2, color = 'grey') +
    ggplot2::geom_curve(aes(x = .data$x, y = .data$y, xend = max_lim*1.15*cos(pi*0.05), yend = max_lim*1.15*sin(pi*0.05)),
               data = data.frame(x = max_lim*1.15, y = 0, xend = max_lim*1.15, yend = 0.25),
               arrow = arrow(length = unit(6, 'pt')), angle = 0, color = 'grey') +
    ggplot2::geom_label(aes(color = .data$type, label = .data$variable),
               fontface = "bold", alpha = 0.9) +
    ggplot2::geom_label(aes(x = .data$x/2, y = .data$y/2,label = round(.data$effect,2)), size = 2, alpha = 0.9) +
    ggplot2::geom_label(aes(x=0,y= 0, label = 'y'), alpha = 0.5) +
    ggplot2::xlab('') + ggplot2::ylab('')
  return(plot_out)
}
