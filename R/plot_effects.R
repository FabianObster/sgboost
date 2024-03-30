#' Variable importance of a sparse group boosting model
#'
#' @description
#' Returns the variable importance of a sparse-group mboost model in a dataframe.
#'
#' @param sgb_model mboost model to compute the variable importance for.
#' @importFrom dplyr filter mutate %>%
#' @importFrom stringr str_detect str_replace
#' @importFrom mboost varimp
#' @importFrom rlang .data
#'
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

plot_effects <- function(sgb_model) {
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
  df$y <- -df$x1+df$x4+df$x5
  group_df <- data.frame(
   group_name = c(1,1,1,2,2),
   var_name = c('x1','x2','x3','x4','x5')
  )

  sgb_formula <- as.formula(create_formula(alpha = 0.3, group_df = group_df))
  sgb_model <- mboost(formula = sgb_formula, data = df)
  sgb_varimp <- get_varimp(sgb_model)$varimp %>%
    arrange(-relative_importance) %>%
    mutate(cum_importance = cumsum(relative_importance))
  sgb_effects <- get_coef(sgb_model)$raw
  plotdata <- left_join(sgb_effects, sgb_varimp, by = c('predictor','blearner', 'type'))
  plotdata <- plotdata %>%
    mutate(x = abs(effect)*cos(cum_importance*2*pi),
           y = abs(effect)*sin(cum_importance*2*pi),
           xend = 0, yend = 0) #%>%
    #bind_rows(tibble(variable = as.character(sgb_formula[[2]]), x = 0,y = 0, xend = 0, yend = 0, type = 'outcome'))
  max_lim <- max(abs(c(plotdata$x,plotdata$y)))
  max_diam <- max(abs(plotdata$effect))
  plotdata %>%
  ggplot(aes(x = x, y = y, xend = x*0.1, yend = y*0.1)) +
    geom_segment(arrow = arrow(length = unit(6,'pt')), aes(color = type)) +
    theme_classic() +
    theme(legend.title=element_blank())  +
    ylim(c(-max_diam*1.2,max_diam*1.2)) + xlim(c(-max_diam*1.2,max_diam*1.2)) +
    coord_equal() + ggforce::geom_circle(aes(x0 = 0,y0=0, r = max_lim*1.15), linetype = 2,
                                         color = 'lightgray') +
    ggforce::geom_circle(aes(x0 = 0,y0=0, r = max_lim*1.15*0.5), linetype = 3,
                         color = 'lightgray') +
    geom_segment(aes(x = 0, y = 0, xend = max_lim*1.15,yend = 0), data = data.frame(),
                 linetype = 2, color = 'grey') +
    geom_curve(aes(x = x, y = y, xend = max_lim*1.15*cos(pi*0.05), yend = max_lim*1.15*sin(pi*0.05)),
               data = data.frame(x = max_lim*1.15, y = 0, xend = max_lim*1.15, yend = 0.25),
               arrow = arrow(length = unit(6, 'pt')), angle = 0, color = 'grey') +
    geom_label(aes(color = type, label = variable),
               fontface = "bold", alpha = 0.9) +
    geom_label(aes(x = x/2, y = y/2,label = round(effect,2)), size = 2, alpha = 0.9) +
    geom_label(aes(x=0,y= 0, label = as.character(sgb_formula[[2]])), alpha = 0.5)
  return(list(varimp = sgb_varimp, group_importance = group_importance))
}
