#' Balances selection frequencies for unequal groups
#'
#' @description
#' Returns optimal degrees of freedom for group boosting to achieve more balanced variables selection.
#' Groups should be defined through `group_df`. Each base_learner
#'
#' @param df data.frame to be analyzed
#' @param group_df input data.frame containing variable names with group structure.
#'  All variables in `df` to used in the analysis must be present in this data.frame.
#' @param var_name Name of column in group_df containing the variable names
#' to be used as predictors. Default is `"var_name"`. should not contain categorical
#' variables with more than two categories, as they are then treated as a group only.
#' @param group_name Name of column in group_df indicating the group structure of the variables.
#' Default is `"group_name`.
#' @param n_reps Number of samples to be drawn in each iteration
#' @param iterations Number of iterations performed in the algorithm. Default is `"20"`
#' @param nu Learning rate as the step size to move away from the current estimate.
#' Default is `0.5`
#' @param red_fact Factor by which the learning rate is reduced if the algorithm overshoots,
#' meaning the loss increases. Default is `0.9`
#' @param min_weights The minimum weight size to be used. Default is `0.01`
#' @param max_weights The maximum weight size to be used. Default is `0.99`
#' @param blearner Type of baselearner. Default is `'bols'`.
#' @param outcome_name String indicating the name of dependent variable. Default is `"y"`
#' @param intercept Logical, should intercept be used?
#' @importFrom dplyr select group_by mutate filter bind_rows case_when %>%
#' @importFrom stringr str_replace_all
#' @importFrom rlang .data
#' @importFrom mboost mboost boost_control
#' @importFrom stats rnorm
#' @return Character containing the formula to be passed to [mboost::mboost()]
#'  yielding the sparse-group boosting for a given value mixing parameter `alpha`.
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
#' summary(sgb_model)
balance <- function(df = NULL, group_df = NULL, blearner = "bols",
                           outcome_name = "y", group_name = "group_name",
                           var_name = "var_name", n_reps = 3000, iterations = 15,
                    nu = 0.5, red_fact = 0.9, min_weights = 0.01,
                    max_weights = 0.99, intercept = TRUE) {
  stopifnot("group_df must be a data.frame" = is.data.frame(group_df))
  stopifnot(
    "group_name and var_name have to be columns of group_df" =
      (group_name %in% colnames(group_df) &
        var_name %in% colnames(group_df))
  )
  if (blearner != "bols") {
    warning("passing a baselearner other than bols does not guarantee
            that mboost() returns a sparse-group boosting model")
  }
  sample_data <- function(df, distr_fun = 'normal'){
    if(distr_fun == 'normal'){
      ret_df <- df %>% dplyr::mutate(y_sim = rnorm(0,1, n = dim(df)[1]))
    } else if(distr_fun == 'binomial'){
      ret_df <- df %>% dplyr::mutate(y_sim = sample(c(0,1), size = dim(df)[1], replace = T))
    }
    return(ret_df)
  }
  iter_df <- group_df %>%
    dplyr::mutate(group_weights = 0.5, iter = 1, n = NA, perc = NA, corr = NA, error = NA)
  opt_iter <- 1
  set.seed(1)
  for(iteration in 2:iterations){
    mb_formula <- create_formula(alpha = 0, group_df = iter_df %>%
                                   dplyr::filter(.data$iter == iteration-1), group_weights = 'group_weights',
                                 outcome_name = 'y_sim', intercept = T)
    sel <- vector()
    for(reps in 1:n_reps){
      sim_df <- sample_data(df)
      mb_model <- mboost::mboost(as.formula(mb_formula), data = sim_df,
                         control = boost_control(mstop = 1))
      sel <- c(sel,get_varimp(mb_model)$varimp$predictor %>% unique())
    }
    sel_df <- data.frame(rep = 1:n_reps,selection = 1, var_name = stringr::str_replace_all(sel, ',.*', '')) %>%
      dplyr::right_join(group_df, by = 'var_name') %>%
      dplyr::filter(!is.na(.data$rep))
    sel_df <- sel_df %>%
      dplyr::group_by(.data$group_name, .data$var_name) %>%
      dplyr::tally() %>%
      dplyr::ungroup() %>%
      dplyr::mutate(perc = .data$n/sum(.data$n), iter = iteration-1,
             corr = 1/length(unique(group_df$group_name))-.data$perc,
             error = sum(.data$corr^2)) %>%
      dplyr::right_join(iter_df %>% dplyr::filter(.data$iter == iteration-1) %>%
                          dplyr::select(-n, -perc, -corr,-error),
                 by = c('group_name', 'var_name', 'iter')) %>%
      dplyr::group_by(group_name) %>%
      dplyr::mutate(perc = max(.data$perc,na.rm=T), n = max(.data$n, na.rm = T),
             iter = max(.data$iter, na.rm = T), corr = max(.data$corr, na.rm = T),
             error = max(.data$error, na.rm = T))


    iter_df <- iter_df %>% dplyr::filter(.data$iter != iteration-1) %>% dplyr::bind_rows(sel_df)
    if(utils::tail(iter_df,1)$error == min(iter_df$error)){
      opt_iter <- iteration-1
      new_iter <- sel_df %>%
        dplyr::mutate(group_weights = .data$group_weights+nu*.data$corr)
    } else{
      nu <- nu*red_fact
      new_iter <- sel_df %>%
        dplyr::mutate(group_weights = .data$group_weights+nu*.data$corr)

      new_iter$opt_weights <- dplyr::filter(iter_df, .data$iter == opt_iter)$group_weights
      new_iter$opt_corr <- dplyr::filter(iter_df, .data$iter == opt_iter)$corr
      new_iter <- new_iter %>%
        dplyr::mutate(group_weights = 0.3*.data$opt_weights+0.7*(.data$group_weights+nu*.data$corr))
    }
    if(any(new_iter$group_weights > 1 | new_iter$group_weights < 0 )){

      new_iter <- new_iter %>%
        dplyr::mutate(group_weights = dplyr::case_when(.data$group_weights >= 1 ~ max_weights, .data$group_weights > 0 ~ min_weights))
    }
    iter_df <- iter_df %>%
      dplyr::bind_rows(new_iter %>%
                  dplyr::select(-n, -perc, -corr,-error) %>%
                  dplyr::mutate(iter = iteration))
    print(iteration)
  }
  return(list('selection_df' = iter_df, 'opt_weights' = iter_df %>% dplyr::filter(error == min(.data$error, na.rm = T))))
}
