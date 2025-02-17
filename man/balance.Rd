% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/balance.R
\name{balance}
\alias{balance}
\title{Balances selection frequencies for unequal groups}
\usage{
balance(
  df = NULL,
  group_df = NULL,
  blearner = "bols",
  outcome_name = "y",
  group_name = "group_name",
  var_name = "var_name",
  n_reps = 3000,
  iterations = 15,
  nu = 0.5,
  red_fact = 0.9,
  min_weights = 0.01,
  max_weights = 0.99,
  intercept = TRUE,
  verbose = F
)
}
\arguments{
\item{df}{data.frame to be analyzed}

\item{group_df}{input data.frame containing variable names with group structure.
All variables in \code{df} to used in the analysis must be present in this data.frame.}

\item{blearner}{Type of baselearner. Default is \code{'bols'}.}

\item{outcome_name}{String indicating the name of dependent variable. Default is \code{"y"}}

\item{group_name}{Name of column in group_df indicating the group structure of the variables.
Default is \verb{"group_name}.}

\item{var_name}{Name of column in group_df containing the variable names
to be used as predictors. Default is \code{"var_name"}. should not contain categorical
variables with more than two categories, as they are then treated as a group only.}

\item{n_reps}{Number of samples to be drawn in each iteration}

\item{iterations}{Number of iterations performed in the algorithm. Default is \code{"20"}}

\item{nu}{Learning rate as the step size to move away from the current estimate.
Default is \code{0.5}}

\item{red_fact}{Factor by which the learning rate is reduced if the algorithm overshoots,
meaning the loss increases. Default is \code{0.9}}

\item{min_weights}{The minimum weight size to be used. Default is \code{0.01}}

\item{max_weights}{The maximum weight size to be used. Default is \code{0.99}}

\item{intercept}{Logical, should intercept be used?}

\item{verbose}{Logical, should iteration be printed?}
}
\value{
Character containing the formula to be passed to \code{\link[mboost:gamboost]{mboost::mboost()}}
yielding the sparse-group boosting for a given value mixing parameter \code{alpha}.
}
\description{
Returns optimal degrees of freedom for group boosting to achieve more balanced variables selection.
Groups should be defined through \code{group_df}. Each base_learner
}
\examples{
library(mboost)
library(dplyr)
set.seed(1)
df <- data.frame(
  x1 = rnorm(100), x2 = rnorm(100), x3 = rnorm(100),
  x4 = rnorm(100), x5 = runif(100)
)
df <- df \%>\%
  mutate_all(function(x) {
    as.numeric(scale(x))
  })
df$y <- df$x1 + df$x4 + df$x5
group_df <- data.frame(
  group_name = c(1, 1, 1, 2, 2),
  var_name = c("x1", "x2", "x3", "x4", "x5")
)

sgb_formula <- create_formula(alpha = 0.3, group_df = group_df)
sgb_model <- mboost(formula = sgb_formula, data = df)
summary(sgb_model)
}
