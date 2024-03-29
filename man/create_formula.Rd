% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_formula.R
\name{create_formula}
\alias{create_formula}
\title{Create a sparse-group boosting formula}
\usage{
create_formula(
  alpha = 0.05,
  group_df = NULL,
  blearner = "bols",
  outcome_name = "y",
  group_name = "group_name",
  var_name = "var_name",
  intercept = FALSE
)
}
\arguments{
\item{alpha}{mixing parameter. For alpha = 0 only group baselearners and for
alpha = 1 only idividual baselearners are defined.}

\item{group_df}{data frame containing variable names with group structure}

\item{blearner}{type of baselearner}

\item{outcome_name}{name of dependent variable}

\item{group_name}{name of column in group_df indicating the group structure of the variables}

\item{var_name}{name of column in group_df containing the variable names
to be used as predictors}

\item{intercept}{logical. Should intercept be used?}
}
\value{
formula to be passed to mboost() yielding the sparse group boosting for a given value mixing parameter alpha.
}
\description{
Creates a mboost formula that allows to fit a sparse group boosting model based on
boosted Ridge Regression with mixing parameter alpha. The formula consists of a
group baselearner part with degrees of freedom
1-alpha and individual baselearners with degrees of freedom alpha.
}
\examples{
\dontrun{
library(mboost)
library(tidyverse)
set.seed(1)
df <- data.frame(
 x1 = rnorm(100),x2 = rnorm(100),x3 = rnorm(100),
 x4 = rnorm(100), x5 = runif(100)
 )
df <- df \%>\%
mutate_all(function(x){as.numeric(scale(x))})
df$y <- df$x1+df$x4+df$x5
group_df <- data.frame(
 group_name = c(1,1,1,2,2),
 var_name = c('x1','x2','x3','x4','x5')
)

sgb_formula <- create_formula(alpha = 0.3, group_df = group_df)
sgb_model <- mboost(formula = sgb_formula, data = df)
summary(sgb_model)
plot(sgb_model)}
}
