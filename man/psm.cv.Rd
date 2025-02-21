% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/psm.cv.R
\name{psm.cv}
\alias{psm.cv}
\title{Cross validation for Partial Splines Model}
\usage{
psm.cv(
  y,
  x,
  tx,
  k.fold = 4,
  alpha.v = c(1e-04, 0.001, 0.01, 0.1),
  knots.v = c(0.1, 0.2, 0.3),
  plot.it = F,
  ...
)
}
\arguments{
\item{y}{response variable, must be a vector}

\item{x}{explanatory variables, must be a dataframe}

\item{tx}{spline variable, must be a vector}

\item{k.fold}{number of folds}

\item{alpha.v}{vector of alpha values to compare}

\item{knots.v}{vector of prop.knots values to compare}

\item{plot.it}{if TRUE, it plots a point for each combination of the parameters; the size of the point is related to the MSE on the test-set. The smaller point is circled in red}

\item{...}{further arguments passed to 'PartialSplines'}
}
\value{
returns a list containing the optimal values for alpha and prop.knots and a dataframe with the mse for each combination of the parameters
}
\description{
The aim of this function is to find the best combination of parameters 'prop.knots' and 'alpha' for Partial Splines function through a k-fold Cross Validation.
}
\examples{

x <- data.frame(x = rnorm(100))
tx <- rnorm(100)
eps <- rnorm(100)
y <- x[,1] + tx^4 + eps
cv <- psm.cv(y, x, tx)
alpha <- cv$alpha.opt
prop.knots <- cv$knots.opt

}
