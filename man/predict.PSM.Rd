% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predict.PSM.R
\name{predict.PSM}
\alias{predict.PSM}
\title{Predict method for Partial Splines models}
\usage{
\method{predict}{PSM}(object, x, tx)
}
\arguments{
\item{object}{an object of class 'PSM'}

\item{x}{dataframe containing the values for each linear variable in the model with which to predict}

\item{tx}{a vector containing the values for the spline variable in the model with which to predict}
}
\value{
returns a vector containing predicted values
}
\description{
Predicted values based on a psm object
}
\examples{

x <- data.frame(x = rnorm(100))
tx <- rnorm(100)
eps <- rnorm(100)
y <- x[,1] + tx^4 + eps
psm <- PartialSplines(y, x, tx)
x.new <- data.frame(x = rnorm(5))
tx.new <- rnorm(5)
y.pred <- predict(psm, x.new, tx.new)
y.pred


}
