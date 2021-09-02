#' Predict method for Partial Splines models
#'
#' @description Predicted values based on a psm object
#'
#' @param object an object of class 'PSM'
#' @param x dataframe containing the values for each linear variable in the model with which to predict
#' @param tx a vector containing the values for the spline variable in the model with which to predict
#'
#' @return returns a vector containing predicted values
#'
#' @export
#'
#' @examples
#'
#' x <- data.frame(x = rnorm(100))
#' tx <- rnorm(100)
#' eps <- rnorm(100)
#' y <- x[,1] + tx^4 + eps
#' psm <- PartialSplines(y, x, tx)
#' x.new <- data.frame(x = rnorm(5))
#' tx.new <- rnorm(5)
#' y.pred <- predict(psm, x.new, tx.new)
#' y.pred
#'
#'
predict.PSM <- function(object, x, tx) {
  if(!is.data.frame(x)) stop("x must be a data frame")
  d <- object$delta
  mod <- object$details$mod
  nodi <- object$details$knots.basis
  r <- object$r

  z <- bs(tx, knots = nodi, Boundary.knots = range(r))
  z <- z[, - ncol(z)]

  out <- predict.lm(mod, x) + z %*% d
  as.vector(out)

}
