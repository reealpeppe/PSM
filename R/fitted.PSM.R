#' Extract Model Fitted Values
#'
#' @description extracts fitted values from a Partial Splines Model
#'
#' @param object an object of class 'PSM'
#'
#' @return returns a vector containing fitted values
#' @export
#'
#' @examples
#' x <- data.frame(x = rnorm(100))
#' tx <- rnorm(100)
#' eps <- rnorm(100)
#' y <- x[,1] + tx^4 + eps
#' psm <- PartialSplines(y, x, tx)
#' fitted(psm)

fitted.PSM <- function(object) {
  X <- object$X
  b <- object$beta
  d <- object$delta
  r <- object$r
  tx <- object$tx
  nodi <- object$detail$knots.basis
  Z <- bs(tx, knots = nodi, Boundary.knots = range(r))
  Z <- Z[, -ncol(Z)]
  y.hat <- X %*% b + Z %*% d
  y.hat
}
